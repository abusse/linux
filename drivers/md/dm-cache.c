/****************************************************************************
 *  dm-cache.c
 *  Device mapper target for block-level disk caching
 *
 *  Copyright (C) International Business Machines Corp., 2006
 *  Copyright (C) Ming Zhao, Florida International University, 2008-2009
 *  Copyright (C) Anselm Busse, Technische Universitaet Berlin, 2009-2010
 *
 *  Author: Anselm Busse (abusse@cs.tu-berlin.de)
 *  Contributors:
 *    Ming Zhao (Initial work on this module for SAN, iSCSI, and AoE use)
 *    Eric Van Hensbergen (metoring of this intern project at IBM in 2006)
 *    Reng Zeng (code update for 2.6.27-kenrel as part of his COP6611 course
 *    project at FIU in 2008)
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; under version 2 of the License.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 ****************************************************************************/

#include <asm/atomic.h>
#include <asm/checksum.h>
#include <linux/module.h>
#include <linux/init.h>
#include <linux/list.h>
#include <linux/blkdev.h>
#include <linux/bio.h>
#include <linux/slab.h>
#include <linux/hash.h>
#include <linux/spinlock.h>
#include <linux/workqueue.h>
#include <linux/pagemap.h>

#include "dm.h"
#include <linux/dm-io.h>
#include <linux/dm-kcopyd.h>

#if defined(CONFIG_DEBUG_FS)
	#include <linux/debugfs.h>
	struct dentry *debugfs;
#endif

#define WITH_TRIM 0

#define DMC_DEBUG 0
#define DMC_ADVANCED_INFO 1

#define DM_MSG_PREFIX "cache"
#define DMC_PREFIX "dm-cache: "

#if DMC_DEBUG
#define DPRINTK( s, arg... ) printk(DMC_PREFIX s "\n", ##arg)
#else
#define DPRINTK( s, arg... )
#endif

/* Default cache parameters */
#define DEFAULT_CACHE_SIZE	65536
#define DEFAULT_CACHE_ASSOC	1024
#define DEFAULT_BLOCK_SIZE	8
#define CONSECUTIVE_BLOCKS	512

/* Write policy */
#define WRITE_THROUGH 0
#define WRITE_BACK 1
#define WRITE_HYBRID 2
#define DEFAULT_WRITE_POLICY WRITE_THROUGH

#define DEFAULT_WB_ON_DTR 0
#define DEFAULT_AGE_MAX 4


/* Number of pages for I/O */
#define DMCACHE_COPY_PAGES 1024

/* States of a cache block */
#define INVALID		0
#define VALID		1	/* Valid */
#define DIRTY		2	/* Locally modified */
#define VRESERVED	3	/* Allocated but data not in place yet */
#define DRESERVED	4
#define WRITEBACK	5	/* In the process of write back */

/* list for queued BIOs waiting for running copy */
struct queued_bios {
	struct list_head list;
	struct cacheblock *block;
	struct bio_list *bios;
};

/* Cache block metadata structure */
struct cacheblock {
	sector_t block   : 56;	/* Sector number of the cached block */
	sector_t state   :  3;	/* State of a block */
	sector_t visited :  1;	/* Pending bios in bios_list */
	sector_t access  :  4;	/* counter for second chance algorithm */
};

/* Cach block with advanced metadata */
struct adv_cacheblock {
	struct cache_c *dmc;
	struct cacheblock *cacheblock;
};

/*
 * Cache context
 */
struct cache_c {
	struct dm_dev *src_dev;		/* Source device */
	struct dm_dev *cache_dev;	/* Cache device */
	struct dm_kcopyd_client *kcp_client; /* Kcopyd client for writing back data */

	struct cacheblock *cache;	/* Hash table for cache blocks */
	unsigned int *marker;
	sector_t size;			/* Cache size */
	unsigned int bits;		/* Cache size in bits */
	unsigned int assoc;		/* Cache associativity */
	unsigned int assoc_bits;	/* Cache associativity in bits */
	unsigned int assoc_mask;	/* */
	unsigned long hash_pattern;	/* pattern for block hashing */
	unsigned int sets;		/* Number of sets */
	unsigned int block_size;	/* Cache block size */
	unsigned int block_shift;	/* Cache block size in bits */
	unsigned int block_mask;	/* Cache block mask */
	unsigned int consecutive_shift;	/* Consecutive blocks size in bits */
	unsigned int write_policy;	/* Cache write policy */
	unsigned int wb_on_dtr;		/* Write back dirty pages on cache destruction */
	unsigned int age_max;		/* Maximum age for a cacheblock */
	unsigned int age_default;	/* Default age for a cacheblock */

	spinlock_t lock;		/* Lock to protect page allocation/deallocation */
	struct page_list *pages;	/* Pages for I/O */
	unsigned int nr_pages;		/* Number of pages */
	unsigned int nr_free_pages;	/* Number of free pages */
	wait_queue_head_t destroyq;	/* Wait queue for I/O completion */
	atomic_t nr_jobs;		/* Number of I/O jobs */
	struct dm_io_client *io_client;	/* Client memory pool*/

	/* quele and lock for delayed BIOs */
	spinlock_t queue_lock;		/* Lock to protect operations on the bio list */
	struct queued_bios queue;	/* List of pending bios */

	/* Stats */
	unsigned long reads;		/* Number of reads */
	unsigned long writes;		/* Number of writes */
	unsigned long cache_hits;	/* Number of cache hits */
	unsigned long replace;		/* Number of cache replacements */
	unsigned long writeback;	/* Number of replaced dirty blocks */
	unsigned long dirty;		/* Number of submitted dirty blocks */

#if defined(CONFIG_DEBUG_FS)
	struct dentry *debugfs;		/* Entry for debugfs options */
	struct debugfs_blob_wrapper blob_cache;
#endif
};

/* Structure for a kcached job */
struct kcached_job {
	struct list_head list;
	struct cache_c *dmc;
	struct bio *bio;	/* Original bio */
	struct dm_io_region src;
	struct dm_io_region dest;
	struct cacheblock *cacheblock;
	int rw;
	/*
	 * When the original bio is not aligned with cache blocks,
	 * we need extra bvecs and pages for padding.
	 */
	struct bio_vec *bvec;
	unsigned int nr_pages;
	struct page_list *pages;
};

/****************************************************************************
 *  Wrapper functions for using the new dm_io API
 ****************************************************************************/
static int dm_io_sync_vm(unsigned int num_regions, struct dm_io_region
			*where, int rw, void *data, unsigned long *error_bits,
			struct cache_c *dmc)
{
	struct dm_io_request iorq;

	iorq.bi_rw = rw;
	iorq.mem.type = DM_IO_VMA;
	iorq.mem.ptr.vma = data;
	iorq.notify.fn = NULL;
	iorq.client = dmc->io_client;

	return dm_io(&iorq, num_regions, where, error_bits);
}

static int dm_io_async_bvec(unsigned int num_regions, struct dm_io_region
	*where, int rw, struct bio_vec *bvec, io_notify_fn fn, void *context)
{
	struct kcached_job *job = (struct kcached_job *)context;
	struct cache_c *dmc = job->dmc;
	struct dm_io_request iorq;

	iorq.bi_rw = (rw | (1 << BIO_RW_SYNCIO));
	iorq.mem.type = DM_IO_BVEC;
	iorq.mem.ptr.bvec = bvec;
	iorq.notify.fn = fn;
	iorq.notify.context = context;
	iorq.client = dmc->io_client;

	return dm_io(&iorq, num_regions, where, NULL);
}


/****************************************************************************
 *  Functions and data structures for implementing a kcached to handle async
 *  I/O. Code for page and queue handling is borrowed from kcopyd.c.
 ****************************************************************************/

/*
 * Functions for handling pages used by async I/O.
 * The data asked by a bio request may not be aligned with cache blocks, in
 * which case additional pages are required for the request that is forwarded
 * to the server. A pool of pages are reserved for this purpose.
 */

static struct page_list *alloc_pl(void)
{
	struct page_list *pl;

	pl = kmalloc(sizeof(*pl), GFP_KERNEL);
	if (!pl)
		return NULL;

	pl->page = alloc_page(GFP_KERNEL);
	if (!pl->page) {
		kfree(pl);
		return NULL;
	}

	return pl;
}

static void free_pl(struct page_list *pl)
{
	__free_page(pl->page);
	kfree(pl);
}

static void drop_pages(struct page_list *pl)
{
	struct page_list *next;

	while (pl) {
		next = pl->next;
		free_pl(pl);
		pl = next;
	}
}

static int kcached_get_pages(struct cache_c *dmc, unsigned int nr,
				struct page_list **pages)
{
	struct page_list *pl;

	spin_lock(&dmc->lock);
	if (dmc->nr_free_pages < nr) {
		DPRINTK("%s: No free pages: %u<%u", __func__,
			dmc->nr_free_pages, nr);
		spin_unlock(&dmc->lock);
		return -ENOMEM;
	}

	dmc->nr_free_pages -= nr;
	for (*pages = pl = dmc->pages; --nr; pl = pl->next)
		;

	dmc->pages = pl->next;
	pl->next = NULL;

	spin_unlock(&dmc->lock);

	return 0;
}

static void kcached_put_pages(struct cache_c *dmc, struct page_list *pl)
{
	struct page_list *cursor;

	spin_lock(&dmc->lock);
	for (cursor = pl; cursor->next; cursor = cursor->next)
		dmc->nr_free_pages++;

	dmc->nr_free_pages++;
	cursor->next = dmc->pages;
	dmc->pages = pl;

	spin_unlock(&dmc->lock);
}

static int alloc_bio_pages(struct cache_c *dmc, unsigned int nr)
{
	unsigned int i;
	struct page_list *pl = NULL, *next;

	for (i = 0; i < nr; i++) {
		next = alloc_pl();
		if (!next) {
			if (pl)
				drop_pages(pl);
			return -ENOMEM;
		}
		next->next = pl;
		pl = next;
	}

	kcached_put_pages(dmc, pl);
	dmc->nr_pages += nr;

	return 0;
}

static void free_bio_pages(struct cache_c *dmc)
{
	BUG_ON(dmc->nr_free_pages != dmc->nr_pages);
	drop_pages(dmc->pages);
	dmc->pages = NULL;
	dmc->nr_free_pages = dmc->nr_pages = 0;
}

static struct workqueue_struct *_kcached_wq;
static struct work_struct _kcached_work;

static inline void wake(void)
{
	queue_work(_kcached_wq, &_kcached_work);
}

#define MIN_JOBS 1024

static struct kmem_cache *_job_cache;
static mempool_t *_job_pool;

static DEFINE_SPINLOCK(_job_lock);

static LIST_HEAD(_complete_jobs);
static LIST_HEAD(_io_jobs);
static LIST_HEAD(_pages_jobs);

#if DMC_ADVANCED_INFO
static void cache_debug_table_set(struct cache_c *dmc, unsigned long set)
{
	int j;
	unsigned long index;
	struct cacheblock *cache = dmc->cache;

	index = set * dmc->assoc;
	DMINFO("Set %lu", set);
	DMINFO("   Marker: %lu", (unsigned long) dmc->marker[set]);
	for (j = 0; j < dmc->assoc; j++) {
		DMINFO("   Cacheblock %u (%lu)", j, index+j);
		DMINFO("      Status:  %u", cache[index+j].state);
		DMINFO("      Counter: %u", cache[index+j].access);
		DMINFO("      Block:   %llu", (unsigned long long) cache[index+j].block);
	}
	DMINFO("");
}

static void cache_debug_print_cache(struct cache_c *dmc)
{
	DMINFO("size:         %lu sectors (%u bits)", dmc->size, dmc->bits);
	DMINFO("assoc:        %u (%u bits)", dmc->assoc, dmc->assoc_bits);
	DMINFO("sets:         %u", dmc->sets);
	DMINFO("assoc_mask    %u", dmc->assoc_mask);
	DMINFO("hash_pattern: %lu", dmc->hash_pattern);
	DMINFO("block size:   %u", dmc->block_size);
	DMINFO("nr of pages:  %u (%u free)", dmc->nr_pages, dmc->nr_free_pages);
	DMINFO("age_max:      %u", dmc->age_max);
	DMINFO("age_default:  %u", dmc->age_default);
	DMINFO("reads:        %lu", dmc->reads);
	DMINFO("writes:       %lu", dmc->writes);
	DMINFO("hits:         %lu (hitrate %lu)", dmc->cache_hits, (dmc->reads + dmc->writes) > 0 ? dmc->cache_hits * 100 / (dmc->reads + dmc->writes) : 0);
	DMINFO("replace:      %lu", dmc->replace);
	DMINFO("writeback:    %lu", dmc->writeback);
	DMINFO("dirty:        %lu", dmc->dirty);
}

#endif

static struct bio *cache_bio_list_remove(struct cache_c *dmc,
	struct cacheblock *cacheblock)
{
	struct queued_bios *list, *tmp;
	struct bio *bios;
	struct list_head *pos;

	if (cacheblock->visited == 0)
		return NULL;

	list = &dmc->queue;

	list_for_each(pos, &(dmc->queue.list)) {
		tmp = list_entry(pos, struct queued_bios, list);
		if (tmp->block == cacheblock) {
			list_del(&(tmp->list));
			bios = bio_list_get(tmp->bios);
			kfree(tmp->bios);
			kfree(tmp);

			cacheblock->visited = 0;

			return bios;
		}
	}

	return NULL;
}

static struct bio_list *cache_bio_list_get(struct cache_c *dmc,
	struct cacheblock *cacheblock)
{
	struct queued_bios *tmp;
	struct list_head *pos;

	if (cacheblock->visited == 0)
		return NULL;

	list_for_each(pos, &(dmc->queue.list)) {
		tmp = list_entry(pos, struct queued_bios, list);
		if (tmp->block == cacheblock)
			return tmp->bios;
	}

	return NULL;
}

static void cache_bio_list_add(struct cache_c *dmc, struct bio *bio,
	struct cacheblock *cacheblock)
{
	struct bio_list *list;
	struct queued_bios *entry;

	list = cache_bio_list_get(dmc, cacheblock);

	if (list == NULL) {
		list = kmalloc(sizeof(struct bio_list), GFP_KERNEL);
		if (list == NULL)
			DMERR("%s: Memory allocation failed", __func__);

		entry = kmalloc(sizeof(struct queued_bios), GFP_KERNEL);
		if (entry == NULL)
			DMERR("%s: Memory allocation failed", __func__);

		bio_list_init(list);

		entry->bios = list;
		entry->block = cacheblock;
		list_add(&(entry->list), &(dmc->queue.list));

		cacheblock->visited = 1;
	}

	bio_list_add(list, bio);
}



static int jobs_init(void)
{
	_job_cache = kmem_cache_create("kcached-jobs",
					sizeof(struct kcached_job),
					__alignof__(struct kcached_job),
					0, NULL);
	if (!_job_cache)
		return -ENOMEM;

	_job_pool = mempool_create(MIN_JOBS, mempool_alloc_slab,
					mempool_free_slab, _job_cache);
	if (!_job_pool) {
		kmem_cache_destroy(_job_cache);
		return -ENOMEM;
	}

	return 0;
}

static void jobs_exit(void)
{
	BUG_ON(!list_empty(&_complete_jobs));
	BUG_ON(!list_empty(&_io_jobs));
	BUG_ON(!list_empty(&_pages_jobs));

	mempool_destroy(_job_pool);
	kmem_cache_destroy(_job_cache);
	_job_pool = NULL;
	_job_cache = NULL;
}

/*
 * Functions to push and pop a job onto the head of a given job list.
 */
static inline struct kcached_job *pop(struct list_head *jobs)
{
	struct kcached_job *job = NULL;
	unsigned long flags;

	spin_lock_irqsave(&_job_lock, flags);

	if (!list_empty(jobs)) {
		job = list_entry(jobs->next, struct kcached_job, list);
		list_del(&job->list);
	}
	spin_unlock_irqrestore(&_job_lock, flags);

	return job;
}

static inline void push(struct list_head *jobs, struct kcached_job *job)
{
	unsigned long flags;

	spin_lock_irqsave(&_job_lock, flags);
	list_add_tail(&job->list, jobs);
	spin_unlock_irqrestore(&_job_lock, flags);
}


/****************************************************************************
 * Functions for asynchronously fetching data from source device and storing
 * data in cache device. Because the requested data may not align with the
 * cache blocks, extra handling is required to pad a block request and extract
 * the requested data from the results.
 ****************************************************************************/

static void io_callback(unsigned long error, void *context)
{
	struct kcached_job *job = (struct kcached_job *) context;

	if (error) {
		/* TODO: Erorr handling */
		DMERR("%s: Error handling not implemented!!!", __func__);
		return;
	}

	if (job->rw == READ) {
		job->rw = WRITE;
		push(&_io_jobs, job);
	} else
		push(&_complete_jobs, job);
	wake();
}

/*
 * Fetch data from the source device asynchronously.
 * For a READ bio, if a cache block is larger than the requested data, then
 * additional data are prefetched. Larger cache block size enables more
 * aggressive read prefetching, which is useful for read-mostly usage.
 * For a WRITE bio, if a cache block is larger than the requested data, the
 * entire block needs to be fetched, and larger block size incurs more overhead.
 * In scenarios where writes are frequent, 4KB is a good cache block size.
 */
static int do_fetch(struct kcached_job *job)
{
	int r = 0, i, j;
	struct bio *bio = job->bio;
	struct cache_c *dmc = job->dmc;
	unsigned int offset, head, tail, remaining, nr_vecs, idx = 0;
	struct bio_vec *bvec;
	struct page_list *pl;

	offset = (unsigned int) (bio->bi_sector & dmc->block_mask);
	head = to_bytes(offset);
	tail = to_bytes(dmc->block_size) - bio->bi_size - head;

	DPRINTK("__func__: %llu(%llu->%llu,%llu), head:%u,tail:%u", __func__,
		bio->bi_sector, job->src.sector, job->dest.sector,
		job->src.count, head, tail);

	if (bio_data_dir(bio) == READ) {
		/* The original request is a READ */
		if (job->nr_pages == 0) {
			/* The request is aligned to cache block */
			r = dm_io_async_bvec(1, &job->src, READ,
						bio->bi_io_vec + bio->bi_idx,
						io_callback, job);
			return r;
		}

		nr_vecs = bio->bi_vcnt - bio->bi_idx + job->nr_pages;
		bvec = kmalloc(nr_vecs * sizeof(*bvec), GFP_NOIO);
		if (!bvec) {
			DMERR("%s: No memory", __func__);
			return 1;
		}

		pl = job->pages;
		i = 0;
		while (head) {
			bvec[i].bv_len = min(head, (unsigned int)PAGE_SIZE);
			bvec[i].bv_offset = 0;
			bvec[i].bv_page = pl->page;
			head -= bvec[i].bv_len;
			pl = pl->next;
			i++;
		}

		remaining = bio->bi_size;
		j = bio->bi_idx;
		while (remaining) {
			bvec[i] = bio->bi_io_vec[j];
			remaining -= bvec[i].bv_len;
			i++; j++;
		}

		while (tail) {
			bvec[i].bv_len = min(tail, (unsigned int)PAGE_SIZE);
			bvec[i].bv_offset = 0;
			bvec[i].bv_page = pl->page;
			tail -= bvec[i].bv_len;
			pl = pl->next;
			i++;
		}

		job->bvec = bvec;
		r = dm_io_async_bvec(1, &job->src, READ, job->bvec, io_callback, job);
		return r;
	} else {
		/* The original request is a WRITE */
		pl = job->pages;

		if (head && tail) {
			/* Special case */
			bvec = kmalloc(job->nr_pages * sizeof(*bvec), GFP_KERNEL);
			if (!bvec) {
				DMERR("%s: No memory", __func__);
				return 1;
			}
			for (i = 0; i < job->nr_pages; i++) {
				bvec[i].bv_len = PAGE_SIZE;
				bvec[i].bv_offset = 0;
				bvec[i].bv_page = pl->page;
				pl = pl->next;
			}
			job->bvec = bvec;
			r = dm_io_async_bvec(1, &job->src, READ, job->bvec,
						io_callback, job);
			return r;
		}

		bvec = kmalloc((job->nr_pages + bio->bi_vcnt - bio->bi_idx)
				* sizeof(*bvec), GFP_KERNEL);
		if (!bvec) {
			DMERR("%s: No memory", __func__);
			return 1;
		}

		i = 0;
		while (head) {
			bvec[i].bv_len = min(head, (unsigned int)PAGE_SIZE);
			bvec[i].bv_offset = 0;
			bvec[i].bv_page = pl->page;
			head -= bvec[i].bv_len;
			pl = pl->next;
			i++;
		}

		remaining = bio->bi_size;
		j = bio->bi_idx;
		while (remaining) {
			bvec[i] = bio->bi_io_vec[j];
			remaining -= bvec[i].bv_len;
			i++; j++;
		}

		if (tail) {
			idx = i;
			bvec[i].bv_offset = (to_bytes(offset) + bio->bi_size) &
						(PAGE_SIZE - 1);
			bvec[i].bv_len = PAGE_SIZE - bvec[i].bv_offset;
			bvec[i].bv_page = pl->page;
			tail -= bvec[i].bv_len;
			pl = pl->next; i++;
			while (tail) {
				bvec[i].bv_len = PAGE_SIZE;
				bvec[i].bv_offset = 0;
				bvec[i].bv_page = pl->page;
				tail -= bvec[i].bv_len;
				pl = pl->next; i++;
			}
		}

		job->bvec = bvec;
		r = dm_io_async_bvec(1, &job->src, READ, job->bvec + idx,
					io_callback, job);

		return r;
	}
}

/*
 * Store data to the cache source device asynchronously.
 * For a READ bio request, the data fetched from the source device are returned
 * to kernel and stored in cache at the same time.
 * For a WRITE bio request, the data are written to the cache and source device
 * at the same time.
 */
static int do_store(struct kcached_job *job)
{
	int i, j, r = 0;
	struct bio *bio = job->bio, *clone;
	struct cache_c *dmc = job->dmc;
	unsigned int offset, head, tail, remaining, nr_vecs;
	struct bio_vec *bvec;

	offset = (unsigned int) (bio->bi_sector & dmc->block_mask);
	head = to_bytes(offset);
	tail = to_bytes(dmc->block_size) - bio->bi_size - head;

	DPRINTK("%s: %llu(%llu->%llu,%llu), head:%u,tail:%u", __func__,
		bio->bi_sector, job->src.sector, job->dest.sector,
		job->src.count, head, tail);

	/* A READ is acknowledged as soon as the requested data is fetched, and
	 * does not have to wait for it being stored in cache. The bio is cloned
	 * so that the original one can be ended here. But to avoid copying
	 * pages, we reuse the pages allocated for the original bio, and mark
	 * each of them to prevent the pages being freed before the cache
	 * insertion is completed.
	 */
	if (bio_data_dir(bio) == READ) {
		clone = bio_clone(bio, GFP_NOIO);
		for (i = bio->bi_idx; i < bio->bi_vcnt; i++)
			get_page(bio->bi_io_vec[i].bv_page);

		DPRINTK("bio ended for %llu:%u", bio->bi_sector, bio->bi_size);
		bio_endio(bio, 0);
		bio = clone;
		job->bio = clone;
	}

	if (job->nr_pages == 0) /* Original request is aligned with cache blocks */
		r = dm_io_async_bvec(1, &job->dest, WRITE, bio->bi_io_vec + bio->bi_idx,
			io_callback, job);
	else {
		if (bio_data_dir(bio) == WRITE && head > 0 && tail > 0) {
			DPRINTK("Special case: %lu %u %u", bio_data_dir(bio), head, tail);
			nr_vecs = job->nr_pages + bio->bi_vcnt - bio->bi_idx;
			if (offset && (offset + bio->bi_size < PAGE_SIZE))
				nr_vecs++;

			DPRINTK("Create %u new vecs", nr_vecs);
			bvec = kmalloc(nr_vecs * sizeof(*bvec), GFP_KERNEL);
			if (!bvec) {
				DMERR("%s: No memory", __func__);
				return 1;
			}

			i = 0;
			while (head) {
				bvec[i].bv_len = min(head, job->bvec[i].bv_len);
				bvec[i].bv_offset = 0;
				bvec[i].bv_page = job->bvec[i].bv_page;
				head -= bvec[i].bv_len;
				i++;
			}
			remaining = bio->bi_size;
			j = bio->bi_idx;
			while (remaining) {
				bvec[i] = bio->bi_io_vec[j];
				remaining -= bvec[i].bv_len;
				i++; j++;
			}
			j = (to_bytes(offset) + bio->bi_size) / PAGE_SIZE;
			bvec[i].bv_offset = (to_bytes(offset) + bio->bi_size) -
						j * PAGE_SIZE;
			bvec[i].bv_len = PAGE_SIZE - bvec[i].bv_offset;
			bvec[i].bv_page = job->bvec[j].bv_page;
			tail -= bvec[i].bv_len;
			i++; j++;
			while (tail) {
				bvec[i] = job->bvec[j];
				tail -= bvec[i].bv_len;
				i++; j++;
			}
			kfree(job->bvec);
			job->bvec = bvec;
		}

		r = dm_io_async_bvec(1, &job->dest, WRITE, job->bvec, io_callback, job);
	}

	return r;
}

static int do_io(struct kcached_job *job)
{
	int r = 0;

	if (job->rw == READ) { /* Read from source device */
		r = do_fetch(job);
	} else { /* Write to cache device */
		r = do_store(job);
	}

	return r;
}

static int do_pages(struct kcached_job *job)
{
	int r = 0;

	r = kcached_get_pages(job->dmc, job->nr_pages, &job->pages);

	if (r == -ENOMEM) /* can't complete now */
		return 1;

	/* this job is ready for io */
	push(&_io_jobs, job);
	return 0;
}

/*
 * Flush the bios that are waiting for this cache insertion or write back.
 */
static void flush_bios(struct cache_c *dmc, struct cacheblock *cacheblock)
{
	struct bio *bio;
	struct bio *n;

	spin_lock(&dmc->queue_lock);
	bio = cache_bio_list_remove(dmc, cacheblock);

	if (cacheblock->state == DRESERVED)
		cacheblock->state = DIRTY;
	else
		cacheblock->state = VALID;

	spin_unlock(&dmc->queue_lock);

	while (bio) {
		n = bio->bi_next;
		bio->bi_next = NULL;
		DPRINTK("Flush bio: %llu->%llu (%u bytes)",
			cacheblock->block, bio->bi_sector, bio->bi_size);
		if (bio_data_dir(bio) == WRITE)
			cacheblock->state = DIRTY;

		generic_make_request(bio);
		bio = n;
	}
}

static int do_complete(struct kcached_job *job)
{
	int i, r = 0;
	struct bio *bio = job->bio;

	DPRINTK("%s: %llu", __func__ bio->bi_sector);

	if (bio_data_dir(bio) == READ) {
		for (i = bio->bi_idx; i < bio->bi_vcnt; i++)
			put_page(bio->bi_io_vec[i].bv_page);

		bio_put(bio);
	} else
		bio_endio(bio, 0);

	if (job->nr_pages > 0) {
		kfree(job->bvec);
		kcached_put_pages(job->dmc, job->pages);
	}

	flush_bios(job->dmc, job->cacheblock);
	mempool_free(job, _job_pool);

	if (atomic_dec_and_test(&job->dmc->nr_jobs))
		wake_up(&job->dmc->destroyq);

	return r;
}

/*
 * Run through a list for as long as possible.  Returns the count
 * of successful jobs.
 */
static int process_jobs(struct list_head *jobs,
	int (*fn)(struct kcached_job *))
{
	struct kcached_job *job;
	int r, count = 0;

	while ((job = pop(jobs))) {
		r = fn(job);

		if (r < 0) {
			/* error this rogue job */
			DMERR("%s: Job processing error", __func__);
		}

		if (r > 0) {
			/*
			 * We couldn't service this job ATM, so
			 * push this job back onto the list.
			 */
			push(jobs, job);
			break;
		}

		count++;
	}

	return count;
}

static void do_work(struct work_struct *ignored)
{
	process_jobs(&_complete_jobs, do_complete);
	process_jobs(&_pages_jobs, do_pages);
	process_jobs(&_io_jobs, do_io);
}

static void queue_job(struct kcached_job *job)
{
	atomic_inc(&job->dmc->nr_jobs);
	if (job->nr_pages > 0) /* Request pages */
		push(&_pages_jobs, job);
	else /* Go ahead to do I/O */
		push(&_io_jobs, job);
	wake();
}

static int kcached_init(struct cache_c *dmc)
{
	int r;

	spin_lock_init(&dmc->lock);
	spin_lock_init(&dmc->queue_lock);
	dmc->pages = NULL;
	dmc->nr_pages = dmc->nr_free_pages = 0;
	r = alloc_bio_pages(dmc, DMCACHE_COPY_PAGES);
	if (r) {
		DMERR("%s: Could not allocate bio pages", __func__);
		return r;
	}

	init_waitqueue_head(&dmc->destroyq);
	atomic_set(&dmc->nr_jobs, 0);

	return 0;
}

void kcached_client_destroy(struct cache_c *dmc)
{
	/* Wait for completion of all jobs submitted by this client. */
	wait_event(dmc->destroyq, !atomic_read(&dmc->nr_jobs));

	free_bio_pages(dmc);
}


/****************************************************************************
 * Functions for writing back dirty blocks.
 * We leverage kcopyd to write back dirty blocks because it is convenient to
 * use and it is not reasonble to reimplement the same function here. But we
 * need to reserve pages for both kcached and kcopyd. TODO: dynamically change
 * the number of reserved pages.
 ****************************************************************************/

static void copy_callback(int read_err, unsigned int write_err, void *context)
{
	struct adv_cacheblock *cacheblock = (struct adv_cacheblock *) context;

	cacheblock->cacheblock->state = VALID;

	flush_bios(cacheblock->dmc, cacheblock->cacheblock);

	vfree(context);
}

static void copy_block(struct cache_c *dmc, struct dm_io_region src,
	struct dm_io_region dest, struct adv_cacheblock *cacheblock)
{
	DPRINTK("Copying: %llu:%llu->%llu:%llu",
			src.sector, src.count * 512, dest.sector, dest.count * 512);
	dm_kcopyd_copy(dmc->kcp_client, &src, 1, &dest, 0, \
			(dm_kcopyd_notify_fn) copy_callback, (void *)cacheblock);
}

static void write_back(struct cache_c *dmc, sector_t index, unsigned int length)
{
	struct dm_io_region src, dest;
	struct adv_cacheblock *cacheblock;
	unsigned int i;

	cacheblock = vmalloc(sizeof(struct adv_cacheblock));
	if (!cacheblock)
		DMERR("%s: could not get memory for writeback", __func__);

	cacheblock->dmc = dmc;
	cacheblock->cacheblock = &(dmc->cache[index]);

	DPRINTK("Write back block %llu(%llu, %u)", index, cacheblock->cacheblock->block, length);
	src.bdev = dmc->cache_dev->bdev;
	src.sector = index << dmc->block_shift;
	src.count = dmc->block_size * length;
	dest.bdev = dmc->src_dev->bdev;
	dest.sector = cacheblock->cacheblock->block;
	dest.count = dmc->block_size * length;

	for (i = 0; i < length; i++)
		dmc->cache[index+i].state = WRITEBACK;

	copy_block(dmc, src, dest, cacheblock);
}


/****************************************************************************
 *  Functions for implementing the various cache operations.
 ****************************************************************************/

/*
 * Map a block from the source device to a block in the cache device.
 */
static unsigned long hash_block(struct cache_c *dmc, sector_t block)
{
	unsigned long set_number, value;

	value = (unsigned long)(block >> dmc->block_shift);
	set_number = value & dmc->hash_pattern;

	return set_number;
}

/*
 * Lookup a block in the cache.
 *
 * Return value:
 *  1: cache hit (cache_block stores the index of the matched block)
 *  0: cache miss but frame is allocated for insertion; cache_block stores the
 *     frame's index:
 *      If there are empty frames, then the first encounted is used.
 *      If there are clean frames, then the LRU clean block is replaced.
 *  2: cache miss and frame is not allocated; cache_block stores the LRU dirty
 *     block's index:
 *      This happens when the entire set is dirty.
 * -1: cache miss and no room for insertion:
 *      This happens when the entire set in transition modes (RESERVED or
 *      WRITEBACK).
 *
 */
static int cache_lookup(struct cache_c *dmc, sector_t block, sector_t *cache_block)
{
	unsigned long set_number = hash_block(dmc, block);
	sector_t index;
	int i = 0, res = -1;
	unsigned int cache_assoc = dmc->assoc;
	struct cacheblock *cache = dmc->cache;
	int invalid = -1;

	index = set_number * cache_assoc;
	for (i = 0; i < cache_assoc; i++, index++) {
		if (cache[index].state != INVALID) {
			if (cache[index].block == block) {
				if (cache[index].access < dmc->age_max)
					cache[index].access++;

				*cache_block = index;
				res = 1;
				break;
			}
		} else {
			if (invalid == -1)
				invalid = i;
		}
	}

	if (res != 1) {
		/* Cache miss */
		if (invalid != -1) {
			/* Choose the first empty frame */
			*cache_block = (set_number * cache_assoc) + invalid;
			res = 0;
		} else {
			/* find a block for replacement */
			index = set_number * cache_assoc;
			for (i = 0; i < cache_assoc && res != 0; i++) {
				if (cache[index + dmc->marker[set_number]].access <= 0) {
					/* block has exceeded its lifetime */
					switch (cache[index + dmc->marker[set_number]].state) {
					case INVALID:
					case VALID:
						*cache_block = index + dmc->marker[set_number];
						res = 0;
						break;
					case DIRTY:
						write_back(dmc, index + dmc->marker[set_number], 1);
					case VRESERVED:
					case DRESERVED:
					case WRITEBACK:
						break;
					}
				} else {
					cache[index + dmc->marker[set_number]].access--;
					dmc->marker[set_number] = (dmc->marker[set_number] + 1) & dmc->assoc_mask;
				}
			}
		}
	}

	if (-1 == res)
		DPRINTK("%s: Block %llu(%lu):%s",
			__func__, block, set_number, "NO ROOM");
	else
		DPRINTK("%s: Block %llu(%lu):%llu(%s)",
			 __func__, block, set_number, *cache_block,
			res == 1 ? "HIT" : (res == 0 ? "MISS" : "WB NEEDED"));

	return res;
}

/*
 * Insert a block into the cache (in the frame specified by cache_block).
 */
static int cache_insert(struct cache_c *dmc, sector_t block,
			sector_t cache_block, int state)
{
	struct cacheblock *cache = dmc->cache;

	if (cache[cache_block].state == DIRTY || cache[cache_block].state == WRITEBACK)
		DMERR("%s: overwriting dirty block", __func__);

	/*
	 * Mark the block as RESERVED because although it is allocated,
	 * the data are not in place until kcopyd finishes its job.
	 */
	cache[cache_block].block = block;
	cache[cache_block].state = state;
	cache[cache_block].access = dmc->age_default;

	return 1;
}

/*
 * Invalidate a block (specified by cache_block) in the cache.
 */
static void cache_invalidate(struct cache_c *dmc, sector_t cache_block)
{
	struct cacheblock *cache = dmc->cache;

	DPRINTK("Cache invalidate: Block %llu(%llu)",
		 cache_block, cache[cache_block].block);
	if (cache[cache_block].state == DIRTY || cache[cache_block].state == WRITEBACK)
		DMERR("%s: block is dirty or not written back", __func__);

	cache[cache_block].state = INVALID;
}

/*
 * Handle a cache hit:
 *  For READ, serve the request from cache is the block is ready; otherwise,
 *  queue the request for later processing.
 *  For write, invalidate the cache block if write-through. If write-back,
 *  serve the request from cache if the block is ready, or queue the request
 *  for later processing if otherwise.
 */
static int cache_hit(struct cache_c *dmc, struct bio *bio, sector_t cache_block)
{
	unsigned int offset = (unsigned int)(bio->bi_sector & dmc->block_mask);
	struct cacheblock *cache = dmc->cache;

	dmc->cache_hits++;

	if (bio_data_dir(bio) == READ) { /* READ hit */
		bio->bi_bdev = dmc->cache_dev->bdev;
		bio->bi_sector = (cache_block << dmc->block_shift)  + offset;

		spin_lock(&dmc->queue_lock);

		/* TODO check INVALID */
		if (cache[cache_block].state != VRESERVED && cache[cache_block].state != DRESERVED) {
			/* Valid cache block */
			spin_unlock(&dmc->queue_lock);
			return 1;
		}

		/* Cache block is not ready yet */
		DPRINTK("Add to bio list %s(%llu)",
				dmc->cache_dev->name, bio->bi_sector);
		cache_bio_list_add(dmc, bio, &cache[cache_block]);

		spin_unlock(&dmc->queue_lock);
		return 0;
	} else {
		/* WRITE hit */
		if (dmc->write_policy == WRITE_THROUGH) {
			/* Invalidate cached data */
			cache_invalidate(dmc, cache_block);
#if WITH_TRIM
			blkdev_issue_discard(dmc->cache_dev->bdev,
				(cache_block << dmc->block_shift) + offset,
				dmc->block_size, GFP_KERNEL,
				DISCARD_FL_BARRIER);
#endif

			bio->bi_bdev = dmc->src_dev->bdev;
			return 1;
		}

		spin_lock(&dmc->queue_lock);

		/* In the middle of write back */
		/* TODO move write to cache */
		if (cache[cache_block].state == WRITEBACK) {
			/* Delay this write until the block is written back */
			//bio->bi_bdev = dmc->src_dev->bdev;
			bio->bi_bdev = dmc->cache_dev->bdev;
			bio->bi_sector = (cache_block << dmc->block_shift) + offset;
			DPRINTK("Add to bio list %s(%llu)",
					dmc->src_dev->name, bio->bi_sector);
			cache_bio_list_add(dmc, bio, &cache[cache_block]);
			spin_unlock(&dmc->queue_lock);
			return 0;
		}

		/* Cache block not ready yet */
		if (cache[cache_block].state == VRESERVED || cache[cache_block].state == DRESERVED) {
			bio->bi_bdev = dmc->cache_dev->bdev;
			bio->bi_sector = (cache_block << dmc->block_shift) + offset;
			DPRINTK("Add to bio list %s(%llu)",
					dmc->cache_dev->name, bio->bi_sector);
			cache_bio_list_add(dmc, bio, &cache[cache_block]);
			spin_unlock(&dmc->queue_lock);
			return 0;
		}

		/* Serve the request from cache */
		cache[cache_block].state = DIRTY;
		bio->bi_bdev = dmc->cache_dev->bdev;
		bio->bi_sector = (cache_block << dmc->block_shift) + offset;

		spin_unlock(&dmc->queue_lock);
		return 1;
	}
}

static struct kcached_job *new_kcached_job(struct cache_c *dmc, struct bio *bio,
			sector_t request_block, sector_t cache_block)
{
	struct dm_io_region src, dest;
	struct kcached_job *job;

	src.bdev = dmc->src_dev->bdev;
	src.sector = request_block;
	src.count = dmc->block_size;
	dest.bdev = dmc->cache_dev->bdev;
	dest.sector = cache_block << dmc->block_shift;
	dest.count = src.count;

	job = mempool_alloc(_job_pool, GFP_NOIO);
	job->dmc = dmc;
	job->bio = bio;
	job->src = src;
	job->dest = dest;
	job->cacheblock = &dmc->cache[cache_block];

	return job;
}

/*
 * Handle a read cache miss:
 *  Update the metadata; fetch the necessary block from source device;
 *  store data to cache device.
 */
static int cache_read_miss(struct cache_c *dmc, struct bio *bio,
				sector_t cache_block)
{
	struct cacheblock *cache = dmc->cache;
	unsigned int offset, head, tail;
	struct kcached_job *job;
	sector_t request_block, left;

	offset = (unsigned int)(bio->bi_sector & dmc->block_mask);
	request_block = bio->bi_sector - offset;

	if (cache[cache_block].state == VALID) {
		DPRINTK("Replacing %llu->%llu",
			cache[cache_block].block, request_block);
		dmc->replace++;
	} else {
		DPRINTK("Insert block %llu at empty frame %llu",
			request_block, cache_block);
	}

	cache_insert(dmc, request_block, cache_block, VRESERVED); /* Update metadata first */

	job = new_kcached_job(dmc, bio, request_block, cache_block);

	head = to_bytes(offset);

	left = (dmc->src_dev->bdev->bd_inode->i_size>>9) - request_block;
	if (left < dmc->block_size) {
		tail = to_bytes(left) - bio->bi_size - head;
		job->src.count = left;
		job->dest.count = left;
	} else
		tail = to_bytes(dmc->block_size) - bio->bi_size - head;

	/* Requested block is aligned with a cache block */
	if (0 == head && 0 == tail)
		job->nr_pages = 0;
	else /* Need new pages to store extra data */
		job->nr_pages = dm_div_up(head, PAGE_SIZE) + dm_div_up(tail, PAGE_SIZE);

	job->rw = READ; /* Fetch data from the source device */

	DPRINTK("Queue job for %llu (need %u pages)",
		bio->bi_sector, job->nr_pages);
	queue_job(job);

	return 0;
}

/*
 * Handle a write cache miss:
 *  If write-through, forward the request to source device.
 *  If write-back, update the metadata; fetch the necessary block from source
 *  device; write to cache device.
 */
static int cache_write_miss(struct cache_c *dmc, struct bio *bio, sector_t cache_block)
{
	struct cacheblock *cache = dmc->cache;
	unsigned int offset, head, tail;
	struct kcached_job *job;
	sector_t request_block, left;

	if (dmc->write_policy == WRITE_THROUGH || dmc->write_policy == WRITE_HYBRID) {
		/* Forward request to source */
		bio->bi_bdev = dmc->src_dev->bdev;
		return 1;
	}

	offset = (unsigned int)(bio->bi_sector & dmc->block_mask);
	request_block = bio->bi_sector - offset;

	if (cache[cache_block].state == VALID) {
		DPRINTK("Replacing %llu->%llu",
			cache[cache_block].block, request_block);
		dmc->replace++;
	} else {
		DPRINTK("Insert block %llu at empty frame %llu",
			request_block, cache_block);
	}

	/* Write delay */
	cache_insert(dmc, request_block, cache_block, DRESERVED); /* Update metadata first */

	job = new_kcached_job(dmc, bio, request_block, cache_block);

	head = to_bytes(offset);
	left = (dmc->src_dev->bdev->bd_inode->i_size>>9) - request_block;
	if (left < dmc->block_size) {
		tail = to_bytes(left) - bio->bi_size - head;
		job->src.count = left;
		job->dest.count = left;
	} else
		tail = to_bytes(dmc->block_size) - bio->bi_size - head;

	if (0 == head && 0 == tail) {
		/* Requested is aligned with a cache block */
		job->nr_pages = 0;
		job->rw = WRITE;
	} else if (head && tail) {
		/* Special case: need to pad both head and tail */
		job->nr_pages = dm_div_up(to_bytes(job->src.count), PAGE_SIZE);
		job->rw = READ;
	} else {
		if (head) {
			/* Fetch head only*/
			job->src.count = to_sector(head);
			job->nr_pages = dm_div_up(head, PAGE_SIZE);
		} else {
			/* Fetch tail only */
			job->src.sector = bio->bi_sector + to_sector(bio->bi_size);
			job->src.count = to_sector(tail);
			job->nr_pages = dm_div_up(tail, PAGE_SIZE);
		}
		job->rw = READ;
	}

	queue_job(job);

	return 0;
}

/* Handle cache misses */
static int cache_miss(struct cache_c *dmc, struct bio *bio, sector_t cache_block)
{
	if (bio_data_dir(bio) == READ)
		return cache_read_miss(dmc, bio, cache_block);
	else
		return cache_write_miss(dmc, bio, cache_block);
}


/****************************************************************************
 *  Functions for implementing the operations on a cache mapping.
 ****************************************************************************/

/*
 * Decide the mapping and perform necessary cache operations for a bio request.
 *
 * results:
 * < 0: error
 * = 0: we will handle the io by resubmitting it later
 * = 1: simple remap complete
 * = 2: we want to push back the io
 */
static int cache_map(struct dm_target *ti, struct bio *bio,
		      union map_info *map_context)
{
	struct cache_c *dmc = (struct cache_c *) ti->private;
	sector_t request_block, cache_block = 0, offset;
	int res;

	offset = bio->bi_sector & dmc->block_mask;
	request_block = bio->bi_sector - offset;

	DPRINTK("Got a %s for %llu ((%llu:%llu), %u bytes)",
		bio_rw(bio) == WRITE ? "WRITE" : (bio_rw(bio) == READ ?
		"READ":"READA"), bio->bi_sector, request_block, offset,
		bio->bi_size);

	bio_data_dir(bio) == READ ? dmc->reads++ : dmc->writes++;

	res = cache_lookup(dmc, request_block, &cache_block);
	if (res == 1) {
		/* Cache hit; server request from cache */
		res = cache_hit(dmc, bio, cache_block);
		return res;
	} else if (res == 0) {
		/* Cache miss; replacement block is found */
		res = cache_miss(dmc, bio, cache_block);
		return res;
	}

	/* Forward to source device */
	bio->bi_bdev = dmc->src_dev->bdev;

	return 1;
}

struct meta_dmc {
	sector_t size;
	unsigned int block_size;
	unsigned int assoc;
	unsigned int write_policy;
	unsigned int wb_on_dtr;
	unsigned short age_max;
	unsigned short age_default;
	unsigned int chksum;
};

/* Load metadata stored by previous session from disk. */
static int load_metadata(struct cache_c *dmc)
{
	struct dm_io_region where;
	unsigned long bits;
	sector_t dev_size = dmc->cache_dev->bdev->bd_inode->i_size >> 9;
	sector_t meta_size, total_size, i, limit, read = 0;
	struct meta_dmc *meta_dmc;
	unsigned int chksum = 0, chksum_sav, consecutive_blocks;

	where.bdev = dmc->cache_dev->bdev;
	limit = (BIO_MAX_PAGES - 2) * (PAGE_SIZE >> SECTOR_SHIFT);

	/* read driver meta data from disk */
	meta_size = dm_div_up(sizeof(struct cache_c), 512);
	meta_dmc = (struct meta_dmc *)vmalloc(meta_size * 512);
	if (!meta_dmc) {
		DMERR("%s: Unable to allocate memory", __func__);
		return 1;
	}

	for (i = 0; i < meta_size; i += where.count) {
		where.sector = dev_size - meta_size + i;
		where.count = min(meta_size - i, limit);
		dm_io_sync_vm(1, &where, READ, (void *) meta_dmc + (i*512), &bits, dmc);
	}

	DPRINTK("Loaded cache conf: block size(%u), cache size(%llu), " \
		"associativity(%u), write policy(%u), chksum(%u)",
		meta_dmc->block_size, meta_dmc->size,
		meta_dmc->assoc, meta_dmc->write_policy,
		meta_dmc->chksum);

	/* restore driver meta data */
	dmc->block_size = meta_dmc->block_size;
	dmc->block_shift = ffs(dmc->block_size) - 1;
	dmc->block_mask = dmc->block_size - 1;

	dmc->size = meta_dmc->size;
	dmc->bits = ffs(dmc->size) - 1;

	dmc->assoc = meta_dmc->assoc;
	consecutive_blocks = dmc->assoc < CONSECUTIVE_BLOCKS ?
				dmc->assoc : CONSECUTIVE_BLOCKS;
	dmc->consecutive_shift = ffs(consecutive_blocks) - 1;

	dmc->assoc_bits = ffs(dmc->assoc) - 1;
	dmc->assoc_mask = (~((unsigned int)0)) >> (unsigned int) ((sizeof(unsigned int) * 8) - ffs(dmc->assoc) + 1);


	dmc->sets = dmc->size >> dmc->assoc_bits;

	dmc->hash_pattern = (~((unsigned long long)0)) >> (unsigned int) ((sizeof(unsigned long long) * 8) - ffs(dmc->sets) + 1);

	dmc->write_policy = meta_dmc->write_policy;

	dmc->wb_on_dtr = meta_dmc->wb_on_dtr;

	dmc->age_max = meta_dmc->age_max;
	dmc->age_default = meta_dmc->age_default;

	chksum_sav = meta_dmc->chksum;

	vfree((void *)meta_dmc);


	/* restore cache status */
	total_size = dm_div_up(dmc->size * sizeof(struct cacheblock), 512)
			   + dm_div_up(dmc->sets * sizeof(unsigned int), 512)
			   + dm_div_up(sizeof(struct cache_c), 512);

	/* read meta data of the cacheblocks from disk */
	dmc->cache = (struct cacheblock *)vmalloc(sizeof(struct cacheblock) * dmc->size);
	if (!meta_dmc) {
		DMERR("%s: Unable to allocate memory", __func__);
		return 1;
	}
	meta_size = dm_div_up(dmc->size * sizeof(struct cacheblock), 512);
	for (i = 0; i < meta_size; i += where.count) {
		where.sector = dev_size - total_size + read;
		where.count = min(meta_size - i, limit);
		dm_io_sync_vm(1, &where, READ, (void *) dmc->cache + (i*512), &bits, dmc);
		read += where.count;
	}
	chksum = csum_partial((void *) dmc->cache, dmc->size * sizeof(struct cacheblock), chksum);

	DMINFO("Allocate %lluKB (%luB per) mem for %llu-entry cache" \
	       "(capacity:%lluMB, associativity:%u, block size:%u " \
	       "sectors(%uKB), %s)",
	       (unsigned long long) meta_size >> 10, (unsigned long) sizeof(struct cacheblock),
	       (unsigned long long) dmc->size,
	       (unsigned long long) dmc->size * dmc->block_size >> (20-SECTOR_SHIFT),
	       dmc->assoc, dmc->block_size,
	       dmc->block_size >> (10-SECTOR_SHIFT),
	       dmc->write_policy ? "write-back" : "write-through");

	/* read markers from disk */
	meta_size = dm_div_up(dmc->sets * sizeof(unsigned int), 512);
	dmc->marker = (unsigned int *) vmalloc(meta_size * 512);
	if (!dmc->marker) {
		DMERR("%s: Unable to allocate memory", __func__);
		return 1;
	}
	for (i = 0; i < meta_size; i += where.count) {
		where.sector = dev_size - total_size + read;
		where.count = min(meta_size - i, limit);
		dm_io_sync_vm(1, &where, READ, (void *) dmc->marker + (i*512), &bits, dmc);
		read += where.count;
	}

	chksum = csum_partial(dmc->marker, dmc->sets * sizeof(unsigned int), chksum);

	if (chksum != chksum_sav) { /* Check the checksum of the metadata */
		DMERR("Cache metadata loaded from disk is corrupted");
		vfree((void *)dmc->cache);
		vfree((void *)dmc->marker);
		return 1;
	}

	DMINFO("Cache metadata loaded from disk (offset %llu)",
	       (unsigned long long) dev_size - (unsigned long long) total_size);

	return 0;
}

/* Store metadata to disk. */
static int dump_metadata(struct cache_c *dmc)
{
	struct dm_io_region where;
	unsigned long bits;
	sector_t dev_size = dmc->cache_dev->bdev->bd_inode->i_size >> 9;
	sector_t total_size = 0, meta_size, i, limit, written = 0;
	struct meta_dmc *meta_dmc;
	unsigned int chksum = 0;

	where.bdev = dmc->cache_dev->bdev;
	limit = (BIO_MAX_PAGES - 2) * (PAGE_SIZE >> SECTOR_SHIFT);
	total_size = dm_div_up(dmc->size * sizeof(struct cacheblock), 512)
			   + dm_div_up(dmc->sets * sizeof(unsigned int), 512)
			   + dm_div_up(sizeof(struct cache_c), 512);

	/* write meta data of the cacheblocks to disk */
	meta_size = dm_div_up(dmc->size * sizeof(struct cacheblock), 512);
	for (i = 0; i < meta_size; i += where.count) {
		where.sector = dev_size - total_size + written;
		where.count = min(meta_size - i, limit);
		dm_io_sync_vm(1, &where, WRITE, (void *) dmc->cache + (i*512), &bits, dmc);
		written += where.count;
	}
	chksum = csum_partial((void *) dmc->cache, dmc->size * sizeof(struct cacheblock), chksum);

	/* write markers to disk */
	meta_size = dm_div_up(dmc->sets * sizeof(unsigned int), 512);
	for (i = 0; i < meta_size; i += where.count) {
		where.sector = dev_size - total_size + written;
		where.count = min(meta_size - i, limit);
		dm_io_sync_vm(1, &where, WRITE, (void *) dmc->marker + (i*512), &bits, dmc);
		written += where.count;
	}
	chksum = csum_partial(dmc->marker, dmc->sets * sizeof(unsigned int), chksum);

	/* write meta data of the driver to disk */
	meta_size = dm_div_up(sizeof(struct cache_c), 512);
	meta_dmc = (struct meta_dmc *)vmalloc(meta_size * 512);
	if (!meta_dmc) {
		DMERR("%s: Unable to allocate memory", __func__);
		return 1;
	}

	meta_dmc->block_size = dmc->block_size;
	meta_dmc->size = dmc->size;
	meta_dmc->assoc = dmc->assoc;
	meta_dmc->write_policy = dmc->write_policy;
	meta_dmc->wb_on_dtr = dmc->wb_on_dtr;
	meta_dmc->age_max = dmc->age_max;
	meta_dmc->age_default = dmc->age_default;
	meta_dmc->chksum = chksum;

	DPRINTK("Store metadata to disk: block size(%u), cache size(%llu), " \
		"associativity(%u), write policy(%u), checksum(%u)",
		meta_dmc->block_size, (unsigned long long) meta_dmc->size,
		meta_dmc->assoc, meta_dmc->write_policy,
		meta_dmc->chksum);

	for (i = 0; i < meta_size; i += where.count) {
		where.sector = dev_size - total_size + written;
		where.count = min(meta_size - i, limit);
		dm_io_sync_vm(1, &where, WRITE, (void *) meta_dmc + (i*512), &bits, dmc);
		written += where.count;
	}

	vfree((void *) meta_dmc);


	DMINFO("Cache metadata saved to disk (offset %llu)",
	       (unsigned long long) dev_size - (unsigned long long) total_size);

	return 0;
}

#if defined(CONFIG_DEBUG_FS)
static struct dentry *cache_build_debugfs(struct cache_c *dmc)
{
	struct dentry *root, *node;

	root = debugfs_create_dir(dmc->src_dev->name, debugfs);

	if (root) {
		/* export properties */
		node = debugfs_create_dir("properties", root);
		debugfs_create_u64("size", 0600, node, (u64 *) &dmc->size);
		debugfs_create_u32("associativity", 0600, node, (u32 *) &dmc->assoc);

		/* export stats */
		node = debugfs_create_dir("statistics", root);
		debugfs_create_u64("writebacks", 0600, node, (u64 *) &dmc->writeback);
		debugfs_create_u64("replaces", 0600, node, (u64 *) &dmc->replace);
		debugfs_create_u64("cache_hits", 0600, node, (u64 *) &dmc->cache_hits);
		debugfs_create_u64("writes", 0600, node, (u64 *) &dmc->writes);
		debugfs_create_u64("reads", 0600, node, (u64 *) &dmc->reads);

		/* export the cache */
		dmc->blob_cache.data = dmc->cache;
		dmc->blob_cache.size = dmc->size * sizeof(struct cacheblock);
		debugfs_create_blob("cache", 0600, root, &dmc->blob_cache);
	}

	return root;
}
#endif

/*
 * Construct a cache mapping.
 *  arg[0]: path to source device
 *  arg[1]: path to cache device
 *  arg[2]: cache persistence (if set, cache conf is loaded from disk)
 * Cache configuration parameters (if not set, default values are used.
 *  arg[3]: cache block size (in sectors)
 *  arg[4]: cache size (in blocks)
 *  arg[5]: cache associativity
 *  arg[6]: write caching policy
 */
static int cache_ctr(struct dm_target *ti, unsigned int argc, char **argv)
{
	struct cache_c *dmc;
	unsigned int consecutive_blocks, persistence = 0;
	sector_t data_size, meta_size, dev_size, localsize, order;
	unsigned long long cache_size;
	int r = -EINVAL;

	if (argc < 2) {
		ti->error = "dm-cache: Need at least 2 arguments (src dev and cache dev)";
		goto bad;
	}

	dmc = kmalloc(sizeof(*dmc), GFP_KERNEL);
	if (dmc == NULL) {
		ti->error = "dm-cache: Failed to allocate cache context";
		r = ENOMEM;
		goto bad;
	}

	r = dm_get_device(ti, argv[0], 0, &dmc->src_dev);
	if (r) {
		ti->error = "dm-cache: Source device lookup failed";
		goto bad1;
	}

	r = dm_get_device(ti, argv[1], 0, &dmc->cache_dev);
	if (r) {
		ti->error = "dm-cache: Cache device lookup failed";
		goto bad2;
	}

	dmc->io_client = dm_io_client_create(DMCACHE_COPY_PAGES);
	if (IS_ERR(dmc->io_client)) {
		r = PTR_ERR(dmc->io_client);
		ti->error = "Failed to create io client\n";
		goto bad3;
	}

	r = dm_kcopyd_client_create(DMCACHE_COPY_PAGES, &dmc->kcp_client);
	if (r) {
		ti->error = "Failed to initialize kcopyd client\n";
		goto bad4;
	}

	r = kcached_init(dmc);
	if (r) {
		ti->error = "Failed to initialize kcached";
		goto bad5;
	}

	if (argc >= 3) {
		if (sscanf(argv[2], "%u", &persistence) != 1) {
			ti->error = "dm-cache: Invalid cache persistence";
			r = -EINVAL;
			goto bad6;
		}
	}
	if (persistence == 1) {
		if (load_metadata(dmc)) {
			ti->error = "dm-cache: Invalid cache configuration";
			r = -EINVAL;
			goto bad6;
		}
		goto init; /* Skip reading cache parameters from command line */
	} else if (persistence != 0) {
		ti->error = "dm-cache: Invalid cache persistence";
		r = -EINVAL;
		goto bad6;
	}

	if (argc >= 4) {
		if (sscanf(argv[3], "%u", &dmc->block_size) != 1) {
			ti->error = "dm-cache: Invalid block size";
			r = -EINVAL;
			goto bad6;
		}
		if (!dmc->block_size || (dmc->block_size & (dmc->block_size - 1))) {
			ti->error = "dm-cache: Invalid block size";
			r = -EINVAL;
			goto bad6;
		}
	} else {
		dmc->block_size = DEFAULT_BLOCK_SIZE;
	}

	dmc->block_shift = ffs(dmc->block_size) - 1;
	dmc->block_mask = dmc->block_size - 1;

	/* cache size must be 2^n with n > 5 */
	if (argc >= 5) {
		if (sscanf(argv[4], "%llu", &cache_size) != 1) {
			ti->error = "dm-cache: Invalid cache size";
			r = -EINVAL;
			goto bad6;
		}
		dmc->size = (sector_t) cache_size;
		if (dmc->size < 64 || (dmc->size & (dmc->size - 1))) {
			ti->error = "dm-cache: Invalid cache size";
			r = -EINVAL;
			goto bad6;
		}
	} else {
		dmc->size = DEFAULT_CACHE_SIZE;
	}

	localsize = dmc->size;
	dmc->bits = ffs(dmc->size) - 1;

	if (argc >= 6) {
		if (sscanf(argv[5], "%u", &dmc->assoc) != 1) {
			ti->error = "dm-cache: Invalid cache associativity";
			r = -EINVAL;
			goto bad6;
		}
		if (!dmc->assoc || (dmc->assoc & (dmc->assoc - 1)) ||
			dmc->size < dmc->assoc) {
			ti->error = "dm-cache: Invalid cache associativity";
			r = -EINVAL;
			goto bad6;
		}
	} else{
		dmc->assoc = DEFAULT_CACHE_ASSOC;
	}

	dmc->assoc_bits = ffs(dmc->assoc) - 1;
	dmc->assoc_mask = (~((unsigned int)0)) >> (unsigned int) ((sizeof(unsigned int) * 8) - ffs(dmc->assoc) + 1);


	dmc->sets = dmc->size >> dmc->assoc_bits;

	dmc->hash_pattern = (~((unsigned long long)0)) >> (unsigned int) ((sizeof(unsigned long long) * 8) - ffs(dmc->sets) + 1);

	dev_size = dmc->cache_dev->bdev->bd_inode->i_size >> 9;
	data_size = dmc->size * dmc->block_size;
	meta_size = dm_div_up(dmc->size * sizeof(sector_t), 512) + 1;
	if ((data_size + meta_size) > dev_size) {
		DMERR("Requested cache size exeeds the cache device's capacity" \
			"(%llu+%llu>%llu)",
			(unsigned long long) data_size, (unsigned long long) meta_size,
			(unsigned long long) dev_size);
		ti->error = "dm-cache: Invalid cache size";
		r = -EINVAL;
		goto bad6;
	}
	consecutive_blocks = dmc->assoc < CONSECUTIVE_BLOCKS ?
				dmc->assoc : CONSECUTIVE_BLOCKS;
	dmc->consecutive_shift = ffs(consecutive_blocks) - 1;

	if (argc >= 7) {
		if (sscanf(argv[6], "%u", &dmc->write_policy) != 1) {
			ti->error = "dm-cache: Invalid cache write policy";
			r = -EINVAL;
			goto bad6;
		}
		if (dmc->write_policy < 0 && dmc->write_policy > 2) {
			ti->error = "dm-cache: Invalid cache write policy";
			r = -EINVAL;
			goto bad6;
		}
	} else {
		dmc->write_policy = DEFAULT_WRITE_POLICY;
	}

	if (argc >= 8) {
		if (sscanf(argv[7], "%u", &dmc->age_max) != 1) {
			ti->error = "dm-cache: Invalid write back on destruction setting";
			r = -EINVAL;
			goto bad6;
		}
	} else {
		dmc->wb_on_dtr = DEFAULT_WB_ON_DTR;
	}

	if (argc >= 9) {
		if (sscanf(argv[8], "%u", &dmc->age_max) != 1) {
			ti->error = "dm-cache: Invalid maximum age for cacheblocks";
			r = -EINVAL;
			goto bad6;
		}
	} else {
		dmc->age_max = DEFAULT_AGE_MAX;
	}

	if (argc >= 10) {
		if (sscanf(argv[9], "%u", &dmc->age_default) != 1) {
			ti->error = "dm-cache: Invalid default age for cacheblocks";
			r = -EINVAL;
			goto bad6;
		} else if (dmc->age_default > dmc->age_max) {
			DMWARN("dm-cache: default age greater than maximum age");
		}
	} else {
		dmc->age_default = dmc->age_max;
	}

	order = dmc->size * sizeof(struct cacheblock);
	localsize = data_size >> 11;

	DMINFO("Allocate %lluKB (%luB per) mem for %llu-entry cache" \
		"(capacity:%lluMB, associativity:%u, block size:%u " \
		"sectors(%uKB), %s)",
		(unsigned long long) order >> 10, (unsigned long) sizeof(struct cacheblock),
		(unsigned long long) dmc->size,
		(unsigned long long) data_size >> (20-SECTOR_SHIFT),
		dmc->assoc, dmc->block_size,
		dmc->block_size >> (10-SECTOR_SHIFT),
		dmc->write_policy ? "write-back" : "write-through");

	dmc->cache = (struct cacheblock *)vmalloc(order);
	if (!dmc->cache) {
		ti->error = "Unable to allocate memory";
		r = -ENOMEM;
		goto bad6;
	}
	memset(dmc->cache, 0, order);

	/*
	 * we will use a multiple of 512 so save and restore
	 * of the metadata will be faster and use less memory
	 */
	order = dm_div_up(dmc->sets * sizeof(unsigned int), 512) * 512;
	dmc->marker = (unsigned int *)vmalloc(order);
	if (!dmc->marker) {
		ti->error = "Unable to allocate memory";
		r = -ENOMEM;
		goto bad6;
	}
	memset(dmc->marker, 0, order);

#if WITH_TRIM
	blkdev_issue_discard(dmc->cache_dev->bdev, ti->begin,
		dmc->block_size * dmc->size, GFP_KERNEL, DISCARD_FL_BARRIER);
#endif

init:	/* Initialize the cache structs */
	INIT_LIST_HEAD(&dmc->queue.list);

#if defined(CONFIG_DEBUG_FS)
	dmc->debugfs = cache_build_debugfs(dmc);
#endif

	dmc->reads = 0;
	dmc->writes = 0;
	dmc->cache_hits = 0;
	dmc->replace = 0;
	dmc->writeback = 0;
	dmc->dirty = 0;

	ti->split_io = dmc->block_size;
	ti->private = dmc;

	return 0;

bad6:
	kcached_client_destroy(dmc);
bad5:
	dm_kcopyd_client_destroy(dmc->kcp_client);
bad4:
	dm_io_client_destroy(dmc->io_client);
bad3:
	dm_put_device(ti, dmc->cache_dev);
bad2:
	dm_put_device(ti, dmc->src_dev);
bad1:
	kfree(dmc);
bad:
	return r;
}

static void cache_flush(struct cache_c *dmc)
{
	struct cacheblock *cache = dmc->cache;
	sector_t i = 0;
	unsigned int j;

	DMINFO("Flush dirty blocks...");
	while (i < dmc->size) {
		j = 1;
		if (cache[i].state == DIRTY) {
			while ((i+j) < dmc->size && cache[i+j].state == DIRTY &&
				(cache[i+j].block == cache[i].block + j * dmc->block_size)) {
				j++;
			}
			dmc->dirty += j;
			write_back(dmc, i, j);
		}
		i += j;
	}
}

/*
 * Destroy the cache mapping.
 */
static void cache_dtr(struct dm_target *ti)
{
	struct cache_c *dmc = (struct cache_c *) ti->private;

#if defined(CONFIG_DEBUG_FS)
	if (dmc->debugfs)
		debugfs_remove_recursive(dmc->debugfs);
#endif

	if (dmc->wb_on_dtr)
		cache_flush(dmc);

	kcached_client_destroy(dmc);

	dm_kcopyd_client_destroy(dmc->kcp_client);

	if (dmc->reads + dmc->writes > 0)
		DMINFO("stats: reads(%lu), writes(%lu), cache hits(%lu, 0.%lu)," \
			"replacement(%lu), replaced dirty blocks(%lu), " \
			"flushed dirty blocks(%lu)",
			dmc->reads, dmc->writes, dmc->cache_hits,
			dmc->cache_hits * 100 / (dmc->reads + dmc->writes),
			dmc->replace, dmc->writeback, dmc->dirty);

	dump_metadata(dmc); /* Always dump metadata to disk before exit */
	vfree((void *)dmc->cache);
	vfree((void *)dmc->marker);
	dm_io_client_destroy(dmc->io_client);

	dm_put_device(ti, dmc->src_dev);
	dm_put_device(ti, dmc->cache_dev);
	kfree(dmc);
}

/*
 * Report cache status:
 *  Output cache stats upon request of device status;
 *  Output cache configuration upon request of table status.
 */
static int cache_status(struct dm_target *ti, status_type_t type,
			 char *result, unsigned int maxlen)
{
	struct cache_c *dmc = (struct cache_c *) ti->private;
	int sz = 0;

	switch (type) {
	case STATUSTYPE_INFO:
		DMEMIT("stats: reads(%lu), writes(%lu), cache hits(%lu, 0.%lu)," \
			"replacement(%lu), replaced dirty blocks(%lu)",
			dmc->reads, dmc->writes, dmc->cache_hits,
			(dmc->reads + dmc->writes) > 0 ? \
			dmc->cache_hits * 100 / (dmc->reads + dmc->writes) : 0,
			dmc->replace, dmc->writeback);
		break;
	case STATUSTYPE_TABLE:
		DMEMIT("conf: capacity(%lluM), associativity(%u), block size(%uK), %s",
			(unsigned long long) dmc->size * dmc->block_size >> 11,
			dmc->assoc, dmc->block_size>>(10-SECTOR_SHIFT),
			dmc->write_policy ? "write-back":"write-through");
		break;
	}
	return 0;
}

static int cache_message(struct dm_target *ti, unsigned int argc, char **argv)
{
	struct cache_c *dmc = (struct cache_c *) ti->private;
	unsigned long s;

#if DMC_ADVANCED_INFO
	unsigned long array[10] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
	sector_t *data, *cache;
	struct dm_io_region io;
	unsigned long bits;
#endif

	if (argc >= 1) {
		if (sscanf(argv[0], "%lu", &s) != 1) {
			ti->error = "dm-cache: No message specified";
			return -EINVAL;
		}
	} else
		return -EINVAL;

	switch (s) {
	case 0:
		if (dmc->wb_on_dtr)
			dmc->wb_on_dtr = 0;
		else
			dmc->wb_on_dtr = 1;
		DMINFO("dm-cache: wb on destruction set to %s", dmc->wb_on_dtr ? "disabled" : "enabled");
		break;
#if DMC_ADVANCED_INFO
	case 1:
		cache_debug_print_cache(dmc);
		break;
	case 2:
		sscanf(argv[1], "%lu", &s);
		cache_debug_table_set(dmc, s);
		break;
	case 3:
		sscanf(argv[1], "%lu", &s);
		DMINFO("set: %lu", hash_block(dmc, s));
		break;
	case 4:
		for (s = 0; s < dmc->size; s++)
			array[dmc->cache[s].state]++;

		DMINFO("INVALID: %lu", array[INVALID]);
		DMINFO("VALID: %lu", array[VALID]);
		DMINFO("DIRTY: %lu", array[DIRTY]);
		DMINFO("VRESERVED: %lu", array[VRESERVED]);
		DMINFO("DRESERVED: %lu", array[DRESERVED]);
		DMINFO("WRITEBACK: %lu", array[WRITEBACK]);
		break;
	case 5:
		dmc->writeback = 0;
		dmc->replace = 0;
		dmc->cache_hits = 0;
		dmc->writes = 0;
		dmc->reads = 0;
		break;
	case 6:
		data = vmalloc(dmc->block_size * BLOCK_SIZE);
		if (!data)
			return -EINVAL;

		cache = vmalloc(dmc->block_size * BLOCK_SIZE);
		if (!cache) {
			vfree(data);
			return -EINVAL;
		}
		for (s = 0 ; s < dmc->size; s++) {
			if (dmc->cache[s].state == VALID) {

				io.bdev = dmc->src_dev->bdev;
				io.sector = dmc->cache[s].block;
				io.count = 1;
				dm_io_sync_vm(1, &io, READ, (void *) data, &bits, dmc);

				io.bdev = dmc->cache_dev->bdev;
				io.sector = s * dmc->block_size;
				io.count = 1;
				dm_io_sync_vm(1, &io, READ, (void *) cache, &bits, dmc);

				if (csum_partial(data, dmc->block_size * BLOCK_SIZE, 0) !=
						csum_partial(cache, dmc->block_size * BLOCK_SIZE, 0))
					DMINFO("missmatch in cacheblock %lu", s);
			}
		}
		vfree(data);
		vfree(cache);

		DMINFO("cache is consistent");

		break;
	default:
		DMINFO("unknown message");
#else
	default:
		DMINFO("unknown message, compile with DM_ADVANCED_INFO enabled for more options.")
#endif
	}

	return 0;
}

/****************************************************************************
 *  Functions for manipulating a cache target.
 ****************************************************************************/

static struct target_type cache_target = {
	.name    = "cache",
	.version = {2, 0, 0},
	.module  = THIS_MODULE,
	.ctr     = cache_ctr,
	.dtr     = cache_dtr,
	.map     = cache_map,
	.status  = cache_status,
	.message = cache_message,
};

/*
 * Initiate a cache target.
 */
int __init dm_cache_init(void)
{
	int r;

	r = jobs_init();
	if (r)
		return r;

	_kcached_wq = create_singlethread_workqueue("kcached");
	if (!_kcached_wq) {
		DMERR("failed to start kcached");
		return -ENOMEM;
	}
	INIT_WORK(&_kcached_work, do_work);

	r = dm_register_target(&cache_target);
	if (r < 0) {
		DMERR("cache: register failed %d", r);
		destroy_workqueue(_kcached_wq);
	}

#if defined(CONFIG_DEBUG_FS)
	debugfs = debugfs_create_dir("dm-cache", NULL);
#endif

	return r;
}

/*
 * Destroy a cache target.
 */
static void __exit dm_cache_exit(void)
{
#if defined(CONFIG_DEBUG_FS)
	debugfs_remove_recursive(debugfs);
#endif

	dm_unregister_target(&cache_target);

	jobs_exit();
	destroy_workqueue(_kcached_wq);
}

module_init(dm_cache_init);
module_exit(dm_cache_exit);

MODULE_DESCRIPTION(DM_NAME " cache target");
MODULE_AUTHOR("Anselm Busse <abusse@cs.tu-berlin.de>");
MODULE_LICENSE("GPL");
