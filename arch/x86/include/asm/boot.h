/* SPDX-License-Identifier: GPL-2.0 */
/* * Copyright (c) Intel Corporation (2011).
*
* Disclaimer: The codes contained in these modules may be specific to the
* Intel Software Development Platform codenamed: Knights Ferry, and the 
* Intel product codenamed: Knights Corner, and are not backward compatible 
* with other Intel products. Additionally, Intel will NOT support the codes 
* or instruction set in future products.
*
* Intel offers no warranty of any kind regarding the code.  This code is
* licensed on an "AS IS" basis and Intel is not obligated to provide any support,
* assistance, installation, training, or other services of any kind.  Intel is 
* also not obligated to provide any updates, enhancements or extensions.  Intel 
* specifically disclaims any warranty of merchantability, non-infringement, 
* fitness for any particular purpose, and any other warranty.
*
* Further, Intel disclaims all liability of any kind, including but not
* limited to liability for infringement of any proprietary rights, relating
* to the use of the code, even if Intel is notified of the possibility of
* such liability.  Except as expressly stated in an Intel license agreement
* provided with this code and agreed upon with Intel, no license, express
* or implied, by estoppel or otherwise, to any intellectual property rights
* is granted herein.
*/

#ifndef _ASM_X86_BOOT_H
#define _ASM_X86_BOOT_H


#include <asm/pgtable_types.h>
#include <uapi/asm/boot.h>

/* Physical address where kernel should be loaded. */
#define LOAD_PHYSICAL_ADDR ((CONFIG_PHYSICAL_START \
				+ (CONFIG_PHYSICAL_ALIGN - 1)) \
				& ~(CONFIG_PHYSICAL_ALIGN - 1))

/* Minimum kernel alignment, as a power of two */
#ifdef CONFIG_X86_64
# define MIN_KERNEL_ALIGN_LG2	PMD_SHIFT
#else
# define MIN_KERNEL_ALIGN_LG2	(PAGE_SHIFT + THREAD_SIZE_ORDER)
#endif
#define MIN_KERNEL_ALIGN	(_AC(1, UL) << MIN_KERNEL_ALIGN_LG2)

#ifndef CONFIG_X86_MIC_EMULATION
#if (CONFIG_PHYSICAL_ALIGN & (CONFIG_PHYSICAL_ALIGN-1)) || \
	(CONFIG_PHYSICAL_ALIGN < MIN_KERNEL_ALIGN)
# error "Invalid value for CONFIG_PHYSICAL_ALIGN"
#endif
#endif

#if defined(CONFIG_KERNEL_BZIP2)
# define BOOT_HEAP_SIZE		0x400000
#elif defined(CONFIG_KERNEL_ZSTD)
/*
 * Zstd needs to allocate the ZSTD_DCtx in order to decompress the kernel.
 * The ZSTD_DCtx is ~160KB, so set the heap size to 192KB because it is a
 * round number and to allow some slack.
 */
# define BOOT_HEAP_SIZE		 0x30000
#else
# define BOOT_HEAP_SIZE		 0x10000
#endif

#ifdef CONFIG_X86_64
# define BOOT_STACK_SIZE	0x4000

/*
 * Used by decompressor's startup_32() to allocate page tables for identity
 * mapping of the 4G of RAM in 4-level paging mode:
 * - 1 level4 table;
 * - 1 level3 table;
 * - 4 level2 table that maps everything with 2M pages;
 *
 * The additional level5 table needed for 5-level paging is allocated from
 * trampoline_32bit memory.
 */
# define BOOT_INIT_PGT_SIZE	(6*4096)

/*
 * Total number of page tables kernel_add_identity_map() can allocate,
 * including page tables consumed by startup_32().
 *
 * Worst-case scenario:
 *  - 5-level paging needs 1 level5 table;
 *  - KASLR needs to map kernel, boot_params, cmdline and randomized kernel,
 *    assuming all of them cross 256T boundary:
 *    + 4*2 level4 table;
 *    + 4*2 level3 table;
 *    + 4*2 level2 table;
 *  - X86_VERBOSE_BOOTUP needs to map the first 2M (video RAM):
 *    + 1 level4 table;
 *    + 1 level3 table;
 *    + 1 level2 table;
 * Total: 28 tables
 *
 * Add 4 spare table in case decompressor touches anything beyond what is
 * accounted above. Warn if it happens.
 */
# define BOOT_PGT_SIZE_WARN	(28*4096)
# define BOOT_PGT_SIZE		(32*4096)

#else /* !CONFIG_X86_64 */
# define BOOT_STACK_SIZE	0x1000
#endif

#ifndef __ASSEMBLY__
extern unsigned int output_len;
extern const unsigned long kernel_total_size;

unsigned long decompress_kernel(unsigned char *outbuf, unsigned long virt_addr,
				void (*error)(char *x));
#endif

#endif /* _ASM_X86_BOOT_H */
