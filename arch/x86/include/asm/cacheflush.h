/* SPDX-License-Identifier: GPL-2.0 */
#ifndef _ASM_X86_CACHEFLUSH_H
#define _ASM_X86_CACHEFLUSH_H

#ifdef CONFIG_X86_EARLYMIC
void mic_flush_icache(void);
void mic_icache_dirty(struct page *page);
#endif

/* Caches aren't brain-dead on the intel. */
#include <asm-generic/cacheflush.h>
#include <asm/special_insns.h>

void clflush_cache_range(void *addr, unsigned int size);

#endif /* _ASM_X86_CACHEFLUSH_H */
