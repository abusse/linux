/* SPDX-License-Identifier: GPL-2.0-only */
/*
 * Copyright (C) 2020 ARM Ltd.
 */
#ifndef __ASM_VDSO_PROCESSOR_H
#define __ASM_VDSO_PROCESSOR_H

#ifndef __ASSEMBLY__

/* REP NOP (PAUSE) is a good thing to insert into busy-wait loops. */
static __always_inline void rep_nop(void)
{
#ifdef CONFIG_X86_EARLYMIC
	/* no pause; approximate with delay */
	asm volatile("delay %0" :: "r" (1000) : "memory");
#else
	asm volatile("rep; nop" ::: "memory");
#endif
}

static __always_inline void cpu_relax(void)
{
	rep_nop();
}

struct getcpu_cache;

notrace long __vdso_getcpu(unsigned *cpu, unsigned *node, struct getcpu_cache *unused);

#endif /* __ASSEMBLY__ */

#endif /* __ASM_VDSO_PROCESSOR_H */
