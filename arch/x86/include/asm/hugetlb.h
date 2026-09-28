/* SPDX-License-Identifier: GPL-2.0 */
#ifndef _ASM_X86_HUGETLB_H
#define _ASM_X86_HUGETLB_H

#include <asm/page.h>
#include <asm-generic/hugetlb.h>

#define hugepages_supported() boot_cpu_has(X86_FEATURE_PSE)

#ifdef CONFIG_X86_EARLYMIC
static inline pte_t arch_make_huge_pte(pte_t entry, unsigned int shift,
				       vm_flags_t flags)
{
	if (shift == PMD_SHIFT)
		entry = pte_mkpmd(entry);
	else
		entry = pte_mk64k(entry);
	return entry;
}
#define arch_make_huge_pte arch_make_huge_pte
#endif
#endif /* _ASM_X86_HUGETLB_H */
