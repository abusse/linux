/* SPDX-License-Identifier: GPL-2.0 */
/* * Copyright (c) 2010 - 2012 Intel Corporation.
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
#ifndef _ASM_X86_KDEBUG_H
#define _ASM_X86_KDEBUG_H

#include <linux/notifier.h>

struct pt_regs;

/* Grossly misnamed. */
enum die_val {
	DIE_OOPS = 1,
	DIE_INT3,
	DIE_DEBUG,
	DIE_PANIC,
	DIE_NMI,
	DIE_DIE,
	DIE_KERNELDEBUG,
	DIE_TRAP,
	DIE_GPF,
	DIE_CALL,
	DIE_PAGE_FAULT,
	DIE_NMIUNKNOWN,
	DIE_KDEBUG_ENTER,
	DIE_KDEBUG_LEAVE,
};

enum show_regs_mode {
	SHOW_REGS_SHORT,
	/*
	 * For when userspace crashed, but we don't think it's our fault, and
	 * therefore don't print kernel registers.
	 */
	SHOW_REGS_USER,
	SHOW_REGS_ALL
};

extern void die(const char *, struct pt_regs *,long);
void die_addr(const char *str, struct pt_regs *regs, long err, long gp_addr);
extern int __must_check __die(const char *, struct pt_regs *, long);
extern void show_stack_regs(struct pt_regs *regs);
extern void __show_regs(struct pt_regs *regs, enum show_regs_mode,
			const char *log_lvl);
extern void show_iret_regs(struct pt_regs *regs, const char *log_lvl);
extern unsigned long oops_begin(void);
extern void oops_end(unsigned long, struct pt_regs *, int signr);

#endif /* _ASM_X86_KDEBUG_H */
