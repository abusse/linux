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

/*
 *  linux/arch/x86_64/kernel/head.S -- start in 32bit and switch to 64bit
 *
 *  Copyright (C) 2000 Andrea Arcangeli <andrea@suse.de> SuSE
 *  Copyright (C) 2000 Pavel Machek <pavel@suse.cz>
 *  Copyright (C) 2000 Karsten Keil <kkeil@suse.de>
 *  Copyright (C) 2001,2002 Andi Kleen <ak@suse.de>
 *  Copyright (C) 2005 Eric Biederman <ebiederm@xmission.com>
 */


#include <linux/linkage.h>
#include <linux/threads.h>
#include <linux/init.h>
#include <asm/segment.h>
#include <asm/pgtable.h>
#include <asm/page.h>
#include <asm/msr.h>
#include <asm/cache.h>
#include <asm/processor-flags.h>
#include <asm/percpu.h>
#include <asm/nops.h>

#ifdef CONFIG_PARAVIRT
#include <asm/asm-offsets.h>
#include <asm/paravirt.h>
#define GET_CR2_INTO(reg) GET_CR2_INTO_RAX ; movq %rax, reg
#else
#define GET_CR2_INTO(reg) movq %cr2, reg
#define INTERRUPT_RETURN iretq
#endif

/* we are not able to switch in one step to the final KERNEL ADDRESS SPACE
 * because we need identity-mapped pages.
 *
 */

#define pud_index(x)	(((x) >> PUD_SHIFT) & (PTRS_PER_PUD-1))

L4_PAGE_OFFSET = pgd_index(__PAGE_OFFSET)
L3_PAGE_OFFSET = pud_index(__PAGE_OFFSET)
L4_START_KERNEL = pgd_index(__START_KERNEL_map)
L3_START_KERNEL = pud_index(__START_KERNEL_map)

	.text
	__HEAD
#ifdef CONFIG_X86_MIC_EMULATION
	.code32
	.balign 4
r_base = .
startup_32:
	cli
/*
	movl	$__KERNEL_DS, %eax	# Initialize the %ds segment register
	movl	%eax, %ds
*/

	lgdt    pGDT32 - __START_KERNEL_map

	/*
	 *  Enable MTRRs with default type UC
	 *  and set MTTR0 to define 0-1GB as WB
	 */
	xor %edx, %edx
	mov $0x06,%eax	/* PhysBase 0, Type WB */
	mov $0x200,%ecx	/* IA32_MTRR_PHYSBASE0 */
	wrmsr
	mov $0xff, %edx
	mov $0xc0000800, %eax
	mov $0x201, %ecx	/* IA32_MTRR_PHYSMASK0 */
	wrmsr

	mov $0x800,%eax		/* Enable variable range MTRRs, default UC */
	xor %edx, %edx
	mov $0x2ff,%ecx		/* IA32_MTRR_DEF_TYPE */
	wrmsr

	/* The code below (page table setup) expects the physical start of the kernel in edi */
	movl	$0x100000, %edi

	/* Enable PAE mode */
	movl	$X86_CR4_PAE, %eax
	movl	%eax, %cr4		# Enable PAE mode

					# Setup trampoline 4 level pagetables
	leal	(init_level4_pgt - r_base)(%edi), %eax
	movl	%eax, %cr3

	/* Enable Long mode in EFER */
	xorl	%eax,%eax
	movl	$MSR_EFER, %ecx
	rdmsr

	/* Enable Long Mode */
	btsl    $_EFER_LME, %eax
	xorl	%edx, %edx
	wrmsr

	xorl	%eax,%eax

	# Enable paging and in turn activate Long Mode
	# Enable protected mode

	mov	$(X86_CR0_PG | X86_CR0_PE), %eax
	mov	%eax, %cr0
	/*
	 * At this point we're in long mode but in 32bit compatibility mode
	 * with EFER.LME = 1, CS.L = 0, CS.D = 1 (and in turn
	 * EFER.LMA = 1). Now we want to jump in 64bit mode, to do that we use
	 * the new gdt/idt that has __KERNEL_CS with CS.L = 1.
	 */
	ljmp $__KERNEL_CS, $(startup_64 - __START_KERNEL_map)

	# Duplicate the global descriptor table
	# so the kernel can live anywhere
	.balign 4
tgdt:
	.short	tgdt_end - tgdt		# gdt limit
	.long	tgdt - r_base
	.short 0
	.quad	0x00cf9b000000ffff	# __KERNEL32_CS
	.quad	0x00af9b000000ffff	# __KERNEL_CS
	.quad	0x00cf93000000ffff	# __KERNEL_DS
tgdt_end:

	.balign 4
startup_32_vector:
	.long	startup_32 - r_base
	.word	__KERNEL32_CS, 0

	.balign 4
startup_64_vector:
	.long	startup_64 - r_base
	.word	__KERNEL_CS, 0

ENTRY(sv_boot_level4_pgt)
	.quad	level3_ident_pgt - __START_KERNEL_map + _KERNPG_TABLE
	.fill	510,8,0
	.quad	level3_kernel_pgt - __START_KERNEL_map + _KERNPG_TABLE

ENTRY(cpu_gdt_table)
	.quad   0x0000000000000000      /* NULL descriptor */
	.quad   0x0                     /* unused */
	.quad   0x00af9a000000ffff      /* __KERNEL_CS */
	.quad   0x00cf92000000ffff      /* __KERNEL_DS */
	.quad   0x00cffa000000ffff      /* __USER32_CS */
	.quad   0x00cff2000000ffff      /* __USER_DS, __USER32_DS  */
	.quad   0x00affa000000ffff      /* __USER_CS */
	.quad   0x00cf9a000000ffff      /* __KERNEL32_CS */
	.quad   0,0                     /* TSS */
	.quad   0,0                     /* LDT */
	.quad   0,0,0                   /* three TLS descriptors */
	.quad   0x0000f40000000000      /* node/CPU stored in limit */
gdt_end:
	.globl pGDT32
pGDT32:
	.word   gdt_end-cpu_gdt_table-1
	.long   cpu_gdt_table-__START_KERNEL_map
#endif /* CONFIG_X86_MIC_EMULATION */
	.code64
	.globl startup_64
startup_64:
	/*
	 * At this point the CPU runs in 64bit mode CS.L = 1 CS.D = 0,
	 * and someone has loaded an identity mapped page table
	 * for us.  These identity mapped page tables map all of the
	 * kernel pages and possibly all of memory.
	 *
	 * %rsi holds a physical pointer to real_mode_data.
	 *
	 * We come here either directly from a 64bit bootloader, or from
	 * arch/x86_64/boot/compressed/head.S.
	 *
	 * We only come here initially at boot nothing else comes here.
	 *
	 * Since we may be loaded at an address different from what we were
	 * compiled to run at we first fixup the physical addresses in our page
	 * tables and then reload them.
	 */

	/*
	 * Compute the delta between the address I am compiled to run at and the
	 * address I am actually running at.
	 */
	leaq	_text(%rip), %rbp
	subq	$_text - __START_KERNEL_map, %rbp

	/* Is the address not 2M aligned? */
	movq	%rbp, %rax
	andl	$~PMD_PAGE_MASK, %eax
	testl	%eax, %eax
	jnz	bad_address

	/*
	 * Is the address too large?
	 */
	leaq	_text(%rip), %rax
	shrq	$MAX_PHYSMEM_BITS, %rax
	jnz	bad_address

	/*
	 * Fixup the physical addresses in the page table
	 */
	addq	%rbp, early_level4_pgt + (L4_START_KERNEL*8)(%rip)

	addq	%rbp, level3_kernel_pgt + (510*8)(%rip)
	addq	%rbp, level3_kernel_pgt + (511*8)(%rip)

	addq	%rbp, level2_fixmap_pgt + (506*8)(%rip)

	/*
	 * Set up the identity mapping for the switchover.  These
	 * entries should *NOT* have the global bit set!  This also
	 * creates a bunch of nonsense entries but that is fine --
	 * it avoids problems around wraparound.
	 */
	leaq	_text(%rip), %rdi
	leaq	early_level4_pgt(%rip), %rbx

	movq	%rdi, %rax
	shrq	$PGDIR_SHIFT, %rax

	leaq	(4096 + _KERNPG_TABLE)(%rbx), %rdx
	movq	%rdx, 0(%rbx,%rax,8)
	movq	%rdx, 8(%rbx,%rax,8)

	addq	$4096, %rdx
	movq	%rdi, %rax
	shrq	$PUD_SHIFT, %rax
	andl	$(PTRS_PER_PUD-1), %eax
	movq	%rdx, 4096(%rbx,%rax,8)
	incl	%eax
	andl	$(PTRS_PER_PUD-1), %eax
	movq	%rdx, 4096(%rbx,%rax,8)

	addq	$8192, %rbx
	movq	%rdi, %rax
	shrq	$PMD_SHIFT, %rdi
	addq	$(__PAGE_KERNEL_LARGE_EXEC & ~_PAGE_GLOBAL), %rax
	leaq	(_end - 1)(%rip), %rcx
	shrq	$PMD_SHIFT, %rcx
	subq	%rdi, %rcx
	incl	%ecx

1:
	andq	$(PTRS_PER_PMD - 1), %rdi
	movq	%rax, (%rbx,%rdi,8)
	incq	%rdi
	addq	$PMD_SIZE, %rax
	decl	%ecx
	jnz	1b

	/*
	 * Fixup the kernel text+data virtual addresses. Note that
	 * we might write invalid pmds, when the kernel is relocated
	 * cleanup_highmap() fixes this up along with the mappings
	 * beyond _end.
	 */
	leaq	level2_kernel_pgt(%rip), %rdi
	leaq	4096(%rdi), %r8
	/* See if it is a valid page table entry */
1:	testq	$1, 0(%rdi)
	jz	2f
	addq	%rbp, 0(%rdi)
	/* Go to the next page */
2:	addq	$8, %rdi
	cmp	%r8, %rdi
	jne	1b

	/* Fixup phys_base */
	addq	%rbp, phys_base(%rip)

	movq	$(early_level4_pgt - __START_KERNEL_map), %rax
	jmp 1f
ENTRY(secondary_startup_64)
	/*
	 * At this point the CPU runs in 64bit mode CS.L = 1 CS.D = 0,
	 * and someone has loaded a mapped page table.
	 *
	 * %rsi holds a physical pointer to real_mode_data.
	 *
	 * We come here either from startup_64 (using physical addresses)
	 * or from trampoline.S (using virtual addresses).
	 *
	 * Using virtual addresses from trampoline.S removes the need
	 * to have any identity mapped pages in the kernel page table
	 * after the boot processor executes this code.
	 */

	movq	$(init_level4_pgt - __START_KERNEL_map), %rax
1:

	/* Enable PAE mode and PGE */
#ifndef CONFIG_X86_EARLYMIC
	movl	$(X86_CR4_PAE | X86_CR4_PGE), %ecx
#else
	movl	$(X86_CR4_PAE), %ecx
#endif
	movq	%rcx, %cr4

	/* Setup early boot stage 4 level pagetables. */
	addq	phys_base(%rip), %rax
	movq	%rax, %cr3

	/* Ensure I am executing from virtual addresses */
	movq	$1f, %rax
	jmp	*%rax
1:

	/* Check if nx is implemented */
	movl	$0x80000001, %eax
	cpuid
	movl	%edx,%edi

	/* Setup EFER (Extended Feature Enable Register) */
	movl	$MSR_EFER, %ecx
	rdmsr
	btsl	$_EFER_SCE, %eax	/* Enable System Call */
	btl	$20,%edi		/* No Execute supported? */
	jnc     1f
	btsl	$_EFER_NX, %eax
	btsq	$_PAGE_BIT_NX,early_pmd_flags(%rip)
1:	wrmsr				/* Make changes effective */

	/* Setup cr0 */
#define CR0_STATE	(X86_CR0_PE | X86_CR0_MP | X86_CR0_ET | \
			 X86_CR0_NE | X86_CR0_WP | X86_CR0_AM | \
			 X86_CR0_PG)
	movl	$CR0_STATE, %eax
	/* Make changes effective */
	movq	%rax, %cr0

	/* ap_code tells us whether we are in parallel boot code or serial boot code (BSP or hotplug) */
	mov ap_code(%rip), %eax
	testl	%eax, %eax
	jz	skip_for_bsp

	/* Look up our apic id, get our cpu if from the apic->cpu mapping table
	 * then use our cpuu id to look up our stack pointer in the stack pointer map
	 */

	movq apic_base_addr(%rip),%rax		
	addq $0x20, %rax					
	movl (%rax),%eax					
	mov $23,%cl
	shr %cl,%eax  
	/* move the apic id to rcx */					  
	mov %rax, %rcx

	/* don't overwrite our apic id as it is needed later to setup gs */
	mov %ecx,%ebx
	shl $3,%ebx							
	lea smp_stack_pointer_map(%rip),%rsp
	add %rbx,%rsp						
	movq (%rsp),%rsp				 

	mov ap_code(%rip), %ebx
	testl %ebx, %ebx
	jnz  skip_for_secondary

skip_for_bsp:
	/* Setup a boot time stack */
	movq stack_start(%rip), %rsp

skip_for_secondary:   
	/* zero EFLAGS after setting rsp */
	pushq $0
	popfq

	/*
	 * We must switch to a new descriptor in kernel space for the GDT
	 * because soon the kernel won't have access anymore to the userspace
	 * addresses where we're currently running on. We have to do that here
	 * because in 32bit we couldn't load a 64bit linear address.
	 */
	lgdt	early_gdt_descr(%rip)

	/* set up data segments */
	xorl %eax,%eax
	movl %eax,%ds
	movl %eax,%ss
	movl %eax,%es

	/*
	 * We don't really need to load %fs or %gs, but load them anyway
	 * to kill any stale realmode selectors.  This allows execution
	 * under VT hardware.
	 */
	movl %eax,%fs
	movl %eax,%gs

	/* Set up %gs.
	 *
	 * The base of %gs always points to the bottom of the irqstack
	 * union.  If the stack protector canary is enabled, it is
	 * located at %gs:40.  Note that, on SMP, the boot cpu uses
	 * init data section till per cpu areas are set up.
	 */

	mov ap_code(%rip), %ebx
	testl	%ebx, %ebx
	jz	skip_for_bsp2

	/* use our apic id to look up our gs address in the per cpu gs map */
	shl $3,%ecx						/* get offset into gs address array */
	lea smp_gs_map(%rip),%r11		/* get the base of the gs address map */
	add %rcx,%r11					/* offset to the correct location for this cpu */
	movq (%r11),%rax				/* get our gs address */

	mov ap_code(%rip), %ebx
	testl %ebx, %ebx
	jnz  skip_for_secondary2

skip_for_bsp2:
	movq	initial_gs(%rip),%rax

skip_for_secondary2:   

	movl	$MSR_GS_BASE,%ecx
	movl	initial_gs(%rip),%eax
	movl	initial_gs+4(%rip),%edx
	wrmsr	

	/* rsi is pointer to real mode structure with interesting info.
	   pass it to C */
	movq	%rsi, %rdi
	
	/* Finally jump to run C code and to be on real kernel address
	 * Since we are running on identity-mapped space we have to jump
	 * to the full 64bit address, this is only possible as indirect
	 * jump.  In addition we need to ensure %cs is set so we make this
	 * a far return.
	 *
	 * Note: do not change to far jump indirect with 64bit offset.
	 *
	 * AMD does not support far jump indirect with 64bit offset.
	 * AMD64 Architecture Programmer's Manual, Volume 3: states only
	 *	JMP FAR mem16:16 FF /5 Far jump indirect,
	 *		with the target specified by a far pointer in memory.
	 *	JMP FAR mem16:32 FF /5 Far jump indirect,
	 *		with the target specified by a far pointer in memory.
	 *
	 * Intel64 does support 64bit offset.
	 * Software Developer Manual Vol 2: states:
	 *	FF /5 JMP m16:16 Jump far, absolute indirect,
	 *		address given in m16:16
	 *	FF /5 JMP m16:32 Jump far, absolute indirect,
	 *		address given in m16:32.
	 *	REX.W + FF /5 JMP m16:64 Jump far, absolute indirect,
	 *		address given in m16:64.
	 */
	movq	initial_code(%rip),%rax
	pushq	$0		# fake return address to stop unwinder
	pushq	$__KERNEL_CS	# set correct cs
	pushq	%rax		# target address in negative space
	lretq

#ifdef CONFIG_HOTPLUG_CPU
/*
 * Boot CPU0 entry point. It's called from play_dead(). Everything has been set
 * up already except stack. We just set up stack here. Then call
 * start_secondary().
 */
ENTRY(start_cpu0)
	movq stack_start(%rip),%rsp
	movq	initial_code(%rip),%rax
	pushq	$0		# fake return address to stop unwinder
	pushq	$__KERNEL_CS	# set correct cs
	pushq	%rax		# target address in negative space
	lretq
ENDPROC(start_cpu0)
#endif

	/* SMP bootup changes these two */
	__REFDATA
	.balign	8
	GLOBAL(initial_code)
	.quad	x86_64_start_kernel
	GLOBAL(initial_gs)
	.quad	INIT_PER_CPU_VAR(irq_stack_union)

	GLOBAL(stack_start)
	.quad  init_thread_union+THREAD_SIZE-8
	.word  0
	__FINITDATA

bad_address:
	jmp bad_address

	__INIT
	.globl early_idt_handlers
early_idt_handlers:
	# 104(%rsp) %rflags
	#  96(%rsp) %cs
	#  88(%rsp) %rip
	#  80(%rsp) error code
	i = 0
	.rept NUM_EXCEPTION_VECTORS
	.if (EXCEPTION_ERRCODE_MASK >> i) & 1
	ASM_NOP2
	.else
	pushq $0		# Dummy error code, to make stack frame uniform
	.endif
	pushq $i		# 72(%rsp) Vector number
	jmp early_idt_handler
	i = i + 1
	.endr

/* This is global to keep gas from relaxing the jumps */
ENTRY(early_idt_handler)
	cld

	cmpl $2,early_recursion_flag(%rip)
	jz  1f
	incl early_recursion_flag(%rip)

	pushq %rax		# 64(%rsp)
	pushq %rcx		# 56(%rsp)
	pushq %rdx		# 48(%rsp)
	pushq %rsi		# 40(%rsp)
	pushq %rdi		# 32(%rsp)
	pushq %r8		# 24(%rsp)
	pushq %r9		# 16(%rsp)
	pushq %r10		#  8(%rsp)
	pushq %r11		#  0(%rsp)

	cmpl $__KERNEL_CS,96(%rsp)
	jne 11f

	cmpl $14,72(%rsp)	# Page fault?
	jnz 10f
	GET_CR2_INTO(%rdi)	# can clobber any volatile register if pv
	call early_make_pgtable
	andl %eax,%eax
	jz 20f			# All good

10:
	leaq 88(%rsp),%rdi	# Pointer to %rip
	call early_fixup_exception
	andl %eax,%eax
	jnz 20f			# Found an exception entry

11:
#ifdef CONFIG_EARLY_PRINTK
	GET_CR2_INTO(%r9)	# can clobber any volatile register if pv
	movl 80(%rsp),%r8d	# error code
	movl 72(%rsp),%esi	# vector number
	movl 96(%rsp),%edx	# %cs
	movq 88(%rsp),%rcx	# %rip
	xorl %eax,%eax
	leaq early_idt_msg(%rip),%rdi
	call early_printk
	cmpl $2,early_recursion_flag(%rip)
	jz  1f
	call dump_stack
#ifdef CONFIG_KALLSYMS	
	leaq early_idt_ripmsg(%rip),%rdi
	movq 40(%rsp),%rsi	# %rip again
	call __print_symbol
#endif
#endif /* EARLY_PRINTK */
1:	hlt
	jmp 1b

20:	# Exception table entry found or page table generated
	popq %r11
	popq %r10
	popq %r9
	popq %r8
	popq %rdi
	popq %rsi
	popq %rdx
	popq %rcx
	popq %rax
	addq $16,%rsp		# drop vector number and error code
	decl early_recursion_flag(%rip)
	INTERRUPT_RETURN
ENDPROC(early_idt_handler)

	__INITDATA

	.balign 4
early_recursion_flag:
	.long 0

#ifdef CONFIG_EARLY_PRINTK
early_idt_msg:
	.asciz "PANIC: early exception %02lx rip %lx:%lx error %lx cr2 %lx\n"
early_idt_ripmsg:
	.asciz "RIP %s\n"
#endif /* CONFIG_EARLY_PRINTK */

#define NEXT_PAGE(name) \
	.balign	PAGE_SIZE; \
GLOBAL(name)

/* Automate the creation of 1 to 1 mapping pmd entries */
#define PMDS(START, PERM, COUNT)			\
	i = 0 ;						\
	.rept (COUNT) ;					\
	.quad	(START) + (i << PMD_SHIFT) + (PERM) ;	\
	i = i + 1 ;					\
	.endr

	__INITDATA
NEXT_PAGE(early_level4_pgt)
	.fill	511,8,0
	.quad	level3_kernel_pgt - __START_KERNEL_map + _PAGE_TABLE

NEXT_PAGE(early_dynamic_pgts)
	.fill	512*EARLY_DYNAMIC_PAGE_TABLES,8,0

	.data

#ifndef CONFIG_XEN
NEXT_PAGE(init_level4_pgt)
	.fill	512,8,0
#ifdef CONFIG_MK1OM
/*
 * v3.10's non-Xen boot builds identity maps at runtime via early_dynamic_pgts and
 * no longer provides a static level3_ident_pgt. The KNC CC6/PC6 resume trampolines
 * (C-step only) still expect one to seed their low-memory identity page tables, so
 * provide the same static identity L3/L2 the Xen path uses (maps the first 1G).
 */
NEXT_PAGE(level3_ident_pgt)
	.quad	level2_ident_pgt - __START_KERNEL_map + _KERNPG_TABLE
	.fill	511, 8, 0
NEXT_PAGE(level2_ident_pgt)
	PMDS(0, __PAGE_KERNEL_IDENT_LARGE_EXEC, PTRS_PER_PMD)
#endif
#else
NEXT_PAGE(init_level4_pgt)
	.quad   level3_ident_pgt - __START_KERNEL_map + _KERNPG_TABLE
	.org    init_level4_pgt + L4_PAGE_OFFSET*8, 0
	.quad   level3_ident_pgt - __START_KERNEL_map + _KERNPG_TABLE
	.org    init_level4_pgt + L4_START_KERNEL*8, 0
	/* (2^48-(2*1024*1024*1024))/(2^39) = 511 */
	.quad   level3_kernel_pgt - __START_KERNEL_map + _PAGE_TABLE

NEXT_PAGE(level3_ident_pgt)
	.quad	level2_ident_pgt - __START_KERNEL_map + _KERNPG_TABLE
	.fill	511, 8, 0
NEXT_PAGE(level2_ident_pgt)
	/* Since I easily can, map the first 1G.
	 * Don't set NX because code runs from these pages.
	 */
	PMDS(0, __PAGE_KERNEL_IDENT_LARGE_EXEC, PTRS_PER_PMD)
#endif

NEXT_PAGE(level3_kernel_pgt)
	.fill	L3_START_KERNEL,8,0
	/* (2^48-(2*1024*1024*1024)-((2^39)*511))/(2^30) = 510 */
	.quad	level2_kernel_pgt - __START_KERNEL_map + _KERNPG_TABLE
	.quad	level2_fixmap_pgt - __START_KERNEL_map + _PAGE_TABLE

NEXT_PAGE(level2_kernel_pgt)
	/*
	 * 512 MB kernel mapping. We spend a full page on this pagetable
	 * anyway.
	 *
	 * The kernel code+data+bss must not be bigger than that.
	 *
	 * (NOTE: at +512MB starts the module area, see MODULES_VADDR.
	 *  If you want to increase this then increase MODULES_VADDR
	 *  too.)
	 */
	PMDS(0, __PAGE_KERNEL_LARGE_EXEC,
		KERNEL_IMAGE_SIZE/PMD_SIZE)

NEXT_PAGE(level2_fixmap_pgt)
	.fill	506,8,0
	.quad	level1_fixmap_pgt - __START_KERNEL_map + _PAGE_TABLE
	/* 8MB reserved for vsyscalls + a 2MB hole = 4 + 1 entries */
	.fill	5,8,0

NEXT_PAGE(level1_fixmap_pgt)
	.fill	512,8,0

#undef PMDS

	.data
	.align 16
	.globl early_gdt_descr
early_gdt_descr:
	.word	GDT_ENTRIES*8-1
early_gdt_descr_base:
	.quad	INIT_PER_CPU_VAR(gdt_page)

	.align 16
ENTRY(smp_stack_pointer_map)
	.fill   256, 8, 0

	.align 16
ENTRY(smp_gs_map)
	.fill   256, 8, GDT_ENTRIES*8-1

ENTRY(ap_code)
	.fill 1, 8, 0

ENTRY(apic_base_addr)
	.fill 1, 8, 0

ENTRY(phys_base)
	/* This must match the first entry in level2_kernel_pgt */
	.quad   0x0000000000000000

#include "../../x86/xen/xen-head.S"
	
	.section .bss, "aw", @nobits
	.align L1_CACHE_BYTES
ENTRY(idt_table)
	.skip IDT_ENTRIES * 16

	.align L1_CACHE_BYTES
ENTRY(nmi_idt_table)
	.skip IDT_ENTRIES * 16

	__PAGE_ALIGNED_BSS
NEXT_PAGE(empty_zero_page)
	.skip PAGE_SIZE
