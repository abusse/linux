/*
 * k1om forward-port to 3.0: temporary stubs for MIC platform bits whose full
 * definitions (Intel io_apic.c MIC irq-chip, micpm) are not yet ported to the
 * 3.0 driver/irq APIs. Linking these lets the tree build+link on 3.0; the card
 * interrupt/PM layer still needs a proper port before this boots on hardware.
 */
#include <linux/kernel.h>
#include <linux/module.h>

#ifndef CONFIG_MIC_PM
/* Normally defined by drivers/micpm/mic_pm.c (=1). With micpm disabled the MIC
 * elapsed-time-counter clocksource is off and the kernel falls back to TSC. */
int mic_etc_enabled;
EXPORT_SYMBOL(mic_etc_enabled);
#endif

/* Was arch/x86/kernel/apic/io_apic.c in the MPSS tree (2.6.38 irq API). */
void arch_setup_sbox_irqs(unsigned int *irqs, int n)
{
	static int warned;
	if (!warned) {
		pr_warn("k1om/3.0: arch_setup_sbox_irqs() not yet ported - SBOX IRQs unconfigured\n");
		warned = 1;
	}
}
