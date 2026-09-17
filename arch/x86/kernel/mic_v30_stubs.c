/*
 * k1om forward-port to 3.0: config-dependent MIC symbol.
 * arch_setup_sbox_irqs() is now fully implemented in apic/io_apic.c.
 */
#include <linux/kernel.h>
#include <linux/module.h>

#ifndef CONFIG_MIC_PM
/* Normally provided by drivers/micpm/mic_pm.c (=1). With micpm disabled the MIC
 * elapsed-time-counter clocksource is off and the kernel uses TSC. */
int mic_etc_enabled;
EXPORT_SYMBOL(mic_etc_enabled);
#endif
