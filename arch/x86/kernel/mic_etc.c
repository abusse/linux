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
/*
 * mic_etc.c: Clocksource rotuines for the MIC SBox Elapsed Time Counter
 *
 * (C) Copyright 2009 Intel Corporation
  *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; version 2
 * of the License.
 *
 * 
  */

#include <linux/clocksource.h>
#include <linux/clockchips.h>
#include <linux/delay.h>
#include <linux/errno.h>
#include <linux/init.h>
#include <linux/slab.h>
#include <linux/cpu.h>

#include <asm/mic_def.h>

extern void __iomem  *mic_sbox_mmio_va;
static unsigned long etc_readl(void);


#define MIC_ETC_MASK			CLOCKSOURCE_MASK(64)
#define MIC_ETC_CLOCKSOURCE_RATING	250

/*
 * intel chipset (ioh) specification mentions
 * a PCIe SSC downspread of 0.5% at 30-33khz, since this is a triangular
 * wave, the effective downspread is 0.25% of ETC_TICKRATE, this value
 * is used as a max until we know better
 */
#define MAX_ETC_COMP		(ETC_TICK_RATE / 4 / 100)

#define	MIC_READ_MMIO_REG(var,offset)	\
              var.value = readl((const volatile void __iomem *)((unsigned long)(mic_sbox_mmio_va) +  (offset)));
 		
#define	MIC_WRITE_MMIO_REG(var,offset) \
	writel(var.value,(volatile void __iomem *)((unsigned long)(mic_sbox_mmio_va) +  (offset))); 	 

												
u64 mic_etc_read(struct clocksource *cs);
EXPORT_SYMBOL_GPL(mic_etc_read);

/*
 * Compensation applied to ETC since it may not tick @ ETC_TICK_RATE due to
 * PCIe SSC, this value is calculated by the host driver
 */
int etc_comp;
static int __init set_etc_comp(char *str)
{
	etc_comp = simple_strtol(str, NULL, 10);
	etc_comp = min(MAX_ETC_COMP, etc_comp);
	etc_comp = max(-MAX_ETC_COMP, etc_comp);
	return 1;
}

__setup("etc_comp=", set_etc_comp);

struct clocksource clocksource_micetc = {
	.name		= "micetc",
	.rating		= MIC_ETC_CLOCKSOURCE_RATING,
	.read		= mic_etc_read,
	.mask		= MIC_ETC_MASK,
	.flags		= CLOCK_SOURCE_IS_CONTINUOUS,
};

static unsigned long etc_readl(void)
{
	sboxElapsedTimeLowReg 	low1Reg,low2Reg;
	sboxElapsedTimeHighReg	hi1Reg,hi2Reg;
		
	do {
		MIC_READ_MMIO_REG(hi1Reg,SBOX_ELAPSED_TIME_HIGH);
		MIC_READ_MMIO_REG(low1Reg,SBOX_ELAPSED_TIME_LOW);
		MIC_READ_MMIO_REG(hi2Reg,SBOX_ELAPSED_TIME_HIGH);
	}while(hi1Reg.value != hi2Reg.value);	 	
	return((unsigned long)(((unsigned long)hi1Reg.value << 32)  | low1Reg.value));
}
/*
 *    mic_etc_read	- Read a 64-bit count from ETC which is made up of
 *    two 32 bit counts.		  
 */
u64 mic_etc_read(struct clocksource *cs)
{

	return((u64)etc_readl() >> 5);
}

/*
 *  Initialize MIC SBOX Elasped Time Counter	 
 */
static void mic_etc_init( u32 __iomem  *mmio_va)
{
	sboxThermalStatusInterruptReg	treg;
	unsigned long i=0;
	/* Reset ETC freeze bit  */
	treg.value = 0;
		
	MIC_WRITE_MMIO_REG(treg,SBOX_THERMAL_STATUS_INTERRUPT);		
	do {
		MIC_READ_MMIO_REG(treg,SBOX_THERMAL_STATUS_INTERRUPT);
	} while(treg.bits.etc_freeze && (i++ < 10));
	
	if(i >= 10) {
		printk(KERN_INFO"Failed to clear ETC freeze bit");
		return;
	}	 
			
	clocksource_register_hz(&clocksource_micetc, ETC_TICK_RATE + etc_comp);
	printk(KERN_INFO"Registering ETC as clocksource with mult = %u\n" ,clocksource_micetc.mult);
	printk(KERN_INFO"Registering ETC as clocksource with shift = %u\n" ,clocksource_micetc.shift);
	return;
}

void  __init mic_timer_init(void)
{
	if(!mic_sbox_mmio_va) {
		printk(KERN_INFO"Mapping MIC mmio in mic_timer_init");
		mic_sbox_mmio_va = ioremap(SBOX_BASE, MIC_SBOX_MMIO_SIZE);
		if(!mic_sbox_mmio_va) {
			printk(KERN_ERR"Failed to map SBOX MMIO space into kernel");
			return ;
		}	
	}
	mic_etc_init(mic_sbox_mmio_va);
}

