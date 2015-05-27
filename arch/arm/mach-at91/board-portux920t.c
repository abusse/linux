/*
 * linux/arch/arm/mach-at91/board-portux920t.c
 *
 *  Copyright (C) 2007 taskit GmbH
 *  Copyright (C) 2011 TU-Berlin KBS
 *
 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <linux/types.h>
#include <linux/init.h>
#include <linux/mm.h>
#include <linux/module.h>
#include <linux/platform_device.h>
#include <linux/spi/spi.h>
#include <linux/mtd/physmap.h>
#include <linux/clk.h>
#include <linux/portuxinput.h>

#include <mach/hardware.h>
#include <asm/setup.h>
#include <asm/mach-types.h>
#include <asm/irq.h>

#include <asm/mach/arch.h>
#include <asm/mach/map.h>
#include <asm/mach/irq.h>

#include <mach/board.h>
#include <mach/gpio.h>
#include <mach/cpu.h>

#include "generic.h"
#include "clock.h"

static void __init portux_init_early(void)
{
	/* Set cpu type: PQFP */
	at91rm9200_set_type(ARCH_REVISON_9200_PQFP);

	/* Initialize processor: 18.432 MHz crystal */
	at91_initialize(18432000);

	/* DBGU on ttyS0. (Rx & Tx only) */
	at91_register_uart(0, 0, 0);

	/* USART0 on ttyS1 (Rx, Tx, CTS, RTS) */
	at91_register_uart(AT91RM9200_ID_US0, 1, ATMEL_UART_CTS | ATMEL_UART_RTS);

	/* USART1 on ttyS2 (Rx, Tx, CTS, RTS, DTR, DSR, DCD, RI) */
	at91_register_uart(AT91RM9200_ID_US1, 2, ATMEL_UART_CTS | ATMEL_UART_RTS
			   | ATMEL_UART_DTR | ATMEL_UART_DSR | ATMEL_UART_DCD
			   | ATMEL_UART_RI);

	/* USART0 on ttyS3 (Rx, Tx, CTS, RTS) */
	at91_register_uart(AT91RM9200_ID_US2, 3, 0);

	/* USART3 on ttyS4 (Rx, Tx, CTS, RTS) */
	at91_register_uart(AT91RM9200_ID_US3, 4, ATMEL_UART_CTS | ATMEL_UART_RTS);
	
	/* set serial console to ttyS0 (ie, DBGU) */
	at91_set_serial_console(0);
}

static struct macb_platform_data __initdata portux_eth_data = {
	.phy_irq_pin	= AT91_PIN_PC4,
	.is_rmii	= 1,
};

static struct at91_usbh_data __initdata portux_usbh_data = {
	.ports		= 1,
	.vbus_pin	= {-EINVAL, -EINVAL},
	.overcurrent_pin= {-EINVAL, -EINVAL},
};

static struct at91_udc_data __initdata portux_udc_data = {
	.vbus_pin	= AT91_PIN_PC5,
	.pullup_pin	= AT91_PIN_PB17,
};

static struct at91_mmc_data __initdata portux_mmc_data = {
	.slot_b		= 0,
	.wire4		= 0,
	.det_pin	= -EINVAL,
	.wp_pin		= -EINVAL,
	.vcc_pin	= -EINVAL,
};

#define PORTUX_FLASH_BASE	AT91_CHIPSELECT_0
#define PORTUX_FLASH_SIZE	0x1000000

static struct physmap_flash_data portux_flash_data = {
	.width		= 2,
};

static struct resource portux_flash_resource = {
	.start		= PORTUX_FLASH_BASE,
	.end		= PORTUX_FLASH_BASE + PORTUX_FLASH_SIZE - 1,
	.flags		= IORESOURCE_MEM,
};

static struct platform_device portux_flash = {
	.name		= "physmap-flash",
	.id		= 0,
	.dev		= {
				.platform_data	= &portux_flash_data,
			},
	.resource	= &portux_flash_resource,
	.num_resources	= 1,
};

#if defined (CONFIG_INPUT_PORTUXMATRIX) || defined (CONFIG_INPUT_PORTUXMATRIX_MODULE) || \
    defined (CONFIG_INPUT_PORTUXIR)     || defined (CONFIG_INPUT_PORTUXIR_MODULE)
static keyarray portuxinput_keyarraymap={
	{{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},
	 {0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}},
	{{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},
	 {0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}},
	{{0,KEY_A},{0,KEY_B},{0,KEY_C},{0,0},{0,0},{0,0},{0,0},{0,0},
	 {0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}},
	{{0,KEY_D},{0,KEY_E},{0,KEY_F},{0,0},{0,0},{0,0},{0,0},{0,0},
	 {0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}},
	{{0,KEY_G},{0,KEY_H},{0,KEY_I},{0,0},{0,0},{0,0},{0,0},{0,0},
	 {0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}},
	{{0,KEY_J},{0,KEY_K},{0,KEY_L},{0,0},{0,0},{0,0},{0,0},{0,0},
	 {0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}},
	{{0,KEY_M},{0,KEY_N},{0,KEY_O},{0,0},{0,0},{0,0},{0,0},{0,0},
	 {0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}},
	{{0,KEY_P},{0,KEY_Q},{0,KEY_R},{0,KEY_S},{0,0},{0,0},{0,0},{0,0},
	 {0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}},
	{{0,KEY_T},{0,KEY_U},{0,KEY_V},{0,0},{0,0},{0,0},{0,0},{0,0},
	 {0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}},
	{{0,KEY_W},{0,KEY_X},{0,KEY_Y},{0,KEY_Z},{0,0},{0,0},{0,0},{0,0},
	 {0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}},
};
#endif

#if defined(CONFIG_INPUT_PORTUXMATRIX) || defined(CONFIG_INPUT_PORTUXMATRIX_MODULE)
static matrixmap portuxinput_matrixmap={{
	{{PORTUXINPUT_MASK_SETKEYMAP,1},{0,KEY_ESC},{0,KEY_UP},{0,KEY_ENTER},{0,0},{0,0},{0,0},{0,0}},
	{{0,KEY_F1},{0,KEY_LEFT},{0,KEY_DOWN},{0,KEY_RIGHT},{0,0},{0,0},{0,0},{0,0}},
	{{0,KEY_F2},{0,KEY_1},{0,KEY_2},{0,KEY_3},{0,0},{0,0},{0,0},{0,0}},
	{{0,KEY_F3},{0,KEY_4},{0,KEY_5},{0,KEY_6},{0,0},{0,0},{0,0},{0,0}},
	{{0,KEY_F4},{0,KEY_7},{0,KEY_8},{0,KEY_9},{0,0},{0,0},{0,0},{0,0}},
	{{0,KEY_F5},{0,0},{0,KEY_0},{0,0},{0,0},{0,0},{0,0},{0,0}},
	{{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}},
	{{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}}
},{
	{{PORTUXINPUT_MASK_SETKEYMAP,0},{0,KEY_ESC},{0,KEY_UP},{0,KEY_ENTER},{0,0},{0,0},{0,0},{0,0}},
	{{0,KEY_F1},{0,KEY_LEFT},{0,KEY_DOWN},{0,KEY_RIGHT},{0,0},{0,0},{0,0},{0,0}},
	{{0,KEY_F2},{0,KEY_SPACE},{PORTUXINPUT_MASK_KEYARRAY,2},{PORTUXINPUT_MASK_KEYARRAY,3},{0,0},{0,0},{0,0},{0,0}},
	{{0,KEY_F3},{PORTUXINPUT_MASK_KEYARRAY,4},{PORTUXINPUT_MASK_KEYARRAY,5},{PORTUXINPUT_MASK_KEYARRAY,6},{0,0},{0,0},{0,0},{0,0}},
	{{0,KEY_F4},{PORTUXINPUT_MASK_KEYARRAY,7},{PORTUXINPUT_MASK_KEYARRAY,8},{PORTUXINPUT_MASK_KEYARRAY,9},{0,0},{0,0},{0,0},{0,0}},
	{{0,KEY_F5},{0,0},{0,KEY_0},{0,KEY_CAPSLOCK},{0,0},{0,0},{0,0},{0,0}},
	{{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}},
	{{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}}
}};
#endif

#if defined (CONFIG_INPUT_PORTUXIR) || defined(CONFIG_INPUT_PORTUXIR_MODULE)
static irmap portuxinput_irmap={{
	{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},
	{0,0},{0,0},{0,0},{0,0},{0,KEY_8},{0,0},{0,0},{0,0},{0xc2,10},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},
	{0,0},{0,0},{0,0},{0,0},{0,KEY_4},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},
	{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,BTN_MOUSE},{0,0},{0,0},{0,0},
	{0,0},{0,0},{0xc1,-10},{0,0},{0,KEY_2},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},
	{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},
	{0,0},{0,0},{0,0},{0,0},{0,KEY_6},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},
	{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},
	{0,0},{0,0},{0xc1,10},{0,0},{0,KEY_1},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},
	{0,0},{0,0},{0,0},{0,0},{0,KEY_9},{0,0},{0,0},{0,0},{0xc2,-10},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},
	{0,0},{0,0},{0,0},{0,0},{0,KEY_5},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},
	{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},
	{0,0},{0,0},{0,0},{0,0},{0,KEY_3},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},
	{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},
	{0,0},{0,0},{0,0},{0,0},{0,KEY_7},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},
	{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}
}};
#endif

#if defined(CONFIG_INPUT_PORTUXINPUT) || defined(CONFIG_INPUT_PORTUXINPUT_MODULE)
static struct portuxinput_data portux_portuxinput_data = {
	.irq = AT91RM9200_ID_IRQ5,
#if defined (CONFIG_INPUT_PORTUXMATRIX) || defined (CONFIG_INPUT_PORTUXMATRIX_MODULE) || \
    defined (CONFIG_INPUT_PORTUXIR)     || defined (CONFIG_INPUT_PORTUXIR_MODULE)
	.keyarraymap = &portuxinput_keyarraymap,
#endif
#if defined(CONFIG_INPUT_PORTUXMATRIX) || defined(CONFIG_INPUT_PORTUXMATRIX_MODULE)
	.matrixkeymap = &portuxinput_matrixmap,
#endif
#if defined (CONFIG_INPUT_PORTUXIR) || defined(CONFIG_INPUT_PORTUXIR_MODULE)
	.irkeymap = &portuxinput_irmap,
#endif
};

static struct platform_device portux_portuxinput_device = {
	.name	= PORTUXINPUT_DEVNAME,
	.id	= -1,
	.dev = {
		.platform_data = &portux_portuxinput_data,
	},
	.num_resources	= 0,
};
#endif
#ifdef CONFIG_FBCON_EPSON_S1D13706
int s1d13xxxfb_init(char *dummy);

static void __init portux_init_video(void)
{
	struct clk *pck0, *pck1, *pllb;
	char dummy;
	
	at91_set_A_periph(AT91_PIN_PC6,0);

	at91_set_B_periph(AT91_PIN_PA24,0);
	at91_set_A_periph(AT91_PIN_PB27,0);

	pck0 = clk_get(0, "pck0");
	pck1 = clk_get(0, "pck1");
	pllb = clk_get(0, "pllb");

	clk_set_parent(pck0, pllb);
	clk_set_parent(pck1, pllb);
	clk_set_rate(pck0, 24000000);
	clk_set_rate(pck1, 24000000);
	clk_enable(pck0);
	clk_enable(pck1);

	at91_sys_write(AT91_SMC_CSR(2), 0x2300208F);
	at91_set_gpio_output(AT91_PIN_PC2, 0);
	at91_set_gpio_output(AT91_PIN_PC3, 1);

	s1d13xxxfb_init(&dummy);
}
#else
static void __init portux_init_video(void) {}
#endif

#ifdef CONFIG_W1_MASTER_AT91GPIO
static struct at91_gpio_w1_data portux_w1_data = {
	.pin = AT91_PIN_PB16,
};

static struct platform_device portux_w1_device = {
	.name	= "w1_at91gpio",
	.id	= -1,
	.dev = {
		.platform_data = &portux_w1_data,
	},
	.num_resources	= 0,
};

static void __init portux_add_w1(void)
{
	platform_device_register(&portux_w1_device);
}
#else
static void __init portux_add_w1(void) {}
#endif

static struct spi_board_info portux_spi_devices[] = {
	{ /* User accessible spi - cs1 (1Mhz) */
		.modalias = "spidev",
		.chip_select  = 1,
		.max_speed_hz = 1000 *  1000,
	},
	{ /* User accessible spi - cs2 (1MHz) */
		.modalias = "spidev",
		.chip_select  = 2,
		.max_speed_hz = 1 * 1000 *  1000,
	},
	{ /* User accessible spi - cs3 (10MHz) */
		.modalias = "spidev",
		.chip_select  = 3,
		.max_speed_hz = 10 * 1000 *  1000,
	},
};

static struct i2c_board_info portux_i2c_devices[] = {
	{
			I2C_BOARD_INFO("isl1226", 0x6F)
	}
};

/*
 * LEDs
 */
static struct gpio_led portux_leds[] = {
	{
		.name	= "yellow",
		.gpio	= AT91_PIN_PB27,
	},
	{
		.name	= "red",
		.gpio	= AT91_PIN_PC0,
	},
	{
		.name	= "green",
		.gpio	= AT91_PIN_PC1,
	},
};

static void __init portux_board_init(void)
{
	/* Serial */
	at91_add_device_serial();
	at91_set_B_periph(AT91_PIN_PB0,0); /* RTS3 */
	at91_set_B_periph(AT91_PIN_PB1,0); /* CTS3 */
	/* Ethernet */
	at91_add_device_eth(&portux_eth_data);
	/* USB Host */
	at91_add_device_usbh(&portux_usbh_data);
	/* USB Device */
	at91_add_device_udc(&portux_udc_data);
	at91_set_multi_drive(portux_udc_data.pullup_pin, 1);	/* pullup_pin is connected to reset */
	/* I2C */
	at91_add_device_i2c(NULL, 0);
	/* SPI */
	at91_add_device_spi(portux_spi_devices, 3);
	/* MMC */
	at91_add_device_mmc(0, &portux_mmc_data);
	/* LEDs */
	at91_gpio_leds(portux_leds, ARRAY_SIZE(portux_leds));
	/* NOR Flash */
	platform_device_register(&portux_flash);
	/* RTC */
	i2c_register_board_info(0, portux_i2c_devices, ARRAY_SIZE(portux_i2c_devices));

	portux_init_video();
	
	portux_add_w1();

#if defined(CONFIG_INPUT_PORTUXINPUT) || defined(CONFIG_INPUT_PORTUXINPUT_MODULE)
	at91_set_B_periph(AT91_PIN_PA3, 0);
	set_irq_type(((struct portuxinput_data*)portux_portuxinput_device.dev.platform_data)->irq, IRQT_RISING);
	platform_device_register(&portux_portuxinput_device);
#endif
}

MACHINE_START(PORTUX920T, "Portux920T")
	/* Maintainer: taskit GmbH */
	.timer		= &at91rm9200_timer,
	.map_io		= at91_map_io,
	.init_early	= portux_init_early,
	.init_irq	= at91_init_irq_default,
	.init_machine	= portux_board_init,
MACHINE_END
