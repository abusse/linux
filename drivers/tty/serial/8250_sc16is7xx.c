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

#include <linux/serial_reg.h>
#include <linux/moduleparam.h>
#include <linux/delay.h>
#include <linux/hardirq.h>
#include <linux/rtmutex.h>
#include <linux/module.h>
#include <linux/i2c.h>
#include <linux/console.h>
#include "8250/8250.h"
#ifdef	CONFIG_KDB
#include <linux/kdb.h>
#endif

/* Easier for compile time module, we don't actually need a UPIO_I2C */
#ifndef SC16IS_COMPILETIME

#if 0
static char *test_regs_wr[] = {
	"UART_TX/DLL",
	"UART_IER/DLM",
	"UART_FCR",
	"UART_LCR",
	"UART_MCR",
	"UART_BAD",
	"UART_BAD",
	"UART_SCR"
};

static char *test_regs_rd[] = {
	"UART_RX/DLL",
	"UART_IER/DLM",
	"UART_IIR",
	"UART_LCR",
	"UART_MCR",
	"UART_LSR",
	"UART_MSR",
	"UART_SCR"
};
#endif

/*
 * The 8250 core calls these with the port spinlock held (IRQs off) and,
 * for console output, from any context. i2c_transfer() then only
 * rt_mutex_trylock()s the adapter, which is forbidden (WARN) in hard/soft
 * irq context and fails whenever another context owns the bus - and a
 * dropped access silently corrupts 8250 state (a lost IER write stalls TX
 * for good). So drive the adapter's master_xfer() directly under a private
 * spinlock (all sc16is transfers serialize on it; the KNC i2c-pxa transfer
 * is polled and safe in atomic context) and take the i2c core lock on top
 * of it only where that is allowed, with a bounded wait, to keep other
 * core users (i2c-dev) off the bus meanwhile.
 */
#define SC16IS_RETRIES	250	/* x 20 us = 5 ms */
static DEFINE_SPINLOCK(sc16is_lock);
static int sc16is_retries, sc16is_rd_fail, sc16is_wr_fail;
static int sc16is_unlocked, sc16is_timeout;
module_param_named(retries, sc16is_retries, int, 0444);
module_param_named(rd_fail, sc16is_rd_fail, int, 0444);
module_param_named(wr_fail, sc16is_wr_fail, int, 0444);
module_param_named(unlocked, sc16is_unlocked, int, 0444);
module_param_named(timeout, sc16is_timeout, int, 0444);

static int sc16is_xfer(struct i2c_client *client, struct i2c_msg *msgs, int num)
{
	struct i2c_adapter *adap = client->adapter;
	bool core_lock = !(in_irq() || in_nmi() || in_serving_softirq());
	int tries = SC16IS_RETRIES;
	unsigned long flags;
	int locked = 0, ret;

	if (core_lock) {
		while (!(locked = rt_mutex_trylock(&adap->bus_lock)) && tries--) {
			sc16is_retries++;
			udelay(20);
		}
		if (!locked)
			sc16is_timeout++;
	} else {
		sc16is_unlocked++;
	}

	spin_lock_irqsave(&sc16is_lock, flags);
	ret = adap->algo->master_xfer(adap, msgs, num);
	spin_unlock_irqrestore(&sc16is_lock, flags);

	if (locked)
		rt_mutex_unlock(&adap->bus_lock);
	if (ret < 0)
		return ret;
	return ret == num ? 0 : -EIO;
}

unsigned int sc16is_serial_in(struct uart_port *port, int offset)
{
	struct i2c_client *client = port->private_data;
	u8 reg = (u8)offset << port->regshift, data;
	struct i2c_msg msgs[2] = {
		{ .addr = client->addr, .flags = client->flags & I2C_M_TEN,
		  .len = 1, .buf = &reg },
		{ .addr = client->addr,
		  .flags = (client->flags & I2C_M_TEN) | I2C_M_RD,
		  .len = 1, .buf = &data },
	};

	if (!sc16is_xfer(client, msgs, 2))
		return data;
	sc16is_rd_fail++;
	/* Harmless stand-ins instead of the errno's low byte. */
	switch (offset) {
	case UART_IIR:
		return UART_IIR_NO_INT;
	case UART_LSR:
		return UART_LSR_THRE | UART_LSR_TEMT;
	default:
		return 0;
	}
}

void sc16is_serial_out(struct uart_port *port, int offset, int value)
{
	struct i2c_client *client = port->private_data;
	u8 buf[2] = { (u8)offset << port->regshift, (u8)value };
	struct i2c_msg msg = { .addr = client->addr,
			       .flags = client->flags & I2C_M_TEN,
			       .len = 2, .buf = buf };

	if (sc16is_xfer(client, &msg, 1))
		sc16is_wr_fail++;
}
#endif

extern struct uart_port kdb_uart_port;
extern int kdb_serial_line;

static int sc16is7xx_probe(struct i2c_client *client,
		const struct i2c_device_id *id)
{
	struct uart_port port;
	struct uart_8250_port up;
	int error = 0;

	/* i2c-pxa instantiates both possible addresses; skip the empty one. */
	if (i2c_smbus_write_byte_data(client, UART_SCR << 3, 0x5a) < 0 ||
	    i2c_smbus_read_byte_data(client, UART_SCR << 3) != 0x5a)
		return -ENODEV;

	memset(&port, 0, sizeof(struct uart_port));
	/* Setup the port data structure */
#ifdef SC16IS_COMPILETIME
	/* TBD if we do this at all */
	port.iotype = UPIO_I2C;
#else
#define UPIO_SC16IS 0xff
	port.iotype = UPIO_SC16IS;
#endif
	port.serial_in = &sc16is_serial_in;
	port.serial_out = &sc16is_serial_out;
	port.irq = 0;
	port.uartclk = 1843200;
	port.flags = UPF_BOOT_AUTOCONF | UPF_SKIP_TEST;  /* TODO: need to study flags */
	port.fifosize = 64;
	port.regshift = 3;
	port.private_data = client;
	port.iobase = 0x800000000UL;
	port.mapbase = 0x800000000UL;
	port.membase = (char *)0x800000000UL;
	//if (early_serial_setup(&port) < 0) {
	//	printk(KERN_INFO "BAW fail 1\n");
	//}
	memset(&up, 0, sizeof(up));
	up.port = port;
	error = serial8250_register_8250_port(&up);
	printk(KERN_INFO "8250 UART MIC Port error %d\n", error);

#ifdef	CONFIG_KDB

	/*
	 * Remember the line number of the first serial
	 * console.  We'll make this the kdb serial console too.
	 */
	//printk("Configuring KDB\n");

//	if (co && kdb_serial_line == -1) {
		kdb_serial_line = port.line;
		kdb_serial.io_type = port.iotype;
		switch (port.iotype) {
		case SERIAL_IO_MEM:
#ifdef  SERIAL_IO_MEM32
		case SERIAL_IO_MEM32:
#endif
			kdb_serial.iobase = (unsigned long)(port.membase);
			kdb_serial.ioreg_shift = port.regshift;
			break;
		default:
			kdb_serial.iobase = port.iobase;
			kdb_serial.ioreg_shift = port.regshift;
			break;
		}
		//printk("  kdb_serial.io_type = %d\n", kdb_serial.io_type);
		//printk("  kdb_serial.iobase = 0x%lx\n", kdb_serial.iobase);
		//printk("  kdb_serial.ioreg_shift = %d\n", kdb_serial.ioreg_shift);

	memcpy(&kdb_uart_port, &port, sizeof(port));
//	}
#endif	/* CONFIG_KDB */

	return error;
}

static const struct i2c_device_id sc16is7xx_id[] = {
	{ "sc16is7xx", 0 },
	{ "sc16is740", 0 },
	{ }
};

extern int sc16is7xx_probe(struct i2c_client *client,
		const struct i2c_device_id *id);

static struct i2c_driver sc16is7xx_driver = {
	.driver = {
		.name	= "sc16is7xx",
	},
	.probe		= sc16is7xx_probe,
	.remove		= NULL, /* TODO: */
	.id_table	= sc16is7xx_id,
};

static int __init sc16is7xx_init(void)
{
	int error = i2c_add_driver(&sc16is7xx_driver);
	printk(KERN_INFO "sc16is7xx_init adding driver %d\n", error);
	return error;
}

module_init(sc16is7xx_init);

static void __exit sc16is7xx_exit(void)
{
	i2c_del_driver(&sc16is7xx_driver);
}
module_exit(sc16is7xx_exit);
