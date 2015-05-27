/*
 * Intersil ISL1226 rtc class driver
 *
 * Copyright 2006 Hebert Valerio Riedel <hvr@gnu.org>
 * Copyright 2007 Achim Ehrlich <aehrlich@taskit.de>
 *  This program is free software; you can redistribute  it and/or modify it
 *  under  the terms of  the GNU General  Public License as published by the
 *  Free Software Foundation;  either version 2 of the  License, or (at your
 *  option) any later version.
 *
 */

#include <linux/module.h>
#include <linux/i2c.h>
#include <linux/bcd.h>
#include <linux/rtc.h>
#include <linux/slab.h>

#include <linux/kernel.h>


#define DRV_NAME "isl1226"
#define DRV_VERSION "0.1"

/* Register map */
/* rtc section */
#define ISL1226_REG_SC  0x30
#define ISL1226_REG_MN  0x31
#define ISL1226_REG_HR  0x32
#define ISL1226_REG_HR_MIL     (1<<7) /* 24h/12h mode */
#define ISL1226_REG_HR_PM      (1<<5) /* PM/AM bit in 12h mode */
#define ISL1226_REG_DT  0x33
#define ISL1226_REG_MO  0x34
#define ISL1226_REG_YR  0x35
#define ISL1226_REG_DW  0x36
#define ISL1226_REG_Y2K 0x37
#define ISL1226_RTC_SECTION_LEN 8

/* control/status section */
#define ISL1226_REG_SR  0x3f
#define ISL1226_REG_SR_ARST    (1<<7) /* auto reset */
#define ISL1226_REG_SR_RWEL    (1<<2) /* RWEL write access to control registers*/
#define ISL1226_REG_SR_WRTC    (1<<1) /* write rtc */
#define ISL1226_REG_SR_AL0     (1<<5) /* alarm */
#define ISL1226_REG_SR_AL1     (1<<6) /* alarm */
#define ISL1226_REG_SR_BAT     (1<<7) /* battery */
#define ISL1226_REG_SR_RTCF    (1<<0) /* rtc fail */
#define ISL1226_REG_INT 0x11
#define ISL1226_REG_09  0x09 /* reserved */
#define ISL1226_REG_ATR 0x12
#define ISL1226_REG_DTR 0x13
#define ISL1226_REG_PWR 0x14
#define ISL1226_REG_PWR_SBIB (1<<7) /*serial bus enable*/
#define ISL1226_REG_PWR_BSW (1<<6)  /*power control bit*/

/* alarm 0 section */
#define ISL1226_REG_SCA0 0x00
#define ISL1226_REG_MNA0 0x01
#define ISL1226_REG_HRA0 0x02
#define ISL1226_REG_DTA0 0x03
#define ISL1226_REG_MOA0 0x04
#define ISL1226_REG_DWA0 0x06
#define ISL1226_ALARM_SECTION_LEN 8

/* alarm 1 section */
#define ISL1226_REG_SCA1 0x08
#define ISL1226_REG_MNA1 0x09
#define ISL1226_REG_HRA1 0x0A
#define ISL1226_REG_DTA1 0x0B
#define ISL1226_REG_MOA1 0x0C
#define ISL1226_REG_DWA1 0x0E
#define ISL1226_ALARM_SECTION_LEN 8

#define ISL1226_MAX_MSG_LEN	12

/* i2c configuration */
#define ISL1226_I2C_ADDR 0x6F
#define ISL1226_I2C_ADDR_EEPROM 0x00 

static struct i2c_driver isl1226_driver;

/*
static struct i2c_driver isl1226_driver = {
	.driver		= {
		.name	= DRV_NAME,
	},
	.id		= I2C_DRIVERID_ISL1226,
	.attach_adapter = &isl1226_attach_adapter,
	.detach_client	= &isl1226_detach_client,
};*/

/* block read */
static int
isl1226_i2c_read_regs(struct i2c_client *client, u16 reg, u8 buf[],
		       unsigned len)
{
	u8 reg_addr[2] = {(reg >> 8) & 0xff, reg & 0xff };
	u8 i2c_buf[ISL1226_MAX_MSG_LEN];
	struct i2c_msg msgs[2] = {
		{ client->addr, 0, 2, reg_addr },
		{ client->addr, I2C_M_RD, len+1, i2c_buf}
	};
	int ret;
	
	memset (i2c_buf, 0, ISL1226_MAX_MSG_LEN);
	BUG_ON(len == 0);
	BUG_ON(reg > ISL1226_REG_SR);
	BUG_ON(reg + len > ISL1226_REG_SR + 1);

	ret = i2c_transfer(client->adapter, msgs, 2);
	if(ret == 2)
		memcpy(&buf[0], &i2c_buf[0], len);
	return ret;
}

/* block write */
static int
isl1226_i2c_set_regs(struct i2c_client *client, u16 reg, u8 const buf[],
		       unsigned len)
{
	u8 i2c_buf[ISL1226_MAX_MSG_LEN];
	
	struct i2c_msg msgs[1] = {
		{ client->addr, client->flags, len + 2, i2c_buf }
	};
	int ret;
	
	i2c_buf[0] = (u8) ((reg >> 8) & 0xff);
	i2c_buf[1] = (u8) (reg & 0xff);

	BUG_ON(len == 0);
	BUG_ON(reg > ISL1226_REG_SR);
	BUG_ON(reg + len > ISL1226_REG_SR + 2);

	memcpy(&i2c_buf[2], &buf[0], len);

	ret = i2c_transfer(client->adapter, msgs, 1);
	if (ret > 0)
		ret = 0;
	return ret;
}

static int isl1226_i2c_get_sr(struct i2c_client *client)
{
	u8 sr;
	int ret;
	ret = isl1226_i2c_read_regs(client, ISL1226_REG_SR, &sr, 1);
	if (ret > 0)
		return sr;
	return ret;
}

static int isl1226_i2c_get_atr(struct i2c_client *client)
{
	int atr = i2c_smbus_read_byte_data(client, ISL1226_REG_ATR);

	if (atr < 0)
		return -EIO;

	/* The 6bit value in the ATR register controls the load
	 * capacitance C_load * in steps of 0.25pF
	 *
	 * bit (1<<5) of the ATR register is inverted
	 *
	 * C_load(ATR=0x20) =  4.50pF
	 * C_load(ATR=0x00) = 12.50pF
	 * C_load(ATR=0x1f) = 20.25pF
	 *
	 */

	atr &= 0x3f; /* mask out lsb */
	atr ^= 1<<5; /* invert 6th bit */
	atr += 2*9; /* add offset of 4.5pF; unit[atr] = 0.25pF */

	return atr;
}

static int isl1226_i2c_get_dtr(struct i2c_client *client)
{
	int dtr = i2c_smbus_read_byte_data(client, ISL1226_REG_DTR);

	if (dtr < 0)
		return -EIO;

	/* dtr encodes adjustments of {-60,-40,-20,0,20,40,60} ppm */
	dtr = ((dtr & 0x3) * 20) * (dtr & (1<<2) ? -1 : 1);

	return dtr;
}



static int isl1226_write_enable(struct i2c_client *client)
{
	int ret;
        u8 sr;

	sr = ISL1226_REG_SR_WRTC;
	ret = isl1226_i2c_set_regs(client, ISL1226_REG_SR, &sr, 1);
	if (ret < 0) {
		dev_err(&client->dev, "%s: writing SR failed\n", __func__);
		return ret;
	}
	
	sr |= ISL1226_REG_SR_RWEL;
	ret = isl1226_i2c_set_regs(client, ISL1226_REG_SR, &sr, 1);
	if (ret < 0) {
		dev_err(&client->dev, "%s: writing SR failed\n", __func__);
		return ret;
	}
	sr = isl1226_i2c_get_sr(client);
	sr &= (ISL1226_REG_SR_RWEL | ISL1226_REG_SR_RWEL);
	if (sr != (ISL1226_REG_SR_RWEL | ISL1226_REG_SR_RWEL)) {
		dev_err(&client->dev, "verify SR failed\n");
		return -ENXIO;
	}
	return ret;
}


static int isl1226_write_disable(struct i2c_client *client)
{
	int ret;
	u8 sr;
	
// sr is return value, so wtf???
//    sr &= (~ISL1226_REG_SR_RWEL & ~ISL1226_REG_SR_WRTC);
	ret = isl1226_i2c_set_regs(client, ISL1226_REG_SR, &sr, 1);
	
	if (ret < 0) {
		dev_err(&client->dev, "%s: writing SR failed\n", __func__);
		return ret;
	}
        return ret;
}

/*Clearing the BSW Bit is necessary for Intersil 12026 to avoid the RTC run in Batterymode when 
VDD is < VBAT. With cleared bit the RTC switches to Batterymode when VDD > 2.2V. ISL122026 disables
the serial bus per default and writing to RTC is not possible, when running in battery mode. Setting 
the bit is not possible for ISL1226 nor neccessary*/

static int isl1226_set_bsw(struct i2c_client *client)
{
	int ret;
	u8 pwr = 0;
	
	ret = isl1226_i2c_read_regs(client, ISL1226_REG_PWR, &pwr, 1);
	if (ret < 0) {
		dev_err(&client->dev, "%s: reading PWR failed\n", __func__);
		return ret;
	}
	if (pwr & 0x01)
		dev_err (&client->dev, "RTC is a 1226: %02X\n", pwr);
	else if (pwr & ISL1226_REG_PWR_BSW){
		pwr &= ~ISL1226_REG_PWR_BSW;
		isl1226_write_enable(client);
		ret = isl1226_i2c_set_regs(client, ISL1226_REG_PWR, &pwr, 1);
		isl1226_write_disable(client);
		pwr = 0;
		isl1226_i2c_read_regs(client, ISL1226_REG_PWR, &pwr, 1); 
		dev_err (&client->dev, "Clearing BSW: %02X\n", pwr);
	} else 
		dev_err (&client->dev, "BSW bit already cleared: %02X\n", pwr);
	return ret;
}

static int isl1226_rtc_proc(struct device *dev, struct seq_file *seq)
{
	struct i2c_client *const client = to_i2c_client(dev);
	int sr, dtr, atr;

	sr = isl1226_i2c_get_sr(client);
	if (sr < 0) {
		dev_err(&client->dev, "%s: reading SR failed\n", __func__);
		return sr;
	}

	seq_printf(seq, "status_reg\t:%s%s%s%s%s%s%s (0x%.2x)\n",
		   (sr & ISL1226_REG_SR_RTCF) ? " RTCF" : "",
		   (sr & ISL1226_REG_SR_BAT) ? " BAT" : "",
		   (sr & ISL1226_REG_SR_AL0) ? " ALM 0" : "",
		   (sr & ISL1226_REG_SR_AL1) ? " ALM 1" : "",
		   (sr & ISL1226_REG_SR_WRTC) ? " WRTC" : "",
		   (sr & ISL1226_REG_SR_RWEL) ? " RWEL" : "",
		   (sr & ISL1226_REG_SR_ARST) ? " ARST" : "",
		   sr);

	seq_printf(seq, "batt_status\t: %s\n",
		   (sr & ISL1226_REG_SR_RTCF) ? "bad" : "okay");

	dtr = isl1226_i2c_get_dtr(client);
	if (dtr >= 0 -1)
		seq_printf(seq, "digital_trim\t: %d ppm\n", dtr);

	atr = isl1226_i2c_get_atr(client);
	if (atr >= 0)
		seq_printf(seq, "analog_trim\t: %d.%.2d pF\n",
			   atr>>2, (atr&0x3)*25);

	return 0;
}


static int isl1226_i2c_read_time(struct i2c_client *client,
				 struct rtc_time *tm)
{
	/* read RTC registers */
	
	int sr;
	u8 regs[ISL1226_RTC_SECTION_LEN] = { 0, };

	sr = isl1226_i2c_get_sr(client);
	if (sr < 0) {
		dev_err(&client->dev, "%s: reading SR failed\n", __func__);
		return -EIO;
	}
	
	sr = isl1226_i2c_read_regs(client, ISL1226_REG_SC, regs, 4);
	if (sr < 2) {
		dev_err(&client->dev, "%s %i: reading RTC section failed\n",
			__func__, sr);
		return sr;
	}
	sr = isl1226_i2c_read_regs(client, ISL1226_REG_SC+4, &regs[4], ISL1226_RTC_SECTION_LEN);
	if (sr < 2) {
		dev_err(&client->dev, "%s %i: reading RTC section failed\n",
			__func__, sr);
		return sr;
	}
	
	tm->tm_sec = bcd2bin(regs[ISL1226_REG_SC - 0x30]);
	tm->tm_min = bcd2bin(regs[ISL1226_REG_MN - 0x30]);
	tm->tm_hour  = bcd2bin(regs[ISL1226_REG_HR - 0x30] & ~(ISL1226_REG_HR_MIL));
	if (!(regs[ISL1226_REG_HR - 0x30] & (ISL1226_REG_HR_MIL))) {                    /* AM/PM 1-12 format, convert to MIL */
                tm->tm_hour--;                          /* 0 - 11 */
		if (regs[ISL1226_REG_HR - 0x30] & (ISL1226_REG_HR_PM))
			tm->tm_hour += 12;              /* PM */
	}
	tm->tm_mday = bcd2bin(regs[ISL1226_REG_DT -0x30]);
	tm->tm_mon = bcd2bin(regs[ISL1226_REG_MO - 0x30]) - 1; /* rtc starts at 1 */
	tm->tm_year = bcd2bin(regs[ISL1226_REG_YR - 0x30]) + (100 * bcd2bin(regs[ISL1226_REG_Y2K - 0x30]) - 1900);
	return 0;
}

static int isl1226_i2c_read_alarm(struct i2c_client *client,
				  struct rtc_wkalrm *alarm)
{
	struct rtc_time *const tm = &alarm->time;
	u8 regs[ISL1226_ALARM_SECTION_LEN] = { 0, };
	int sr;

	sr = isl1226_i2c_get_sr(client);
	if (sr < 0) {
		dev_err(&client->dev, "%s: reading SR failed\n", __func__);
		return sr;
	}

	sr = isl1226_i2c_read_regs(client, ISL1226_REG_SCA0, regs,
				  ISL1226_ALARM_SECTION_LEN);
	if (sr < 0) {
		dev_err(&client->dev, "%s: reading alarm section failed\n",
			__func__);
		return sr;
	}

	/* MSB of each alarm register is an enable bit */
	tm->tm_sec  = bcd2bin(regs[ISL1226_REG_SCA0-ISL1226_REG_SCA0] & 0x7f);
	tm->tm_min  = bcd2bin(regs[ISL1226_REG_MNA0-ISL1226_REG_SCA0] & 0x7f);
	tm->tm_hour = bcd2bin(regs[ISL1226_REG_HRA0-ISL1226_REG_SCA0] & 0x3f);
	tm->tm_mday = bcd2bin(regs[ISL1226_REG_DTA0-ISL1226_REG_SCA0] & 0x3f);
	tm->tm_mon  = bcd2bin(regs[ISL1226_REG_MOA0-ISL1226_REG_SCA0] & 0x1f)-1;
	tm->tm_wday = bcd2bin(regs[ISL1226_REG_DWA0-ISL1226_REG_SCA0] & 0x03);

	return 0;
}

static int isl1226_rtc_read_time(struct device *dev, struct rtc_time *tm)
{
	return isl1226_i2c_read_time(to_i2c_client(dev), tm);
}

static int isl1226_i2c_set_time(struct i2c_client *client,
				struct rtc_time const *tm)
{
	int ret;
	
	u8 regs[ISL1226_RTC_SECTION_LEN] = { 0, };

	regs[ISL1226_REG_SC - 0x30] = bin2bcd(tm->tm_sec);
	regs[ISL1226_REG_MN - 0x30] = bin2bcd(tm->tm_min);
	regs[ISL1226_REG_HR - 0x30] = bin2bcd(tm->tm_hour) | ISL1226_REG_HR_MIL;

	regs[ISL1226_REG_DT - 0x30] = bin2bcd(tm->tm_mday);
	regs[ISL1226_REG_MO - 0x30] = bin2bcd(tm->tm_mon + 1);
	regs[ISL1226_REG_YR - 0x30] = bin2bcd(tm->tm_year - 100);
	regs[ISL1226_REG_Y2K - 0x30] = bin2bcd((tm->tm_year + 1900)/100);

	regs[ISL1226_REG_DW - 0x30] = 0;
	
	/* set WRTC & RWEL*/
	isl1226_write_enable(client);

	/* write RTC registers */
	ret = isl1226_i2c_set_regs(client, ISL1226_REG_SC, regs, ISL1226_RTC_SECTION_LEN);
	if (ret < 0) {
		dev_err(&client->dev, "%s: writing RTC section failed\n",
			__func__);
		return ret;
	}

	/* clear WRTC again */
	
	isl1226_write_disable(client);

	return 0;
}


static int isl1226_rtc_set_time(struct device *dev, struct rtc_time *tm)
{
	return isl1226_i2c_set_time(to_i2c_client(dev), tm);
}

static int isl1226_rtc_read_alarm(struct device *dev, struct rtc_wkalrm *alarm)
{
	return isl1226_i2c_read_alarm(to_i2c_client(dev), alarm);
}

static struct rtc_class_ops isl1226_rtc_ops = {
	.proc		= isl1226_rtc_proc,
	.read_time	= isl1226_rtc_read_time,
	.set_time	= isl1226_rtc_set_time,
	.read_alarm	= isl1226_rtc_read_alarm,
	//.set_alarm	= isl1226_rtc_set_alarm,
};

/* sysfs interface */

static ssize_t isl1226_sysfs_show_atrim(struct device *dev,
					struct device_attribute *attr,
					char *buf)
{
	int atr;

	atr = isl1226_i2c_get_atr(to_i2c_client(dev));
	if (atr < 0)
		return atr;

	return sprintf(buf, "%d.%.2d pF\n", atr>>2, (atr&0x3)*25);
}
static DEVICE_ATTR(atrim, S_IRUGO, isl1226_sysfs_show_atrim, NULL);

static ssize_t isl1226_sysfs_show_dtrim(struct device *dev,
					struct device_attribute *attr,
					char *buf)
{
	int dtr;

	dtr = isl1226_i2c_get_dtr(to_i2c_client(dev));
	if (dtr < 0)
		return dtr;

	return sprintf(buf, "%d ppm\n", dtr);
}
static DEVICE_ATTR(dtrim, S_IRUGO, isl1226_sysfs_show_dtrim, NULL);

/* simple check to see wether we have a isl1226 */
static int isl1226_i2c_validate_client(struct i2c_client *client)
{
	u8 sr;
	int ret = 0;
	struct rtc_time dummy_tm = { 0, 0, 0, 1, 0, 100, 0, 0 };
	
	sr = isl1226_i2c_get_sr(client);
	if (sr & ISL1226_REG_SR_RTCF) {
                dev_err(&client->dev, "Timer not initialized after power fail. Setting 2002/1/1/0:00\n");
                ret = isl1226_i2c_set_time(client, &dummy_tm);
	}
	return ret;
	return 0;
}

static int
isl1226_sysfs_register(struct device *dev)
{
	int err;

	err = device_create_file(dev, &dev_attr_atrim);
	if (err)
		return err;

	err = device_create_file(dev, &dev_attr_dtrim);
	if (err) {
		device_remove_file(dev, &dev_attr_atrim);
		return err;
	}

	return 0;
}

static int
isl1226_sysfs_unregister(struct device *dev)
{
	device_remove_file(dev, &dev_attr_dtrim);
	device_remove_file(dev, &dev_attr_atrim);

	return 0;
}

static int
isl1226_probe(struct i2c_client *client, const struct i2c_device_id *id)
{
	int rc = 0;
	struct rtc_device *rtc;

	if (!i2c_check_functionality(client->adapter, I2C_FUNC_I2C))
		return -ENODEV;

	if (isl1226_i2c_validate_client(client) < 0)
		return -ENODEV;

	dev_info(&client->dev,
		 "chip found, driver version " DRV_VERSION "\n");

	rtc = rtc_device_register(isl1226_driver.driver.name,
				  &client->dev, &isl1226_rtc_ops,
				  THIS_MODULE);
	if (IS_ERR(rtc))
		return PTR_ERR(rtc);

	i2c_set_clientdata(client, rtc);

	rc = isl1226_i2c_get_sr(client);
	if (rc < 0) {
		dev_err(&client->dev, "reading status failed\n");
		goto exit_unregister;
	}

	isl1226_set_bsw(client);
	if (rc & ISL1226_REG_SR_RTCF)
		dev_warn(&client->dev, "rtc power failure detected, "
			 "please set clock.\n");

	rc = isl1226_sysfs_register(&client->dev);
	if (rc)
		goto exit_unregister;

	return 0;

exit_unregister:
	rtc_device_unregister(rtc);

	return rc;
}

static int
isl1226_remove(struct i2c_client *client)
{
	struct rtc_device *rtc = i2c_get_clientdata(client);

	isl1226_sysfs_unregister(&client->dev);
	rtc_device_unregister(rtc);

	return 0;
}


static const struct i2c_device_id isl1226_id[] = {
	{ "isl1226", 0 },
	{ }
};
MODULE_DEVICE_TABLE(i2c, isl1226_id);

static struct i2c_driver isl1226_driver = {
	.driver = {
		.name = "rtc-isl1226",
		.owner	= THIS_MODULE,
	},
	.probe = isl1226_probe,
	.remove = isl1226_remove,
	.id_table = isl1226_id,
};

/* module management */

static int __init
isl1226_init(void)
{
	return i2c_add_driver(&isl1226_driver);
}

static void __exit
isl1226_exit(void)
{
	i2c_del_driver(&isl1226_driver);
}

MODULE_AUTHOR("Achim Ehrlich <aehrlich@taskit.de>");
MODULE_DESCRIPTION("Intersil ISL1226 RTC driver");
MODULE_LICENSE("GPL");
MODULE_VERSION(DRV_VERSION);

module_init(isl1226_init);
module_exit(isl1226_exit);
