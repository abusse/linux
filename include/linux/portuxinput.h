#ifndef PORTUXINPUT_H
#define PORTUXINPUT_H

#define PORTUXMATRIX_MAX_X 8
#define PORTUXMATRIX_MAX_Y 8
#define PORTUXMATRIX_MAX_LAYER 2
#define PORTUXIR_MAX_LAYER 2
#define PORTUXIR_MAX_COMMANDCODES 256
#define PORTUXINPUT_MAX_KEYARRAYS 10
#define PORTUXINPUT_MAX_KEYARRAYELEMENTS 16

/**
 * struct portuxinputevent - describes an input event
 * @flags: some additional flags to change the handling of the event
 * @value: distance of a mouse movement or one of the key/button values, that
 * can be reported to the input system, refer to linux/input.h for more
 * information, target keymaps when changing the keymap and keyarray value is
 * also stored here
 * Description: This structure is used by portuxinput_report_event() to report
 * events to the input system. With @flags member one can decide, how @value is
 * treated.
 */
struct portuxinputevent {
	unsigned short flags;
	unsigned int value;
};

typedef struct portuxinputevent matrixmap[PORTUXMATRIX_MAX_LAYER][PORTUXMATRIX_MAX_Y][PORTUXMATRIX_MAX_X];
typedef struct portuxinputevent irmap[PORTUXIR_MAX_LAYER][PORTUXIR_MAX_COMMANDCODES];
typedef struct portuxinputevent keyarray[PORTUXINPUT_MAX_KEYARRAYS][PORTUXINPUT_MAX_KEYARRAYELEMENTS];

#define PORTUXINPUT_MASK_LSHIFT		0x0001
#define PORTUXINPUT_MASK_RSHIFT		0x0002
#define PORTUXINPUT_MASK_LCTRL		0x0004
#define PORTUXINPUT_MASK_RCTRL		0x0008
#define PORTUXINPUT_MASK_LALT		0x0010
#define PORTUXINPUT_MASK_RALT		0x0020
#define PORTUXINPUT_MASK_RELX		0x0001
#define PORTUXINPUT_MASK_RELY		0x0002
#define PORTUXINPUT_MASK_REPEAT		0x0040
#define PORTUXINPUT_MASK_MOUSEMOVE	0x0080
#define PORTUXINPUT_MASK_KEYARRAY	0x0100
#define PORTUXINPUT_MASK_SETKEYMAP	0x4000
#define PORTUXINPUT_MASK_SETKEYMAPRET	0x8000

#define PORTUXINPUT_KEYARRAY_MODE_BLIND		0x0
#define PORTUXINPUT_KEYARRAY_MODE_BACKSPACE	0x1
#define PORTUXINPUT_KEYARRAY_MODE_EVERY		0x2

/* IOCTL */

/**
 * struct matrixentry - used in PORTUXMATRIX_IOC_GETKEY and PORTUXMATRIX_IOC_SETKEY ioctl
 * @x: column on the matrix keyboard
 * @y: row on the matrix keyboard
 * @layer: number of the layer which the event @value ought to happen in
 * @value: the event that should happen, if key in (@x,@y) is pressed
 */
struct matrixentry {
	unsigned char x;
	unsigned char y;
	unsigned char layer;
	struct portuxinputevent value;
};

/**
 * struct irentry - used in PORTUXIR_IOC_GETKEY and PORTUXIR_IOC_SETKEY ioctl
 * @code: code for which the event @value ought to happen
 * @value: event that ought to happen
 */
struct irentry {
	unsigned char code;
	struct portuxinputevent value;
};

/**
 * struct calibration - used in PORTUXTOUCH_IOC_CALIBRATIONSTATUS to monitor touch panel calibration
 * @minx: current minimum x value
 * @maxx: current maximum x value
 * @miny: current minimum y value
 * @maxy: current maximum y value
 * @active: shows if calibration is currently executed
 * @timeout: the timeout used
 * @remaining: number of seconds until calibration is finished
 */
struct calibration {
	unsigned short minx;
	unsigned short maxx;
	unsigned short miny;
	unsigned short maxy;
	unsigned char  active;
	unsigned char  timeout;
	unsigned char  remaining;
};

struct shutdown_t {
	unsigned int countdown;
	unsigned int wakeuptime;
	union {
		char ckey;
		struct {
			unsigned x :4;
			unsigned y :4;
		} skey;
	} wakeupkey;
} __attribute__ ((packed));

struct eeprom_t {
    unsigned short address;
    unsigned char data;
};

#define PORTUXINPUT_IOC_MAGIC '!'
#define PORTUXMATRIX_IOC_SETKEY _IOW(PORTUXINPUT_IOC_MAGIC, 1, struct matrixentry)
#define PORTUXMATRIX_IOC_GETKEY _IOR(PORTUXINPUT_IOC_MAGIC, 2, struct matrixentry)
#define PORTUXMATRIX_IOC_SETKEYMAP _IOW(PORTUXINPUT_IOC_MAGIC, 3, matrixmap)
#define PORTUXTOUCH_IOC_CALIBRATESTART  _IO(PORTUXINPUT_IOC_MAGIC, 6)
#define PORTUXTOUCH_IOC_CALIBRATEFINISH _IO(PORTUXINPUT_IOC_MAGIC, 7)
#define PORTUXTOUCH_IOC_CALIBRATEABORT  _IO(PORTUXINPUT_IOC_MAGIC, 8)
#define PORTUXTOUCH_IOC_CALIBRATE _IOW(PORTUXINPUT_IOC_MAGIC, 9, int)
#define PORTUXTOUCH_IOC_CALIBRATIONSTATUS _IOR(PORTUXINPUT_IOC_MAGIC, 10, struct calibration)
#define PORTUXIR_IOC_SETKEY _IOW(PORTUXINPUT_IOC_MAGIC, 11, struct irentry)
#define PORTUXIR_IOC_GETKEY _IOR(PORTUXINPUT_IOC_MAGIC, 12, struct irentry)
#define PORTUXIR_IOC_SETKEYMAP _IOW(PORTUXINPUT_IOC_MAGIC, 13, irmap)
#define PORTUXIR_IOC_SETFILTER _IOW(PORTUXINPUT_IOC_MAGIC, 14, short)
#define PORTUXINPUT_IOC_GETTOUCHDIV _IO(PORTUXINPUT_IOC_MAGIC, 15)
#define PORTUXINPUT_IOC_SETTOUCHDIV _IOW(PORTUXINPUT_IOC_MAGIC, 16, short)
#define PORTUXINPUT_IOC_GETMATRIXDIV _IO(PORTUXINPUT_IOC_MAGIC, 17)
#define PORTUXINPUT_IOC_SETMATRIXDIV _IOW(PORTUXINPUT_IOC_MAGIC, 18, short)
#define PORTUXINPUT_IOC_SETKEYARRAYMODE _IOW(PORTUXINPUT_IOC_MAGIC, 20, char)
#define PORTUXINPUT_IOC_SETKEYARRAY _IOW(PORTUXINPUT_IOC_MAGIC, 21, keyarray)
#define PORTUXINPUT_IOC_READBATTERY _IOW(PORTUXINPUT_IOC_MAGIC, 22, unsigned int)
#define PORTUXINPUT_IOC_SETALARM _IOW(PORTUXINPUT_IOC_MAGIC, 23, unsigned int)
#define PORTUXMATRIX_IOC_SETLAYER _IOW(PORTUXINPUT_IOC_MAGIC, 24, unsigned char)
#define PORTUXMATRIX_IOC_SETMAXLAYER _IOW(PORTUXINPUT_IOC_MAGIC, 25, unsigned char)
#define PORTUXIR_IOC_SETLAYER _IOW(PORTUXINPUT_IOC_MAGIC, 26, unsigned char)
#define PORTUXIR_IOC_SETMAXLAYER _IOW(PORTUXINPUT_IOC_MAGIC, 27, unsigned char)
#define PORTUXINPUT_IOC_SHUTDOWN _IO(PORTUXINPUT_IOC_MAGIC, 28)
#define PORTUXINPUT_IOC_READEEPROM _IOW(PORTUXINPUT_IOC_MAGIC, 29, struct eeprom_t)
#define PORTUXINPUT_IOC_WRITEEEPROM _IOW(PORTUXINPUT_IOC_MAGIC, 30, struct eeprom_t)
#define PORTUXTOUCH_IOC_LOADCALIBRATION _IOW(PORTUXINPUT_IOC_MAGIC, 31, struct calibration)
#define PORTUXINPUT_IOC_SETWAKEUP _IOW(PORTUXINPUT_IOC_MAGIC, 32, unsigned char)
#define PORTUXINPUT_IOC_CLEARWAKEUP _IOW(PORTUXINPUT_IOC_MAGIC, 33, unsigned char)
#define PORTUXINPUT_IOC_GETWAKEUP _IO(PORTUXINPUT_IOC_MAGIC, 34)
#define PORTUXINPUT_IOC_GETFIRSTSTATUS _IO(PORTUXINPUT_IOC_MAGIC, 35)

#ifdef __KERNEL__

struct portuxinput_data {
	int irq;
	matrixmap *matrixkeymap;
	irmap *irkeymap;
	keyarray *keyarraymap;
};

#include <linux/input.h>
#define PORTUXINPUT_IOC_MAXNR 35
#define PORTUXINPUT_IOC_MAXHANDLER PORTUXINPUT_EVENT_MAXHANDLER

#include <linux/portuxinput-protocol.h>
#include <linux/version.h>

#define PORTUXINPUT_NAME "Portux input device"
#define PORTUXINPUT_DEVNAME "portuxinput"
#define PORTUXINPUT_ID 0xffff

#define PORTUXINPUT_I2C_ADDR 0x10

#define PORTUXINPUT_EVENT_MATRIX	0x00
#define PORTUXINPUT_EVENT_TOUCH		0x01
#define PORTUXINPUT_EVENT_IR		0x02
#define PORTUXINPUT_EVENT_KBD		0x03
#define PORTUXINPUT_EVENT_MAXHANDLER	0x04

struct portuxinput_layerstate {
	unsigned char cur;
	unsigned char last;
	unsigned char max;
	unsigned char ret;
};

typedef void(*matrixhandler_t)(struct matrix_t *);
typedef void(*touchhandler_t)(struct touch_t *);
typedef void(*irhandler_t)(struct ir_t *);
typedef void(*kbdhandler_t)(char *);
typedef void(*eventhandler_t)(void *);
typedef long(*iochandler_t)(unsigned int, unsigned long);

extern int portuxinput_register_eventhandler(int event_id, eventhandler_t handler, iochandler_t iochandler);
extern int portuxinput_deregister_eventhandler(int event_id, eventhandler_t handler);
extern void portuxinput_report_event(struct input_dev *inputdev, struct portuxinputevent *event, unsigned char repeated, struct portuxinput_layerstate *layer);
extern void portuxinput_schedule_ps2command(unsigned char ps2cmd, unsigned char param);
extern struct portuxinput_data *portuxinput_get_platform_data(void);

static inline int portuxinput_register_matrix(matrixhandler_t handler, iochandler_t iochandler)
{
	return portuxinput_register_eventhandler(PORTUXINPUT_EVENT_MATRIX, (eventhandler_t)handler, iochandler);
}
static inline int portuxinput_deregister_matrix(matrixhandler_t handler)
{
	return portuxinput_deregister_eventhandler(PORTUXINPUT_EVENT_MATRIX, (eventhandler_t)handler);
}
static inline int portuxinput_register_touch(touchhandler_t handler, iochandler_t iochandler)
{
	return portuxinput_register_eventhandler(PORTUXINPUT_EVENT_TOUCH, (eventhandler_t)handler, iochandler);
}
static inline int portuxinput_deregister_touch(touchhandler_t handler)
{
	return portuxinput_deregister_eventhandler(PORTUXINPUT_EVENT_TOUCH, (eventhandler_t)handler);
}
static inline int portuxinput_register_ir(irhandler_t handler, iochandler_t iochandler)
{
	return portuxinput_register_eventhandler(PORTUXINPUT_EVENT_IR, (eventhandler_t)handler, iochandler);
}
static inline int portuxinput_deregister_ir(irhandler_t handler)
{
	return portuxinput_deregister_eventhandler(PORTUXINPUT_EVENT_IR, (eventhandler_t)handler);
}
static inline int portuxinput_register_kbd(kbdhandler_t handler, iochandler_t iochandler)
{
	return portuxinput_register_eventhandler(PORTUXINPUT_EVENT_KBD, (eventhandler_t)handler, iochandler);
}
static inline int portuxinput_deregister_kbd(kbdhandler_t handler)
{
	return portuxinput_deregister_eventhandler(PORTUXINPUT_EVENT_KBD, (eventhandler_t)handler);
}

#endif

#endif
