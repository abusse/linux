#ifndef PORTUXINPUT_PROTOCOL_H
#define PORTUXINPUT_PROTOCOL_H

#ifdef __GNUC__
struct matrix_t {
	unsigned x :4;
	unsigned y :4;
	unsigned char repeat;
} __attribute__ ((packed));
#else
struct matrix_t {
	unsigned char code;
	unsigned char repeat;
};
#endif

#ifdef __GNUC__
struct touch_t {
	unsigned short x;
	unsigned short y;
} __attribute__ ((packed));
#else
struct touch_t {
	unsigned short x;
	unsigned short y;
};
#endif

#ifdef __GNUC__
struct ir_t {
	unsigned char addr;
	unsigned char data;
	unsigned char repeat;
} __attribute__ ((packed));
#else
struct ir_t {
	unsigned char addr;
	unsigned char data;
	unsigned char repeat;
};
#endif

#ifdef __GNUC__
struct portuxinput_sram_t {
	unsigned short status;
	unsigned short control;
	unsigned char command;
	unsigned char data[16];
} __attribute__ ((packed));
#else
struct portuxinput_sram_t {
	unsigned short status;
	unsigned short control;
	unsigned char command;
	unsigned char data[16];
};
#endif


#ifndef offsetof
#define offsetof(TYPE, MEMBER) ((unsigned int)&((TYPE *)0)->MEMBER)
#endif

#define PORTUXINPUT_READEEPROM		0x02
#define PORTUXINPUT_WRITEEEPROM		0x03
#define PORTUXINPUT_SETTOUCHDIV		0x04
#define PORTUXINPUT_GETTOUCHDIV		0x05
#define PORTUXINPUT_SETMATRIXDIV	0x06
#define PORTUXINPUT_GETMATRIXDIV	0x07
#define PORTUXINPUT_READMATRIX		0x08
#define PORTUXINPUT_READTOUCH		0x09
#define PORTUXINPUT_READIR		0x0a
#define PORTUXINPUT_READKBD		0x0b
#define PORTUXINPUT_WRITEKBD		0x0c
#define PORTUXINPUT_READBATT		0x0d
#define PORTUXINPUT_SETALARM		0x0e
#define PORTUXINPUT_SHUTDOWN		0xfe
#define PORTUXINPUT_RESET		0xff

#define PORTUXINPUT_STATUS offsetof(struct portuxinput_sram_t, status)
#define PORTUXINPUT_CONTROL offsetof(struct portuxinput_sram_t, control)
#define PORTUXINPUT_COMMAND offsetof(struct portuxinput_sram_t, command)
#define PORTUXINPUT_DATA offsetof(struct portuxinput_sram_t, data)

#define PORTUXINPUT_MASK_ERROR		0x0001
#define PORTUXINPUT_MASK_FINISHED	0x0002
#define PORTUXINPUT_MASK_INTERRUPT	0x0080

#define PORTUXINPUT_MASK_MATRIX		0x0100
#define PORTUXINPUT_MASK_TOUCH		0x0200
#define PORTUXINPUT_MASK_IR		0x0400
#define PORTUXINPUT_MASK_KBD		0x0800
#define PORTUXINPUT_MASK_BATT		0x1000
#define PORTUXINPUT_MASK_ALARM		0x2000

#endif
