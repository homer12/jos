/* See COPYRIGHT for copyright information. */

#include <inc/x86.h>
#include <inc/memlayout.h>
#include <inc/kbdreg.h>
#include <inc/string.h>
#include <inc/assert.h>

#include <kern/console.h>

static void cons_intr(int (*proc)(void));
static void cons_putc(int c);

// Stupid I/O delay routine necessitated by historical PC design flaws
// ?: Maybe 0x84 port here mens nothing. It's just a random port number.
static void
delay(void)
{
	inb(0x84);
	inb(0x84);
	inb(0x84);
	inb(0x84);
}

/***** Serial I/O code *****/

#define COM1		0x3F8

#define COM_RX		0	// In:	Receive buffer (DLAB=0)
#define COM_TX		0	// Out: Transmit buffer (DLAB=0)
#define COM_DLL		0	// Out: Divisor Latch Low (DLAB=1)
#define COM_DLM		1	// Out: Divisor Latch High (DLAB=1)
#define COM_IER		1	// Out: Interrupt Enable Register
#define   COM_IER_RDI	0x01	//   Enable receiver data interrupt
#define COM_IIR		2	// In:	Interrupt ID Register
#define COM_FCR		2	// Out: FIFO Control Register
#define COM_LCR		3	// Out: Line Control Register
#define	  COM_LCR_DLAB	0x80	//   Divisor latch access bit
#define	  COM_LCR_WLEN8	0x03	//   Wordlength: 8 bits
#define COM_MCR		4	// Out: Modem Control Register
#define	  COM_MCR_RTS	0x02	// RTS complement
#define	  COM_MCR_DTR	0x01	// DTR complement
#define	  COM_MCR_OUT2	0x08	// Out2 complement
#define COM_LSR		5	// In:	Line Status Register
#define   COM_LSR_DATA	0x01	//   Data available
#define   COM_LSR_TXRDY	0x20	//   Transmit buffer avail
#define   COM_LSR_TSRE	0x40	//   Transmitter off

static bool serial_exists;

static int
serial_proc_data(void)
{
	// Bit 0 of register (COM1+5) means DATA READY
	if (!(inb(COM1+COM_LSR) & COM_LSR_DATA))
		return -1;
	return inb(COM1+COM_RX);
}

void
serial_intr(void)
{
	if (serial_exists)
		cons_intr(serial_proc_data);
}

static void
serial_putc(int c)
{
	int i;
	
	// Bit 5 = 1 means Controller is ready to accept a new character
	//	to send.
	for (i = 0;
	     !(inb(COM1 + COM_LSR) & COM_LSR_TXRDY) && i < 12800;
	     i++)
		delay();

	outb(COM1 + COM_TX, c);
}

static void
serial_init(void)
{
	/*
	 *	reference this article:
	 *	http://wearcam.org/seatsale/programs/www.beyondlogic.org/serial/serial.htm#14
	 *	and https://www.lammertbies.nl/comm/info/serial-uart.html for IIR register
	 */
	
	// Turn off the FIFO
	// When bit 0 of FCR is cleard, the operation of
	//	transmit and receiver is disabled
	outb(COM1+COM_FCR, 0);

	// Set speed; requires DLAB latch
	// Bit 7 of LCR is for accessing DLAB
	// DIVISOR = 115200 / THE_RATE_YOU_WANT_TO_USE
	// DLL is used for storing the lower byte of DIVISOR
	// DLM is used to store the higher byte
	outb(COM1+COM_LCR, COM_LCR_DLAB);
	outb(COM1+COM_DLL, (uint8_t) (115200 / 9600));
	outb(COM1+COM_DLM, 0);

	// 8 data bits, 1 stop bit, parity off; turn off DLAB latch
	// Bits 1 and 0 = 11 means 8 bits
	outb(COM1+COM_LCR, COM_LCR_WLEN8 & ~COM_LCR_DLAB);

	// No modem controls
	outb(COM1+COM_MCR, 0);


	// Enable rcv interrupts
	// Setting bit 0 of IER high enables the Received Data
	//	Available Interrupt which generates an interrupt when
	//	receiving register/FIFO contains data to be read
	//	by the CPU.
	//	That is, DEVICE ===> CPU
	outb(COM1+COM_IER, COM_IER_RDI);


	// Clear any preexisting overrun indications and interrupts
	// Serial port doesn't exist if COM_LSR returns 0xFF
	serial_exists = (inb(COM1+COM_LSR) != 0xFF);
	// Clear the bit 1 of IIR
	(void) inb(COM1+COM_IIR);
	// Clear the bit 2 and 3 of IIR
	(void) inb(COM1+COM_RX);

}



/***** Parallel port output code *****/
// For information on PC parallel port programming, see the class References
// page.

static void
lpt_putc(int c)
{
	int i;
	
	// 0x378 = data port
	// 0x379 = status port
	// 0x37A = control port

	// Bit 7 of 0x379 = 0 means busy
	for (i = 0; !(inb(0x378+1) & 0x80) && i < 12800; i++)
		delay();
	
	// output a byte
	outb(0x378+0, c);
	
	// 0x37A
	// I CAN'T FIGURE OUT
	outb(0x378+2, 0x08|0x04|0x01);
	outb(0x378+2, 0x08);
}




/***** Text-mode CGA/VGA display output *****/

static unsigned addr_6845;
static uint16_t *crt_buf;
static uint16_t crt_pos;


static void
cga_init(void)
{
	volatile uint16_t *cp;
	uint16_t was;
	unsigned pos;
	
	// CGA_BUF is defined in kern/console.h as 0xB8000
	// MONO_BUF is defined in kern/console.h as 0xB0000
	// CGA_BASE is 3D4
	// MONO_BASE is 3B4
	cp = (uint16_t*) (KERNBASE + CGA_BUF);
	was = *cp;
	*cp = (uint16_t) 0xA55A;	// write a random data into
	if (*cp != 0xA55A) {
		// OK, now we now that this 0xB8000 memory is not usable
		// we will gonna use MONO_BUF
		// https://github.com/chyyuu/ucore_os_lab/blob/master/labcodes/lab1/kern/driver/console.c
		cp = (uint16_t*) (KERNBASE + MONO_BUF);
		addr_6845 = MONO_BASE;
	} else {
		*cp = was;
		addr_6845 = CGA_BASE;
	}

	/* Extract cursor location */
	// https://wiki.osdev.org/Text_Mode_Cursor#Get_Cursor_Position
	outb(addr_6845, 14);	// high byte of position
	pos = inb(addr_6845 + 1) << 8;
	outb(addr_6845, 15);	// low byte of position
	pos |= inb(addr_6845 + 1);

	crt_buf = (uint16_t*) cp;
	crt_pos = pos;
}



static void
cga_putc(int c)
{
	// https://en.wikipedia.org/wiki/VGA-compatible_text_mode#Text_buffer

	// if no attribute given, then use black on white
	// ? But 0x7 stands for Light Gray, isn't it?
	if (!(c & ~0xFF))
		c |= 0x0700;

	switch (c & 0xff) {
	case '\b':
		if (crt_pos > 0) {
			crt_pos--;
			crt_buf[crt_pos] = (c & ~0xff) | ' ';
		}
		break;
	case '\n':
		crt_pos += CRT_COLS;
		/* fallthru */
	case '\r':
		crt_pos -= (crt_pos % CRT_COLS);
		break;
	case '\t':
		// Why bother using cons_putc() ????????
		cons_putc(' ');
		cons_putc(' ');
		cons_putc(' ');
		cons_putc(' ');
		cons_putc(' ');
		break;
	default:
		crt_buf[crt_pos++] = c;		/* write the character */
		break;
	}

	// What is the purpose of this?
	// CRT_ROWS = 25
	// CRT_COLS = 80
	// CRT_SIZE = CRT_ROWS * CRT_COLS
	// all defined in kern/console.h
	if (crt_pos >= CRT_SIZE) {
		int i;
		
		// memmove( dst, src, n )
		// delete the first line and copy the rest 1 line up
		memmove(crt_buf, crt_buf + CRT_COLS, (CRT_SIZE - CRT_COLS) * sizeof(uint16_t));
		
		// clear the last line
		for (i = CRT_SIZE - CRT_COLS; i < CRT_SIZE; i++)
			crt_buf[i] = 0x0700 | ' ';
		
		// modify the cursor position
		crt_pos -= CRT_COLS;
	}

	/* move that little blinky thing */
	outb(addr_6845, 14);
	outb(addr_6845 + 1, crt_pos >> 8);	// high byte of position
	outb(addr_6845, 15);
	outb(addr_6845 + 1, crt_pos);		// low byte of position
}


/***** Keyboard input code *****/

#define NO		0

#define SHIFT		(1<<0)
#define CTL		(1<<1)
#define ALT		(1<<2)

#define CAPSLOCK	(1<<3)
#define NUMLOCK		(1<<4)
#define SCROLLLOCK	(1<<5)

#define E0ESC		(1<<6)

static uint8_t shiftcode[256] =
{
	// For the magic numbers below,
	// refer the Scan Code Set 1
	// https://wiki.osdev.org/PS/2_Keyboard#Scan_Code_Set_1
	// and http://www.vetra.com/scancodes.html
	[0x1D] = CTL,
	[0x2A] = SHIFT,
	[0x36] = SHIFT,
	[0x38] = ALT,
	[0x9D] = CTL,
	[0xB8] = ALT
};

static uint8_t togglecode[256] =
{
	// The same as above
	[0x3A] = CAPSLOCK,
	[0x45] = NUMLOCK,
	[0x46] = SCROLLLOCK
};



// Convert a kbd input to a character
// http://www.vetra.com/scancodes.html
// http://www.asciitable.com/
static uint8_t normalmap[256] =
{
	// 0x18 in ascii means ESC
	NO,   0x1B, '1',  '2',  '3',  '4',  '5',  '6',	// 0x00
	'7',  '8',  '9',  '0',  '-',  '=',  '\b', '\t',
	'q',  'w',  'e',  'r',  't',  'y',  'u',  'i',	// 0x10
	'o',  'p',  '[',  ']',  '\n', NO,   'a',  's',
	'd',  'f',  'g',  'h',  'j',  'k',  'l',  ';',	// 0x20
	'\'', '`',  NO,   '\\', 'z',  'x',  'c',  'v',
	'b',  'n',  'm',  ',',  '.',  '/',  NO,   '*',	// 0x30
	NO,   ' ',  NO,   NO,   NO,   NO,   NO,   NO,
	NO,   NO,   NO,   NO,   NO,   NO,   NO,   '7',	// 0x40
	'8',  '9',  '-',  '4',  '5',  '6',  '+',  '1',
	'2',  '3',  '0',  '.',  NO,   NO,   NO,   NO,	// 0x50
	[0xC7] = KEY_HOME,	      [0x9C] = '\n' /*KP_Enter*/,
	[0xB5] = '/' /*KP_Div*/,      [0xC8] = KEY_UP,
	[0xC9] = KEY_PGUP,	      [0xCB] = KEY_LF,
	[0xCD] = KEY_RT,	      [0xCF] = KEY_END,
	[0xD0] = KEY_DN,	      [0xD1] = KEY_PGDN,
	[0xD2] = KEY_INS,	      [0xD3] = KEY_DEL
};

static uint8_t shiftmap[256] =
{
	NO,   033,  '!',  '@',  '#',  '$',  '%',  '^',	// 0x00
	'&',  '*',  '(',  ')',  '_',  '+',  '\b', '\t',
	'Q',  'W',  'E',  'R',  'T',  'Y',  'U',  'I',	// 0x10
	'O',  'P',  '{',  '}',  '\n', NO,   'A',  'S',
	'D',  'F',  'G',  'H',  'J',  'K',  'L',  ':',	// 0x20
	'"',  '~',  NO,   '|',  'Z',  'X',  'C',  'V',
	'B',  'N',  'M',  '<',  '>',  '?',  NO,   '*',	// 0x30
	NO,   ' ',  NO,   NO,   NO,   NO,   NO,   NO,
	NO,   NO,   NO,   NO,   NO,   NO,   NO,   '7',	// 0x40
	'8',  '9',  '-',  '4',  '5',  '6',  '+',  '1',
	'2',  '3',  '0',  '.',  NO,   NO,   NO,   NO,	// 0x50
	[0xC7] = KEY_HOME,	      [0x9C] = '\n' /*KP_Enter*/,
	[0xB5] = '/' /*KP_Div*/,      [0xC8] = KEY_UP,
	[0xC9] = KEY_PGUP,	      [0xCB] = KEY_LF,
	[0xCD] = KEY_RT,	      [0xCF] = KEY_END,
	[0xD0] = KEY_DN,	      [0xD1] = KEY_PGDN,
	[0xD2] = KEY_INS,	      [0xD3] = KEY_DEL
};

#define C(x) (x - '@')

static uint8_t ctlmap[256] =
{
	NO,      NO,      NO,      NO,      NO,      NO,      NO,      NO,
	NO,      NO,      NO,      NO,      NO,      NO,      NO,      NO,
	C('Q'),  C('W'),  C('E'),  C('R'),  C('T'),  C('Y'),  C('U'),  C('I'),
	C('O'),  C('P'),  NO,      NO,      '\r',    NO,      C('A'),  C('S'),
	C('D'),  C('F'),  C('G'),  C('H'),  C('J'),  C('K'),  C('L'),  NO,
	NO,      NO,      NO,      C('\\'), C('Z'),  C('X'),  C('C'),  C('V'),
	C('B'),  C('N'),  C('M'),  NO,      NO,      C('/'),  NO,      NO,
	[0x97] = KEY_HOME,
	[0xB5] = C('/'),		[0xC8] = KEY_UP,
	[0xC9] = KEY_PGUP,		[0xCB] = KEY_LF,
	[0xCD] = KEY_RT,		[0xCF] = KEY_END,
	[0xD0] = KEY_DN,		[0xD1] = KEY_PGDN,
	[0xD2] = KEY_INS,		[0xD3] = KEY_DEL
};

static uint8_t *charcode[4] = {
	normalmap,
	shiftmap,
	ctlmap,
	ctlmap
};

/*
 * Get data from the keyboard.  If we finish a character, return it.  Else 0.
 * Return -1 if no data.
 */
static int
kbd_proc_data(void)
{
	// reference this blog
	// http://blog.chinaunix.net/uid-368446-id-2414233.html


	int c;
	uint8_t stat, data;
	static uint32_t shift;
	

	// KBSTATP = 0x64 in inc/kbdreg.h	
	// KBS_DIB = 0x01
	// Bit 0 of KBSTATP means Output Buffer Status
	// 0 = empty, 1 = full
	// must be set before attemting to
	//	read data from IO port 0x60
	stat = inb(KBSTATP);
	if ((stat & KBS_DIB) == 0)
		return -1;
	

	// Ignore data from mouse.
	// KBS_TERR = 0x20
	// Bit 5 of KBSTATP means MOUSE OUTPUT BUFFER FULL or 
	//	TRANSMIT TIMEOUT
	// anyway, we dont's use this bit
	if (stat & KBS_TERR)
		return -1;
	
	// read a byte from DATA PORT
	data = inb(KBDATAP);
	
	// return 0 in function cons_intr means continue for
	//	the next loop

	if (data == 0xE0) {
		// E0 escape character
		// set the flag and wait for the next byte
		shift |= E0ESC;
		return 0;
	} else if (data & 0x80) {
		// Key released
		
		// Left Ctrl: 1D / 9D
		// Right Ctrl: E0 1D / E0 9D
		// Left Shift: 2A / AA
		// Right Shift: 36 / B6
		// Left Alt: 38 / B8
		// Right Alt: E0 38 / E0 B8
		
		
		// I think this is for the reason of SHIFT
		//  because we only set
		//  [0x2A] = SHIFT, and
		//  [0x36] = SHIFT in `shiftcode`,
		//  so we need to take off the 0x80 to 
		//  get the right index into
		//  the array `shiftcode`
		data = (shift & E0ESC ? data : data & 0x7F);


		// clear the SHIFT flag and E0ESCAPE flag
		shift &= ~(shiftcode[data] | E0ESC);
		return 0;
	} else if (shift & E0ESC) {
		// Last character was an E0 escape; or with 0x80

		// For example, in Scan Code Set 1,
		// 0x52 is mapped to 'Keypad 0',
		// while 0xE0 0x52 is mapped to 'Insert'
		// So we have to prepare a array of 
		// length 256 ( like normalmap ) to 
		// hold up to 128*2 characters.
		data |= 0x80;

		// clear the E0ESCAPE flag
		shift &= ~E0ESC;
	}

	shift |= shiftcode[data];
	shift ^= togglecode[data];
	
	// CTL | SHIFT can yields four different result
	//	{ 00, 01, 10, 11 }, which can be used as the index
	//	into the char*[4] array `charcode`
	// 
	// In fact, only threey char array are implementd,
	//	namely, `normalmap`, `shiftmap`, `ctlmap`.
	c = charcode[shift & (CTL | SHIFT)][data];
	if (shift & CAPSLOCK) {
		if ('a' <= c && c <= 'z')
			c += 'A' - 'a';
		else if ('A' <= c && c <= 'Z')
			c += 'a' - 'A';
	}

	// Process special keys
	// Ctrl-Alt-Del: reboot
	if (!(~shift & (CTL | ALT)) && c == KEY_DEL) {
		cprintf("Rebooting!\n");
		outb(0x92, 0x3); // courtesy of Chris Frost
	}

	return c;
}

void
kbd_intr(void)
{
	cons_intr(kbd_proc_data);
}

static void
kbd_init(void)
{
}



/***** General device-independent console code *****/
// Here we manage the console input buffer,
// where we stash characters received from the keyboard or serial port
// whenever the corresponding interrupt occurs.

#define CONSBUFSIZE 512

static struct {
	uint8_t buf[CONSBUFSIZE];
	uint32_t rpos;
	uint32_t wpos;
} cons;

// called by device interrupt routines to feed input characters
// into the circular console input buffer.
static void
cons_intr(int (*proc)(void))
{
	int c;

	while ((c = (*proc)()) != -1) {
		if (c == 0)
			continue;
		cons.buf[cons.wpos++] = c;
		if (cons.wpos == CONSBUFSIZE)
			cons.wpos = 0;
	}
}

// return the next input character from the console, or 0 if none waiting
int
cons_getc(void)
{
	int c;

	// poll for any pending input characters,
	// so that this function works even when interrupts are disabled
	// (e.g., when called from the kernel monitor).
	serial_intr();
	kbd_intr();

	// grab the next character from the input buffer.
	if (cons.rpos != cons.wpos) {
		c = cons.buf[cons.rpos++];
		if (cons.rpos == CONSBUFSIZE)
			cons.rpos = 0;
		return c;
	}
	return 0;
}

// output a character to the console
static void
cons_putc(int c)
{
	serial_putc(c);
	lpt_putc(c);
	cga_putc(c);
}

// initialize the console devices
void
cons_init(void)
{
	cga_init();
	kbd_init();
	serial_init();

	if (!serial_exists)
		cprintf("Serial port does not exist!\n");
}


// `High'-level console I/O.  Used by readline and cprintf.

void
cputchar(int c)
{
	cons_putc(c);
}

int
getchar(void)
{
	int c;

	while ((c = cons_getc()) == 0)
		/* do nothing */;
	return c;
}

int
iscons(int fdnum)
{
	// used by readline
	return 1;
}
