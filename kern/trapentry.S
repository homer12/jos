/* See COPYRIGHT for copyright information. */

#include <inc/mmu.h>
#include <inc/memlayout.h>
#include <inc/trap.h>



###################################################################
# exceptions/interrupts
###################################################################

/* TRAPHANDLER defines a globally-visible function for handling a trap.
 * It pushes a trap number onto the stack, then jumps to _alltraps.
 * Use TRAPHANDLER for traps where the CPU automatically pushes an error code.
 *
 * You shouldn't call a TRAPHANDLER function from C, but you may
 * need to _declare_ one in C (for instance, to get a function pointer
 * during IDT setup).  You can declare the function with
 *   void NAME();
 * where NAME is the argument passed to TRAPHANDLER.
 */
#define TRAPHANDLER(name, num)						\
	.globl name;		/* define global symbol for 'name' */	\
	.type name, @function;	/* symbol type is function */		\
	.align 2;		/* align function definition */		\
	name:			/* function starts here */		\
	pushl $(num);							\
	jmp _alltraps

/* Use TRAPHANDLER_NOEC for traps where the CPU doesn't push an error code.
 * It pushes a 0 in place of the error code, so the trap frame has the same
 * format in either case.
 */
#define TRAPHANDLER_NOEC(name, num)					\
	.globl name;							\
	.type name, @function;						\
	.align 2;							\
	name:								\
	pushl $0;							\
	pushl $(num);							\
	jmp _alltraps

.text

/*
 * Lab 3: Your code here for generating entry points for the different traps.
 */

/* 0: DIVIDE 0 ERROR */
TRAPHANDLER_NOEC( trap_0_handler, 0 )

/* 1: DEBUG EXCEPTION */
TRAPHANDLER_NOEC( trap_1_handler, 1 )

/* 2: NMI interrupt   */
TRAPHANDLER_NOEC( trap_2_handler, 2 )

/* 3: BREAKPOINT */
TRAPHANDLER_NOEC( trap_3_handler, 3 )

/* 4: OVERFLOW  */
TRAPHANDLER_NOEC( trap_4_handler, 4 )

/* 5: BOUND RANGE EXCEEDED  */
TRAPHANDLER_NOEC( trap_5_handler, 5 )

/* 6: INVALID OPCODE  */
TRAPHANDLER_NOEC( trap_6_handler, 6 )

/* 7: DEVICE NOT AVAILABLE  */
TRAPHANDLER_NOEC( trap_7_handler, 7 )

/* 8: DOUBLE FAULT with error code */
TRAPHANDLER( trap_8_handler, 8 )

/* 9: RESERVED */

/* 10: INVALID TSS with error code */
TRAPHANDLER( trap_10_handler, 10 )

/* 11: SEGMENT NOT PRESENT with error code */
TRAPHANDLER( trap_11_handler, 11 )

/* 12: STACK SEGMENT FAULT  with error code */
TRAPHANDLER( trap_12_handler, 12 )

/* 13: GENERAL PROTECTION  with error code */
TRAPHANDLER( trap_13_handler, 13 )

/* 14: PAGE FAULT  with error code */
TRAPHANDLER( trap_14_handler, 14 )

/* 15: RESERVED  */

/* 16: X87 FLOATING-POINT ERROR  */
TRAPHANDLER_NOEC( trap_16_handler, 16 )

/* 17: ALIGNMENT CHECK with error code */
TRAPHANDLER( trap_17_handler, 17 )

/* 18: MACHINE CHECK  */
TRAPHANDLER_NOEC( trap_18_handler, 18 )

/* 19: SIMD FLOATING-POINT EXCEPTION  */
TRAPHANDLER_NOEC( trap_19_handler, 19 )

/* 48: SYSTEM CALL  */
TRAPHANDLER_NOEC( trap_48_handler, 48)

/* 500: CATCHALL  */
TRAPHANDLER_NOEC( trap_500_handler, 500)




/*
 * Lab 3: Your code here for _alltraps
 */
_alltraps:
	push %ds
	push %es
	pushal
	
	movw $(GD_KD), %ax
	movw %ax, %es
	movw %ax, %ds
	pushl %esp
	
	call trap

	movl $0x4, %esp
	popal
	iret
	
