/* tc-msp430.c -- Assembler code for the Texas Instruments MSP430

  Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2009, 2010, 2012
  Free Software Foundation, Inc.
  Contributed by Dmitry Diky <diwil@mail.ru>

  This file is part of GAS, the GNU Assembler.

  GAS is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 3, or (at your option)
  any later version.

  GAS is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with GAS; see the file COPYING.  If not, write to
  the Free Software Foundation, 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1301, USA.  */

#include <limits.h>

#include "as.h"
#include "subsegs.h"
#include "opcode/msp430.h"
#include "safe-ctype.h"
#include "dwarf2dbg.h"

static int msp430x_repeats;

const char comment_chars[] = ";";
const char line_comment_chars[] = "#";
const char line_separator_chars[] = "{";
const char EXP_CHARS[] = "eE";
const char FLT_CHARS[] = "dD";

static struct hash_control *msp430_hash;
static symbolS *msp430_register_table[REGNO_MAX + 1];

/* TRUE iff the operand includes an offset encoded into or after the
 * instruction (formerly mode==OP_EXP). */
#define OP_HAS_IMMEDIATE(_op)					\
  (((_op).am == AMs_Immediate && (_op).reg == REGNO_PC)		\
   ||((_op).am == AM_Indexed && (_op).reg != REGNO_CG2))

/* TRUE iff the operand is an index offset to a register that might
 * have an odd value.  This is used in verifying that the actual
 * offset may be even, thus allowing its use for aligned addresses. */
#define OP_INDEXED_PERMITS_ODD(_op)		\
  ((_op).am == AM_Indexed			\
   && (_op).reg != REGNO_PC			\
   && (_op).reg != REGNO_CG1			\
   && (_op).reg != REGNO_SP)

/* TRUE iff the operand is an immediate appearing in a destination
 * operand in a context where the value of that immediate is permitted
 * to be odd.  This is a legacy check to ensure that offsets added to
 * registers known to be even will produce a word-aligned address
 * (legacy did not check SP, which this does). */
#define OP_DST_IMMEDIATE_PERMITS_ODD(_op)	\
  OP_INDEXED_PERMITS_ODD(_op)

/* TRUE iff the operand is an immediate appearing in a source operand
 * in a context where the value of that immediate is permitted to be
 * odd.  This is a legacy check to ensure that offsets added to
 * registers known to be even will produce a word-aligned address
 * (legacy did not check SP, which this does). */
#define OP_SRC_IMMEDIATE_PERMITS_ODD(_op)		\
  (((_op).am == AMs_Immediate && (_op).reg == REGNO_PC)	\
   || OP_DST_IMMEDIATE_PERMITS_ODD(_op))

enum immediate_range_e
{
  IMM_RANGE_INT16,		/* signed 16-bit integer */
  IMM_RANGE_INT20,		/* signed 20-bit integer */
  IMM_RANGE_REPETITION_COUNT,	/* repetition count 1..16 */
  IMM_RANGE_ROTATE_COUNT,	/* rotation count 1..4 */
};

/* Test that the value is representable in 16 bits, either signed or
 * unsigned */
#define MSP430_16_IN_RANGE(_v) ((_v) >= -((offsetT)1 << 15) && (_v) < ((offsetT)1 << 16))

/* Test that the value is representable in 15 bits, signed only */
#define MSP430_S16_IN_RANGE(_v) ((_v) >= -((offsetT)1 << 15) && (_v) < ((offsetT)1 << 15))

/* Test that the value is representable in 20 bits, either signed or
 * unsigned */
#define MSP430_20_IN_RANGE(_v) ((_v) >= -((offsetT)1 << 19) && (_v) < ((offsetT)1 << 20))

/* Test that the value is representable in 10 bits, signed only */
#define MSP430_S10_IN_RANGE(_v) ((_v) >= -((offsetT)1 << 9) && (_v) < ((offsetT)1 << 9))

/* Test whether the value is odd */
#define MSP430_ODD(_v) ((_v) & 1)

static int
valid_immediate (offsetT value, enum immediate_range_e imm_range)
{
  bfd_boolean valid;
  switch (imm_range)
    {
    case IMM_RANGE_INT16:
      valid = MSP430_16_IN_RANGE (value);
      if (!valid)
	as_bad (_("value %ld out of range for 16-bit immediate"),
		(long int) value);
      break;
    case IMM_RANGE_INT20:
      valid = MSP430_20_IN_RANGE (value);
      if (!valid)
	as_bad (_("value %ld out of range for 20-bit immediate"),
		(long int) value);
      break;
    case IMM_RANGE_REPETITION_COUNT:
      valid = value >= 1 && value <= 16;
      if (!valid)
	as_bad (_("value %ld not a repetition count (1..16)"),
		(long int) value);
      break;
    case IMM_RANGE_ROTATE_COUNT:
      valid = value >= 1 && value <= 4;
      if (!valid)
	as_bad (_("value %ld not a rotate count (1..4)"), (long int) value);
      break;
    default:
      gas_assert (0);
      valid = FALSE;
    }
  return valid;
}

/** List known silicon errata with a description of the problem (where
 * this can be found).  Errata descriptions are available in PDF files
 * that can be found on the device-specific web page at TI.  Errata
 * numbers are consistent across the product line.
 *
 * Note that not all documented errata are currently recognized by the
 * assembler.  In fact, most are completely ignored.  Future work...
 *
 * Legacy errata descriptions are from previous versions of
 * binutils. */
typedef enum msp430_errata_e
{
  /* CPU4:  PUSH #4, PUSH #8
   *
   * The single operand instruction PUSH cannot use the internal
   * constants (CG) 4 and 8.  The other internal constants (0, 1, 2,
   * –1) can be used. The number of clock cycles is different:
   *
   * - PUSH #CG uses address mode 00, requiring 3 cycles, 1-word instruction
   * - PUSH #4/#8 uses address mode 11, requiring 5 cycles, 2-word instruction
   *
   * Workaround:
   * - Assembler generate code not referencing constant generator
   */
  ERRATUM_CPU4 = 4,

  /* CALL and PUSH with @SP+, @SP, and X(SP) uses the SP to calculate the address, then decrements it */
  ERRATUM_CPU7 = 7,

  /* CPU8: Using odd values in the SP register
   *
   * The SP can be written with odd values. In the original CPU, an
   * odd SP value could be combined with an odd offset (for example,
   * mov. #value, 5(SP)). In the new CPU, the SP can be written with
   * an odd value, but the first time the SP is used, the LSB is
   * forced to 0.
   *
   * Workaround:
   * - Do not use odd values with the SP.
   */
  ERRATUM_CPU8 = 8,		/* UNHANDLED */

  /* CPU11: Invalid status register after program counter access
   *
   * When addressing the program counter (PC) in register mode when
   * the PC is the destination, the Status Register (SR) may be
   * erroneous. The instructions BIS, BIC, and MOV do not affect SR
   * contents. Only CPU flags are affected. This bug does not apply to
   * LPMx control bits.
   *
   * Workaround: None
   */
  ERRATUM_CPU11 = 11,		/* UNHANDLED */

  /* CPU12: CMP or BIT with PC destination
   *
   * Any instruction immediately following a CMP(.B) or BIT(.B)
   * instruction when the PC is the destination address using register
   * mode is ignored or erroneously executed. When the following
   * instruction is longer than one word, the second word is fetched
   * by the CPU and decoded as the instruction, leading to
   * unpredictable behavior. Affected source-addressing modes are
   * indexed and indirect addressing modes.
   *
   * Example:
   *   cmp &200,PC
   *   add #4,R8
   * The add command is not executed.
   *
   * Workaround:
   * - Insert a NOP instruction after the BIT or CMP instruction. The
   *   NOP is ignored, and program execution continues as expected.
   */
  ERRATUM_CPU12 = 12,		/* UNHANDLED */

  /* CPU13: Arithmetic operations and the SR
   *
   * Performing arithmetic operations with the Status Register (SR) as
   * the destination address does not update the SR as intended. The
   * result in SR can be invalid, leading to erroneous low-power mode
   * entry. Arithmetic operations are defined as all instructions that
   * modify the SR flag bits (RRA, SUB, XOR, and ADD, for example).
   *
   * Workaround: None
   */
  ERRATUM_CPU13 = 13,		/* UNHANDLED */

  /* CPU15: Modifying the Program Counter (PC) behaves differently
   *        than in previous devices
   *
   * When using instructions with immediate or indirect addressing
   * mode to modify the PC, a different value compared to previous
   * devices must be added to get to the same destination.
   *
   * NOTE: The MOV instruction is not affected
   *
   * Example: Previous device (MSP430F4619)
   *    label_1 ADD.W #Branch1-label_1-4h,PC
   * MSP430F5438:
   *    label_1 ADD.W #Branch1-label_1-2h,PC
   *
   * Workaround:
   * - Additional NOP after the PC-modifying instruction; or
   * - Change the offset value in software
   */
  ERRATUM_CPU15 = 15,		/* UNHANDLED */

  /* CPU16 Indexed addressing with instructions calla, mova, and bra
   *
   * With indexed addressing mode and instructions calla, mova, and bra, it is not possible
   * to reach memory above 64k if the register content is < 64k.
   * Example: Assume R5 = FFFEh. The instruction calla 0004h(R5) results in a 20-bit call
   * of address 0002h instead of 10002h.
   *
   * Workaround:
   * - Use different addressing mode to reach memory above 64k.
   * - First use adda [index],[Rx] to calculate address in upper memory and then use
   *   calla [Rx].
   */
  ERRATUM_CPU16 = 16,		/* UNHANDLED */

  /* CPU18: LPM instruction can corrupt PC/SR registers
   *
   * The PC and SR registers have the potential to be corrupted when:
   * - An instruction using register, absolute, indexed, indirect,
   *   indirect auto-increment, or symbolic mode is used to set the
   *   LPM bits (for example, BIS &xyh, SR).
   * and
   * - This instruction is followed by a CALL or CALLA instruction.
   *
   * Upon servicing an interrupt service routine, the program counter
   * (PC) is pushed twice onto the stack instead of the correct
   * operation where the PC, then the SR registers are pushed onto the
   * stack. This corrupts the SR and possibly the PC on RETI from the
   * ISR.
   *
   * Workaround:
   * - Insert a NOP or __no_operation() intrinsic function between the
   *   instruction to enter low-power mode and the CALL or CALLA
   *   instruction.
   */
  ERRATUM_CPU18 = 18,		/* UNHANDLED */

  /* CPU19: CPUOFF can change register values
   *
   * If a CPUOFF command is followed by an instruction with an
   * indirect addressed operand (for example, mov @R8, R9, and RET),
   * an unintentional register-read operation can occur during the
   * wakeup of the CPU. If the unintentional read occurs to a
   * read-sensitive register (for example, UCB0RXBUF or TAIV), which
   * changes its value or the value of other registers (IFGs), the bug
   * leads to lost interrupts or wrong register read values.
   *
   * Workaround:
   * - Insert a NOP instruction after each CPUOFF instruction.
   */
  ERRATUM_CPU19 = 19,		/* UNHANDLED */

  /* CPU20: An unexpected Vacant Memory Access Flag (VMAIFG) can be
   *        triggered due to the CPU autoincrement of the MAB + 2
   *        outside the range of a valid memory block.
   *
   * The VMAIFG is triggered if a PC-modifying instruction (for
   * example, ret, push, call, pop, jmp, br) is fetched from the last
   * address of a section of memory (for example, flash or RAM) that
   * is not contiguous to a higher valid section on the memory map.
   *
   * Workaround:
   * - If code is affected, edit the linker command file to make the
   *   last four bytes of affected memory sections unavailable.
   */
  ERRATUM_CPU20 = 20,		/* UNHANDLED */

  /* NO DESCRIPTION */
  ERRATUM_CPU21 = 21,		/* UNHANDLED */

  /* NO DESCRIPTION */
  ERRATUM_CPU22 = 22,		/* UNHANDLED */

  /* NO DESCRIPTION */
  ERRATUM_CPU23 = 23,		/* UNHANDLED */

  /* CPU24: Program counter corruption following entry into low power mode
   *
   * The program counter is corrupted when an interrupt event occurs
   * in the time between (and including) one cycle before and one
   * cycle after the CPUOFF bit is set in the status register. This
   * failure occurs when the BIS instruction is followed by a CALL or
   * CALLA instruction using the following addressing modes:
   *
   * - BIS &, SR
   *   CALLA indir, indir autoinc, reg
   *
   * - BIS INDEX, SR
   *   CALLA indir, indir autoinc, reg
   *
   * - BIS reg, SR
   *   CALLA reg, indir, indir autoinc
   *
   * Due to the instruction emulation, the EINT instruction, as well
   * as the __enable_interrupts() and possibly the __bis_SR_register()
   * intrinsic functions are affected.
   *
   * Workaround:
   * - Insert a NOP instruction or __no_operation() intrinsic function
   *   call between the BIS and CALL or CALLA instructions.
   */
  ERRATUM_CPU24 = 24,		/* UNHANDLED */

  /* CPU25: DMA transfer does not execute during low power mode
   *
   * If the following instruction sequence is used ([] denotes an
   * addressing mode):
   * 
   *   BIS [register|index|absolute|symbolic],SR
   *   CALLA [register]
   *
   * ...to enter a low power mode, AND the DMARMWDIS bit is set then
   * DMA transfers will be blocked for the duration of the low power
   * mode.
   *
   * Workaround:
   * 1. Insert a NOP instruction or __no_operation() intrinsic
   *    function call between the BIS and CALLA instructions ... OR
   *    ...
   *  2. Temporarily clear the DMARMWDIS bit when entering low power
   *     mode
   */
  ERRATUM_CPU25 = 25,		/* UNHANDLED */

  /* CPU26: CALL SP does not behave as expected
   *
   * When the intention is to execute code from the stack, a CALL SP
   * instruction skips the first piece of data (instruction) on the
   * stack. The second piece of data at SP + 2 is used as the first
   * executable instruction.
   *
   * Workaround:
   * - Write the op code for a NOP as the first instruction on the
   *   stack. Begin the intended subroutine at address SP + 2.
   */
  ERRATUM_CPU26 = 26,		/* UNHANDLED */

  /* CPU27: Program Counter (PC) is corrupted during the context save
   *        of a nested interrupt
   *
   * When a low-power mode is entered within an interrupt service
   * routine that has enabled nested interrupts (by setting the GIE
   * bit), and the instruction that sets the low-power mode is
   * directly followed by a RETI instruction, an incorrect value of PC
   * + 2 is pushed to the stack during the context save. Hence, the
   * RETI instruction is not executed on return from the nested
   * interrupt, and the PC becomes corrupted.
   *
   * Workaround:
   * - Insert a NOP or __no_operation() intrinsic function between the
   *   instruction that sets the lower power mode and the RETI
   *   instruction.
   */
  ERRATUM_CPU27 = 27,		/* UNHANDLED */

  /* CPU28: PC is corrupted when using certain extended addressing
   *        mode combinations
   *
   * An extended memory instruction that modifies the program counter
   * executes incorrectly when preceded by an extended memory
   * write-back instruction under the following conditions:
   *
   * - First instruction:
   *     2-operand instruction, extended mode using (register,index),
   *     (register,absolute), or (register,symbolic) addressing modes
   * - Second instruction:
   *     2-operand instruction, extended mode using the (indirect,PC),
   *     (indirect auto-increment,PC), or (indexed [with ind 0], PC)
   *     addressing modes
   *
   * Example:
   *    BISX.A   R6,&AABCD
   *    ANDX.A   @R4+,PC
   *
   * Workaround:
   * - Insert a NOP or a __no_operation() intrinsic function between
   *   the two instructions.
   * or
   * - Do not use an extended memory instruction to modify the PC.
   */
  ERRATUM_CPU28 = 28,		/* UNHANDLED */

  /* CPU29: Using a certain instruction sequence to enter low-power
   *        mode(s) affects the instruction width of the first
   *        instruction in an NMI ISR
   *
   * If there is a pending NMI request when the CPU enters a low-power
   * mode (LPMx) using an instruction of Indexed source addressing
   * mode, and that instruction is followed by a 20-bit wide
   * instruction of Register source and Destination addressing modes,
   * the first instruction of the ISR is executed as a 20-bit wide
   * instruction.
   *
   * Example:
   *   main:
   *       ...
   *       MOV.W    [indexed],SR            ; Enter LPMx
   *       MOVX.A   [register],[register]   ; 20-bit wide instruction
   *       ...
   *   ISR_start:
   *       MOV.B    [indexed],[register]    ; ERROR - Executed as a 20-bit instruction!
   *
   * Note: [ ] indicates addressing mode
   *
   * Workaround:
   * - Insert a NOP or a __no_operation() intrinsic function following
   *   the instruction that enters the LPMx using indexed addressing
   *   mode.
   * or
   * - Use a NOP or a __no_operation() intrinsic function as first
   *   instruction in the ISR.
   * or
   * - Do not use the indexed mode to enter LPMx.
   */
  ERRATUM_CPU29 = 29,		/* UNHANDLED */

  /* CPU30: ADDA, SUBA, CMPA [immediate],PC behave as if immediate
   *        value were offset by -2
   *
   * The extended address instructions ADDA, SUBA, and CMPA in
   * immediate addressing mode are represented by 4 bytes of opcode
   * (see the MSP430F5xx Family User's Guide (SLAU208) for more
   * details). In cases where the program counter (PC) is used as the
   * destination register, only 2 bytes of the current instruction's
   * 4-byte opcode are accounted for in the PC value. The resulting
   * operation executes as if the immediate value were offset by a
   * value of -2.
   *
   * Example:
   *   Ideal: ADDA   #Immediate-4, PC
   * ...is equivalent to...
   *   Actual: ADDA    #Immediate-2, PC
   * NOTE: The MOV instruction is not affected.
   *
   * Workaround:
   * - Modify immediate value in software to account for the offset of 2.
   * or
   * - Use extended 20-bit instructions (addx.a, subx.a, cmpx.a) instead.
   */
  ERRATUM_CPU30 = 30,		/* UNHANDLED */

  /* CPU31: Instruction PUSHX.A @SP+ corrupts stack pointer
   *
   * When the instruction PUSHX.A is executed using the indirect
   * auto-increment mode with the stack pointer (SP) as the source
   * register [PUSHX.A @SP+] the SP is consequently corrupted. Instead
   * of decrementing the value of the SP by four, the value of the SP
   * is replaced with the data pointed to by the SP previous to the
   * PUSHX.A instruction execution.
   *
   * Workaround:
   * None. The compiler must not generate a PUSHX.A instruction that
   * involves the SP.
   */
  ERRATUM_CPU31 = 31,		/* UNHANDLED */

  /* CPU32: CALLA PC executes incorrectly
   *
   * When the instruction CALLA PC is executed, the program counter
   * (PC) that is pushed onto the stack during the context save is
   * incorrectly offset by a value of -2.
   * 
   * Workaround:
   * None. The compiler must not generate a CALLA PC instruction.
   */
  ERRATUM_CPU32 = 32,		/* UNHANDLED */

  /* CPU33: Instruction sequence PUSH &addr; CALLA x(SP); generates
   *        incorrect access for CALLA
   *
   * When the Stack Pointer (SP) is used as the destination register
   * in the CALLA index(Rdst) instruction and is preceded by a PUSH or
   * PUSHX instruction in any of the following addressing modes:
   * Absolute, Symbolic, Indexed, Indirect register or Indirect auto
   * increment, the "index" of the CALLA instruction is not sign
   * extended to 20-bits and is always treated as a positive
   * value. This causes the Program Counter to be set to a wrong
   * address location when the index of the CALLA instruction
   * represents a negative offset.
   *
   * Note:
   * 1. This erratum only applies when the instruction sequence is:
   *    PUSH or PUSHX followed by CALLA index(SP)
   * 2. This erratum does not apply if the PUSH or PUSHX instruction
   *    is used in the Register or Immediate addressing mode
   * 3. This erratum only applies when SP is used as the destination
   *    register in the CALLA index(Rdst) instruction
   *    
   * Workaround:
   * Place a "NOP" instruction in between the PUSH or PUSHX and the
   * CALLA index(SP) instructions.
   */
  ERRATUM_CPU33 = 33,		/* UNHANDLED */

  /* CPU34: CPU may be halted if a conditional jump is followed by a
   *        rotate PC instruction
   *
   * If a conditional jump instruction (JZ, JNZ, JC, JNC, JN, JGE, JL)
   * is followed by an Address Rotate instruction on the PC (RRCM,
   * RRAM, RLAM, RRUM) and the jump is not performed, the CPU is
   * halted.
   *
   * Workaround:
   * Insert a NOP between the conditional jump and the rotate PC
   * instructions.
   */
  ERRATUM_CPU34 = 34,		/* UNHANDLED */

  /* CPU35: Instruction BIT.B @Rx,PC uses the wrong PC value
   *
   * The BIT(.B/.W) instruction in indirect register addressing mode
   * uses the wrong PC value. This instruction is represented by 2
   * bytes of opcode. If the Program Counter (PC) is used as the
   * destination register, the 2 opcode bytes of the current BIT
   * instruction are not accounted for. The resulting operation
   * executes the instruction using the wrong PC value and this
   * affects the results in the Status Register (SR).
   *
   * Workaround: None
   */
  ERRATUM_CPU35 = 35,		/* UNHANDLED */

  /* CPU40: PC is corrupted when executing jump/conditional jump
   *        instruction that is followed by instruction with PC as
   *        destination register or a data section
   *
   * If the value at the memory location immediately following a
   * jump/conditional jump instruction is 0X40h or 0X50h (where X =
   * don't care), which could either be an instruction opcode (for
   * instructions like RRCM, RRAM, RLAM, RRUM) with PC as destination
   * register or a data section (const data in flash memory or data
   * variable in RAM), then the PC value gets auto-incremented by 2
   * after the jump instruction is executed; thus branching to a wrong
   * address location in code and leading to wrong program execution.
   *
   * For example, a conditional jump instruction followed by data
   * section (0140h).
   * 
   *   @0x8012     Loop      DEC.W R6
   *   @0x8014               DEC.W R7
   *   @0x8016               JNZ Loop
   *   @0x8018     Value1    DW 0140h
   *
   * Workaround:
   * - In assembly, insert a NOP between the jump/conditional jump
   *   instruction and program code with instruction that contains PC
   *   as destination register or the data section.
   */
  ERRATUM_CPU40 = 40,		/* UNHANDLED */
} msp430_errata_e;

/* sed -e '1,/enum msp430_errata/d' -e '/^\}/,$d' tc-msp430.c \
   | grep '^ *ERRATUM_*' \
   | cut -d= -f1 \
   | sed 's@ *$@,@' */
static const int recognized_errata[] = {
  ERRATUM_CPU4,
  ERRATUM_CPU7,
  ERRATUM_CPU8,
  ERRATUM_CPU11,
  ERRATUM_CPU12,
  ERRATUM_CPU13,
  ERRATUM_CPU15,
  ERRATUM_CPU16,
  ERRATUM_CPU18,
  ERRATUM_CPU19,
  ERRATUM_CPU20,
  ERRATUM_CPU21,
  ERRATUM_CPU22,
  ERRATUM_CPU23,
  ERRATUM_CPU24,
  ERRATUM_CPU25,
  ERRATUM_CPU26,
  ERRATUM_CPU27,
  ERRATUM_CPU28,
  ERRATUM_CPU29,
  ERRATUM_CPU30,
  ERRATUM_CPU31,
  ERRATUM_CPU32,
  ERRATUM_CPU33,
  ERRATUM_CPU34,
  ERRATUM_CPU35,
  ERRATUM_CPU40,
};

static int msp430_cpu = MSP430_CPU_MSP430;
static int msp430_mpy = MSP430_MPY_NONE;
static int *msp430_errata = 0;

/* Profiling capability:
   It is a performance hit to use gcc's profiling approach for this tiny target.
   Even more -- jtag hardware facility does not perform any profiling functions.
   However we've got gdb's built-in simulator where we can do anything.
   Therefore my suggestion is:

   We define new section ".profiler" which holds all profiling information.
   We define new pseudo operation .profiler which will instruct assembler to
   add new profile entry to the object file. Profile should take place at the
   present address.

   Pseudo-op format:

      .profiler flags,function_to_profile [, cycle_corrector, extra]

   where 'flags' is a combination of the following chars:
	    s - function Start
	    x - function eXit
	    i - function is in Init section
	    f - function is in Fini section
	    l - Library call
	    c - libC standard call
	    d - stack value Demand (saved at run-time in simulator)
	    I - Interrupt service routine
	    P - Prologue start
	    p - Prologue end
	    E - Epilogue start
	    e - Epilogue end
	    j - long Jump/ sjlj unwind
	    a - an Arbitrary code fragment
	    t - exTra parameter saved (constant value like frame size)
	  '""' optional: "sil" == sil

      function_to_profile - function's address
      cycle_corrector     - a value which should be added to the cycle
			      counter, zero if omitted
      extra - some extra parameter, zero if omitted.

      For example:
      ------------------------------
	.global fxx
	.type fxx,@function
      fxx:
      .LFrameOffset_fxx=0x08
      .profiler "scdP", fxx	; function entry.
				; we also demand stack value to be displayed
	push r11
	push r10
	push r9
	push r8
      .profiler "cdp",fxx,0, .LFrameOffset_fxx	; check stack value at this point
						; (this is a prologue end)
						; note, that spare var filled with the frame size
	mov r15,r8
	....
      .profiler cdE,fxx		; check stack
	pop r8
	pop r9
	pop r10
	pop r11
      .profiler xcde,fxx,3	; exit adds 3 to the cycle counter
      ret			; cause 'ret' insn takes 3 cycles
      -------------------------------

      This profiling approach does not produce any overhead and
      absolutely harmless.
      So, even profiled code can be uploaded to the MCU.  */
#define MSP430_PROFILER_FLAG_ENTRY	1	/* s */
#define MSP430_PROFILER_FLAG_EXIT	2	/* x */
#define MSP430_PROFILER_FLAG_INITSECT	4	/* i */
#define MSP430_PROFILER_FLAG_FINISECT	8	/* f */
#define MSP430_PROFILER_FLAG_LIBCALL	0x10	/* l */
#define MSP430_PROFILER_FLAG_STDCALL	0x20	/* c */
#define MSP430_PROFILER_FLAG_STACKDMD	0x40	/* d */
#define MSP430_PROFILER_FLAG_ISR	0x80	/* I */
#define MSP430_PROFILER_FLAG_PROLSTART	0x100	/* P */
#define MSP430_PROFILER_FLAG_PROLEND	0x200	/* p */
#define MSP430_PROFILER_FLAG_EPISTART	0x400	/* E */
#define MSP430_PROFILER_FLAG_EPIEND	0x800	/* e */
#define MSP430_PROFILER_FLAG_JUMP	0x1000	/* j */
#define MSP430_PROFILER_FLAG_FRAGMENT	0x2000	/* a */
#define MSP430_PROFILER_FLAG_EXTRA	0x4000	/* t */
#define MSP430_PROFILER_FLAG_notyet	0x8000	/* ? */

static int
pow2value (int y)
{
  int n = 0;
  unsigned int x;

  x = y;

  if (!x)
    return 1;

  for (; x; x = x >> 1)
    if (x & 1)
      n++;

  return n == 1;
}

/* Invoke read routines to parse anordinary expression.
 *
 * s points to a standalone operand.  op is where to store the
 * resulting expression.  The call returns a pointer to the suffix of
 * s that remained unparsed.
 *
 * Note that the underlying GAS parsing code may assume that it can
 * modify the contents of the buffer. */
static char *
parse_exp (char *s, expressionS * op)
{
  char *in_save = input_line_pointer;
  input_line_pointer = s;
  expression (op);
  s = input_line_pointer;
  if (op->X_op == O_absent)
    as_bad (_("missing operand"));
  input_line_pointer = in_save;
  return s;
}

/* Extract one operand into a dynamically allocated mutable buffer.
 *
 * The operand begins at input_line_pointer, and is terminated by
 * comma (','), semicolon (';'), or newline ('\n').  Leading and
 * interior whitespace is removed.  If the terminating character is
 * comma (','), it is consumed (but not part of the operand). */
static char *
get_operand (void)
{
  char *sp = input_line_pointer;
  char *end_ilp;
  char *dest;
  char *dp;
  int operand_length = 0;

  while (*sp && ',' != *sp && ';' != *sp && '\n' != *sp)
    {
      if (!ISSPACE (*sp))
	++operand_length;
      ++sp;
    }
  end_ilp = sp;

  dp = dest = xmalloc (operand_length + 1);
  sp = input_line_pointer;
  while (0 < operand_length)
    {
      if (!ISSPACE (*sp))
	{
	  *dp++ = *sp;
	  --operand_length;
	}
      ++sp;
    }
  *dp = 0;
  input_line_pointer = end_ilp;
  if (',' == *input_line_pointer)
    ++input_line_pointer;
  return dest;
}

static void
msp430_profiler (int dummy ATTRIBUTE_UNUSED)
{
  char *flag_token = 0;
  char *flags;
  int p_flags = 0;
  int ops = 0;
  int left;
  char *s;
  segT seg;
  int subseg;
  char *end = 0;
  expressionS exp;
  expressionS exp1;

  s = input_line_pointer;
  end = input_line_pointer;

  while (*end && *end != '\n')
    end++;

  while (*s && *s != '\n')
    {
      if (*s == ',')
	ops++;
      s++;
    }

  left = 3 - ops;

  if (ops < 1)
    {
      as_bad (_(".profiler pseudo requires at least two operands."));
      input_line_pointer = end;
      return;
    }

  flags = flag_token = get_operand ();
  while (*flags)
    {
      switch (*flags)
	{
	case '"':
	  break;
	case 'a':
	  p_flags |= MSP430_PROFILER_FLAG_FRAGMENT;
	  break;
	case 'j':
	  p_flags |= MSP430_PROFILER_FLAG_JUMP;
	  break;
	case 'P':
	  p_flags |= MSP430_PROFILER_FLAG_PROLSTART;
	  break;
	case 'p':
	  p_flags |= MSP430_PROFILER_FLAG_PROLEND;
	  break;
	case 'E':
	  p_flags |= MSP430_PROFILER_FLAG_EPISTART;
	  break;
	case 'e':
	  p_flags |= MSP430_PROFILER_FLAG_EPIEND;
	  break;
	case 's':
	  p_flags |= MSP430_PROFILER_FLAG_ENTRY;
	  break;
	case 'x':
	  p_flags |= MSP430_PROFILER_FLAG_EXIT;
	  break;
	case 'i':
	  p_flags |= MSP430_PROFILER_FLAG_INITSECT;
	  break;
	case 'f':
	  p_flags |= MSP430_PROFILER_FLAG_FINISECT;
	  break;
	case 'l':
	  p_flags |= MSP430_PROFILER_FLAG_LIBCALL;
	  break;
	case 'c':
	  p_flags |= MSP430_PROFILER_FLAG_STDCALL;
	  break;
	case 'd':
	  p_flags |= MSP430_PROFILER_FLAG_STACKDMD;
	  break;
	case 'I':
	  p_flags |= MSP430_PROFILER_FLAG_ISR;
	  break;
	case 't':
	  p_flags |= MSP430_PROFILER_FLAG_EXTRA;
	  break;
	default:
	  as_warn (_("unknown profiling flag - ignored."));
	  break;
	}
      flags++;
    }
  xfree (flag_token);

  if (p_flags
      && (!pow2value (p_flags & (MSP430_PROFILER_FLAG_ENTRY
				 | MSP430_PROFILER_FLAG_EXIT))
	  || !pow2value (p_flags & (MSP430_PROFILER_FLAG_PROLSTART
				    | MSP430_PROFILER_FLAG_PROLEND
				    | MSP430_PROFILER_FLAG_EPISTART
				    | MSP430_PROFILER_FLAG_EPIEND))
	  || !pow2value (p_flags & (MSP430_PROFILER_FLAG_INITSECT
				    | MSP430_PROFILER_FLAG_FINISECT))))
    {
      as_bad (_
	      ("ambiguous flags combination - '.profiler' directive ignored."));
      input_line_pointer = end;
      return;
    }

  /* Generate temp symbol which denotes current location.  */
  if (now_seg == absolute_section)	/* Paranoia ?  */
    {
      exp1.X_op = O_constant;
      exp1.X_add_number = abs_section_offset;
      as_warn (_("profiling in absolute section?"));
    }
  else
    {
      exp1.X_op = O_symbol;
      exp1.X_add_symbol = symbol_temp_new_now ();
      exp1.X_add_number = 0;
    }

  /* Generate a symbol which holds flags value.  */
  exp.X_op = O_constant;
  exp.X_add_number = p_flags;

  /* Save current section.  */
  seg = now_seg;
  subseg = now_subseg;

  /* Now go to .profiler section.  */
  obj_elf_change_section (".profiler", SHT_PROGBITS, 0, 0, 0, 0, 0);

  /* Save flags.  */
  emit_expr (&exp, 2);

  /* Save label value.  */
  emit_expr (&exp1, 2);

  while (ops--)
    {
      /* Now get profiling info.  */
      char *halt = get_operand ();
      /* Process like ".word xxx" directive.  */
      parse_exp (halt, &exp);
      emit_expr (&exp, 2);
      xfree (halt);
    }

  /* Fill the rest with zeros.  */
  exp.X_op = O_constant;
  exp.X_add_number = 0;
  while (left--)
    emit_expr (&exp, 2);

  /* Return to current section.  */
  subseg_set (seg, subseg);
}

struct tag_value_pair_t
{
  const char *tag;
  unsigned long value;
};

static const struct tag_value_pair_t cpu_tag_value_map[] = {
  {"430", MSP430_CPU_MSP430},
  {"430x", MSP430_CPU_MSP430X},
  {"430xv2", MSP430_CPU_MSP430XV2},
  {0, 0}
};

static const struct tag_value_pair_t mpy_tag_value_map[] = {
  {"none", MSP430_MPY_NONE},
  {"16", MSP430_MPY_16},
  {"16se", MSP430_MPY_16SE},
  {"32", MSP430_MPY_32},
  {"32dw", MSP430_MPY_32DW},
  {0, 0}
};

static const struct tag_value_pair_t *
find_pair_by_tag (const char *tag, const struct tag_value_pair_t *map)
{
  while (map->tag)
    {
      if (0 == strcmp (tag, map->tag))
	return map;
      ++map;
    }
  return 0;
}

#if 0
static const struct tag_value_pair_t *
find_pair_by_value (unsigned long value, const struct tag_value_pair_t *map)
{
  while (map->tag)
    {
      if (map->value == value)
	return map;
      ++map;
    }
  return 0;
}
#endif

enum
{
  OPTION_MMCU = 'm',
  OPTION_CPU = OPTION_MD_BASE + 0,
  OPTION_MPY,
  OPTION_ERRATA,
};

static int
erratum_applies (int erratum)
{
  if (!msp430_errata)
    if (ERRATUM_CPU4 == erratum)
      return msp430_cpu < MSP430_CPU_MSP430X;
  return 0;
}

static int
cpu_from_text (const char *text)
{
  const struct tag_value_pair_t *mp =
    find_pair_by_tag (text, cpu_tag_value_map);
  if (!mp)
    as_fatal (_("unrecognized cpu type %s"), text);
  return mp->value;
}

static int
mpy_from_text (const char *text)
{
  const struct tag_value_pair_t *mp =
    find_pair_by_tag (text, mpy_tag_value_map);
  if (!mp)
    as_fatal (_("unrecognized hardware multiplier type %s"), text);
  return mp->value;
}

static void
set_arch_mach (int cpu, int mpy ATTRIBUTE_UNUSED)
{
  unsigned long mach = bfd_mach_msp430;
  if (MSP430_CPU_MSP430X <= cpu)
    mach = bfd_mach_msp430x;
  bfd_set_arch_mach (stdoutput, TARGET_ARCH, mach);
}

/* Like get_symbol_end, but accepts sequences that start with
 * digits and doesn't strip out name ends */
static char
get_token_end (void)
{
  char c;

  while (is_part_of_name (c = *input_line_pointer++))
    ;
  *--input_line_pointer = 0;
  return (c);
}

static void
msp430_set_mcu (int dummy ATTRIBUTE_UNUSED)
{
  SKIP_WHITESPACE ();
  if (!is_end_of_line[(unsigned char) *input_line_pointer])
    {
      char *tag_start = input_line_pointer;
      int ch = get_token_end ();
      md_parse_option (OPTION_MMCU, tag_start);
      *input_line_pointer = ch;
      demand_empty_rest_of_line ();
    }
  else
    as_bad (_("missing value for arch"));
}

static void
msp430_set (int option)
{
  char *tag = 0;

  SKIP_WHITESPACE ();
  if (!is_end_of_line[(unsigned char) *input_line_pointer])
    {
      char *tag_start = input_line_pointer;
      char e = get_token_end ();
      int tag_len = input_line_pointer + 1 - tag_start;
      tag = (char *) xmalloc (tag_len);
      memcpy (tag, tag_start, tag_len);
      *input_line_pointer = e;
      demand_empty_rest_of_line ();
    }

  switch (option)
    {
    case OPTION_CPU:
      if (!tag)
	as_bad (_("missing value for cpu"));
      msp430_cpu = cpu_from_text (tag);
      break;
    case OPTION_MPY:
      if (!tag)
	as_bad (_("missing value for mpy"));
      msp430_mpy = mpy_from_text (tag);
      break;
    default:
      abort ();
    }

  set_arch_mach (msp430_cpu, msp430_mpy);

}

static void
msp430_set_errata (int dummy ATTRIBUTE_UNUSED)
{
}

int
md_parse_option (int c, char *arg)
{
  switch (c)
    {
    case OPTION_MMCU:
      /* TODO: warn about unrecognized mcu option  */
      return 1;
      break;

    case OPTION_CPU:
      msp430_cpu = cpu_from_text (arg);
      return 1;
      break;

    case OPTION_MPY:
      msp430_mpy = mpy_from_text (arg);
      return 1;
      break;

    case OPTION_ERRATA:
      break;
    }

  return 0;
}

static void msp430_repeat_insn (int dummy ATTRIBUTE_UNUSED);

const pseudo_typeS md_pseudo_table[] = {
  {"arch", msp430_set_mcu, 0},
  {"profiler", msp430_profiler, 0},
  {"rpt", msp430_repeat_insn, 0},
  {"cpu", msp430_set, OPTION_CPU},
  {"mpy", msp430_set, OPTION_MPY},
  {"errata", msp430_set_errata, 0},
  {NULL, NULL, 0}
};

const char *md_shortopts = "m:";

struct option md_longopts[] = {
  {"mmcu", required_argument, NULL, OPTION_MMCU},
  {"mcpu", required_argument, NULL, OPTION_CPU},
  {"mmpy", required_argument, NULL, OPTION_MPY},
  {"merrata", required_argument, NULL, OPTION_ERRATA},
  {NULL, no_argument, NULL, 0}
};

size_t md_longopts_size = sizeof (md_longopts);

void
md_show_usage (FILE * stream)
{
  fprintf (stream,
	   _("MSP430 as-specific options:\n"
	     "  -mmcu=[msp430-name] select microcontroller type (ignored)\n"
	     "  -mcpu={430,430x,430xv2} select cpu model\n"
	     "  -mmpy={none,16,16se,32,32dw} select hardware multiplier type\n"
	     "  -merrata=[cpuX,...] list relevant errata\n"));
}

symbolS *
md_undefined_symbol (char *name)
{
  if ('r' == TOLOWER (name[0]) && ISDIGIT (name[1]))
    {
      char *rp = name + 1;
      unsigned int regno = *rp++ - '0';

      if (ISDIGIT (*rp))
	regno = (regno * 10) + *rp++ - '0';
      if (0 == *rp && regno <= REGNO_MAX)
	return msp430_register_table[regno];
    }
  return 0;
}

char *
md_atof (int type, char *litP, int *sizeP)
{
  return ieee_md_atof (type, litP, sizeP, FALSE);
}

void
md_begin (void)
{
  int regno;
  struct msp430_opcode_s const *opcode;
  msp430_hash = hash_new ();

  for (opcode = msp430_opcodes; opcode->name; opcode++)
    hash_insert (msp430_hash, opcode->name, (char *) opcode);
  for (regno = REGNO_MIN; regno <= REGNO_MAX; ++regno)
    {
      char regname[4];
      snprintf (regname, sizeof (regname), "r%u", regno);
      msp430_register_table[regno] =
	symbol_create (regname, reg_section, regno, &zero_address_frag);
    }

  set_arch_mach (msp430_cpu, msp430_mpy);
}

static void
msp430_address_substitute_CG (struct msp430_opcode_s const *opcode,
			      struct msp430_operand_s *op)
{
  offsetT supported_cg2;

  /* Substitute CG2 if applicable.  */
  if (!OP_HAS_IMMEDIATE (*op)
      || (op->exp.X_op != O_constant && op->exp.X_op != O_big))
    return;
  if (op->am != AMs_Immediate || op->reg != REGNO_PC)	/* not #N */
    return;
  
  if (opcode_variant (opcode) == V_CG2_TWO)
    supported_cg2 = 2;
  else
    supported_cg2 = 0;
  if (op->exp.X_add_number == supported_cg2)
    {
      op->reg = REGNO_CG2;
      op->am = AM_Register;
      op->ol = 0;
    }
}

static void
msp430_substitute_CG (struct msp430_operand_s *op, opwidth_t op_width,
		      int workaround)
{
  /* Substitute register mode with a constant generator if applicable.  */
  if (!OP_HAS_IMMEDIATE (*op)
      || (op->exp.X_op != O_constant && op->exp.X_op != O_big))
    return;
  if (op->am != AMs_Immediate || op->reg != REGNO_PC)	/* not #N */
    return;
  offsetT x = op->exp.X_add_number;

  if ((BYTE_OP == op_width && MASK_8 (x) == MASK_8 (-1))
      || (WORD_OP == op_width && MASK_16 (x) == MASK_16 (-1))
      || (ADDR_OP == op_width && MASK_20 (x) == MASK_20 (-1)))
    x = -1;

  if (x == 0)
    {
      op->reg = REGNO_CG2;
      op->am = 0;		/* AM_Register */
      op->ol = 0;
    }
  else if (x == 1)
    {
      op->reg = REGNO_CG2;
      op->am = 1;		/* AM_Indexed */
      op->ol = 0;
    }
  else if (x == 2)
    {
      op->reg = REGNO_CG2;
      op->am = 2;		/* AMs_IndirectRegister */
      op->ol = 0;
    }
  else if (x == -1)
    {
      op->reg = REGNO_CG2;
      op->am = 3;		/* AMs_IndirectAutoIncrement */
      op->ol = 0;
    }
  else if (x == 4 && !workaround)
    {
      op->reg = REGNO_CG1;
      op->am = 2;		/* AMs_IndirectRegister */
      op->ol = 0;
    }
  else if (x == 8 && !workaround)
    {
      op->reg = REGNO_CG1;
      op->am = 3;		/* AMs_IndirectAutoIncrement */
      op->ol = 0;
    }
}

static bfd_boolean
msp430_srcoperand (struct msp430_operand_s *op,
		   const char *operand_string,
		   enum immediate_range_e imm_range)
{
  symbolS * symbol;
  char *opstr = "";

  if (operand_string)
    {
      opstr = alloca (strlen (operand_string) + 1);
      strcpy (opstr, operand_string);
    }

  /* Check if an immediate #VALUE.  The hash sign should be only at the beginning!  */
  if (*opstr == '#')
    {
      int vshift = -1;
      int extractor_len = 0;

      /* Check if there is:
         llo(x) - least significant 16 bits, x &= 0xffff
         lhi(x) - x = (x >> 16) & 0xffff,
         hlo(x) - x = (x >> 32) & 0xffff,
         hhi(x) - x = (x >> 48) & 0xffff
         The value _MUST_ be constant expression: #hlo(1231231231).  */

      if (strncasecmp (opstr, "#llo(", 5) == 0)
	{
	  vshift = 0;
	  extractor_len = 3;
	}
      else if (strncasecmp (opstr, "#lhi(", 5) == 0)
	{
	  vshift = 1;
	  extractor_len = 3;
	}
      else if (strncasecmp (opstr, "#hlo(", 5) == 0)
	{
	  vshift = 2;
	  extractor_len = 3;
	}
      else if (strncasecmp (opstr, "#hhi(", 5) == 0)
	{
	  vshift = 3;
	  extractor_len = 3;
	}
      else if (strncasecmp (opstr, "#lo(", 4) == 0)
	{
	  vshift = 0;
	  extractor_len = 2;
	}
      else if (strncasecmp (opstr, "#hi(", 4) == 0)
	{
	  vshift = 1;
	  extractor_len = 2;
	}

      op->reg = REGNO_PC;
      op->am = AMs_Immediate;
      op->ol = 1;		/* Immediate  will follow an instruction.  */

      /* Extract the value after the hash and any extractor */
      opstr = parse_exp (opstr + 1 + extractor_len, &(op->exp));
      if (0 != *opstr)
	{
	  as_bad (_("garbage after immediate: %s"), operand_string);
	  return FALSE;
	}
      if (op->exp.X_op == O_constant)
	{
	  if (0 <= vshift)
	    {
	      int is_negative = 0 > op->exp.X_add_number;
	      unsigned int shift_bits = vshift * 16;

	      if (shift_bits < (8 * sizeof (op->exp.X_add_number)))
		{
		  offsetT shifted = op->exp.X_add_number >> shift_bits;
		  shifted = MASK_16 (shifted);
		  if (shifted == MASK_16 (-1))
		    shifted = -1;
		  op->exp.X_add_number = shifted;
		}
	      else
		op->exp.X_add_number = is_negative ? -1 : 0;
	    }

	  return valid_immediate (op->exp.X_add_number, imm_range);
	}
      if (op->exp.X_op == O_symbol || op->exp.X_op == O_subtract)
	return TRUE;
      if (op->exp.X_op == O_big)
	{
	  if (0 <= vshift)
	    {
	      op->exp.X_op = O_constant;
	      op->exp.X_add_number = MASK_16 (generic_bignum[vshift]);
	      if (op->exp.X_add_number == MASK_16 (-1))
		op->exp.X_add_number = -1;
	      return valid_immediate (op->exp.X_add_number, imm_range);
	    }
	  as_bad (_("operand too big (use #llo(), etc.): %s"),
		  operand_string);
	  return FALSE;
	}
      as_bad (_("invalid immediate operand: %s"), operand_string);
      return FALSE;
    }

  /* Check if absolute &VALUE (assume that we can construct something like ((a&b)<<7 + 25).  */
  if (*opstr == '&')
    {
      op->reg = REGNO_CG1;	/* reg 2 in absolute addr mode.  */
      op->am = AM_Absolute;	/* mode As == 01 bin.  */
      op->ol = 1;		/* Immediate value followed by instruction.  */
      opstr = parse_exp (opstr + 1, &(op->exp));
      if (0 != *opstr)
	{
	  as_bad (_("garbage after absolute: %s"), operand_string);
	  return FALSE;
	}
      if (op->exp.X_op == O_constant)
	return valid_immediate (op->exp.X_add_number, imm_range);
      if (op->exp.X_op == O_symbol || op->exp.X_op == O_subtract)
	return TRUE;

      as_bad (_("invalid absolute operand: %s"), operand_string);
      return FALSE;
    }

  /* Check if indirect register mode @Rn / postincrement @Rn+.  */
  if (*opstr == '@')
    {
      expressionS deref;
      char *plusp = strchr (opstr, '+');

      if (NULL != plusp)
	*plusp = 0;
      opstr = parse_exp (opstr + 1, &deref);
      if (NULL != plusp)
	*plusp = '+';
      if (deref.X_op != O_register)
	{
	  as_bad (_("invalid indirect operand: %s"), operand_string);
	  return FALSE;
	}

      op->ol = 0;
      op->reg = deref.X_add_number;
      op->am = AMs_IndirectRegister;
      if ('+' == *opstr)
	{
	  op->am = AMs_IndirectAutoIncrement;
	  ++opstr;
	}
      if (0 != *opstr)
	{
	  as_bad (_("garbage after indirection: %s"), operand_string);
	  return FALSE;
	}
      return TRUE;
    }

  /* Check if register indexed X(Rn).  */
  do
    {
      char *lparenp = strrchr (opstr, '(');
      char *rparenp;
      char *irendp;
      expressionS index_register;

      /* Commit to register if the operand ends with a parenthesized
       * register. */
      if (NULL == lparenp)
	break;
      rparenp = strchr (lparenp + 1, ')');
      if (NULL == rparenp)
	break;

      *rparenp = 0;
      irendp = parse_exp (lparenp + 1, &index_register);
      *rparenp = ')';
      if (index_register.X_op != O_register || irendp != rparenp)
	break;

      op->am = AM_Indexed;
      op->ol = 1;
      op->reg = index_register.X_add_number;

      /* Extract what precedes (Rn) which should be an expression */
      *lparenp = 0;
      opstr = parse_exp (opstr, &op->exp);
      *lparenp = '(';
      if (opstr != lparenp)
	{
	  as_bad (_("garbage after offset in indexed operand: %s"),
		  operand_string);
	  return FALSE;
	}

      /* Validate constant, convert to indirect if possible */
      if (op->exp.X_op == O_constant)
	{
	  if (!valid_immediate (op->exp.X_add_number, imm_range))
	    return FALSE;

	  if (op->exp.X_add_number == 0)
	    {
	      op->am = AMs_IndirectRegister;
	      op->ol = 0;
	    }
	  return TRUE;
	}
      if (op->exp.X_op == O_symbol || op->exp.X_op == O_subtract)
	return TRUE;

      as_bad (_("invalid indexed operand: %s"), operand_string);
      return FALSE;
    }
  while (0);

  /* Assume a symbolic, but correct if it's a register */
  opstr = parse_exp (opstr, &(op->exp));
  if (op->exp.X_op == O_register)
    {
      int regno = op->exp.X_add_number;

      if (0 != *opstr)
	{
	  as_bad (_("garbage after register operand: %s"), operand_string);
	  return FALSE;
	}
      memset (op, 0, sizeof (*op));
      op->reg = regno;
      op->am = AM_Register;
      op->ol = 0;
      return TRUE;
    }
  if (0 != *opstr)
    {
      as_bad (_("garbage after operand: %s"), operand_string);
      return FALSE;
    }
  op->reg = REGNO_PC;
  op->am = AM_Symbolic;
  op->ol = 1;
  /* Make the offset PC-relative */
  symbol = make_expr_symbol (&op->exp);
  memset (&op->exp, 0, sizeof (op->exp));
  op->exp.X_op = O_subtract;
  op->exp.X_add_symbol = symbol;
  op->exp.X_op_symbol = expr_build_dot ();

  return TRUE;
}

static bfd_boolean
convert_operand_to_dst (struct msp430_operand_s *dop,
			const struct msp430_operand_s *sop,
			const char *operand_string)
{
  if (sop != dop)
    *dop = *sop;

  if (dop->am <= AM_Indexed)
    return TRUE;

  /* Destination does not recognize indirect register: convert to
   * indexed register with zero offset */
  if (dop->am == AMs_IndirectRegister)
    {
      dop->am = AM_Indexed;
      dop->ol = 1;
      memset (&dop->exp, 0, sizeof (dop->exp));
      dop->exp.X_op = O_constant;
      dop->exp.X_add_number = 0;
      return TRUE;
    }

  /* Note that symbols and absolute addresses are indexed registers */
  as_bad (_("invalid destination (must be register or indexed register): %s"),
	  operand_string);
  return FALSE;
}


static bfd_boolean
msp430_dstoperand (struct msp430_operand_s *op,
		   const char *operand_string,
		   enum immediate_range_e imm_range)
{
  if (!msp430_srcoperand (op, operand_string, imm_range))
    return FALSE;
  return convert_operand_to_dst (op, op, operand_string);
}

static void
msp430_repeat_insn (int dummy ATTRIBUTE_UNUSED)
{
  char *operand = 0;
  struct msp430_operand_s op;
  char *line = input_line_pointer;

  if (msp430_cpu < MSP430_CPU_MSP430X)
    {
      as_bad (_("Repeatable instructions require 430X-based mcu"));
      return;
    }

  if (msp430x_repeats)
    as_warn (_("two consecutive .rpt pseudo-ops. Previous .rpt discarded"));
  msp430x_repeats = 0;

  if (!*line || *line == '\n')
    {
      as_bad (_("rpt pseudo-op requires 1 operand"));
      return;
    }

  memset (&op, 0, sizeof (op));
  operand = get_operand ();

  if (msp430_srcoperand (&op, operand, IMM_RANGE_REPETITION_COUNT))
    {
      if (op.am != AM_Register	/* Rn */
	  && op.am != AMs_Immediate)	/* #N */
	as_bad (_("rpt pseudo-op requires immediate or register operand"));
      else if (op.am == AM_Register)	/* rpt Rn */
	msp430x_repeats = 0x80 | op.reg;
      else
	msp430x_repeats = op.exp.X_add_number - 1;
    }

  xfree (operand);
}

enum msp430_fixup_e {
  FX_INVALID,

  /* R_MSP430_16, odd check SRC */
  FX_16S,

  /* R_MSP430_16, odd checks DST */
  FX_16D,

  /* R_MSP430X_SRC, odd check SRC */
  FX_X20S,

  /* R_MSP430X_DST, odd check DST */
  FX_X20D,

  /* R_MSP430X_DST, odd check SRC (for pushx) */
  FX_X20D_SRCI,

  /* R_MSP430X_DST_2ND, odd check DST */
  FX_X20D2,

  /* R_MSP430X_S */
  FX_A20S,
  
  /* R_MSP430X_S, disallows byte variant (for bra) */
  FX_A20S_W,
  
  /* R_MSP430X_D */
  FX_A20D,

  /* R_MSP430X_INDXD */
  FX_A16
};

static void
record_fixup (int where,
	      int size,
	      opwidth_t op_width,
	      struct msp430_operand_s *op,
	      enum msp430_fixup_e fixup)
{
  int is_pcrel;
  bfd_reloc_code_real_type reloc = BFD_RELOC_NONE;

  is_pcrel = (op->reg == REGNO_PC && op->am == AM_Indexed && op->exp.X_op == O_subtract);
  switch (fixup)
    {
    case FX_16D:
      reloc = is_pcrel ? BFD_RELOC_MSP430_16_PCREL : BFD_RELOC_MSP430_16;
      if (op_width == BYTE_OP || OP_DST_IMMEDIATE_PERMITS_ODD (*op))
	reloc = is_pcrel ? BFD_RELOC_MSP430_16_PCREL_BYTE : BFD_RELOC_MSP430_16_BYTE;
      break;
    case FX_16S:
      reloc = is_pcrel ? BFD_RELOC_MSP430_16_PCREL : BFD_RELOC_MSP430_16;
      if (op_width == BYTE_OP || OP_SRC_IMMEDIATE_PERMITS_ODD (*op))
	reloc = is_pcrel ? BFD_RELOC_MSP430_16_PCREL_BYTE : BFD_RELOC_MSP430_16_BYTE;
      break;
    case FX_X20S:
      reloc = is_pcrel ? BFD_RELOC_MSP430X_PCREL_SRC : BFD_RELOC_MSP430X_SRC;
      if (op_width == BYTE_OP || OP_SRC_IMMEDIATE_PERMITS_ODD (*op))
	reloc = is_pcrel ? BFD_RELOC_MSP430X_PCREL_SRC_BYTE : BFD_RELOC_MSP430X_SRC_BYTE;
      break;
    case FX_X20D:
      reloc = is_pcrel ? BFD_RELOC_MSP430X_PCREL_DST : BFD_RELOC_MSP430X_DST;
      if (op_width == BYTE_OP || OP_DST_IMMEDIATE_PERMITS_ODD (*op))
	reloc = is_pcrel ? BFD_RELOC_MSP430X_PCREL_DST_BYTE : BFD_RELOC_MSP430X_DST_BYTE;
      break;
    case FX_X20D_SRCI:
      reloc = is_pcrel ? BFD_RELOC_MSP430X_PCREL_DST : BFD_RELOC_MSP430X_DST;
      if (op_width == BYTE_OP || OP_SRC_IMMEDIATE_PERMITS_ODD (*op))
	reloc = is_pcrel ? BFD_RELOC_MSP430X_PCREL_DST_BYTE : BFD_RELOC_MSP430X_DST_BYTE;
      break;
    case FX_X20D2:
      reloc = is_pcrel ? BFD_RELOC_MSP430X_PCREL_DST_2ND : BFD_RELOC_MSP430X_DST_2ND;
      if (op_width == BYTE_OP || OP_DST_IMMEDIATE_PERMITS_ODD (*op))
	reloc = is_pcrel ? BFD_RELOC_MSP430X_PCREL_DST_2ND_BYTE : BFD_RELOC_MSP430X_DST_2ND_BYTE;
      break;
    case FX_A20S_W:
    case FX_A20S:
      /* NB: No A20S that are PCREL */
      reloc = /* is_pcrel ? BFD_RELOC_MSP430X_PCREL_S : */ BFD_RELOC_MSP430X_S;
      if (FX_A20S_W != fixup && (op_width == BYTE_OP || OP_SRC_IMMEDIATE_PERMITS_ODD (*op)))
	reloc = /* is_pcrel ? BFD_RELOC_MSP430X_PCREL_S_BYTE : */ BFD_RELOC_MSP430X_S_BYTE;
      break;
    case FX_A20D:
      reloc = is_pcrel ? BFD_RELOC_MSP430X_PCREL_D : BFD_RELOC_MSP430X_D;
      break;
    case FX_A16:
      reloc = is_pcrel ? BFD_RELOC_MSP430X_PCREL_INDXD : BFD_RELOC_MSP430X_INDXD;
      break;
    default:
      gas_assert (0);
    }
  fix_new_exp (frag_now, where, size, &op->exp, is_pcrel, reloc);
}

/* Parse instruction operands.
   Return binary opcode.  */

static unsigned int
msp430_operands (struct msp430_opcode_s const *opcode, char *line)
{
  int bin = opcode->bin_opcode;	/* Opcode mask.  */
  enum msp430_fixup_e fixup;
  int insn_len_words = 0;
  char *l1 = 0;
  char *l2 = 0;
  char *frag = 0;
  int where = 0;
  struct msp430_operand_s op1, op2;
  bfd_boolean bad_operand = FALSE;
  static short ZEROS = 0;
  opwidth_t op_width;

  /* Opcode is the one from opcodes table
     line contains something like
     [.w] @r2+, 5(R1)
     or
     .b @r2+, 5(R1)
     or
     .a @r2+, 5(R1)   */

  /* Extract operation width */
  op_width = DEFAULT_OP;
  if (*line == '.')
    {
      int opchar;
      opchar = TOLOWER (line[1]);
      if (opchar == 'b')
	op_width = BYTE_OP;
      else if (opchar == 'w')
	op_width = WORD_OP;
      else if (opchar == 'a')
	op_width = ADDR_OP;
      if (op_width == DEFAULT_OP || !ISSPACE (line[2]))
	{
	  as_bad (_("unrecognized opcode width: %s%s"), opcode->name, line);
	  return 0;
	}
      line += 2;
    }

  /* Verify specified width is supported by operation */
  if ((op_width == WORD_OP && !(opcode_modifier (opcode) & MOD_W))
      || (op_width == BYTE_OP && !(opcode_modifier (opcode) & MOD_B))
      || (op_width == ADDR_OP && !(opcode_modifier (opcode) & MOD_A)))
    {
      static char *const modifier[] = { "", ".w", ".b", ".a" };
      as_bad (_("%s%s is not a supported operation/width combination"),
	      opcode->name, modifier[op_width]);
      return 0;
    }
  if (DEFAULT_OP == op_width)
    op_width = WORD_OP;

  if (opcode_format (opcode) == FMT_X_DOUBLE_OPERAND
      || opcode_format (opcode) == FMT_X_SINGLE_OPERAND
      || opcode_format (opcode) == FMT_X_EMULATED)
    {
      switch (op_width)
	{
	case DEFAULT_OP:
	case WORD_OP:
	  bin |= NON_ADDR_OPERATION;
	  break;
	case BYTE_OP:
	  bin |= NON_ADDR_OPERATION;
	  bin |= BYTE_OPERATION_X;
	  break;
	case ADDR_OP:
	  bin |= BYTE_OPERATION_X;
	  break;
	}
    }
  else
    {
      if (msp430x_repeats)
	{
	  as_bad (_("%s instruction is not repeatable"), opcode->name);
	  msp430x_repeats = 0;
	  return 0;
	}

      if (opcode_format (opcode) < FMT_X && op_width == BYTE_OP)	/* 430 instructions */
	bin |= BYTE_OPERATION;
    }

  if (opcode->insn_opnumb && (!*line || *line == '\n'))
    {
      as_bad (_("instruction %s requires %d operand(s)"),
	      opcode->name, opcode->insn_opnumb);
      return 0;
    }

  input_line_pointer = line;

  memset (&op1, 0, sizeof (op1));
  memset (&op2, 0, sizeof (op2));

  switch (opcode_format (opcode))
    {
    case FMT_EMULATED:		/* Emulated.  */
      switch (opcode_variant (opcode))
	{
	case V_NOOP:		/* Exemplar: setz */
	  /* Set/clear SR bits instructions, ret, nop  */
	  insn_len_words = 1;
	  frag = frag_more (2 * insn_len_words);
	  bfd_putl16 ((bfd_vma) bin, frag);
	  break;
	case V_NONE:		/* Exemplar: inv */
	  /* Something which works with destination operand.  */
	  l1 = get_operand ();
	  if (!msp430_dstoperand (&op1, l1, IMM_RANGE_INT16))
	    break;

	  bin |= (op1.reg | (op1.am << 7));
	  insn_len_words = 1 + op1.ol;
	  frag = frag_more (2 * insn_len_words);
	  where = frag - frag_now->fr_literal;
	  bfd_putl16 ((bfd_vma) bin, frag);

	  if (OP_HAS_IMMEDIATE (op1))
	    {
	      where += 2;
	      bfd_putl16 ((bfd_vma) ZEROS, frag + 2);
	      record_fixup (where, 2, op_width, &op1, FX_16D);
	    }
	  break;

	case V_SHIFT:		/* Exemplar: rlc */
	  {
	    /* Shift instruction simulated by add op1, op2 */
	    l1 = get_operand ();
	    if (!msp430_srcoperand (&op1, l1, IMM_RANGE_INT16))
	      break;
	    if (!convert_operand_to_dst (&op2, &op1, l1))
	      break;

	    bin |= (op2.reg | (op1.reg << 8) | (op1.am << 4) | (op2.am << 7));

	    insn_len_words = 1 + op1.ol + op2.ol;	/* insn size in words.  */
	    frag = frag_more (2 * insn_len_words);
	    where = frag - frag_now->fr_literal;
	    bfd_putl16 ((bfd_vma) bin, frag);

	    if (OP_HAS_IMMEDIATE (op1))
	      {
		where += 2;	/* Advance 'where' as we do not know _where_.  */
		bfd_putl16 ((bfd_vma) ZEROS, frag + 2);
		record_fixup (where, 2, op_width, &op1, FX_16S);
	      }

	    if (OP_HAS_IMMEDIATE (op2))
	      {
		bfd_putl16 ((bfd_vma) ZEROS,
			    frag + 2 + ((insn_len_words == 3) ? 2 : 0));
		record_fixup (where + 2, 2, op_width, &op2, FX_16D);
	      }
	    break;
	  }
	case V_BR:		/* Exemplar: br */
	  /* Branch instruction => mov dst, r0.  */
	  l1 = get_operand ();
	  if (!msp430_srcoperand (&op1, l1, IMM_RANGE_INT16))
	    break;
	  msp430_substitute_CG (&op1, WORD_OP, 0);

	  bin |= ((op1.reg << 8) | (op1.am << 4));
	  insn_len_words = 1 + op1.ol;
	  frag = frag_more (2 * insn_len_words);
	  where = frag - frag_now->fr_literal;
	  bfd_putl16 ((bfd_vma) bin, frag);

	  if (OP_HAS_IMMEDIATE (op1))
	    {
	      where += 2;
	      bfd_putl16 ((bfd_vma) ZEROS, frag + 2);
	      /* NB: Use DST_IMMEDIATE since this is going into r0 */
	      record_fixup (where, 2, op_width, &op1, FX_16D);
	    }
	  break;
	}
      break;

    case FMT_DOUBLE_OPERAND:	/* Exemplar: and */
      /* Format 1, double operand.  */
      l1 = get_operand ();
      l2 = get_operand ();
      bad_operand = !msp430_srcoperand (&op1, l1, IMM_RANGE_INT16);
      bad_operand |= !msp430_dstoperand (&op2, l2, IMM_RANGE_INT16);
      if (bad_operand)
	break;

      msp430_substitute_CG (&op1, op_width, 0);

      bin |= (op2.reg | (op1.reg << 8) | (op1.am << 4) | (op2.am << 7));

      insn_len_words = 1 + op1.ol + op2.ol;	/* insn size in words.  */
      frag = frag_more (2 * insn_len_words);
      where = frag - frag_now->fr_literal;
      bfd_putl16 ((bfd_vma) bin, frag);

      if (OP_HAS_IMMEDIATE (op1))
	{
	  where += 2;		/* Advance where as we do not know _where_.  */
	  bfd_putl16 ((bfd_vma) ZEROS, frag + 2);
	  record_fixup (where, 2, op_width, &op1, FX_16S);
	}

      if (OP_HAS_IMMEDIATE (op2))
	{
	  bfd_putl16 ((bfd_vma) ZEROS,
		      frag + 2 + ((insn_len_words == 3) ? 2 : 0));
	  record_fixup (where + 2, 2, op_width, &op2, FX_16D);
	}
      break;

    case FMT_SINGLE_OPERAND:	/* Exemplar: sxt, call, reti */
      /* Single-operand mostly instr.  */
      if (opcode_variant (opcode) == V_RETI)	/* Exemplar: reti */
	{
	  /* reti instruction.  */
	  insn_len_words = 1;
	  frag = frag_more (2 * insn_len_words);
	  bfd_putl16 ((bfd_vma) bin, frag);
	  break;
	}

      l1 = get_operand ();
      if (!msp430_srcoperand (&op1, l1, IMM_RANGE_INT16))
	break;			/* Error in operand.  */
      msp430_substitute_CG (&op1, op_width, erratum_applies (ERRATUM_CPU4)
			    && (opcode->bin_opcode == 0x1200));

      bin |= op1.reg | (op1.am << 4);
      insn_len_words = 1 + op1.ol;
      frag = frag_more (2 * insn_len_words);
      where = frag - frag_now->fr_literal;
      bfd_putl16 ((bfd_vma) bin, frag);

      if (OP_HAS_IMMEDIATE (op1))
	{
	  bfd_putl16 ((bfd_vma) ZEROS, frag + 2);
	  record_fixup (where + 2, 2, op_width, &op1,
			opcode_variant (opcode) == V_PUSH ? FX_16S : FX_16D);
	}
      break;

    case FMT_JUMP:		/* Exemplar: jmp */
      {
	expressionS exp;
	
	/* Conditional jumps instructions.  */
	l1 = get_operand ();
	parse_exp(l1, &exp);
	if (exp.X_op == O_absent)
	  break;
      
	insn_len_words = 1;
	frag = frag_more (2 * insn_len_words);	/* Instr size is 1 word.  */

	if (exp.X_op == O_constant)
	  {
	    int x = exp.X_add_number;

	    if (MSP430_ODD (x))
	      {
		as_bad (_("jump offset must be even: %s"), l1);
		break;
	      }
	    bin |= MASK_10 (exp.X_add_number / 2);
	    bfd_putl16 ((bfd_vma) bin, frag);
	  }
	else if (exp.X_op == O_symbol || exp.X_op == O_subtract)
	  {
	    where = frag - frag_now->fr_literal;
	    bfd_putl16 ((bfd_vma) bin, frag);
	    fix_new_exp (frag_now, where, 2, &exp, TRUE, BFD_RELOC_MSP430_10_PCREL);
	  }
	else
	  as_bad (_("unrecognized jump target: %s"), l1);
      }
      break;

    case FMT_X_DOUBLE_OPERAND:	/* Exemplar: movx */
      /* Extended Format 1 ( double operand). */
      l1 = get_operand ();
      l2 = get_operand ();
      bad_operand = !msp430_srcoperand (&op1, l1, IMM_RANGE_INT20);
      bad_operand |= !msp430_dstoperand (&op2, l2, IMM_RANGE_INT20);
      if (bad_operand)
	break;			/* Error occurred.  All warnings were done before.  */

      msp430_substitute_CG (&op1, op_width, 0);

      if (msp430x_repeats
	  && (OP_HAS_IMMEDIATE (op1) || OP_HAS_IMMEDIATE (op2)))
	{
	  as_bad (_("Repeated instruction must have register mode operands"));
	  msp430x_repeats = 0;
	  break;
	}
      bin |= msp430x_repeats;
      msp430x_repeats = 0;

      bin |= (op2.reg | (op1.reg << 8) | (op1.am << 4) | (op2.am << 7)) << 16;

      insn_len_words = 2 + op1.ol + op2.ol;	/* insn size in words, opcode is 2 words wide.  */
      frag = frag_more (2 * insn_len_words);
      where = frag - frag_now->fr_literal;
      bfd_putl32 ((bfd_vma) bin, frag);

      if (OP_HAS_IMMEDIATE (op1))
	{
	  bfd_putl16 ((bfd_vma) ZEROS, frag + 4);
	  record_fixup (where, 2, op_width, &op1, FX_X20S);
	}

      if (OP_HAS_IMMEDIATE (op2))
	{
	  bfd_putl16 ((bfd_vma) ZEROS,
		      frag + 4 + ((insn_len_words == 4) ? 2 : 0));
	  record_fixup (where, 2, op_width, &op2, OP_HAS_IMMEDIATE (op1) ? FX_X20D2 : FX_X20D);
	}
      break;

    case FMT_X_SINGLE_OPERAND:	/* Exemplar: swpbx, rrcx, pushx */
      /* Extended format 2 (single-operand). */
      l1 = get_operand ();
      if (!msp430_srcoperand (&op1, l1, IMM_RANGE_INT20))
	break;			/* Error in operand.  */

      msp430_substitute_CG (&op1, op_width, 0);
      if (opcode_variant (opcode) != V_PUSHX && OP_HAS_IMMEDIATE (op1) && op1.am == AMs_Immediate)	/* #N */
	{
	  as_bad (_("bad operand [%s]"), l1);
	  break;
	}

      if (msp430x_repeats && OP_HAS_IMMEDIATE (op1))
	{
	  as_bad (_("Repeated instruction must have register mode operand"));
	  msp430x_repeats = 0;
	  break;
	}
      bin |= msp430x_repeats;
      msp430x_repeats = 0;

      /* sxtx.a | swpbx.a opcode */
      if (opcode_variant (opcode) == V_SWPSXT && op_width == ADDR_OP)
	bin ^= BYTE_OPERATION_X;

      bin |= (op1.reg | (op1.am << 4)) << 16;
      insn_len_words = 2 + op1.ol;	/* insn size in words, opcode is 2 words wide.  */
      frag = frag_more (2 * insn_len_words);
      where = frag - frag_now->fr_literal;
      bfd_putl32 ((bfd_vma) bin, frag);

      if (OP_HAS_IMMEDIATE (op1))
	{
	  bfd_putl16 ((bfd_vma) ZEROS, frag + 4);
	  record_fixup (where, 2, op_width, &op1,
			opcode_variant (opcode) == V_PUSHX ? FX_X20D_SRCI : FX_X20D);
	}
      break;

    case FMT_X_EXCEPTION:	/* Exemplar: calla, popm, pushm, rrcm */
      /* calla, pushm, popm, rrcm, rrum, rram, rlam */
      bin = opcode->bin_opcode;	/* remove WB/AL bits */
      l1 = get_operand ();
      switch (opcode_variant (opcode))
	{
	case V_CALLA:		/* Exemplar: calla */
	  if (!msp430_srcoperand (&op1, l1, IMM_RANGE_INT20))
	    break;		/* Error in operand.  */

	  insn_len_words = 1 + op1.ol;
	  frag = frag_more (insn_len_words * 2);

	  if (!OP_HAS_IMMEDIATE (op1) && op1.am != AM_Indexed)
	    {
	      bin |= op1.reg | 0x0040 | (op1.am << 4);
	      bfd_putl16 ((bfd_vma) bin, frag);
	    }
	  else
	    {
	      fixup = FX_A20D;
	      bfd_putl16 ((bfd_vma) ZEROS, frag + 2);
	      where = frag - frag_now->fr_literal;
	      if (op1.am == AM_Indexed)
		{
		  if (op1.reg == REGNO_PC || op1.reg == REGNO_CG1)
		    {
		      bin |= 0x0080;
		      if (op1.reg == REGNO_PC)
			bin |= 0x0010;
		    }
		  else
		    {
		      bin |= 0x0050 | op1.reg;
		      fixup = FX_A16;
		    }
		}
	      else
		{
		  gas_assert (op1.am == AMs_Immediate);
		  bin |= 0x00b0;
		}
	      bfd_putl16 ((bfd_vma) bin, frag);
	      record_fixup (where, 2, op_width, &op1, fixup);
	    }
	  break;
	case V_ROTM:		/* Exemplar: rrcm */
	  l2 = get_operand ();
	  bad_operand = !msp430_srcoperand (&op1, l1, IMM_RANGE_ROTATE_COUNT);
	  bad_operand |= !msp430_dstoperand (&op2, l2, IMM_RANGE_INT20);
	  if (bad_operand)
	    break;		/* An error occurred.  All warnings were done before.  */

	  if (op_width != ADDR_OP)
	    bin |= (1 << 4);

	  if (!OP_HAS_IMMEDIATE (op1) || op1.am != AMs_Immediate)	/* not #imm */
	    {
	      as_bad (_("bad operand [%s]"), l1);
	      break;
	    }

	  bin |= ((op1.exp.X_add_number - 1) & 0x0003) << 10;

	  if (OP_HAS_IMMEDIATE (op2))
	    {
	      as_bad (_("bad operand [%s]"), l2);
	      break;
	    }
	  bin |= op2.reg;

	  insn_len_words = 1;
	  frag = frag_more (2 * insn_len_words);
	  bfd_putl16 ((bfd_vma) bin, frag);
	  break;
	case V_PUSHM:		/* Exemplar: pushm */
	case V_POPM:		/* Exemplar: popm */
	  l2 = get_operand ();
	  bad_operand =
	    !msp430_srcoperand (&op1, l1, IMM_RANGE_REPETITION_COUNT);
	  bad_operand |= !msp430_dstoperand (&op2, l2, IMM_RANGE_INT20);
	  if (bad_operand)
	    break;		/* An error occurred.  All warnings were done before.  */

	  if (!OP_HAS_IMMEDIATE (op1))
	    {
	      as_bad (_("bad operand [%s]"), l1);
	      break;
	    }

	  if (op_width != ADDR_OP)
	    bin |= (1 << 8);
	  bin |= ((op1.exp.X_add_number - 1) & 0x000F) << 4;

	  if (OP_HAS_IMMEDIATE (op2))
	    {
	      as_bad (_("bad operand [%s]"), l2);
	      break;
	    }
	  if (opcode_variant (opcode) == V_POPM)
	    bin |= (op2.reg - op1.exp.X_add_number + 1) & 0x000F;
	  else
	    bin |= op2.reg;

	  insn_len_words = 1;
	  frag = frag_more (2 * insn_len_words);
	  bfd_putl16 ((bfd_vma) bin, frag);
	  break;
	}
      break;
    case FMT_X_ADDRESS:	/* Exemplar: adda, mova */
      /* mova, adda, suba, cmpa */
      l1 = get_operand ();
      l2 = get_operand ();
      bad_operand = !msp430_srcoperand (&op1, l1, IMM_RANGE_INT20);
      bad_operand |= !msp430_dstoperand (&op2, l2, IMM_RANGE_INT20);
      if (bad_operand)
	break;			/* Error in operand.  */

      msp430_address_substitute_CG (opcode, &op1);

      insn_len_words = 1 + op1.ol + op2.ol;
      frag = frag_more (insn_len_words * 2);
      where = frag - frag_now->fr_literal;
      bin = opcode->bin_opcode;	/* remove WB/AL bits */
      if (opcode_variant (opcode) == V_MOVA)	/* Exemplar: mova */
	{
	  if (!OP_HAS_IMMEDIATE (op1) && op1.am == AM_Register)
	    {			/* Rsrc */
	      bin |= op1.reg << 8;
	      if (op2.am != AM_Indexed)
		{
		  /* mova Rsrc, Rdst */
		  bin |= 0x00c0 | op2.reg;
		  bfd_putl16 ((bfd_vma) bin, frag);
		}
	      else
		{
		  fixup = FX_A20D;
		  gas_assert (op2.am == AM_Indexed);
		  if (op2.reg == REGNO_CG1)	/* AM_Absolute */
		    bin |= 0x0060;
		  else
		    {
		      /* mova Rsrc, z16(Rdst) */
		      bin |= 0x0070 | op2.reg;
		      bfd_putl16 ((bfd_vma) bin, frag);
		      bfd_putl16 ((bfd_vma) ZEROS, frag + 2);
		      fixup = FX_A16;
		    }
		  bfd_putl16 ((bfd_vma) bin, frag);
		  bfd_putl16 ((bfd_vma) ZEROS, frag + 2);
		  record_fixup (where, 2, op_width, &op2, fixup);
		}
	    }
	  else if (!OP_HAS_IMMEDIATE (op2) && op2.am == AM_Register)
	    {			/* Rdst */
	      fixup = FX_INVALID;
	      if (!OP_HAS_IMMEDIATE (op1) && op1.am == AMs_IndirectRegister)
		{
		  /* mova @Rsrc, Rdst */
		  bin |= 0x0000 | op1.reg << 8 | op2.reg;
		  bfd_putl16 ((bfd_vma) bin, frag);
		}
	      else if (!OP_HAS_IMMEDIATE (op1)
		       && op1.am == AMs_IndirectAutoIncrement)
		{
		  /* mova @Rsrc+, Rdst */
		  bin |= 0x0010 | op1.reg << 8 | op2.reg;
		  bfd_putl16 ((bfd_vma) bin, frag);
		}
	      else if (OP_HAS_IMMEDIATE (op1) && op1.am == AM_Indexed)
		{
		  if (op1.reg == REGNO_CG1)	/* AM_Absolute */
		    {
		      /* mova &abs20, Rdst */
		      bin |= 0x0020 | op2.reg;
		      fixup = FX_A20S;
		    }
		  else		/* AM_Symbolic or AM_Indexed */
		    {
		      /* mova z16(Rsrc), Rdst */
		      bin |= 0x0030 | op1.reg << 8 | op2.reg;
		      fixup = FX_A16;
		    }
		  bfd_putl16 ((bfd_vma) bin, frag);
		  bfd_putl16 ((bfd_vma) ZEROS, frag + 2);
		}
	      else if (OP_HAS_IMMEDIATE (op1) && op1.am == AMs_Immediate)
		{
		  /* mova #imm20, Rdst */
		  bin |= 0x0080 | op2.reg;
		  bfd_putl16 ((bfd_vma) bin, frag);
		  bfd_putl16 ((bfd_vma) ZEROS, frag + 2);
		  fixup = FX_A20S;
		}
	      else
		as_bad (_
			("source operand address mode %d not allowed with mova instruction"),
			op1.am);
	      if (fixup != FX_INVALID)
		record_fixup (where, 2, op_width, &op1, fixup);
	    }
	  else
	    as_bad (_("unsupported operands for mova: %s, %s"), l1, l2);
	  break;
	}
      else			/* Exemplar: adda */
	/* adda, suba, cmpa */
	{
	  if (!OP_HAS_IMMEDIATE (op2) && op2.am == AM_Register)
	    {
	      if (!OP_HAS_IMMEDIATE (op1) && op1.am == AM_Register)
		{		/* Rsrc, Rdst */
		  bin |= 0x0040 | op1.reg << 8 | op2.reg;
		  bfd_putl16 ((bfd_vma) bin, frag);
		}
	      else if (OP_HAS_IMMEDIATE (op1) && op1.am == AMs_Immediate)
		{
		  /* #imm20, Rdst */
		  bin |= 0x0080 | op2.reg;
		  bfd_putl16 ((bfd_vma) bin, frag);
		  bfd_putl16 ((bfd_vma) ZEROS, frag + 2);
		  record_fixup (where, 2, op_width, &op1, FX_A20S);
		}
	      else
		as_bad (_
			("source operand address mode not allowed with %s instruction"),
			opcode->name);
	    }
	  else
	    as_bad (_
		    ("destination operand address mode not allowed with %s instruction"),
		    opcode->name);
	  break;
	}
      break;

    case FMT_X_EMULATED:	/* Extended emulated.  */
      switch (opcode_variant (opcode))
	{
	case V_NONE:		/* Exemplar: popx */
	  /* single operand instruction emulated with Extended type 1 (double operand) instructions.  */
	  l1 = get_operand ();
	  if (!msp430_dstoperand (&op1, l1, IMM_RANGE_INT20))
	    break;

	  if (msp430x_repeats)
	    {
	      if ((bin >> 20) && 0x3 == 1)
		{
		  as_bad (_("%s instruction is not repeatable"),
			  opcode->name);
		  msp430x_repeats = 0;
		  break;
		}
	      if (OP_HAS_IMMEDIATE (op1))
		{
		  as_bad (_
			  ("Repeated instruction must have register mode operand"));
		  msp430x_repeats = 0;
		  break;
		}
	      bin |= msp430x_repeats;
	      msp430x_repeats = 0;
	    }

	  bin |= (op1.reg | (op1.am << 7)) << 16;
	  insn_len_words = 2 + op1.ol;
	  frag = frag_more (2 * insn_len_words);
	  where = frag - frag_now->fr_literal;
	  bfd_putl32 ((bfd_vma) bin, frag);

	  if (OP_HAS_IMMEDIATE (op1))
	    {
	      bfd_putl16 ((bfd_vma) ZEROS, frag + 4);
	      record_fixup (where, 2, op_width, &op1, FX_X20D);
	    }
	  break;
	case V_MOVA:		/* Exemplar clra: ops = #0, dst=Rdst */
	  l1 = get_operand ();
	  if (!msp430_dstoperand (&op1, l1, IMM_RANGE_INT20))
	    break;
	  /* Technically as an emulated instruction the destination
	   * could also be z16(Rdst) or &abs20, but the ISA
	   * description specifies only Rdst.  Use the real mova #0
	   * for the others. */
	  if (OP_HAS_IMMEDIATE (op1))
	    {
	      as_bad (_("%s operand must be register"), opcode->name);
	      break;
	    }
	  insn_len_words = 1;
	  frag = frag_more (insn_len_words * 2);
	  where = frag - frag_now->fr_literal;
	  bin |= op1.reg;
	  bfd_putl16 ((bfd_vma) bin, frag);
	  break;
	case V_X_SHIFT:	/* Exemplar: rlax */
	  {
	    /* Shift instruction.  */
	    l1 = get_operand ();
	    if (!msp430_srcoperand (&op1, l1, IMM_RANGE_INT20))
	      break;
	    if (!convert_operand_to_dst (&op2, &op1, l1))
	      break;

	    if (msp430x_repeats && OP_HAS_IMMEDIATE (op2))
	      {
		as_bad (_
			("Repeated instruction must have register mode operands"));
		msp430x_repeats = 0;
		break;
	      }
	    bin |= msp430x_repeats;
	    msp430x_repeats = 0;

	    bin |=
	      (op2.
	       reg | (op1.reg << 8) | (op1.am << 4) | (op2.am << 7)) << 16;

	    insn_len_words = 2 + op1.ol + op2.ol;	/* insn size in words.  */
	    frag = frag_more (2 * insn_len_words);
	    where = frag - frag_now->fr_literal;
	    bfd_putl32 ((bfd_vma) bin, frag);

	    if (OP_HAS_IMMEDIATE (op1))
	      {
		bfd_putl16 ((bfd_vma) ZEROS, frag + 4);
		record_fixup (where, 2, op_width, &op1, FX_X20S);
	      }

	    if (OP_HAS_IMMEDIATE (op2))
	      {
		bfd_putl16 ((bfd_vma) ZEROS,
			    frag + 4 + ((insn_len_words == 4) ? 2 : 0));
		record_fixup (where, 2, op_width, &op2, OP_HAS_IMMEDIATE (op1) ? FX_X20D2 : FX_X20D);
	      }
	  }
	  break;
	case V_RETA:		/* Exemplar: reta */
	  if (msp430x_repeats)
	    {
	      as_bad (_("%s instruction is not repeatable"), opcode->name);
	      msp430x_repeats = 0;
	      break;
	    }
	  bin = opcode->bin_opcode;	/* remove WB/AL bits */
	  insn_len_words = 1;
	  frag = frag_more (2 * insn_len_words);
	  bfd_putl16 ((bfd_vma) bin, frag);
	  break;
	case V_EMU_ADDR:	/* Exemplar: incda */
	  /* incda, decda, tsta */
	  if (msp430x_repeats)
	    {
	      as_bad (_("%s instruction is not repeatable"), opcode->name);
	      msp430x_repeats = 0;
	      break;
	    }
	  bin = opcode->bin_opcode;	/* remove WB/AL bits */
	  l1 = get_operand ();
	  if (!msp430_dstoperand (&op1, l1, IMM_RANGE_INT20))
	    break;

	  insn_len_words = 1;
	  if (!OP_HAS_IMMEDIATE (op1) && op1.am == AM_Register)
	    {
	      frag = frag_more (2 * insn_len_words);
	      bin |= op1.reg;
	      bfd_putl16 ((bfd_vma) bin, frag);
	    }
	  else
	    as_bad (_
		    ("destination operand address mode not allowed with %s instruction"),
		    opcode->name);
	  break;
	case V_BRA:		/* Exemplar: bra */
	  /* bra, emulated with Address type instruction */
	  if (msp430x_repeats)
	    {
	      as_bad (_("%s instruction is not repeatable"), opcode->name);
	      msp430x_repeats = 0;
	      break;
	    }

	  bin = opcode->bin_opcode;	/* remove WB/AL bits */
	  l1 = get_operand ();
	  if (!msp430_srcoperand (&op1, l1, IMM_RANGE_INT20))
	    break;		/* Error in operand.  */
	  msp430_substitute_CG (&op1, ADDR_OP, 0);

	  insn_len_words = 1 + op1.ol;
	  frag = frag_more (insn_len_words * 2);
	  where = frag - frag_now->fr_literal;
	  fixup = FX_INVALID;
	  if (!OP_HAS_IMMEDIATE (op1) && op1.am == AM_Register)
	    {
	      /* mova Rsrc, PC */
	      bin |= 0x00C0 | op1.reg << 8;
	      bfd_putl16 ((bfd_vma) bin, frag);
	    }
	  else if (!OP_HAS_IMMEDIATE (op1) && op1.am == AMs_IndirectRegister)
	    {
	      /* mova @Rsrc, PC */
	      bin |= 0x0000 | op1.reg << 8;
	      bfd_putl16 ((bfd_vma) bin, frag);
	    }
	  else if (!OP_HAS_IMMEDIATE (op1)
		   && op1.am == AMs_IndirectAutoIncrement)
	    {
	      /* mova @Rsrc+, PC */
	      bin |= 0x0010 | op1.reg << 8;
	      bfd_putl16 ((bfd_vma) bin, frag);
	    }
	  else if (OP_HAS_IMMEDIATE (op1) && op1.am == AM_Indexed)
	    {
	      if (op1.reg == REGNO_CG1)
		{
		  /* mova &abs20, PC */
		  bin |= 0x0020;
		  fixup = FX_A20S_W;
		}
	      else
		{
		  /* mova z16(Rsrc), PC */
		  bin |= 0x0030 | op1.reg << 8;
		  fixup = FX_A16;
		}
	      bfd_putl16 ((bfd_vma) bin, frag);
	      bfd_putl16 ((bfd_vma) ZEROS, frag + 2);
	    }
	  else if (OP_HAS_IMMEDIATE (op1) && op1.am == AMs_Immediate)
	    {
	      /* mova #imm20, Rdst */
	      bin |= 0x0080;
	      bfd_putl16 ((bfd_vma) bin, frag);
	      bfd_putl16 ((bfd_vma) ZEROS, frag + 2);
	      fixup = FX_A20S_W;
	    }
	  else
	    as_bad (_
		    ("source operand address mode not allowed with bra instruction"));
	  if (fixup != FX_INVALID)
	    record_fixup (where, 2, op_width, &op1, fixup);
	}
      break;

    default:
      as_bad (_("Illegal instruction or not implemented opcode."));
    }

  if (0 < insn_len_words && 0 == had_errors ())
    dwarf2_emit_insn (2 * insn_len_words);

  if (l1)
    xfree (l1);
  if (l2)
    xfree (l2);

  return 0;
}

void
md_assemble (char *str)
{
  struct msp430_opcode_s const *opcode;
  char *cmda;
  char *cmd;
  int i = 0;
  int cmd_length;

  while (ISSPACE (*str))
    ++str;
  cmd = str;
  while (*str && (!ISSPACE (*str)) && '.' != *str)
    ++str;
  cmd_length = str - cmd;
  if (0 == cmd_length)
    {
      as_bad (_("can't find opcode "));
      return;
    }

  cmda = alloca (1 + cmd_length);
  do
    cmda[i] = TOLOWER (cmd[i]);
  while (++i < cmd_length);
  cmda[i] = 0;

  opcode = (struct msp430_opcode_s const *) hash_find (msp430_hash, cmda);

  if (opcode == NULL)
    {
      as_bad (_("unknown opcode `%s'"), cmda);
      return;
    }

  if (msp430_cpu < MSP430_CPU_MSP430X && opcode_format (opcode) >= FMT_X)
    {
      as_bad (_("Extended instruction (%s) requires 430X-based mcu"),
	      opcode->name);
      return;
    }

  {
    char *t = input_line_pointer;

    msp430_operands (opcode, str);
    input_line_pointer = t;
  }
}

/* GAS will call this function for each section at the end of the assembly,
   to permit the CPU backend to adjust the alignment of a section.  */

valueT
md_section_align (asection * seg, valueT addr)
{
  int align = bfd_get_section_alignment (stdoutput, seg);

  return ((addr + (1 << align) - 1) & (-1 << align));
}

/* If you define this macro, it should return the offset between the
   address of a PC relative fixup and the position from which the PC
   relative adjustment should be made.  On many processors, the base
   of a PC relative instruction is the next instruction, so this
   macro would return the length of an instruction.  */

long
md_pcrel_from_section (fixS * fixp, segT sec)
{
  long rv;
  if (fixp->fx_addsy != (symbolS *) NULL
      && (!S_IS_DEFINED (fixp->fx_addsy)
	  || (S_GET_SEGMENT (fixp->fx_addsy) != sec)))
    return 0;
  if (fixp->fx_r_type == BFD_RELOC_MSP430_10_PCREL)
    /* The only real PC-relative operand */
    rv = fixp->fx_frag->fr_address + fixp->fx_where;
  else
    /* NB: 430X relocations/fixups do additional adjustments to
     * account for the value not being written at "where", which
     * instead points to the extension word.  Those are independent of
     * pcrel so do not belong here. */
    rv = fixp->fx_where - fixp->fx_dot_value;
  return rv;
}

/* GAS will call this for each fixup.  It should store the correct
   value in the object file.  */
void
md_apply_fix (fixS * fixp, valueT * valuep, segT seg)
{
  unsigned char *where;
  unsigned long insn;
  long value;

  value = *valuep;
  if (fixp->fx_addsy == (symbolS *) NULL)
    fixp->fx_done = 1;
  else if (fixp->fx_pcrel && fixp->fx_addsy != (symbolS *)NULL)
    {
      segT s = S_GET_SEGMENT (fixp->fx_addsy);
      fixp->fx_done = (s == seg || s == absolute_section);
    }
  if (fixp->fx_subsy != (symbolS *) NULL)
    as_bad_where (fixp->fx_file, fixp->fx_line, _("expression too complex"));

  fixp->fx_no_overflow = 1;

  if (fixp->fx_done)
    {
      /* Fetch the instruction, insert the fully resolved operand
         value, and stuff the instruction back again.  */

      where = (unsigned char *) fixp->fx_frag->fr_literal + fixp->fx_where;

      insn = bfd_getl16 (where);

      switch (fixp->fx_r_type)
	{
	case BFD_RELOC_MSP430X_PCREL_D:
	case BFD_RELOC_MSP430X_PCREL_INDXD:
	  /* operand located 2 bytes after where */
	  value -= 2;
	  break;
	case BFD_RELOC_MSP430X_PCREL_SRC:
	case BFD_RELOC_MSP430X_PCREL_SRC_BYTE:
	case BFD_RELOC_MSP430X_PCREL_DST:
	case BFD_RELOC_MSP430X_PCREL_DST_BYTE:
	  /* operand located 4 bytes after where */
	  value -= 4;
	  break;
	case BFD_RELOC_MSP430X_PCREL_DST_2ND:
	case BFD_RELOC_MSP430X_PCREL_DST_2ND_BYTE:
	  /* operand located 6 bytes after where */
	  value -= 6;
	  break;
	default:
	  break;
	}

      switch (fixp->fx_r_type)
	{
	case BFD_RELOC_32:	/* Required; used for elf header? */
	  bfd_putl32 ((bfd_vma) MASK_32 (value), where);
	  break;

	case BFD_RELOC_MSP430_10:
	case BFD_RELOC_MSP430_10_PCREL:
	  if (MSP430_ODD (value))
	    as_bad_where (fixp->fx_file, fixp->fx_line,
			  _("odd address operand: %ld"), value);

	  /* If we get here at all, it's because the operand
	   * expression involved a PC-relative calculation, but the
	   * final value could be determined without resolving an
	   * external symbol.  That value does not account for the
	   * fact that the PC will have been incremented by two at the
	   * point where the offset is applied.  Do that now. */
	  value -= 2;

	  /* Jumps are in words.  */
	  value >>= 1;

	  if (!MSP430_S10_IN_RANGE (value))
	    as_bad_where (fixp->fx_file, fixp->fx_line,
			  _("operand out of range: %ld"), value);

	  value = MASK_10 (value);	/* get rid of extended sign */
	  bfd_putl16 ((bfd_vma) (value | insn), where);
	  break;

	case BFD_RELOC_MSP430_16:
	case BFD_RELOC_MSP430_16_PCREL:
	  if (MSP430_ODD (value))
	    as_bad_where (fixp->fx_file, fixp->fx_line,
			  _("odd operand: %ld"), value);
	   /*FALLTHRU*/;
	case BFD_RELOC_16:	/* Required; used for elf header? */
	case BFD_RELOC_MSP430_16_BYTE:
	case BFD_RELOC_MSP430_16_PCREL_BYTE:
	  if (!MSP430_16_IN_RANGE (value))
	    as_bad_where (fixp->fx_file, fixp->fx_line,
			  _("operand out of range: %ld"), value);
	  bfd_putl16 ((bfd_vma) MASK_16 (value), where);
	  break;

	case BFD_RELOC_MSP430X_SRC:
	case BFD_RELOC_MSP430X_PCREL_SRC:
	  if (MSP430_ODD (value))
	    as_bad_where (fixp->fx_file, fixp->fx_line,
			  _("odd operand: %ld"), value);
	   /*FALLTHRU*/;
	case BFD_RELOC_MSP430X_SRC_BYTE:
	case BFD_RELOC_MSP430X_PCREL_SRC_BYTE:
	  if (!MSP430_20_IN_RANGE (value))
	    as_bad_where (fixp->fx_file, fixp->fx_line,
			  _("operand out of range: %ld"), value);
	  value = MASK_20 (value);
	  bfd_putl16 ((bfd_vma) (bfd_getl16 (where) & 0xf87f) |
		      ((value >> 9) & 0x0780), where);
	  bfd_putl16 ((bfd_vma) MASK_16 (value), where + 4);
	  break;

	case BFD_RELOC_MSP430X_DST:
	case BFD_RELOC_MSP430X_PCREL_DST:
	  if (MSP430_ODD (value))
	    as_bad_where (fixp->fx_file, fixp->fx_line,
			  _("odd operand: %ld"), value);
	   /*FALLTHRU*/;
	case BFD_RELOC_MSP430X_DST_BYTE:
	case BFD_RELOC_MSP430X_PCREL_DST_BYTE:
	  if (!MSP430_20_IN_RANGE (value))
	    as_bad_where (fixp->fx_file, fixp->fx_line,
			  _("operand out of range: %ld"), value);
	  value = MASK_20 (value);
	  bfd_putl16 ((bfd_vma) (bfd_getl16 (where) & 0xfff0) |
		      ((value >> 16) & 0x000f), where);
	  bfd_putl16 ((bfd_vma) MASK_16 (value), where + 4);
	  break;

	case BFD_RELOC_MSP430X_DST_2ND:
	case BFD_RELOC_MSP430X_PCREL_DST_2ND:
	  if (MSP430_ODD (value))
	    as_bad_where (fixp->fx_file, fixp->fx_line,
			  _("odd operand: %ld"), value);
	   /*FALLTHRU*/;
	case BFD_RELOC_MSP430X_DST_2ND_BYTE:
	case BFD_RELOC_MSP430X_PCREL_DST_2ND_BYTE:
	  if (!MSP430_20_IN_RANGE (value))
	    as_bad_where (fixp->fx_file, fixp->fx_line,
			  _("operand out of range: %ld"), value);
	  value = MASK_20 (value);
	  bfd_putl16 ((bfd_vma) (bfd_getl16 (where) & 0xfff0) |
		      ((value >> 16) & 0x000f), where);
	  bfd_putl16 ((bfd_vma) MASK_16 (value), where + 6);
	  break;

	case BFD_RELOC_MSP430X_S:
	  if (MSP430_ODD (value))
	    as_bad_where (fixp->fx_file, fixp->fx_line,
			  _("odd operand: %ld"), value);
	   /*FALLTHRU*/;
	case BFD_RELOC_MSP430X_S_BYTE:
	  if (!MSP430_20_IN_RANGE (value))
	    as_bad_where (fixp->fx_file, fixp->fx_line,
			  _("operand out of range: %ld"), value);
	  value = MASK_20 (value);
	  bfd_putl16 ((bfd_vma) (bfd_getl16 (where) & 0xf0ff) |
		      ((value >> 8) & 0x0f00), where);
	  bfd_putl16 ((bfd_vma) MASK_16 (value), where + 2);
	  break;

	case BFD_RELOC_MSP430X_D:
	case BFD_RELOC_MSP430X_PCREL_D:
	  if (MSP430_ODD (value))
	    as_bad_where (fixp->fx_file, fixp->fx_line,
			  _("odd operand: %ld"), value);
	   /*FALLTHRU*/;
	case BFD_RELOC_MSP430X_D_BYTE:
	  if (!MSP430_20_IN_RANGE (value))
	    as_bad_where (fixp->fx_file, fixp->fx_line,
			  _("operand out of range: %ld"), value);
	  value = MASK_20 (value);
	  bfd_putl16 ((bfd_vma) (bfd_getl16 (where) & 0xfff0) |
		      ((value >> 16) & 0x000f), where);
	  /* 16 least-significant bits */
	  bfd_putl16 ((bfd_vma) MASK_16 (value), where + 2);
	  break;

	case BFD_RELOC_MSP430X_PCREL_INDXD:
	  if (MSP430_ODD (value))
	    as_bad_where (fixp->fx_file, fixp->fx_line,
			  _("odd operand: %ld"), value);
	   /*FALLTHRU*/;
	case BFD_RELOC_MSP430X_INDXD:
	  if (!MSP430_S16_IN_RANGE (value))
	    as_bad_where (fixp->fx_file, fixp->fx_line,
			  _("operand out of range: %ld"), value);
	  bfd_putl16 ((bfd_vma) MASK_16 (value), where + 2);
	  break;

	case BFD_RELOC_MSP430_RL_PCREL:
	case BFD_RELOC_MSP430_2X_PCREL:
	default:
	  as_fatal (_("line %d: unknown relocation type: 0x%x (%s)"),
		    fixp->fx_line, fixp->fx_r_type,
		    bfd_get_reloc_code_name (fixp->fx_r_type));
	  break;
	}
    }
  else
    {
      int reloc_is_pcrel =
	fixp->fx_r_type == BFD_RELOC_MSP430X_PCREL_D ||
	fixp->fx_r_type == BFD_RELOC_MSP430X_PCREL_DST ||
	fixp->fx_r_type == BFD_RELOC_MSP430X_PCREL_DST_2ND ||
	fixp->fx_r_type == BFD_RELOC_MSP430X_PCREL_DST_2ND_BYTE ||
	fixp->fx_r_type == BFD_RELOC_MSP430X_PCREL_DST_BYTE ||
	fixp->fx_r_type == BFD_RELOC_MSP430X_PCREL_INDXD ||
	fixp->fx_r_type == BFD_RELOC_MSP430X_PCREL_SRC ||
	fixp->fx_r_type == BFD_RELOC_MSP430X_PCREL_SRC_BYTE ||
	fixp->fx_r_type == BFD_RELOC_MSP430_10_PCREL ||
	fixp->fx_r_type == BFD_RELOC_MSP430_16_PCREL ||
	fixp->fx_r_type == BFD_RELOC_MSP430_16_PCREL_BYTE ||
	fixp->fx_r_type == BFD_RELOC_MSP430_2X_PCREL ||
	fixp->fx_r_type == BFD_RELOC_MSP430_RL_PCREL;

      /* TODO: This is useless; the fx_addnumber field is unused.
	 Probably fx_offset was desired, as that becames the addend in
	 the relocation.  Not changed at this time, because currently
	 the BFD code repeats the adjustments that were made above. */
      fixp->fx_addnumber = value;
      /* If the value we calculate is not the same as is going into
       * the relocation, the expression semantics is not preserved.
       * This can happen with symbol-. where symbol is in a different
       * section.  We could fix this by updating the offset in
       * relocation, although object file backwards compatibility has
       * to be validated. */
      if (value != (long)fixp->fx_offset && fixp->fx_pcrel && !reloc_is_pcrel)
	as_bad_where (fixp->fx_file, fixp->fx_line,
		      _("expression %d too complex for %s (offset %ld will be %ld)"),
		      fixp->fx_pcrel, bfd_get_reloc_code_name (fixp->fx_r_type),
		      value, fixp->fx_offset);
    }
}

/* GAS will call this to generate a reloc, passing the resulting reloc
   to `bfd_install_relocation'.  This currently works poorly, as
   `bfd_install_relocation' often does the wrong thing, and instances of
   `tc_gen_reloc' have been written to work around the problems, which
   in turns makes it difficult to fix `bfd_install_relocation'.  */

/* If while processing a fixup, a reloc really needs to be created
   then it is done here.  */

arelent *
tc_gen_reloc (asection * seg ATTRIBUTE_UNUSED, fixS * fixp)
{
  arelent *reloc;

  reloc = xmalloc (sizeof (arelent));

  reloc->sym_ptr_ptr = xmalloc (sizeof (asymbol *));
  *reloc->sym_ptr_ptr = symbol_get_bfdsym (fixp->fx_addsy);

  reloc->address = fixp->fx_frag->fr_address + fixp->fx_where;
  reloc->howto = bfd_reloc_type_lookup (stdoutput, fixp->fx_r_type);
  if (reloc->howto == (reloc_howto_type *) NULL)
    {
      as_bad_where (fixp->fx_file, fixp->fx_line,
		    _("reloc %d not supported by object file format"),
		    (int) fixp->fx_r_type);
      return NULL;
    }

  if (fixp->fx_r_type == BFD_RELOC_VTABLE_INHERIT
      || fixp->fx_r_type == BFD_RELOC_VTABLE_ENTRY)
    reloc->address = fixp->fx_offset;

  reloc->addend = fixp->fx_offset;

  return reloc;
}

void
md_convert_frag (bfd * abfd ATTRIBUTE_UNUSED,
		 asection * sec ATTRIBUTE_UNUSED,
		 fragS * fragP ATTRIBUTE_UNUSED)
{
  gas_assert (0);
}


int
md_estimate_size_before_relax (fragS * fragp ATTRIBUTE_UNUSED,
			       asection * seg ATTRIBUTE_UNUSED)
{
  gas_assert (0);
  return 0;
}
