/* Disassemble MSP430 instructions.
   Copyright (C) 2002, 2004, 2005, 2007, 2009, 2010
   Free Software Foundation, Inc.
   
   Contributed by Dmitry Diky <diwil@mail.ru>
        
   This file is part of the GNU opcodes library.

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   It is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>

#include "dis-asm.h"
#include "opintl.h"
#include "libiberty.h"

#define DASM_SECTION
#include "opcode/msp430.h"
#undef DASM_SECTION

static unsigned short
msp430dis_opcode (bfd_vma addr, disassemble_info * info)
{
  bfd_byte buffer[2];
  int status;

  status = info->read_memory_func (addr, buffer, 2, info);
  if (status != 0)
    {
      info->memory_error_func (status, addr, info);
      return -1;
    }
  return bfd_getl16 (buffer);
}

static unsigned short
msp430dis_operand (bfd_vma addr, disassemble_info * info, int reg, int am,
		   int *cmd_len)
{
  static int const op_length[][5] = {
    /* PC SP r2 r3 rN AM */
    {0, 0, 0, 0, 0},		/* 0 = AM_Register: Rn */
    {2, 2, 2, 0, 2},		/* 1 = AM_Indexed: x(Rn) */
    {0, 0, 0, 0, 0},		/* 2 = AMs_IndirectRegister: @Rn */
    {2, 0, 0, 0, 0},		/* 3 = AMs_IndirectAutoIncrement: @Rn+ */
  };
  if (reg >= (int) (sizeof (op_length[0]) / sizeof (op_length[0][0])))
    reg = sizeof (op_length[0]) / sizeof (op_length[0][0]) - 1;

  if (op_length[am][reg])
    {
      bfd_byte buffer[2];
      int status = info->read_memory_func (addr, buffer, 2, info);
      if (status != 0)
	{
	  info->memory_error_func (status, addr, info);
	  return -1;
	}
      *cmd_len += 2;
      return bfd_getl16 (buffer);
    }
  return 0;
}

typedef enum
{
  OP_OTHER = 0,
  OP_DECIMAL = 0x03,
  OP_16BIT = 0x04,
  OP_20BIT = 0x05,
  OP_MASK_BITS = 0x07,
  OP_IS_HEX = 0x08,
  OP_16BIT_HEX = OP_16BIT | OP_IS_HEX,
  OP_20BIT_HEX = OP_20BIT | OP_IS_HEX,
  OP_IS_430X_INSN = 0x10,
} operand_t;

static void
msp430_decode_operand (int reg, int am, int op_addr, int dst, operand_t size,
		       char *op, char *comm)
{
  int is_hex = size & OP_IS_HEX;
  int is_430x = size & OP_IS_430X_INSN;
  size = size & OP_MASK_BITS;
  
  if (NULL == op)
    return;
  switch (am)
    {
    case AM_Register:
      if (reg == REGNO_CG2)	/* #0 */
	{
	  sprintf (op, "#0");
	  sprintf (comm, "r3 As==00");
	}
      else
	sprintf (op, "r%d", reg);
      break;
    case AM_Indexed:
      if (reg == REGNO_PC)	/* Symbolic: ADDR */
	{
	  int mem_addr;
	  
	  mem_addr = op_addr + dst;
	  if (!is_430x && size == OP_16BIT)
	    {
	      if (MASK_16 (op_addr) == op_addr)
		mem_addr = MASK_16 (mem_addr);
	      sprintf (comm, "PC rel. 0x%04x", MASK_16 (mem_addr));
	    }
	  else
	    sprintf (comm, "PC rel. 0x%05x", MASK_20 (mem_addr));
	  sprintf (op, "%d(r0)", dst);
	}
      else if (reg == REGNO_CG1)	/* Absolute: &ADDR */
	{
	  if (size == OP_20BIT)
	    sprintf (op, "&0x%05x", MASK_20 (dst));
	  else
	    sprintf (op, "&0x%04x", MASK_16 (dst));
	}
      else if (reg == REGNO_CG2)	/* #1 */
	{
	  sprintf (op, "#1");
	  sprintf (comm, "r3 As==01");
	}
      else			/* Indexed: x(Rn) */
	{
	  sprintf (op, "%d(r%d)", dst, reg);
	  if (size == OP_20BIT)
	    sprintf (comm, "0x%05x(r%d)", MASK_20 (dst), reg);
	  else
	    sprintf (comm, "0x%04x(r%d)", MASK_16 (dst), reg);
	}
      break;
    case AMs_IndirectRegister:
      if (reg == REGNO_CG1)	/* #4 */
	{
	  sprintf (op, "#4");
	  sprintf (comm, "r2 As==10");
	}
      else if (reg == REGNO_CG2)	/* #2 */
	{
	  sprintf (op, "#2");
	  sprintf (comm, "r3 As==10");
	}
      else			/* @Rn */
	sprintf (op, "@r%d", reg);
      break;
    case AMs_IndirectAutoIncrement:
      switch (reg)
	{
	case REGNO_PC:		/* #N */
	  if (OP_16BIT == size)
	    {
	      if (is_hex)
		sprintf (op, "#0x%04x", MASK_16 (dst));
	      else
		{
		  sprintf (op, "#%d", dst);
		  sprintf (comm, "#0x%04x", MASK_16 (dst));
		}
	    }
	  else if (OP_20BIT == size)
	    {
	      if (is_hex)
		sprintf (op, "#0x%05x", MASK_20 (dst));
	      else
		{
		  sprintf (op, "#%d", dst);
		  sprintf (comm, "#0x%05x", MASK_20 (dst));
		}
	    }
	  else
	    sprintf (op, "#%d", dst);
	  break;
	case REGNO_CG1:	/* #8 */
	  sprintf (op, "#8");
	  sprintf (comm, "r2 As==11");
	  break;
	case REGNO_CG2:	/* #-1 */
	  sprintf (op, "#-1");
	  sprintf (comm, "r3 As==11");
	  break;
	default:		/* @Rn+ */
	  sprintf (op, "@r%d+", reg);
	  break;
	}
      break;
    }
}

static void
msp430x_decode_operand (int reg, int am, int op_addr, int dst, operand_t size,
			char *op, char *comm)
{
  msp430_decode_operand (reg, am, op_addr, dst, OP_IS_430X_INSN | size, op, comm);
}

static int
msp430_nooperands (struct msp430_opcode_s const *opcode,
		   bfd_vma addr ATTRIBUTE_UNUSED,
		   unsigned short insn ATTRIBUTE_UNUSED, char *comm)
{
  /* Pop with constant.  */
  if (insn == 0x43b2)
    return 0;
  if (insn == opcode->bin_opcode)
    return 2;

  if (opcode_format (opcode) == FMT_EMULATED)
    {
      if ((insn & 0x0f00) != 3 || (insn & 0x0f00) != 2)
	return 0;

      strcpy (comm, "emulated...");
    }
  else
    strcpy (comm, "return from interupt");

  return 2;
}

static int
msp430_singleoperand (disassemble_info * info,
		      struct msp430_opcode_s const *opcode,
		      bfd_vma addr, unsigned short insn, char *op, char *comm)
{
  int regs = 0, regd = 0;
  int ad = 0, as = 0;
  bfd_vma op_addr;
  int cmd_len = 2;
  short dst = 0;

  regd = insn & 0x0f;
  regs = (insn & 0x0f00) >> 8;
  as = (insn & 0x0030) >> 4;
  ad = (insn & 0x0080) >> 7;

  op_addr = addr + cmd_len;
  switch (opcode_format (opcode))
    {
    case FMT_EMULATED:		/* Emulated work with dst register.  */
      if (regs != REGNO_CG1 && regs != REGNO_CG2 && regs != REGNO_SP)
	return 0;

      /* Check if not clr insn.  */
      if (opcode->bin_opcode == 0x4300 && (ad || as))
	return 0;

      /* Check if really inc, incd insns.  */
      if ((opcode->bin_opcode & 0xff00) == 0x5300
	  && as == AMs_IndirectAutoIncrement)
	return 0;

      dst = msp430dis_operand (op_addr, info, regd, ad, &cmd_len);
      msp430_decode_operand (regd, ad, op_addr, dst, OP_16BIT, op, comm);
      break;

    case FMT_SINGLE_OPERAND:	/* rrc, push, call, swpb, rra, sxt, push, call, reti etc...  */
      dst = msp430dis_operand (op_addr, info, regd, as, &cmd_len);
      if (opcode_variant (opcode) != V_CALL)
	msp430_decode_operand (regd, as, op_addr, dst, OP_16BIT, op, comm);
      else
	msp430_decode_operand (regd, as, op_addr, dst, OP_16BIT_HEX,
			       op, comm);
      break;

    case FMT_JUMP:		/* Jumps.  */
      /* sign extension, word addr to byte addr conversion */
      dst = (short) ((insn & 0x03ff) << 6) >> 5;
      sprintf (op, "$%+-8d", dst + cmd_len);
      sprintf (comm, "abs 0x%x", MASK_16 ((short) op_addr + dst));
      break;

    default:
      cmd_len = 0;
    }

  return cmd_len;
}

static int
msp430_doubleoperand (disassemble_info * info,
		      struct msp430_opcode_s const *opcode,
		      bfd_vma addr,
		      unsigned short insn,
		      char *op1, char *op2, char *comm1, char *comm2)
{
  int regs = 0, regd = 0;
  int ad = 0, as = 0;
  int cmd_len = 2;
  bfd_vma ops_addr;
  bfd_vma opd_addr;
  short ops;
  short opd;

  regd = insn & 0x0f;
  regs = (insn & 0x0f00) >> 8;
  as = (insn & 0x0030) >> 4;
  ad = (insn & 0x0080) >> 7;

  ops_addr = addr + cmd_len;
  if (opcode_format (opcode) == FMT_EMULATED)
    {
      /* Special case: rla and rlc are the only 2 emulated instructions that
         fall into two operand instructions.  */
      /* With dst, there are only:
         Rm             Register,
         x(Rm)          Indexed,
         0xXXXX         Relative,
         &0xXXXX        Absolute 
         emulated_ins   dst
         basic_ins      dst, dst.  */

      if (regd != regs || as != ad)
	return 0;		/* May be 'data' section.  */

      if (ad == 0 && regd == 3)	/* #N */
	{
	  strcpy (comm1, _("Illegal as emulation instr"));
	  return -1;
	}
      ops = msp430dis_operand (ops_addr, info, regs, as, &cmd_len);
      opd_addr = addr + cmd_len;
      opd = msp430dis_operand (opd_addr, info, regd, ad, &cmd_len);

      /* If the 'src' field is not the same as the dst
         then this is not an rla instruction.
         TODO: That assertion is false for symbolics. */
      if (ops != opd)
	return 0;
      msp430_decode_operand (regs, as, ops_addr, ops, OP_16BIT, op1, comm1);

      *op2 = 0;
      *comm2 = 0;
      return cmd_len;
    }
  /* Two operands exactly.  */

  if (ad == AM_Register && regd == REGNO_CG2)
    {
      /* R3 is illegal as dest: may be data section.  */
      strcpy (comm1, _("Illegal as 2-op instr"));
      return -1;
    }
  ops = msp430dis_operand (ops_addr, info, regs, as, &cmd_len);
  opd_addr = addr + cmd_len;
  opd = msp430dis_operand (opd_addr, info, regd, ad, &cmd_len);

  msp430_decode_operand (regs, as, ops_addr, ops, OP_16BIT, op1, comm1);
  msp430_decode_operand (regd, ad, opd_addr, opd, OP_16BIT, op2, comm2);

  return cmd_len;
}

static int
msp430_branchinstr (disassemble_info * info,
		    struct msp430_opcode_s const *opcode ATTRIBUTE_UNUSED,
		    bfd_vma addr ATTRIBUTE_UNUSED,
		    unsigned short insn, char *op1, char *comm1)
{
  int regs = (insn & 0x0f00) >> 8;
  int as = (insn & 0x0030) >> 4;
  int cmd_len = 2;
  bfd_vma op_addr;
  short dst;

  op_addr = addr + cmd_len;
  dst = msp430dis_operand (op_addr, info, regs, as, &cmd_len);
  msp430_decode_operand (regs, as, op_addr, dst, OP_16BIT_HEX, op1, comm1);

  return cmd_len;
}

static opwidth_t
msp430x_opwidth (unsigned int insn)
{
  insn &= NON_ADDR_OPERATION | BYTE_OPERATION_X;

  if (insn == (NON_ADDR_OPERATION | BYTE_OPERATION_X))
    return BYTE_OP;
  if (insn == NON_ADDR_OPERATION)
    return WORD_OP;
  if (insn == BYTE_OPERATION_X)
    return ADDR_OP;
   /*NOTREACHED*/ return DEFAULT_OP;
}

static void
set_repeats (unsigned int insn, int *repeats)
{
  if (0 == (insn & 0x008f))
    return;

  /* Use non-negative number to represent Rn; use a negative number to
   * represent an immediate count (note that the stored value is one
   * less than the count). */
  if (insn & 0x0080)
    *repeats = insn & 0xf;
  else
    *repeats = -(1 + (insn & 0xf));
}

static int
msp430x_singleoperand (disassemble_info * info,
		       struct msp430_opcode_s const *opcode,
		       bfd_vma addr,
		       unsigned int insn, char *op, char *comm, int *repeats)
{
  int reg = (insn >> 16) & 0xf;
  int am = (insn >> 20) & 0x3;
  int cmd_len = 4;
  int dst = 0;
  bfd_vma op_addr;

  op_addr = addr + cmd_len;
  if (opcode_variant (opcode) < V_PUSHX)
    if ((am == AMs_Immediate && reg == REGNO_PC)	/* #N */
	|| (am == AM_Register && reg == REGNO_CG2))	/* r3 */
      {
	strcpy (comm, _("Illegal as 1-op instr"));
	return -1;
      }

  /* If register-mode extension set the repeat count */
  if (am == AM_Register)
    set_repeats (insn, repeats);

  dst = msp430dis_operand (op_addr, info, reg, am,
			   &cmd_len) | ((insn & 0x0000000f) << 16);
  /* BOGOSITY: sign extension */
  dst = (dst << 12) >> 12;
  msp430x_decode_operand (reg, am, op_addr, dst, OP_20BIT, op, comm);

  return cmd_len;
}

static int
msp430x_exception (disassemble_info * info,
		   struct msp430_opcode_s const *opcode,
		   bfd_vma addr,
		   unsigned int insn,
		   char *op1,
		   char *op2, char *comm1, char *comm2, opwidth_t * op_width)
{
  int reg = 0;
  int cmd_len = 2;
  bfd_vma op_addr;
  int n = 0;
  int dst = 0;

  reg = insn & 0xf;
  op_addr = addr + cmd_len;

  switch (opcode_variant (opcode))
    {
    case V_CALLA:
      switch ((insn >> 4) & 0xf)
	{
	case 4:		/* Rdst */
	  msp430x_decode_operand (reg, AM_Register, 0, 0, OP_OTHER, op1, comm1);
	  break;
	case 5:		/* x(Rdst) */
	  dst = msp430dis_operand (op_addr, info, reg, AM_Indexed, &cmd_len);
	  msp430x_decode_operand (reg, AM_Indexed, op_addr, dst,
				  OP_16BIT, op1, comm1);
	  break;
	case 6:		/* @Rdst */
	  msp430x_decode_operand (reg, AMs_IndirectRegister, 0, 0, OP_OTHER, op1,
				  comm1);
	  break;
	case 7:		/* @Rdst+ */
	  msp430x_decode_operand (reg, AMs_IndirectAutoIncrement, 0, 0, OP_OTHER,
				  op1, comm1);
	  break;
	case 8:		/* &abs20 */
	  dst = msp430dis_operand (op_addr, info, REGNO_CG1, AM_Indexed, &cmd_len);
	  dst |= (insn & 0x000f) << 16;
	  msp430x_decode_operand (REGNO_CG1, AM_Symbolic, op_addr,
				  dst, OP_20BIT_HEX, op1, comm1);
	  break;
	case 9:		/* EDE */
	  dst = msp430dis_operand (op_addr, info, REGNO_PC, AM_Indexed, &cmd_len);
	  dst |= (insn & 0x000f) << 16;
	  msp430x_decode_operand (REGNO_PC, AM_Indexed, op_addr, dst,
				  OP_20BIT, op1, comm1);
	  break;
	case 0xb:		/* #imm20 */
	  dst = msp430dis_operand (op_addr, info, REGNO_PC, AMs_Immediate, &cmd_len);
	  dst |= (insn & 0x000f) << 16;
	  msp430x_decode_operand (REGNO_PC, AMs_IndirectAutoIncrement,
				  op_addr, dst, OP_20BIT_HEX, op1, comm1);
	  break;
	}
      break;
    case V_PUSHM:
      n = ((insn >> 4) & 0xf) + 1;
      msp430x_decode_operand (REGNO_PC, AMs_IndirectAutoIncrement, 0, n, OP_DECIMAL, op1, comm1);	/* #N */
      msp430x_decode_operand (reg, AM_Register, 0, 0, OP_OTHER, op2, comm2);	/* Rdst */
      if ((insn & 0x0100) == 0)
	*op_width = ADDR_OP;
      break;
    case V_POPM:
      n = ((insn >> 4) & 0xf) + 1;
      reg = (reg + n - 1) & 0xf;
      msp430x_decode_operand (REGNO_PC, AMs_IndirectAutoIncrement, 0, n, OP_DECIMAL, op1, comm1);	/* #N */
      msp430x_decode_operand (reg, AM_Register, 0, 0, OP_OTHER, op2, comm2);	/* Rdst */
      if ((insn & 0x0100) == 0)
	*op_width = ADDR_OP;
      break;
    case V_ROTM:
      n = ((insn >> 10) & 0x3) + 1;
      msp430x_decode_operand (REGNO_PC, AMs_IndirectAutoIncrement, 0, n, OP_DECIMAL, op1, comm1);	/* #N */
      msp430x_decode_operand (reg, AM_Register, 0, 0, OP_OTHER, op2, comm2);	/* Rdst */
      if ((insn & 0x0010) == 0)
	*op_width = ADDR_OP;
      break;
    default:
      break;
    }
  return cmd_len;
}

static int
msp430x_doubleoperand (disassemble_info * info,
		       struct msp430_opcode_s const *opcode,
		       bfd_vma addr,
		       unsigned int insn,
		       char *op1,
		       char *op2,
		       char *comm1,
		       char *comm2, opwidth_t * op_width, int *repeats)
{
  int regs, regd;
  int as, ad;
  int ops, opd;
  int cmd_len = 4;
  bfd_vma ops_addr;
  bfd_vma opd_addr;

  regd = (insn >> 16) & 0xf;
  regs = (insn >> 24) & 0xf;
  as = (insn >> 20) & 0x3;
  ad = (insn >> 23) & 0x1;

  ops_addr = addr + cmd_len;

  if (ad == AM_Register && regd == REGNO_CG2)
    {
      /* R3 is illegal as dest: may be data section. */
      if (comm1)
	strcpy (comm1, _("Illegal as 2-op instr"));
      else if (comm2)
	strcpy (comm2, _("Illegal as 2-op instr"));
      return -1;
    }
  *op_width = msp430x_opwidth (insn);

  /* If register-mode extension set the repeat count */
  if (as == AM_Register && ad == AM_Register)
    set_repeats (insn, repeats);

  ops = msp430dis_operand (ops_addr, info, regs, as, &cmd_len);
  ops |= (insn & 0x00000780) << 9;
  /* BOGOSITY: sign extension */
  ops = (ops << 12) >> 12;

  opd_addr = addr + cmd_len;
  opd = msp430dis_operand (opd_addr, info, regd, ad, &cmd_len);
  opd |= (insn & 0x0000000f) << 16;
  /* BOGOSITY: sign extension */
  opd = (opd << 12) >> 12;

  msp430x_decode_operand (regs, as, ops_addr, ops, OP_20BIT, op1, comm1);

  if (opcode_variant (opcode) == V_X_SHIFT && ((0 == (as | ad) && ops != opd)	/* non-register extension different ops */
					       || regs != regd))	/* register extension different regs */
    return 0;

  msp430x_decode_operand (regd, ad, opd_addr, opd, OP_20BIT, op2, comm2);

  return cmd_len;
}

static int
msp430x_address (disassemble_info * info,
		 bfd_vma addr,
		 unsigned short insn,
		 char *op1, char *op2, char *comm1, char *comm2)
{
  int cmd_len = 2;
  bfd_vma op_addr;
  int dst = 0;
  typedef struct
  {
    int as, regs;
    int ad, regd;
    int length;
  }
  operands_t;

  static operands_t const operands_table[] = {
    {2, -1, 0, -1, 0},		/* 0 @Rsrc, Rdst */
    {3, -1, 0, -1, 0},		/* 1 @Rsrc+, Rdst */
    {1, 2, 0, -1, 2},		/* 2 &abs20, Rdst */
    {1, -1, 0, -1, 2},		/* 3 x(Rsrc), Rdst */
    {0, 0, 0, 0, 0},		/* 4 */
    {0, 0, 0, 0, 0},		/* 5 */
    {0, -1, 1, 2, 2},		/* 6 Rsrc, &abs20 */
    {0, -1, 1, -1, 2},		/* 7 Rsrc, x(Rdst) */
    {3, 0, 0, -1, 2},		/* 8 #imm20, Rdst */
    {3, 0, 0, -1, 2},		/* 9 #imm20, Rdst */
    {3, 0, 0, -1, 2},		/* a #imm20, Rdst */
    {3, 0, 0, -1, 2},		/* b #imm20, Rdst */
    {0, -1, 0, -1, 0},		/* c Rsrc, Rdst */
    {0, -1, 0, -1, 0},		/* d Rsrc, Rdst */
    {0, -1, 0, -1, 0},		/* e Rsrc, Rdst */
    {0, -1, 0, -1, 0},		/* f Rsrc, Rdst */
  };
  operand_t size;

  op_addr = addr + cmd_len;
  operands_t operands = operands_table[(insn >> 4) & 0xf];
  if (((insn >> 4) & 0xf) == 6)
    dst = msp430dis_opcode (op_addr, info) | ((insn & 0x000f) << 16);
  else if (((insn >> 4) & 0xb) == 3)
    dst = (short) msp430dis_opcode (op_addr, info);
  else if (operands.length != 0)
    dst = msp430dis_opcode (op_addr, info) | ((insn & 0x0f00) << 8);

  if (operands.regs == -1)
    operands.regs = (insn >> 8) & 0x000f;
  if (operands.regd == -1)
    operands.regd = (insn >> 0) & 0x000f;

  if (operands.regd == REGNO_CG2)
    {
      /* R3 is illegal as dest: may be data section. */
      if (comm1)
	strcpy (comm1, _("Illegal as address instr"));
      else if (comm2)
	strcpy (comm2, _("Illegal as address instr"));
      return -1;
    }
  /* 3 and 7 are used for 16-bit indexed offsets */
  size = ((insn >> 4) & 0x0b) == 3 ? OP_16BIT_HEX : OP_20BIT_HEX;
  msp430x_decode_operand (operands.regs, operands.as, op_addr, dst,
			  size, op1, comm1);
  msp430x_decode_operand (operands.regd, operands.ad, op_addr, dst,
			  size, op2, comm2);
  return cmd_len + operands.length;
}

static int
msp430x_emulated (disassemble_info * info,
		  struct msp430_opcode_s const *opcode,
		  bfd_vma addr,
		  unsigned int insn,
		  char *op1, char *comm1, opwidth_t * op_width, int *repeats)
{
  switch (opcode_variant (opcode))
    {
    case V_NONE:
    case V_X_SHIFT:		/* emulated by double operand instruction */
      return msp430x_doubleoperand (info, opcode, addr, insn, (char *) 0, op1,
				    (char *) 0, comm1, op_width, repeats);
    case V_RETA:		/* reta, substituted by mova */
      return 2;
    case V_MOVA:
    case V_EMU_ADDR:		/* substituted by other address instruction */
      return msp430x_address (info, addr, insn, (char *) 0, op1,
			      (char *) 0, comm1);
    case V_BRA:		/* bra, substituted by mova */
      return msp430x_address (info, addr, insn, op1, (char *) 0,
			      comm1, (char *) 0);
    default:
      break;
    }
  return 0;
}

int
print_insn_msp430 (bfd_vma addr, disassemble_info * info)
{
  void *stream = info->stream;
  fprintf_ftype prin = info->fprintf_func;
  struct msp430_opcode_s const *opcode;
  char op1[32], op2[32], comm1[64], comm2[64];
  int cmd_len = 0;
  unsigned int insn;
  int repeats = 0;
  int cpu =
    (bfd_mach_msp430x == info->mach) ? MSP430_CPU_MSP430X : MSP430_CPU_MSP430;

  opwidth_t op_width = DEFAULT_OP;
  static char const *width_modifier[] = { "", "", ".b", ".a" };

  insn = msp430dis_opcode (addr, info);

  /* Detect extension words */
  if (cpu >= MSP430_CPU_MSP430X && ((insn & 0xf800) == 0x1800))
    insn |= msp430dis_opcode (addr + 2, info) << 16;

  *comm1 = 0;
  *comm2 = 0;

  for (opcode = msp430_opcodes; opcode->name; opcode++)
    {
      if ((insn & opcode->bin_mask) == opcode->bin_opcode)
	{
	  *op1 = 0;
	  *op2 = 0;
	  *comm1 = 0;
	  *comm2 = 0;

	  /* unsupported instruction */
	  if (opcode_format (opcode) >= FMT_X && cpu < MSP430_CPU_MSP430X)
	    break;

	  /* r0 as destination. Ad should be zero. Rdst=0 and Ad=0 are encoded in opcode & opcode_mask */
	  if (opcode_format (opcode) == FMT_EMULATED
	      && opcode_variant (opcode) == V_BR)
	    {
	      cmd_len =
		msp430_branchinstr (info, opcode, addr, insn, op1, comm1);
	      if (cmd_len)
		break;
	    }
	  if (opcode_format (opcode) < FMT_X)
	    switch (opcode->insn_opnumb)
	      {
	      case 0:
		cmd_len = msp430_nooperands (opcode, addr, insn, comm1);
		break;
	      case 2:
		cmd_len =
		  msp430_doubleoperand (info, opcode, addr, insn, op1, op2,
					comm1, comm2);
		if (insn & BYTE_OPERATION)
		  op_width = BYTE_OP;
		break;
	      case 1:
		cmd_len =
		  msp430_singleoperand (info, opcode, addr, insn, op1, comm1);
		if (insn & BYTE_OPERATION
		    && opcode_format (opcode) != FMT_JUMP)
		  op_width = BYTE_OP;
		break;
	      default:
		break;
	      }
	  else			/* 430x instruction */
	    switch (opcode_format (opcode))
	      {
	      case FMT_X_SINGLE_OPERAND:
		if (opcode_variant (opcode) == V_SWPSXT	/* swpbx, sxtx */
		    && (insn & (NON_ADDR_OPERATION | BYTE_OPERATION_X)) == 0)	/* .a, special case */
		  insn ^= BYTE_OPERATION_X;	/* make A/L, B/W as ordinary */

		op_width = msp430x_opwidth (insn);

		if (opcode_variant (opcode) == V_SWPSXT && op_width == BYTE_OP)	/* swpbx, sxtx */
		  strcpy (comm1, _("Illegal A/L, B/W bits setting"));

		cmd_len =
		  msp430x_singleoperand (info, opcode, addr, insn, op1, comm1,
					 &repeats);
		break;
	      case FMT_X_EXCEPTION:
		cmd_len =
		  msp430x_exception (info, opcode, addr, insn, op1, op2,
				     comm1, comm2, &op_width);
		break;
	      case FMT_X_DOUBLE_OPERAND:
		cmd_len =
		  msp430x_doubleoperand (info, opcode, addr, insn, op1, op2,
					 comm1, comm2, &op_width, &repeats);
		break;
	      case FMT_X_EMULATED:
		cmd_len = msp430x_emulated (info, opcode, addr, insn, op1,
					    comm1, &op_width, &repeats);
		break;

	      case FMT_X_ADDRESS:
		cmd_len = msp430x_address (info, addr, insn, op1, op2,
					   comm1, comm2);
		break;
	      default:
		break;
	      }
	}

      if (cmd_len)
	break;
    }

  if (cmd_len < 1)
    {
      /* Unknown opcode, or invalid combination of operands.  */
      (*prin) (stream, ".word	0x%04x;	????\t%s%s", MASK_16 (insn), comm1,
	       comm2);
      return 2;
    }


  if (repeats)
    {
      if (repeats < 0)
	(*prin) (stream, ".rpt\t#%d\n\t\t\t\t", -repeats);
      else
	(*prin) (stream, ".rpt\tr%d\n\t\t\t\t", repeats);
    }

  (*prin) (stream, "%s%s", opcode->name, width_modifier[op_width]);

  if (*op1)
    (*prin) (stream, "\t%s", op1);
  if (*op2)
    (*prin) (stream, ",");

  if (strlen (op1) < 7)
    (*prin) (stream, "\t");
  if (!strlen (op1))
    (*prin) (stream, "\t");

  if (*op2)
    (*prin) (stream, "%s", op2);
  if (strlen (op2) < 8)
    (*prin) (stream, "\t");

  if (*comm1 || *comm2)
    (*prin) (stream, ";");
  if (*comm1)
    (*prin) (stream, "%s", comm1);
  if (*comm1 && *comm2)
    (*prin) (stream, ", ");
  if (*comm2)
    (*prin) (stream, "%s", comm2);
  return cmd_len;
}
