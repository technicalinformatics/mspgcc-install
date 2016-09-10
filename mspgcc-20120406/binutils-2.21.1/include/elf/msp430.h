/* MSP430 ELF support for BFD.
   Copyright (C) 2002, 2003, 2004, 2010, 2012 Free Software Foundation, Inc.
   Contributed by Dmitry Diky <diwil@mail.ru>

   This file is part of BFD, the Binary File Descriptor library.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA.  */

#ifndef _ELF_MSP430_H
#define _ELF_MSP430_H

#include "elf/reloc-macros.h"

/* Relocations.  */
START_RELOC_NUMBERS (elf_msp430_reloc_type)
     RELOC_NUMBER (R_MSP430_NONE,		0)
     RELOC_NUMBER (R_MSP430_32,			1)
     RELOC_NUMBER (R_MSP430_10_PCREL,		2)
     RELOC_NUMBER (R_MSP430_16, 		3)
     RELOC_NUMBER (R_MSP430_16_PCREL, 		4)
     RELOC_NUMBER (R_MSP430_16_BYTE, 		5)
     RELOC_NUMBER (R_MSP430_16_PCREL_BYTE, 	6)
     RELOC_NUMBER (R_MSP430_2X_PCREL,		7) /* obsolete */
     RELOC_NUMBER (R_MSP430_RL_PCREL,		8) /* obsolete */
     RELOC_NUMBER (R_MSP430X_SRC_BYTE,		9)
     RELOC_NUMBER (R_MSP430X_SRC,		10)
     RELOC_NUMBER (R_MSP430X_DST_BYTE,		11)
     RELOC_NUMBER (R_MSP430X_DST,		12)
     RELOC_NUMBER (R_MSP430X_DST_2ND_BYTE,	13)
     RELOC_NUMBER (R_MSP430X_DST_2ND,		14)
     RELOC_NUMBER (R_MSP430X_PCREL_SRC_BYTE,	15)
     RELOC_NUMBER (R_MSP430X_PCREL_SRC,		16)
     RELOC_NUMBER (R_MSP430X_PCREL_DST_BYTE,	17)
     RELOC_NUMBER (R_MSP430X_PCREL_DST,		18)
     RELOC_NUMBER (R_MSP430X_PCREL_DST_2ND,	19)
     RELOC_NUMBER (R_MSP430X_PCREL_DST_2ND_BYTE,	20)
     RELOC_NUMBER (R_MSP430X_S_BYTE,		21)
     RELOC_NUMBER (R_MSP430X_S,			22)
     RELOC_NUMBER (R_MSP430X_D_BYTE,		23)
     RELOC_NUMBER (R_MSP430X_D,			24)
     RELOC_NUMBER (R_MSP430X_PCREL_D,		25)
     RELOC_NUMBER (R_MSP430X_INDXD,		26)
     RELOC_NUMBER (R_MSP430X_PCREL_INDXD,	27)
     RELOC_NUMBER (R_MSP430_10,                 28)

END_RELOC_NUMBERS (R_MSP430_max)

/* TODO: Define a set of flags that are appropriate for storage in the e_flags field.
 * Potential members include:
 * - Whether CPUX instructions are present
 * - Whether hardware multiply register references are present (which kind)
 * - The code addressing mode
 * - The data addressing mode
 */

/* Pre-uniarch versions of binutils stored machine types in the
 * e_flags field, with values up to 471 decimal.  Now we store the
 * machine type in the e_mach field, and use e_flags to identify the
 * characteristics of the code.
 *
 * Use the following flag to indicate that this object file uses the
 * uniarch flag layout. */
#define EF_MSP430_UNIARCH        0x10000000

#define EF_MSP430_ARCH_430       0x00000000
#define EF_MSP430_ARCH_430X      0x00000001
#define EF_MSP430_ARCH           0x000000FF
#if 0
/* These are symbol-associated, not archive-associated, attributes.
 * Not sure what to do with them. */
#define EF_MSP430_CPU_430        0x00000000
#define EF_MSP430_CPU_430X       0x00000200
#define EF_MSP430_CPU_430XV2     0x00000300
#define EF_MSP430_CPU            0x00000300
#define EF_MSP430_MPY_NONE       0x00000000
#define EF_MSP430_MPY_16         0x00001000
#define EF_MSP430_MPY_16_SE      (0x00008000 + EF_MSP430_MPY_16)
#define EF_MSP430_MPY_32         0x00002000
#define EF_MSP430_MPY_32_DW      (0x00008000 + EF_MSP430_MPY_32)
#define EF_MSP430_MPY_CLASS      0x00003000
#define EF_MSP430_MPY            0x0000F000
#define EF_MSP430_CODE_NEAR      0x00010000
#define EF_MSP430_CODE_FAR       0x00020000
#define EF_MSP430_CODE_MIXED     0x00030000
#define EF_MSP430_CODE           0x00030000
#define EF_MSP430_DATA_NEAR      0x00040000
#define EF_MSP430_DATA_FAR       0x00080000
#define EF_MSP430_DATA_MIXED     0x000c0000
#define EF_MSP430_DATA           0x000c0000
#define EF_MSP430_A20            0x000F0000
#endif

#endif /* _ELF_MSP430_H */
