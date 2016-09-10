/* This file is tc-msp430.h
   Copyright (C) 2002, 2004, 2005, 2007, 2012 Free Software Foundation, Inc.

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
   along with GAS; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street - Fifth Floor, Boston, MA
   02110-1301, USA.  */

#ifndef TC_MSP430
#define TC_MSP430 1

#define TARGET_FORMAT "elf32-msp430"
#define TARGET_ARCH bfd_arch_msp430

#define TARGET_BYTES_BIG_ENDIAN 0
#define md_number_to_chars number_to_chars_littleendian

#define ONLY_STANDARD_ESCAPES

#define md_operand(x)

#define DOLLAR_DOT

/* True at least until far pointers invalidate this: */
#define WORKING_DOT_WORD

/* Do not allow r3-2 to become r1 */
#define md_register_arithmetic 0

#define LISTING_WORD_SIZE 2

/* TBD: SF 3486339 */
#define TC_IMPLICIT_LCOMM_ALIGNMENT(SIZE, P2VAR) (P2VAR) = 0

#define MD_PCREL_FROM_SECTION(FIXP, SEC) md_pcrel_from_section(FIXP, SEC)
extern long md_pcrel_from_section (struct fix *, segT);

#define MD_APPLY_SYM(FIXP) (0)

/* Allow symbol-. to force PCREL relocations */
#define DIFF_EXPR_OK

#endif /* TC_MSP430 */
