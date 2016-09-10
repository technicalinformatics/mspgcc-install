/*  MSP430-specific support for 32-bit ELF
    Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2010, 2012
    Free Software Foundation, Inc.
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
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
    MA 02110-1301, USA.  */

#include "sysdep.h"
#include "bfd.h"
#include "libiberty.h"
#include "libbfd.h"
#include "elf-bfd.h"
#include "elf/msp430.h"
#include <assert.h>

/* Use RELA instead of REL.  */
#undef USE_REL

#define MSP430_MASK_10(_x) ((_x) & 0x000003FF)
#define MSP430_MASK_16(_x) ((_x) & 0x0000FFFF)
#define MSP430_MASK_20(_x) ((_x) & 0x000FFFFF)

/* Test that the value is representable in 16 bits, either signed or
 * unsigned */
#define MSP430_16_IN_RANGE(_v) ((_v) >= -((bfd_signed_vma)1 << 15) && (_v) < ((bfd_signed_vma)1 << 16))

/* Test that the value is representable in 15 bits, signed only */
#define MSP430_S16_IN_RANGE(_v) ((_v) >= -((bfd_signed_vma)1 << 15) && (_v) < ((bfd_signed_vma)1 << 15))

/* Test that the value is representable in 20 bits, either signed or
 * unsigned */
#define MSP430_20_IN_RANGE(_v) ((_v) >= -((bfd_signed_vma)1 << 19) && (_v) < ((bfd_signed_vma)1 << 20))

/* Test that the value is representable in 10 bits, signed only */
#define MSP430_S10_IN_RANGE(_v) ((_v) >= -((bfd_signed_vma)1 << 9) && (_v) < ((bfd_signed_vma)1 << 9))

/* Test whether the value is odd */
#define MSP430_ODD(_v) ((_v) & 1)

static reloc_howto_type elf_msp430_howto_table[] = {
  /* BFD_RELOC_NONE */
  HOWTO (R_MSP430_NONE,		/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 FALSE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430_NONE",	/* name */
	 FALSE,			/* partial_inplace */
	 0,			/* src_mask */
	 0,			/* dst_mask */
	 FALSE),		/* pcrel_offset */

  /* BFD_RELOC_32: Not used by msp430 back end, but invoked perhaps to
   * write elf header fields. */
  HOWTO (R_MSP430_32,		/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 FALSE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430_32",		/* name */
	 FALSE,			/* partial_inplace */
	 0xffffffff,		/* src_mask */
	 0xffffffff,		/* dst_mask */
	 FALSE),		/* pcrel_offset */

  /* BFD_RELOC_MSP430_10_PCREL: Same as BFD_RELOC_MSP430_10, but the
   * offset of the storage location is subtracted prior to
   * non-relocatable storage. */
  HOWTO (R_MSP430_10_PCREL,	/* type */
	 1,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 10,			/* bitsize */
	 TRUE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430_10_PCREL",	/* name */
	 FALSE,			/* partial_inplace */
	 0x3ff,			/* src_mask */
	 0x3ff,			/* dst_mask */
	 TRUE),			/* pcrel_offset */

  /* BFD_RELOC_MSP430_16: Same as BFD_RELOC_MSP430_16_BYTE, but
   * enforces aligned access by rejecting final values that are not
   * even. */
  HOWTO (R_MSP430_16,		/* type */
	 0,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 16,			/* bitsize */
	 FALSE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430_16",		/* name */
	 FALSE,			/* partial_inplace */
	 0,			/* src_mask */
	 0xffff,		/* dst_mask */
	 FALSE),		/* pcrel_offset */

  /* BFD_RELOC_MSP430_16_PCREL: Same as BFD_RELOC_MSP430_16, but the
   * offset of the storage location is subtracted prior to
   * non-relocatable storage.  */
  HOWTO (R_MSP430_16_PCREL,	/* type */
	 1,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 16,			/* bitsize */
	 TRUE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430_16_PCREL",	/* name */
	 FALSE,			/* partial_inplace */
	 0,			/* src_mask */
	 0xffff,		/* dst_mask */
	 TRUE),			/* pcrel_offset */

  /* BFD_RELOC_MSP430_16_BYTE: A 16-bit value representing an
   * immediate or offset, generally stored in a instruction word after
   * an opcode. */
  HOWTO (R_MSP430_16_BYTE,	/* type */
	 0,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 16,			/* bitsize */
	 FALSE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430_16_BYTE",	/* name */
	 FALSE,			/* partial_inplace */
	 0xffff,		/* src_mask */
	 0xffff,		/* dst_mask */
	 FALSE),		/* pcrel_offset */

  /* BFD_RELOC_MSP430_16_PCREL_BYTE: Same as BFD_RELOC_MSP430_16_BYTE,
   * but the offset of the storage location is subtracted prior to
   * non-relocatable storage. */
  HOWTO (R_MSP430_16_PCREL_BYTE,	/* type */
	 1,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 16,			/* bitsize */
	 TRUE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430_16_PCREL_BYTE",	/* name */
	 FALSE,			/* partial_inplace */
	 0xffff,		/* src_mask */
	 0xffff,		/* dst_mask */
	 TRUE),			/* pcrel_offset */

  /* OBSOLETE: A 13 bit PC relative relocation for complicated polymorphs.  */
  HOWTO (R_MSP430_2X_PCREL,	/* type */
	 1,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 10,			/* bitsize */
	 TRUE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430_2X_PCREL",	/* name */
	 FALSE,			/* partial_inplace */
	 0xfff,			/* src_mask */
	 0xfff,			/* dst_mask */
	 TRUE),			/* pcrel_offset */

  /* OBSOLETE: A 16 bit relaxable relocation for command address.  */
  HOWTO (R_MSP430_RL_PCREL,	/* type */
	 1,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 16,			/* bitsize */
	 TRUE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430_RL_PCREL",	/* name */
	 FALSE,			/* partial_inplace */
	 0,			/* src_mask */
	 0xffff,		/* dst_mask */
	 TRUE),			/* pcrel_offset */

  /* BFD_RELOC_MSP430X_SRC_BYTE: A 20 bit absolute value: high 4 bits
   * written into bits 10..7 of the extension word, low 16 bits
   * written into the word after the opcode (+4). */
  HOWTO (R_MSP430X_SRC_BYTE,	/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 20,			/* bitsize */
	 FALSE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430X_SRC_BYTE",	/* name */
	 FALSE,			/* partial_inplace */
	 0xfffff,		/* src_mask */
	 0,			/* dst_mask */
	 FALSE),		/* pcrel_offset */

  /* BFD_RELOC_MSP430X_SRC: Same as BFD_RELOC_MSP430X_SRC_BYTE, but
   * enforces aligned access by rejecting final values that are not
   * even. */
  HOWTO (R_MSP430X_SRC,		/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 20,			/* bitsize */
	 FALSE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430X_SRC",	/* name */
	 FALSE,			/* partial_inplace */
	 0xfffff,		/* src_mask */
	 0,			/* dst_mask */
	 FALSE),		/* pcrel_offset */

  /* BFD_RELOC_MSP430X_DST_BYTE: A 20 bit absolute value: high 4 bits
   * written into bits 3..0 of the extension word, low 16 bits written
   * into the word after the opcode (+4). */
  HOWTO (R_MSP430X_DST_BYTE,	/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 20,			/* bitsize */
	 FALSE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430X_DST_BYTE",	/* name */
	 FALSE,			/* partial_inplace */
	 0,			/* src_mask */
	 0xfffff,		/* dst_mask */
	 FALSE),		/* pcrel_offset */

  /* BFD_RELOC_MSP430X_DST: Same as BFD_RELOC_MSP430X_DST_BYTE, but
   * enforces aligned access by rejecting final values that are not
   * even. */
  HOWTO (R_MSP430X_DST,		/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 20,			/* bitsize */
	 FALSE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430X_DST",	/* name */
	 FALSE,			/* partial_inplace */
	 0,			/* src_mask */
	 0xfffff,		/* dst_mask */
	 FALSE),		/* pcrel_offset */

  /* BFD_RELOC_MSP430X_DST_2ND_BYTE: Same as
   * BFD_RELOC_MSP430X_DST_BYTE, but the low 16 bits written two words
   * after the opcode (+6). */
  HOWTO (R_MSP430X_DST_2ND_BYTE,	/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 20,			/* bitsize */
	 FALSE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430X_DST_2ND_BYTE",	/* name */
	 FALSE,			/* partial_inplace */
	 0,			/* src_mask */
	 0xfffff,		/* dst_mask */
	 FALSE),		/* pcrel_offset */

  /* BFD_RELOC_MSP430X_DST_2ND: Same as
   * BFD_RELOC_MSP430X_DST_2ND_BYTE, but enforces aligned access by
   * rejecting final values that are not even. */
  HOWTO (R_MSP430X_DST_2ND,	/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 20,			/* bitsize */
	 FALSE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430X_DST_2ND",	/* name */
	 FALSE,			/* partial_inplace */
	 0,			/* src_mask */
	 0xfffff,		/* dst_mask */
	 FALSE),		/* pcrel_offset */

  /* BFD_RELOC_MSP430X_PCREL_SRC_BYTE: Same as
   * BFD_RELOC_MSP430X_SRC_BYTE, but the offset of the storage
   * location is subtracted prior to non-relocatable storage. */
  HOWTO (R_MSP430X_PCREL_SRC_BYTE,	/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 20,			/* bitsize */
	 TRUE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430X_PCREL_SRC_BYTE",	/* name */
	 FALSE,			/* partial_inplace */
	 0xfffff,		/* src_mask */
	 0,			/* dst_mask */
	 TRUE),			/* pcrel_offset */

  /* BFD_RELOC_MSP430X_PCREL_SRC: Same as BFD_RELOC_MSP430X_SRC, but
   * the offset of the storage location is subtracted prior to
   * non-relocatable storage. */
  HOWTO (R_MSP430X_PCREL_SRC,	/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 20,			/* bitsize */
	 TRUE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430X_PCREL_SRC",	/* name */
	 FALSE,			/* partial_inplace */
	 0xfffff,		/* src_mask */
	 0,			/* dst_mask */
	 TRUE),			/* pcrel_offset */

  /* BFD_RELOC_MSP430X_PCREL_DST_BYTE: Same as
   * BFD_RELOC_MSP430X_DST_BYTE, but the offset of the storage
   * location is subtracted prior to non-relocatable storage.  */
  HOWTO (R_MSP430X_PCREL_DST_BYTE,	/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 20,			/* bitsize */
	 TRUE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430X_PCREL_DST_BYTE",	/* name */
	 FALSE,			/* partial_inplace */
	 0,			/* src_mask */
	 0xfffff,		/* dst_mask */
	 TRUE),			/* pcrel_offset */

  /* BFD_RELOC_MSP430X_PCREL_DST: Same as BFD_RELOC_MSP430X_DST, but
   * the offset of the storage location is subtracted prior to
   * non-relocatable storage. */
  HOWTO (R_MSP430X_PCREL_DST,	/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 20,			/* bitsize */
	 TRUE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430X_PCREL_DST",	/* name */
	 FALSE,			/* partial_inplace */
	 0,			/* src_mask */
	 0xfffff,		/* dst_mask */
	 TRUE),			/* pcrel_offset */

  /* BFD_RELOC_MSP430X_PCREL_DST_2ND: Same as
   * BFD_RELOC_MSP430X_DST_2ND, but the offset of the storage location
   * is subtracted prior to non-relocatable storage. */
  HOWTO (R_MSP430X_PCREL_DST_2ND,	/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 20,			/* bitsize */
	 TRUE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430X_PCREL_DST_2ND",	/* name */
	 FALSE,			/* partial_inplace */
	 0,			/* src_mask */
	 0xfffff,		/* dst_mask */
	 TRUE),			/* pcrel_offset */

  /* BFD_RELOC_MSP430X_PCREL_DST_2ND_BYTE: Same as
   * BFD_RELOC_MSP430X_DST_2ND_BYTE, but the offset of the storage
   * location is subtracted prior to non-relocatable storage. */
  HOWTO (R_MSP430X_PCREL_DST_2ND_BYTE,	/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 20,			/* bitsize */
	 TRUE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430X_PCREL_DST_2ND_BYTE",	/* name */
	 FALSE,			/* partial_inplace */
	 0,			/* src_mask */
	 0xfffff,		/* dst_mask */
	 TRUE),			/* pcrel_offset */

  /* BFD_RELOC_MSP430X_S_BYTE: A 20 bit absolute value for address
   * instructions: high 4 bits written into bits 11..8 of the opcode
   * word, low 16 bits written into the word after the opcode
   * (+2). */
  HOWTO (R_MSP430X_S_BYTE,	/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 20,			/* bitsize */
	 FALSE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430X_S_BYTE",	/* name */
	 FALSE,			/* partial_inplace */
	 0xfffff,		/* src_mask */
	 0,			/* dst_mask */
	 FALSE),		/* pcrel_offset */

  /* BFD_RELOC_MSP430X_S: Same as BFD_RELOC_MSP430X_S_BYTE, but
   * enforces aligned access by rejecting final values that are not
   * even. */
  HOWTO (R_MSP430X_S,		/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 20,			/* bitsize */
	 FALSE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430X_S",		/* name */
	 FALSE,			/* partial_inplace */
	 0xfffff,		/* src_mask */
	 0,			/* dst_mask */
	 FALSE),		/* pcrel_offset */

  /* BFD_RELOC_MSP430X_D_BYTE: A 20 bit absolute value for address
   * instructions: high 4 bits written into bits 3..0 of the opcode
   * word, low 16 bits written into the word after the opcode (+2). */
  HOWTO (R_MSP430X_D_BYTE,	/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 20,			/* bitsize */
	 FALSE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430X_D_BYTE",	/* name */
	 FALSE,			/* partial_inplace */
	 0,			/* src_mask */
	 0xfffff,		/* dst_mask */
	 FALSE),		/* pcrel_offset */

  /* BFD_RELOC_MSP430X_D: Same as BFD_RELOC_MSP430X_D_BYTE, but
   * enforces aligned access by rejecting final values that are not
   * even. */
  HOWTO (R_MSP430X_D,		/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 20,			/* bitsize */
	 FALSE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430X_D",		/* name */
	 FALSE,			/* partial_inplace */
	 0,			/* src_mask */
	 0xfffff,		/* dst_mask */
	 FALSE),		/* pcrel_offset */

  /* BFD_RELOC_MSP430X_PCREL_D: Same as BFD_RELOC_MSP430X_D, but the
   * offset of the storage location is subtracted prior to
   * non-relocatable storage. */
  HOWTO (R_MSP430X_PCREL_D,	/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 20,			/* bitsize */
	 TRUE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430X_PCREL_D",	/* name */
	 FALSE,			/* partial_inplace */
	 0,			/* src_mask */
	 0xfffff,		/* dst_mask */
	 TRUE),			/* pcrel_offset */

  /* BFD_RELOC_MSP430X_INDXD: A 16 bit value that will be sign
   * extended and added to a register to produce a 20-bit address for
   * MOVA/CALLA insns: written into the word after the opcode (+2). */
  HOWTO (R_MSP430X_INDXD,	/* type */
	 0,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 16,			/* bitsize */
	 FALSE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430X_INDXD",	/* name */
	 FALSE,			/* partial_inplace */
	 0xffff,		/* src_mask */
	 0xffff,		/* dst_mask */
	 FALSE),		/* pcrel_offset */

  /* BFD_RELOC_MSP430X_PCREL_INDXD: Same as BFD_RELOC_MSP430X_INDXD,
   * but the offset of the storage location is subtracted prior to
   * non-relocatable storage. */
  HOWTO (R_MSP430X_PCREL_INDXD,	/* type */
	 0,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 16,			/* bitsize */
	 TRUE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430X_PCREL_INDXD",	/* name */
	 FALSE,			/* partial_inplace */
	 0xffff,		/* src_mask */
	 0xffff,		/* dst_mask */
	 TRUE),			/* pcrel_offset */

  /* BFD_RELOC_MSP430_10: A 10-bit value representing a PC-relative
   * offset, used in conditional jump instructions.  The relocation
   * value is in bytes, although the representation in the instruction
   * is in words.  (NB: The fact-of PC-relative is encoded in the
   * operation: the value itself is not adjusted except to convert
   * from bytes to words.) */
  HOWTO (R_MSP430_10,	/* type */
	 1,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 10,			/* bitsize */
	 FALSE,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_MSP430_10",		/* name */
	 FALSE,			/* partial_inplace */
	 0x3ff,			/* src_mask */
	 0x3ff,			/* dst_mask */
	 TRUE),			/* pcrel_offset */
};

/* Map BFD reloc types to MSP430 ELF reloc types.  */

struct msp430_reloc_map
{
  bfd_reloc_code_real_type bfd_reloc_val;
  unsigned int elf_reloc_val;
};

static const struct msp430_reloc_map msp430_reloc_map[] = {
  {BFD_RELOC_NONE, R_MSP430_NONE},
  {BFD_RELOC_32, R_MSP430_32},
  {BFD_RELOC_MSP430_10_PCREL, R_MSP430_10_PCREL},
  {BFD_RELOC_16, R_MSP430_16_BYTE},
  {BFD_RELOC_MSP430_16_PCREL, R_MSP430_16_PCREL},
  {BFD_RELOC_MSP430_16, R_MSP430_16},
  {BFD_RELOC_MSP430_16_PCREL_BYTE, R_MSP430_16_PCREL_BYTE},
  {BFD_RELOC_MSP430_16_BYTE, R_MSP430_16_BYTE},
  {BFD_RELOC_MSP430_2X_PCREL, R_MSP430_2X_PCREL},
  {BFD_RELOC_MSP430_RL_PCREL, R_MSP430_RL_PCREL},

  {BFD_RELOC_MSP430X_SRC_BYTE, R_MSP430X_SRC_BYTE},
  {BFD_RELOC_MSP430X_SRC, R_MSP430X_SRC},
  {BFD_RELOC_MSP430X_DST_BYTE, R_MSP430X_DST_BYTE},
  {BFD_RELOC_MSP430X_DST, R_MSP430X_DST},
  {BFD_RELOC_MSP430X_DST_2ND_BYTE, R_MSP430X_DST_2ND_BYTE},
  {BFD_RELOC_MSP430X_DST_2ND, R_MSP430X_DST_2ND},

  {BFD_RELOC_MSP430X_PCREL_SRC_BYTE, R_MSP430X_PCREL_SRC_BYTE},
  {BFD_RELOC_MSP430X_PCREL_SRC, R_MSP430X_PCREL_SRC},
  {BFD_RELOC_MSP430X_PCREL_DST_BYTE, R_MSP430X_PCREL_DST_BYTE},
  {BFD_RELOC_MSP430X_PCREL_DST, R_MSP430X_PCREL_DST},
  {BFD_RELOC_MSP430X_PCREL_DST_2ND_BYTE, R_MSP430X_PCREL_DST_2ND_BYTE},
  {BFD_RELOC_MSP430X_PCREL_DST_2ND, R_MSP430X_PCREL_DST_2ND},

  {BFD_RELOC_MSP430X_S_BYTE, R_MSP430X_S_BYTE},
  {BFD_RELOC_MSP430X_S, R_MSP430X_S},
  {BFD_RELOC_MSP430X_D_BYTE, R_MSP430X_D_BYTE},
  {BFD_RELOC_MSP430X_D, R_MSP430X_D},
  {BFD_RELOC_MSP430X_PCREL_D, R_MSP430X_PCREL_D},
  {BFD_RELOC_MSP430X_INDXD, R_MSP430X_INDXD},
  {BFD_RELOC_MSP430X_PCREL_INDXD, R_MSP430X_PCREL_INDXD},

  {BFD_RELOC_MSP430_10, R_MSP430_10},
};

static reloc_howto_type *
bfd_elf32_bfd_reloc_type_lookup (bfd * abfd ATTRIBUTE_UNUSED,
				 bfd_reloc_code_real_type code)
{
  unsigned int i;

  for (i = 0; i < ARRAY_SIZE (msp430_reloc_map); i++)
    if (msp430_reloc_map[i].bfd_reloc_val == code)
      return &elf_msp430_howto_table[msp430_reloc_map[i].elf_reloc_val];

  return NULL;
}

static reloc_howto_type *
bfd_elf32_bfd_reloc_name_lookup (bfd * abfd ATTRIBUTE_UNUSED,
				 const char *r_name)
{
  unsigned int i;

  for (i = 0; i < ARRAY_SIZE (elf_msp430_howto_table); i++)
    if (elf_msp430_howto_table[i].name != NULL
	&& strcasecmp (elf_msp430_howto_table[i].name, r_name) == 0)
      return &elf_msp430_howto_table[i];

  return NULL;
}

/* Set the howto pointer for an MSP430 ELF reloc.  */

static void
msp430_info_to_howto_rela (bfd * abfd ATTRIBUTE_UNUSED,
			   arelent * cache_ptr, Elf_Internal_Rela * dst)
{
  unsigned int r_type;

  r_type = ELF32_R_TYPE (dst->r_info);
  BFD_ASSERT (r_type < (unsigned int) R_MSP430_max);
  cache_ptr->howto = &elf_msp430_howto_table[r_type];
  if (r_type != cache_ptr->howto->type)
    {
      fprintf (stderr, "r_type %u != %u = %s\n", r_type,
	       cache_ptr->howto->type, cache_ptr->howto->name);
      abort ();
    }
}

/* Look through the relocs for a section during the first phase.
   Since we don't do .gots or .plts, we just need to consider the
   virtual table relocs for gc.  */

static bfd_boolean
elf32_msp430_check_relocs (bfd * abfd, struct bfd_link_info *info,
			   asection * sec, const Elf_Internal_Rela * relocs)
{
  Elf_Internal_Shdr *symtab_hdr;
  struct elf_link_hash_entry **sym_hashes;
  const Elf_Internal_Rela *rel;
  const Elf_Internal_Rela *rel_end;

  if (info->relocatable)
    return TRUE;

  symtab_hdr = &elf_tdata (abfd)->symtab_hdr;
  sym_hashes = elf_sym_hashes (abfd);

  rel_end = relocs + sec->reloc_count;
  for (rel = relocs; rel < rel_end; rel++)
    {
      struct elf_link_hash_entry *h;
      unsigned long r_symndx;

      r_symndx = ELF32_R_SYM (rel->r_info);
      if (r_symndx < symtab_hdr->sh_info)
	h = NULL;
      else
	{
	  h = sym_hashes[r_symndx - symtab_hdr->sh_info];
	  while (h->root.type == bfd_link_hash_indirect
		 || h->root.type == bfd_link_hash_warning)
	    h = (struct elf_link_hash_entry *) h->root.u.i.link;
	}
    }

  return TRUE;
}

/* Perform a single relocation.  By default we use the standard BFD
   routines, but a few relocs, we have to do them ourselves.  */

static bfd_reloc_status_type
msp430_final_link_relocate (reloc_howto_type * howto, bfd * input_bfd,
			    asection * input_section, bfd_byte * contents,
			    Elf_Internal_Rela * rel, bfd_vma relocation)
{
  bfd_reloc_status_type r = bfd_reloc_ok;
  bfd_vma x;
  bfd_signed_vma srel = 0;

  if (howto->type > R_MSP430_32 && howto->type < R_MSP430_max)
    {
      contents += rel->r_offset;
      srel = (bfd_signed_vma) relocation;
      srel += rel->r_addend;

      if (howto->pc_relative)
	{
	  srel -= rel->r_offset;
	  srel -= (input_section->output_section->vma +
		   input_section->output_offset);
	}

      switch (howto->type)
	{
	case R_MSP430_10_PCREL:
	  /* Account for r0 already being incremented by the time the
	   * value is applied. */
	  srel -= 2;
	  break;
	case R_MSP430X_PCREL_D:	/* PC relative dst operand of calla */
	case R_MSP430X_PCREL_INDXD:	/* 16-bit idx in mova/bra instruction PC relative (symbolic) mode operand */
	  srel -= 2;		/* operand located 2 bytes after opcode */
	  break;
	case R_MSP430X_PCREL_SRC:	/* PC-relative 20-bit address operand */
	case R_MSP430X_PCREL_SRC_BYTE:
	case R_MSP430X_PCREL_DST:
	case R_MSP430X_PCREL_DST_BYTE:
	  srel -= 4;		/* operand located 4 bytes after opcode */
	  break;
	case R_MSP430X_PCREL_DST_2ND:
	case R_MSP430X_PCREL_DST_2ND_BYTE:
	  srel -= 6;		/* operand located 6 bytes after opcode */
	  break;
	}
    }

  switch (howto->type)
    {
    case R_MSP430_10:
    case R_MSP430_10_PCREL:
      if (MSP430_ODD (srel))
	r = bfd_reloc_dangerous;

      /* MSP430 addresses commands as words.  */
      srel >>= 1;

      x = bfd_get_16 (input_bfd, contents);
      x = (x & 0xfc00) | MSP430_MASK_10 (srel);
      bfd_put_16 (input_bfd, x, contents);
      if (r == bfd_reloc_ok && !MSP430_S10_IN_RANGE (srel))
	r = bfd_reloc_overflow;
      break;

    case R_MSP430_2X_PCREL:
    case R_MSP430_RL_PCREL:
      r = bfd_reloc_notsupported;
      break;

    case R_MSP430_16:
    case R_MSP430_16_PCREL:
      if (MSP430_ODD (srel))
	r = bfd_reloc_dangerous;
       /*FALLTHRU*/;
    case R_MSP430_16_PCREL_BYTE:
    case R_MSP430_16_BYTE:
      bfd_put_16 (input_bfd, MSP430_MASK_16 (srel), contents);
      if (r == bfd_reloc_ok && !MSP430_16_IN_RANGE (srel))
	r = bfd_reloc_overflow;
      break;

    case R_MSP430X_SRC:
    case R_MSP430X_PCREL_SRC:
      if (MSP430_ODD (srel))
	r = bfd_reloc_dangerous;
       /*FALLTHRU*/;
    case R_MSP430X_SRC_BYTE:
    case R_MSP430X_PCREL_SRC_BYTE:
      /* src(19:16) located at positions 10:7 of extension word */
      /* src(15:0) located just after opcode */
      x = bfd_get_16 (input_bfd, contents);
      /* 4 most-significant bits */
      x = (x & 0xf87f) | ((srel >> 9) & 0x0780);
      bfd_put_16 (input_bfd, x, contents);
      /* 16 least-significant bits */
      bfd_put_16 (input_bfd, MSP430_MASK_16 (srel), contents + 4);
      if (r == bfd_reloc_ok && !MSP430_20_IN_RANGE (srel))
	r = bfd_reloc_overflow;
      break;

    case R_MSP430X_DST:
    case R_MSP430X_PCREL_DST:
      if (MSP430_ODD (srel))
	r = bfd_reloc_dangerous;
       /*FALLTHRU*/;
    case R_MSP430X_DST_BYTE:
    case R_MSP430X_PCREL_DST_BYTE:
      /* dst(19:16) located at positions 3:0 of extension word */
      /* dst(15:0) located just after opcode */
      x = bfd_get_16 (input_bfd, contents);
      x = (x & 0xfff0) | ((srel >> 16) & 0x000f);
      bfd_put_16 (input_bfd, x, contents);
      bfd_put_16 (input_bfd, MSP430_MASK_16 (srel), contents + 4);
      if (r == bfd_reloc_ok && !MSP430_20_IN_RANGE (srel))
	r = bfd_reloc_overflow;
      break;

    case R_MSP430X_DST_2ND:
    case R_MSP430X_PCREL_DST_2ND:
      if (MSP430_ODD (srel))
	r = bfd_reloc_dangerous;
       /*FALLTHRU*/;
    case R_MSP430X_DST_2ND_BYTE:
    case R_MSP430X_PCREL_DST_2ND_BYTE:
      /* dst(19:16) located at positions 3:0 of extension word */
      /* dst(15:0) located after src(15:0) */
      x = bfd_get_16 (input_bfd, contents);
      x = (x & 0xfff0) | ((srel >> 16) & 0x000f);
      bfd_put_16 (input_bfd, x, contents);
      bfd_put_16 (input_bfd, MSP430_MASK_16 (srel), contents + 6);
      if (r == bfd_reloc_ok && !MSP430_20_IN_RANGE (srel))
	r = bfd_reloc_overflow;
      break;

    case R_MSP430X_S:
      if (MSP430_ODD (srel))
	r = bfd_reloc_dangerous;
       /*FALLTHRU*/;
    case R_MSP430X_S_BYTE:
      x = bfd_get_16 (input_bfd, contents);
      x = (x & 0xf0ff) | ((srel >> 8) & 0x0f00);
      /* src(19:16) located at positions 11:8 of opcode */
      /* src(15:0) located just after opcode */
      bfd_put_16 (input_bfd, x, contents);
      bfd_put_16 (input_bfd, MSP430_MASK_16 (srel), contents + 2);
      if (r == bfd_reloc_ok && !MSP430_20_IN_RANGE (srel))
	r = bfd_reloc_overflow;
      break;

    case R_MSP430X_D:
    case R_MSP430X_PCREL_D:
      if (MSP430_ODD (srel))
	r = bfd_reloc_dangerous;
       /*FALLTHRU*/;
    case R_MSP430X_D_BYTE:
      /* dst(19:16) located at positions 3:0 of opcode */
      /* dst(15:0) located just after opcode */
      x = bfd_get_16 (input_bfd, contents);
      x = (x & 0xfff0) | ((srel >> 16) & 0x000f);
      bfd_put_16 (input_bfd, x, contents);
      bfd_put_16 (input_bfd, MSP430_MASK_16 (srel), contents + 2);
      if (r == bfd_reloc_ok && !MSP430_20_IN_RANGE (srel))
	r = bfd_reloc_overflow;
      break;

    case R_MSP430X_PCREL_INDXD:
      if (MSP430_ODD (srel))	/*odd address  */
	r = bfd_reloc_dangerous;
       /*FALLTHRU*/;
    case R_MSP430X_INDXD:
      bfd_put_16 (input_bfd, MSP430_MASK_16 (srel), contents + 2);
      if (r == bfd_reloc_ok && !MSP430_S16_IN_RANGE (srel))
	r = bfd_reloc_overflow;
      break;

    default:
      r = _bfd_final_link_relocate (howto, input_bfd, input_section,
				    contents, rel->r_offset,
				    relocation, rel->r_addend);
    }

  return r;
}

/* Relocate an MSP430 ELF section.  */

static bfd_boolean
elf32_msp430_relocate_section (bfd * output_bfd ATTRIBUTE_UNUSED,
			       struct bfd_link_info *info,
			       bfd * input_bfd,
			       asection * input_section,
			       bfd_byte * contents,
			       Elf_Internal_Rela * relocs,
			       Elf_Internal_Sym * local_syms,
			       asection ** local_sections)
{
  Elf_Internal_Shdr *symtab_hdr;
  struct elf_link_hash_entry **sym_hashes;
  Elf_Internal_Rela *rel;
  Elf_Internal_Rela *relend;

  symtab_hdr = &elf_tdata (input_bfd)->symtab_hdr;
  sym_hashes = elf_sym_hashes (input_bfd);
  relend = relocs + input_section->reloc_count;

  for (rel = relocs; rel < relend; rel++)
    {
      reloc_howto_type *howto;
      unsigned long r_symndx;
      Elf_Internal_Sym *sym;
      asection *sec;
      struct elf_link_hash_entry *h;
      bfd_vma relocation;
      bfd_reloc_status_type r;
      const char *name = NULL;
      int r_type;

      r_type = ELF32_R_TYPE (rel->r_info);
      r_symndx = ELF32_R_SYM (rel->r_info);
      howto = elf_msp430_howto_table + r_type;
      h = NULL;
      sym = NULL;
      sec = NULL;

      if (r_symndx < symtab_hdr->sh_info)
	{
	  sym = local_syms + r_symndx;
	  sec = local_sections[r_symndx];
	  relocation = _bfd_elf_rela_local_sym (output_bfd, sym, &sec, rel);

	  name = bfd_elf_string_from_elf_section
	    (input_bfd, symtab_hdr->sh_link, sym->st_name);
	  name = (name == NULL) ? bfd_section_name (input_bfd, sec) : name;
	}
      else
	{
	  bfd_boolean unresolved_reloc, warned;

	  RELOC_FOR_GLOBAL_SYMBOL (info, input_bfd, input_section, rel,
				   r_symndx, symtab_hdr, sym_hashes,
				   h, sec, relocation,
				   unresolved_reloc, warned);
	}

      if (sec != NULL && elf_discarded_section (sec))
	RELOC_AGAINST_DISCARDED_SECTION (info, input_bfd, input_section,
					 rel, relend, howto, contents);

      if (info->relocatable)
	{
	  BFD_ASSERT (!howto->partial_inplace);
	  if (sym != NULL && ELF_ST_TYPE (sym->st_info) == STT_SECTION)
	    rel->r_addend += sec->output_offset;
	  continue;
	}

      r = msp430_final_link_relocate (howto, input_bfd, input_section,
				      contents, rel, relocation);

      if (r != bfd_reloc_ok)
	{
	  const char *msg = (const char *) NULL;

	  switch (r)
	    {
	    case bfd_reloc_overflow:
	      r = info->callbacks->reloc_overflow
		(info, (h ? &h->root : NULL), name, howto->name,
		 (bfd_vma) 0, input_bfd, input_section, rel->r_offset);
	      break;

	    case bfd_reloc_undefined:
	      r = info->callbacks->undefined_symbol
		(info, name, input_bfd, input_section, rel->r_offset, TRUE);
	      break;

	    case bfd_reloc_outofrange:
	      msg = _("internal error: out of range error");
	      break;

	    case bfd_reloc_notsupported:
	      msg = _("internal error: unsupported relocation error");
	      break;

	    case bfd_reloc_dangerous:
	      r = info->callbacks->reloc_dangerous
		(info, _("unaligned address"), input_bfd, input_section, rel->r_offset);
	      break;

	    default:
	      msg = _("internal error: unknown error");
	      break;
	    }

	  if (msg)
	    r = info->callbacks->warning
	      (info, msg, name, input_bfd, input_section, rel->r_offset);

	  if (!r)
	    return FALSE;
	}

    }

  return TRUE;
}

/* The final processing done just before writing out a MSP430 ELF object
   file.  This gets the MSP430 architecture right based on the machine
   number.  */

static void
msp430_elf_backend_final_write_processing (bfd * abfd,
					   bfd_boolean linker
					   ATTRIBUTE_UNUSED)
{
  Elf_Internal_Ehdr *i_ehdrp;
  unsigned long flags;

  i_ehdrp = elf_elfheader (abfd);
  i_ehdrp->e_machine = EM_MSP430;
  flags = 0;
  switch (bfd_get_mach (abfd))
    {
    default:
    case bfd_mach_msp430:
      flags = EF_MSP430_ARCH_430;
      break;
    case bfd_mach_msp430x:
      flags = EF_MSP430_ARCH_430X;
      break;
    }
  i_ehdrp->e_flags = EF_MSP430_UNIARCH | flags;
}

/* Set the right machine number.  */

static bfd_boolean
msp430_elf_backend_object_p (bfd * abfd ATTRIBUTE_UNUSED)
{
  Elf_Internal_Ehdr *i_ehdrp;
  int bfd_mach;

  i_ehdrp = elf_elfheader (abfd);
  if (EM_MSP430 != i_ehdrp->e_machine)
    return FALSE;
  if (EF_MSP430_UNIARCH & i_ehdrp->e_flags)
    {
      switch (i_ehdrp->e_flags & EF_MSP430_ARCH)
	{
	default:
	case EF_MSP430_ARCH_430:
	  bfd_mach = bfd_mach_msp430;
	  break;
	case EF_MSP430_ARCH_430X:
	  bfd_mach = bfd_mach_msp430x;
	  break;
	}
    }
  else
    {
      bfd_mach = bfd_mach_msp430;
    }
  return bfd_default_set_arch_mach (abfd, bfd_arch_msp430, bfd_mach);
}

#define ELF_ARCH		bfd_arch_msp430
#define ELF_MACHINE_CODE	EM_MSP430
#define ELF_MACHINE_ALT1	EM_MSP430_OLD
#define ELF_MAXPAGESIZE		1
#define	ELF_OSABI		ELFOSABI_STANDALONE

#define TARGET_LITTLE_SYM       bfd_elf32_msp430_vec
#define TARGET_LITTLE_NAME	"elf32-msp430"

#define elf_info_to_howto	             msp430_info_to_howto_rela
#define elf_backend_relocate_section         elf32_msp430_relocate_section
#define elf_backend_check_relocs             elf32_msp430_check_relocs
#define elf_backend_can_gc_sections          1
#define elf_backend_post_process_headers     _bfd_elf_set_osabi
#define elf_backend_final_write_processing   msp430_elf_backend_final_write_processing
#define elf_backend_object_p                 msp430_elf_backend_object_p

#include "elf32-target.h"
