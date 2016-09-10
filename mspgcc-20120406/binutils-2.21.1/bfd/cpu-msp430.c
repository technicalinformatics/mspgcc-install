/* BFD library support routines for the MSP architecture.
   Copyright (C) 2002, 2003, 2005, 2007 Free Software Foundation, Inc.
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
#include "libbfd.h"

/* This routine is provided two arch_infos and works out which MSP
   machine which would be compatible with both and returns a pointer
   to its info structure. */
static const bfd_arch_info_type *
compatible (const bfd_arch_info_type * a, const bfd_arch_info_type * b)
{
  /* If a & b are for different architectures we can do nothing */
  if (a->arch != b->arch)
    return NULL;

  if (a->mach <= b->mach)
    return b;

  return a;
}

/* Architecture for MSP430X and MSP430XV2 */
static const bfd_arch_info_type bfd_msp430x_arch = {
  16,				/* 16 bits in a word */
  20,				/* 20 bits in an address */
  8,				/* 8 bits in a byte */
  bfd_arch_msp430,
  bfd_mach_msp430x,		/* Machine number */
  "msp430",			/* Architecture name.   */
  "msp430:430X",		/* Printable name */
  1,				/* Section align power */
  FALSE,			/* The default machine */
  compatible,
  bfd_default_scan,
  0
};

/* Architecture for MSP430 */
const bfd_arch_info_type bfd_msp430_arch = {
  16,				/* 16 bits in a word */
  16,				/* 16 bits in an address */
  8,				/* 8 bits in a byte */
  bfd_arch_msp430,
  bfd_mach_msp430,		/* Machine number */
  "msp430",			/* Architecture name */
  "msp430:430",			/* Printable name */
  1,				/* Section align power */
  TRUE,				/* The default machine */
  compatible,
  bfd_default_scan,
  &bfd_msp430x_arch
};
