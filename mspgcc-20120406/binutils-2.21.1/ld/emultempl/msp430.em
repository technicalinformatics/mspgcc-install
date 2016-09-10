# This shell script emits a C file. -*- C -*-
#   Copyright 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
#
# This file is part of the GNU Binutils.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
# MA 02110-1301, USA.
#

# Adapt genelf.em for MSP430
# This file is sourced from generic.em
#
fragment <<EOF
#include "elf-bfd.h"
#include "libbfd.h"
#include "elf/msp430.h"

EOF
source_em ${srcdir}/emultempl/elf-generic.em
fragment <<EOF

static void
gld${EMULATION_NAME}_after_open (void)
{
  bfd *ibfd;
  asection *sec;
  asymbol **syms;

  after_open_default ();

  if (link_info.relocatable)
    for (ibfd = link_info.input_bfds; ibfd != NULL; ibfd = ibfd->link_next)
      if ((syms = bfd_get_outsymbols (ibfd)) != NULL
	  && bfd_get_flavour (ibfd) == bfd_target_elf_flavour)
	for (sec = ibfd->sections; sec != NULL; sec = sec->next)
	  if ((sec->flags & (SEC_GROUP | SEC_LINKER_CREATED)) == SEC_GROUP)
	    {
	      struct bfd_elf_section_data *sec_data = elf_section_data (sec);
	      elf_group_id (sec) = syms[sec_data->this_hdr.sh_info - 1];
	    }
}

static void
gld${EMULATION_NAME}_before_allocation (void)
{
  if (link_info.relocatable
      && !_bfd_elf_size_group_sections (&link_info))
    einfo ("%X%P: can not size group sections: %E\n");
  before_allocation_default ();
}

static void
gld${EMULATION_NAME}_after_allocation (void)
{
  gld${EMULATION_NAME}_map_segments (FALSE);
}

static void
gld${EMULATION_NAME}_finish (void)
{
  bfd *obfd = link_info.output_bfd;
  Elf_Internal_Ehdr *o_ehdrp = elf_elfheader (obfd);
  unsigned long flags = 0;
  bfd *ibfd;

  for (ibfd = link_info.input_bfds; ibfd != NULL; ibfd = ibfd->link_next) {
    Elf_Internal_Ehdr * i_ehdrp = elf_elfheader (ibfd);

    if (EF_MSP430_UNIARCH & i_ehdrp->e_flags)
      flags |= i_ehdrp->e_flags;
  }
  if (EF_MSP430_UNIARCH & flags) {
    int bfd_mach;
    switch (flags & EF_MSP430_ARCH)
      {
      default:
      case EF_MSP430_ARCH_430:
        bfd_mach = bfd_mach_msp430;
        break;
      case EF_MSP430_ARCH_430X:
        bfd_mach = bfd_mach_msp430x;
        break;
      }
    bfd_default_set_arch_mach (obfd, bfd_arch_msp430, bfd_mach);
    o_ehdrp->e_flags = flags;
  }
  finish_default();
}
EOF
# Put these extra routines in ld_${EMULATION_NAME}_emulation
#
LDEMUL_AFTER_OPEN=gld${EMULATION_NAME}_after_open
LDEMUL_BEFORE_ALLOCATION=gld${EMULATION_NAME}_before_allocation
LDEMUL_AFTER_ALLOCATION=gld${EMULATION_NAME}_after_allocation
LDEMUL_FINISH=gld${EMULATION_NAME}_finish
