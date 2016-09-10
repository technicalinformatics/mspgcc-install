#!/bin/sh

cat <<EOF
OUTPUT_FORMAT("${OUTPUT_FORMAT}")
OUTPUT_ARCH("${MSP430_NAME}")

${RELOCATING+INCLUDE "memory.x"}
${RELOCATING+INCLUDE "periph.x"}

SECTIONS
{
  /* Read-only sections, merged into text segment.  */
  ${TEXT_DYNAMIC+${DYNAMIC}}
  .hash          ${RELOCATING-0} : { *(.hash)          }
  .dynsym        ${RELOCATING-0} : { *(.dynsym)        }
  .dynstr        ${RELOCATING-0} : { *(.dynstr)        }
  .gnu.version   ${RELOCATING-0} : { *(.gnu.version)   }
  .gnu.version_d ${RELOCATING-0} : { *(.gnu.version_d) }
  .gnu.version_r ${RELOCATING-0} : { *(.gnu.version_r) }

  .rel.init    ${RELOCATING-0} : { *(.rel.init)  }
  .rela.init   ${RELOCATING-0} : { *(.rela.init) }
  .rel.fini    ${RELOCATING-0} : { *(.rel.fini)  }
  .rela.fini   ${RELOCATING-0} : { *(.rela.fini) }

  .rel.text    ${RELOCATING-0} : { *(.rel.text${RELOCATING+ .rel.text.* .rel.gnu.linkonce.t.*})        }
  .rela.text   ${RELOCATING-0} : { *(.rela.text${RELOCATING+ .rela.text.* .rela.gnu.linkonce.t.*})     }
  .rel.rodata  ${RELOCATING-0} : { *(.rel.rodata${RELOCATING+ .rel.rodata.* .rel.gnu.linkonce.r.*})    }
  .rela.rodata ${RELOCATING-0} : { *(.rela.rodata${RELOCATING+ .rela.rodata.* .rela.gnu.linkonce.r.*}) }
  .rel.data    ${RELOCATING-0} : { *(.rel.data${RELOCATING+ .rel.data.* .rel.gnu.linkonce.d.*})        }
  .rela.data   ${RELOCATING-0} : { *(.rela.data${RELOCATING+ .rela.data.* .rela.gnu.linkonce.d.*})     }
  .rel.bss     ${RELOCATING-0} : { *(.rel.bss${RELOCATING+ .rel.bss.* .rel.gnu.linkonce.b.*})          }
  .rela.bss    ${RELOCATING-0} : { *(.rela.bss${RELOCATING+ .rela.bss.* .rela.gnu.linkonce.b.*})       }

  .rel.ctors   ${RELOCATING-0} : { *(.rel.ctors)  }
  .rela.ctors  ${RELOCATING-0} : { *(.rela.ctors) }
  .rel.dtors   ${RELOCATING-0} : { *(.rel.dtors)  }
  .rela.dtors  ${RELOCATING-0} : { *(.rela.dtors) }
  .rel.got     ${RELOCATING-0} : { *(.rel.got)    }
  .rela.got    ${RELOCATING-0} : { *(.rela.got)   }
  .rel.plt     ${RELOCATING-0} : { *(.rel.plt)    }
  .rela.plt    ${RELOCATING-0} : { *(.rela.plt)   }

  .text :
  {
    ${RELOCATING+ . = ALIGN(2);}
    ${RELOCATING+ KEEP(*(.init .init.*)) }
    ${RELOCATING+ KEEP(*(.init0))  /* Start here after reset.               */ }
    ${RELOCATING+ KEEP(*(.init1))  /* User definable.                       */ }
    ${RELOCATING+ KEEP(*(.init2))  /* Initialize stack.                     */ }
    ${RELOCATING+ KEEP(*(.init3))  /* Initialize hardware, user definable.  */ }
    ${RELOCATING+ KEEP(*(.init4))  /* Copy data to .data, clear bss.        */ }
    ${RELOCATING+ KEEP(*(.init5))  /* User definable.                       */ }
    ${RELOCATING+ KEEP(*(.init6))  /* C++ constructors.                     */ }
    ${RELOCATING+ KEEP(*(.init7))  /* User definable.                       */ }
    ${RELOCATING+ KEEP(*(.init8))  /* User definable.                       */ }
    ${RELOCATING+ KEEP(*(.init9))  /* Call main().                          */ }
    ${RELOCATING+ KEEP(*(.fini9))  /* Falls into here after main(). User definable.  */ }
    ${RELOCATING+ KEEP(*(.fini8))  /* User definable.                           */ }
    ${RELOCATING+ KEEP(*(.fini7))  /* User definable.                           */ }
    ${RELOCATING+ KEEP(*(.fini6))  /* C++ destructors.                          */ }
    ${RELOCATING+ KEEP(*(.fini5))  /* User definable.                           */ }
    ${RELOCATING+ KEEP(*(.fini4))  /* User definable.                           */ }
    ${RELOCATING+ KEEP(*(.fini3))  /* User definable.                           */ }
    ${RELOCATING+ KEEP(*(.fini2))  /* User definable.                           */ }
    ${RELOCATING+ KEEP(*(.fini1))  /* User definable.                           */ }
    ${RELOCATING+ KEEP(*(.fini0))  /* Infinite loop after program termination.  */ }
    ${RELOCATING+ KEEP(*(.fini .fini.*))}

    ${CONSTRUCTING+ . = ALIGN(2); }
    ${CONSTRUCTING+ __ctors_start = . ; }
    ${CONSTRUCTING+ KEEP(*(.ctors)) }
    ${CONSTRUCTING+ __ctors_end = . ; }
    ${CONSTRUCTING+ __dtors_start = . ; }
    ${CONSTRUCTING+ KEEP(*(.dtors)) }
    ${CONSTRUCTING+ __dtors_end = . ; }

    ${RELOCATING+ . = ALIGN(2);}
    *(.text${RELOCATING+ .text.* .gnu.linkonce.t.*})
    ${RELOCATING+ . = ALIGN(2);}
  } ${RELOCATING+ > REGION_TEXT}

  .rodata ${RELOCATING-0} :
  {
    ${RELOCATING+ . = ALIGN(2);}
    *(.rodata${RELOCATING+ .rodata.* .gnu.linkonce.r.*})
    ${RELOCATING+ . = ALIGN(2);}
  } ${RELOCATING+ > REGION_TEXT}
  ${RELOCATING+ _etext = .;} /* Past last read-only (loadable) segment */
  
  .data ${RELOCATING-0} :
  {  
    ${RELOCATING+ . = ALIGN(2);}
    ${RELOCATING+ PROVIDE (__data_start = .) ; }
    *(.data${RELOCATING+ .data.* .gnu.linkonce.d.*})
    ${RELOCATING+ . = ALIGN(2);}
    ${RELOCATING+ _edata = . ; } /* Past last read-write (loadable) segment */
  } ${RELOCATING+ > REGION_DATA AT > REGION_TEXT}
  ${RELOCATING+ PROVIDE (__data_load_start = LOADADDR(.data) ); }
  ${RELOCATING+ PROVIDE (__data_size = SIZEOF(.data) ); }
  
  .bss ${RELOCATING-0} :
  {
    ${RELOCATING+ PROVIDE (__bss_start = .) ; }
    *(.bss${RELOCATING+ .bss.*})
    *(COMMON)
    ${RELOCATING+ . = ALIGN(2);}
    ${RELOCATING+ PROVIDE (__bss_end = .) ; }
  } ${RELOCATING+ > REGION_DATA}
  ${RELOCATING+ PROVIDE (__bss_size = SIZEOF(.bss) ); }

  .noinit ${RELOCATING-0} :
  {
    ${RELOCATING+ PROVIDE (__noinit_start = .) ; }
    *(.noinit${RELOCATING+ .noinit.*})
    ${RELOCATING+ . = ALIGN(2);}
    ${RELOCATING+ PROVIDE (__noinit_end = .) ; }
  } ${RELOCATING+ > REGION_DATA}
  ${RELOCATING+ . = ALIGN(2);}
  ${RELOCATING+ _end = . ;  } /* Past last write (loadable) segment */

  .infomem ${RELOCATING-0} :
  {
    *(.infomem)
    ${RELOCATING+ . = ALIGN(2);}
    *(.infomem.*)
  } ${RELOCATING+ > infomem}

  .infomemnobits ${RELOCATING-0} :
  {
    *(.infomemnobits)
    ${RELOCATING+ . = ALIGN(2);}
    *(.infomemnobits.*)
  } ${RELOCATING+ > infomem}

  .infoa ${RELOCATING-0} :
  {
    *(.infoa${RELOCATING+ .infoa.*})
  } ${RELOCATING+ > infoa}

  .infob ${RELOCATING-0} :
  {
    *(.infob${RELOCATING+ .infob.*})
  } ${RELOCATING+ > infob}

  .infoc ${RELOCATING-0} :
  {
    *(.infoc${RELOCATING+ .infoc.*})
  } ${RELOCATING+ > infoc}

  .infod ${RELOCATING-0} :
  {
    *(.infod${RELOCATING+ .infod.*})
  } ${RELOCATING+ > infod}

  .vectors ${RELOCATING-0}:
  {
    ${RELOCATING+ PROVIDE (__vectors_start = .) ; }
    KEEP(*(.vectors*))
    ${RELOCATING+ _vectors_end = . ; }
  } ${RELOCATING+ > vectors}

  .fartext :
  {
    ${RELOCATING+ . = ALIGN(2);}
    *(.fartext)
    ${RELOCATING+ . = ALIGN(2);}
    *(.fartext.*)
    ${RELOCATING+ _efartext = .;}
  } ${RELOCATING+ > REGION_FAR_ROM}

  /* Stabs for profiling information*/
  .profiler 0 : { *(.profiler) }
  
  /* Stabs debugging sections.  */
  .stab 0 : { *(.stab) } 
  .stabstr 0 : { *(.stabstr) }
  .stab.excl 0 : { *(.stab.excl) }
  .stab.exclstr 0 : { *(.stab.exclstr) }
  .stab.index 0 : { *(.stab.index) }
  .stab.indexstr 0 : { *(.stab.indexstr) }
  .comment 0 : { *(.comment) }
 
  /* DWARF debug sections.
     Symbols in the DWARF debugging sections are relative to the beginning
     of the section so we begin them at 0.  */

  /* DWARF 1 */
  .debug          0 : { *(.debug) }
  .line           0 : { *(.line) }

  /* GNU DWARF 1 extensions */
  .debug_srcinfo  0 : { *(.debug_srcinfo) }
  .debug_sfnames  0 : { *(.debug_sfnames) }

  /* DWARF 1.1 and DWARF 2 */
  .debug_aranges  0 : { *(.debug_aranges) }
  .debug_pubnames 0 : { *(.debug_pubnames) }

  /* DWARF 2 */
  .debug_info     0 : { *(.debug_info) *(.gnu.linkonce.wi.*) }
  .debug_abbrev   0 : { *(.debug_abbrev) }
  .debug_line     0 : { *(.debug_line) }
  .debug_frame    0 : { *(.debug_frame) }
  .debug_str      0 : { *(.debug_str) }
  .debug_loc      0 : { *(.debug_loc) }
  .debug_macinfo  0 : { *(.debug_macinfo) }

  /* DWARF 3 */
  .debug_pubtypes 0 : { *(.debug_pubtypes) }
  .debug_ranges   0 : { *(.debug_ranges) }

  ${RELOCATING+ PROVIDE (__stack = ORIGIN(ram) + LENGTH(ram));}
  ${RELOCATING+ PROVIDE (__data_start_rom = _etext);}
  ${RELOCATING+ PROVIDE (__data_end_rom   = _etext + SIZEOF (.data));}
}
EOF
