#!/bin/sh

# This called by genscripts_extra.sh

MSP430_NAME=${EMULATION_NAME}
ARCH=msp430
SCRIPT_NAME=elf32msp430
TEMPLATE_NAME=generic
EXTRA_EM_FILE=msp430
OUTPUT_FORMAT="elf32-msp430"
MACHINE=
MAXPAGESIZE=1
EMBEDDED=yes
