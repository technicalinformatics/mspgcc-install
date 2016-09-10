#objdump: -d --prefix-addresses --show-raw-insn
#name: MSP430 basic instructions

.*: +file format .*msp.*

Disassembly of section .text:
0+000 <[^>]*> 1b f3       	and	#1,	r11	;r3 As==01
0+002 <[^>]*> 3a e3       	inv	r10		
0+004 <[^>]*> 3b e0 ff 00 	xor	#255,	r11	;#0x00ff
0+008 <[^>]*> 3c d2       	bis	#8,	r12	;r2 As==11
0+00a <[^>]*> 3d b0 10 00 	bit	#16,	r13	;#0x0010
0+00e <[^>]*> 3e c0 a0 00 	bic	#160,	r14	;#0x00a0
0+012 <[^>]*> 0f 93       	tst	r15		
0+014 <[^>]*> 1a 83       	dec	r10		
0+016 <[^>]*> 0b 73       	sbc	r11		
0+018 <[^>]*> 1c 53       	inc	r12		
0+01a <[^>]*> 2d 63       	addc	#2,	r13	;r3 As==10
0+01c <[^>]*> 0e 12       	push	r14		
0+01e <[^>]*> 3f 41       	pop	r15		
0+020 <[^>]*> 8a 11       	sxt	r10		
0+022 <[^>]*> 0b 11       	rra	r11		
0+024 <[^>]*> 8c 10       	swpb	r12		
0+026 <[^>]*> 0d 10       	rrc	r13		
0+028 <[^>]*> 30 41       	ret			
0+02a <[^>]*> 31 40 00 00 	mov	#0,	r1	;#0x0000
0+02e <[^>]*> b0 12 00 00 	call	#0x0000	
0+032 <[^>]*> 1e 42 00 00 	mov	&0x0000,r14	
0+036 <[^>]*> 0f 4e       	mov	r14,	r15	
0+038 <[^>]*> 0f 5f       	rla	r15		
0+03a <[^>]*> 0f 7f       	subc	r15,	r15	
0+03c <[^>]*> 3f e3       	inv	r15		
0+03e <[^>]*> b0 12 00 00 	call	#0x0000	
0+042 <[^>]*> 82 4e 00 00 	mov	r14,	&0x0000	
0+046 <[^>]*> 82 4f 00 00 	mov	r15,	&0x0000	
0+04a <[^>]*> 1e 42 00 00 	mov	&0x0000,r14	
0+04e <[^>]*> 0f 4e       	mov	r14,	r15	
0+050 <[^>]*> 0f 5f       	rla	r15		
0+052 <[^>]*> 0f 7f       	subc	r15,	r15	
0+054 <[^>]*> 3f e3       	inv	r15		
0+056 <[^>]*> b0 12 00 00 	call	#0x0000	
0+05a <[^>]*> 82 4e 00 00 	mov	r14,	&0x0000	
0+05e <[^>]*> 82 4f 00 00 	mov	r15,	&0x0000	
0+062 <[^>]*> 3f 40 f0 00 	mov	#240,	r15	;#0x00f0
0+066 <[^>]*> 30 40 00 00 	br	#0x0000	
0+06a <[^>]*> 92 52 00 02 72 01 	add	&0x0200,&0x0172	
0+070 <extract> 3a 40 f0 de 	mov	#-8464,	r10	;#0xdef0
0+074 <extract\+0x4> 3b 40 bc 9a 	mov	#-25924,r11	;#0x9abc
0+078 <extract\+0x8> 3c 40 78 56 	mov	#22136,	r12	;#0x5678
0+07c <extract\+0xc> 3d 40 34 12 	mov	#4660,	r13	;#0x1234
0+080 <extract0> 3a 40 7b 00 	mov	#123,	r10	;#0x007b
0+084 <extract0\+0x4> 0b 43       	clr	r11		
0+086 <extract0\+0x6> 0c 43       	clr	r12		
0+088 <extract0\+0x8> 0d 43       	clr	r13		
