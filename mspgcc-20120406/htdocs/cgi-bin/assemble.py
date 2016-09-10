#!/usr/bin/python

#Assemble lines for TI MSP430
#(C) 2001-2002 Chris Liechti <cliechti@gmx.net>
#
#GPL license
#see http://www.fsf.org/licenses/licenses.html#GPL for more information

import re
import string, sys

#regular expressions used to parsethe asm source
splitter   = re.compile(r'\(\),\t #&')
anypreproc = re.compile(r'^[\t ]*(#.+)$')
define     = re.compile(r'^[\t ]*#define[\t ]+([\w\(\)$_]+)[\t ]+(.*)$') #only single line defines at the moment

c_commentstart = re.compile(r'/\*[^(\*/)]*')
c_commentend = re.compile(r'[^(/\*)]*\*/')
comment = re.compile(r'(;|//).*$')
asmstatement = re.compile(r'^[\t ]*(([\w\.]+)([\t ]+([ $.#&@\(\)\+\-\*/\w]+)(,[\t ]*([ $.#&\(\)\+\-\*/\w]+))?)?)?')

#argument analysaltion
immediate = re.compile(r'#([\w_\-?]+)')
absolute  = re.compile(r'&([\w_\-?]+)')
indexed   = re.compile(r'([\w_\-?]+)\((PC|SP|SR|R0|R1|R2|R3|R4|R5|R6|R7|R8|R9|R10|R11|R12|R13|R14|R15)\)', re.I)
post_inc  = re.compile(r'@(PC|SP|SR|R0|R1|R2|R3|R4|R5|R6|R7|R8|R9|R10|R11|R12|R13|R14|R15)\+', re.I)
indirect  = re.compile(r'@(PC|SP|SR|R0|R1|R2|R3|R4|R5|R6|R7|R8|R9|R10|R11|R12|R13|R14|R15)', re.I)
symbolic  = re.compile(r'[\w_\-?]+')

regnumbers = {
    'PC':   0,      'pc':   0,
    'SP':   1,      'sp':   1,
    'SR':   2,      'sr':   2,
    'R0':   0,      'r0':   0,
    'R1':   1,      'r1':   1,
    'R2':   2,      'r2':   2,
    'R3':   3,      'r3':   3,
    'R4':   4,      'r4':   4,
    'R5':   5,      'r5':   5,
    'R6':   6,      'r6':   6,
    'R7':   7,      'r7':   7,
    'R8':   8,      'r8':   8,
    'R9':   9,      'r9':   9,
    'R10':  10,     'r10':  10,
    'R11':  11,     'r11':  11,
    'R12':  12,     'r12':  12,
    'R13':  13,     'r13':  13,
    'R14':  14,     'r14':  14,
    'R15':  15,     'r15':  15,
}

_doubleopnames = {
    'mov':  0x4,
    'add':  0x5,
    'addc': 0x6,
    'subc': 0x7,
    'sub':  0x8,
    'cmp':  0x9,
    'dadd': 0xa,
    'bit':  0xb,
    'bic':  0xc,
    'bis':  0xd,
    'xor':  0xe,
    'and':  0xf,
    }

_singleopnames = {
    'rrc':  0x0,
    'swpb': 0x1,
    'rra':  0x2,
    'sxt':  0x3,
    'push': 0x4,
    'call': 0x5,
    'reti': 0x6,
    }

_jumpopnames = {
    'jne':  0x0,
    'jnz':  0x0,
    'jeq':  0x1,
    'jz':   0x1,
    'jnc':  0x2,
    'jlo':  0x2,
    'jc':   0x3,
    'jhs':  0x3,
    'jn':   0x4,
    'jge':  0x5,
    'jl':   0x6,
    'jmp':  0x7,
    }

defines = {
    'P1IN':             0x0020,
    'P1OUT':            0x0021,
    'P1DIR':            0x0022,
    'P1IFG':            0x0023,
    'P1IES':            0x0024,
    'P1IE':             0x0025,
    'P1SEL':            0x0026,

    'P2IN':             0x0028,
    'P2OUT':            0x0029,
    'P2DIR':            0x002A,
    'P2IFG':            0x002B,
    'P2IES':            0x002C,
    'P2IE':             0x002D,
    'P2SEL':            0x002E,
    }

#try to convert a string to a number
#treat 0xnn and nnh as hex, other as dec
def myint(s):
    try:
        if s[0:2] == '0x':
            return string.atoi(s[2:],16)
        else:
            return int(s)
    except:
        if s in defines.keys(): return defines[s]
    return s

#return a tuple:
# (insn in lower case without mode suffix, bytemode)
def _insnMode(insn):
    insn = string.lower(insn)
    if insn[-2:] == '.b':
        return insn[:-2],1
    elif insn[-2:] == '.w':
        return insn[:-2], 0
    else:
        return insn, 0

#build opcode from arguments
def _buildDoubleOperand( opcode, bytemode, as, src, ad, dst):
    return opcode<<12 | bytemode<<6 | as<<4 | src<<8 | ad<<7 | dst

#build opcode from arguments
def _buildSingleOperand( opcode, bytemode, ad, dst):
    return 0x1000 | opcode<<7 | bytemode<<6 | ad<<4 | dst

#build opcode from arguments
def _buildJumpOperand( opcode, offset):
    return 0x2000 | opcode<<10 | offset

#return a tuple:
# (adress_mode, register_number, memory_value or None, 0=abs 1=relative to pc, need_addidional_value, is_constreg)
ABS, PCREL = 0, 1
NORMAL, CONSTREG = 0, 1
def _buildArg(insn, argstring, bytemode):
    argstring = string.strip(argstring)
    g = immediate.match(argstring)      #immediate mode
    if g: #here we do the constreg optimisation for the msp 430
        n = myint(g.group(1))
        if bytemode:
            n = n & 0xff
        else:
            n = n & 0xffff
        if n==4 and insn != 'push':   #MSP430 has a push bug....
            return 2, 2, None, ABS, 0, CONSTREG
        elif n==8 and insn != 'push':   #MSP430 has a push bug....
            return 3, 2, None, ABS, 0, CONSTREG
        elif n==0:
            return 0, 3, None, ABS, 0, CONSTREG
        elif n==1:
            return 1, 3, None, ABS, 0, CONSTREG
        elif n==2:
            return 2, 3, None, ABS, 0, CONSTREG
        elif n==-1 or (bytemode and n==0xff) or (not bytemode and n==0xffff):
            return 3, 3, None, ABS, 0, CONSTREG
        else:
            return 3, 0, g.group(1), ABS, 1, NORMAL

    g = absolute.match(argstring)       #absolute mode
    if g: return 1, 2, g.group(1), ABS, 1, NORMAL

    g = indexed.match(argstring)        #indexed mode
    if g: return 1, regnumbers[g.group(2)], g.group(1), ABS, 1, NORMAL

    g = post_inc.match(argstring)       #post_inc mode
    if g: return 3, regnumbers[g.group(1)], None, ABS, 1, NORMAL

    g = indirect.match(argstring)       #indirect mode
    if g: return 2, regnumbers[g.group(1)], None, ABS, 1, NORMAL

    if argstring in regnumbers.keys():  #register mode
        return 0, regnumbers[argstring], None, ABS, 0, NORMAL

    return 1, 0, argstring, PCREL, 1, NORMAL   #symbolic mode

#take a string(insn) and arguments (two of them) and return list, comment, cycles
def assembleDoubleOperandInstruction(insn, arg1, arg2):
    insn, bytemode = _insnMode(insn)

    if insn not in _doubleopnames.keys():
        raise ValueError("not a valid double operand instruction")

    as, src, op1, rel1, c1, is_constreg1 = _buildArg(insn, arg1, bytemode)
    ad, dst, op2, rel2, c2, is_constreg2 = _buildArg(insn, arg2, bytemode)
    if ad > 1: raise ValueError("argument not suitable as destination")
    cycles = 1 + c1 + c2*2
    if ((ad == 0 and dst == 0) and      #destination == PC
       as != 1 and                      #but not if absolute, symbolic or indirect mode
       1
       #~ not (as == 1 and src == 3)       #except constreg 3
    ):
        cycles = cycles + 1 #destination PC adds one
    out = [[
            'OPC',
            _buildDoubleOperand(
                _doubleopnames[insn],
                bytemode,
                as, src,
                ad, dst),
        ]]
    comment = '%s%s %s' % (insn, (bytemode and '.b' or ''), "%s, %s" % (arg1, arg2))
    if op1:
        if rel1: out.append( ['PCREL', myint(op1)] )
        else:    out.append( ['DW', myint(op1)] )
        if not (src == 0 and as == 3): cycles = cycles + 1 #add one, if its not immediate
    if op2:
        if rel2: out.append( ['PCREL', myint(op2)] )
        else:    out.append( ['DW', myint(op2)] )
        cycles = cycles + 1
    return out, comment, cycles

#take a string(insn) and arguments (one of them) and return list, comment, cycles
def assembleSingleOperandInstruction(insn, arg1, arg2=None):
    insn, bytemode = _insnMode(insn)
    if insn not in _singleopnames.keys():
        raise ValueError("not a valid single operand instruction")
    ad, dst, op, rel, c, is_constreg = _buildArg(insn, arg1, bytemode)
    cycles = 1 + c*2
    #~ print "%r" % insn, ad
    if not is_constreg:
        if ad == 0:
            if dst == 0: cycles = cycles + 1 #destination PC adds one
            if insn == 'push': cycles = cycles + 2
            if insn == 'call': cycles = cycles + 3
        elif ad == 1 or ad == 2:
            if insn == 'push' or insn == 'call': cycles = cycles + 1
        elif ad == 3:
            #~ print "ad", ad, c
            if insn == 'push': cycles = cycles + 1
            if insn == 'call': cycles = cycles + 2
    else: #this happens for immediate values provided by the constant generators
        if insn == 'push': cycles = cycles + 2
        if insn == 'call': cycles = cycles + 3
    out = [[
            'OPC',
            _buildSingleOperand(
                _singleopnames[insn],
                bytemode,
                ad, dst),
        ]]
    comment = '%s%s %s' % (insn, (bytemode and '.b' or ''), arg1)
    if op:
        if rel:  out.append( ['PCREL', myint(op)] )
        else:    out.append( ['DW', myint(op)] )
        if not (dst == 0 and ad == 3): cycles = cycles + 1
    return out, comment, cycles

#take a string(insn) and arguments (one of them) and return list, comment, cycles
def assembleJumpInstruction(insn, arg1=None, arg2=None):
    if insn not in _jumpopnames.keys():
        raise ValueError("not a valid jump instruction")
    cycles = 2
    if arg1 in (".", "$"):
        target = -2
    else:
        target = myint(arg1)
        if type(target) != type(0):
            raise TypeError("offset must be a number")
        if target & 1:
            raise ValueError("odd offsets are not allowed")
    if -512 <= target/2 <= 511:
        target = (target/2 & 0x3ff)
    else:
        raise ValueError("jump target out of range")
    comment = '%s %s' % (insn, arg1)
    return [[
           'OPC',
            _buildJumpOperand(_jumpopnames[insn], target),
        ]], comment, cycles

def assembleRETI(insn, arg1, arg2):
    comment = '%s' % (insn,)
    cycles = 5
    return [['OPC', 0x1300]], comment, cycles

#these instructions are emulated by using one of the insn above
#some of depend on the constant registers to be efficient
def emulatedInstruction(insn, arg1, arg2):
    if insn[-2:] == '.b':
        insn = insn[:-2]
        mode = '.b'
    elif insn[-2:] == '.w':
        insn = insn[:-2]
        mode = ''
    else:
        mode = ''
    if   insn == 'adc':     return assembleDoubleOperandInstruction('addc%s'%mode,'#0',arg1)
    elif insn == 'dadc':    return assembleDoubleOperandInstruction('dadd%s'%mode,'#0',arg1)
    elif insn == 'dec':     return assembleDoubleOperandInstruction('sub%s'%mode,'#1',arg1)
    elif insn == 'decd':    return assembleDoubleOperandInstruction('sub%s'%mode,'#2',arg1)
    elif insn == 'inc':     return assembleDoubleOperandInstruction('add%s'%mode,'#1',arg1)
    elif insn == 'incd':    return assembleDoubleOperandInstruction('add%s'%mode,'#2',arg1)
    elif insn == 'sbc':     return assembleDoubleOperandInstruction('subc%s'%mode,'#0',arg1)
    elif insn == 'inv':     return assembleDoubleOperandInstruction('xor%s'%mode,'#-1',arg1)
    elif insn == 'rla':     return assembleDoubleOperandInstruction('add%s'%mode,arg1,arg1)
    elif insn == 'rlc':     return assembleDoubleOperandInstruction('addc%s'%mode,arg1,arg1)
    elif insn == 'clr':     return assembleDoubleOperandInstruction('mov%s'%mode,'#0',arg1)
    elif insn == 'clrc':    return assembleDoubleOperandInstruction('bic','#1','SR')
    elif insn == 'clrn':    return assembleDoubleOperandInstruction('bic','#4','SR')
    elif insn == 'clrz':    return assembleDoubleOperandInstruction('bic','#2','SR')
    elif insn == 'pop':     return assembleDoubleOperandInstruction('mov%s'%mode,'@SP+',arg1)
    elif insn == 'setc':    return assembleDoubleOperandInstruction('bis','#1','SR')
    elif insn == 'setn':    return assembleDoubleOperandInstruction('bis','#4','SR')
    elif insn == 'setz':    return assembleDoubleOperandInstruction('bis','#2','SR')
    elif insn == 'tst':     return assembleDoubleOperandInstruction('cmp%s'%mode,'#0',arg1)
    elif insn == 'br':      return assembleDoubleOperandInstruction('mov',arg1,'PC')
    elif insn == 'dint':    return assembleDoubleOperandInstruction('bic','#8','SR')
    elif insn == 'eint':    return assembleDoubleOperandInstruction('bis','#8','SR')
    elif insn == 'nop':     return assembleDoubleOperandInstruction('mov','R3','R3')
    elif insn == 'ret':     return assembleDoubleOperandInstruction('mov','@SP+','PC')
    else:                   raise ValueError("not a valid emulated instruction")


#string-function matching
instructions = {
    #emulated instructions
    'adc':      emulatedInstruction,
    'dadc':     emulatedInstruction,
    'dec':      emulatedInstruction,
    'decd':     emulatedInstruction,
    'inc':      emulatedInstruction,
    'incd':     emulatedInstruction,
    'sbc':      emulatedInstruction,
    'inv':      emulatedInstruction,
    'rla':      emulatedInstruction,
    'rlc':      emulatedInstruction,
    'clr':      emulatedInstruction,
    'clrc':     emulatedInstruction,
    'clrn':     emulatedInstruction,
    'clrz':     emulatedInstruction,
    'pop':      emulatedInstruction,
    'setc':     emulatedInstruction,
    'setn':     emulatedInstruction,
    'setz':     emulatedInstruction,
    'tst':      emulatedInstruction,
    'br':       emulatedInstruction,
    'dint':     emulatedInstruction,
    'eint':     emulatedInstruction,
    'nop':      emulatedInstruction,
    'ret':      emulatedInstruction,
    #double operand instructions
    'mov':      assembleDoubleOperandInstruction,
    'add':      assembleDoubleOperandInstruction,
    'addc':     assembleDoubleOperandInstruction,
    'subc':     assembleDoubleOperandInstruction,
    'sub':      assembleDoubleOperandInstruction,
    'cmp':      assembleDoubleOperandInstruction,
    'dadd':     assembleDoubleOperandInstruction,
    'and':      assembleDoubleOperandInstruction,
    'bic':      assembleDoubleOperandInstruction,
    'bis':      assembleDoubleOperandInstruction,
    'bit':      assembleDoubleOperandInstruction,
    'xor':      assembleDoubleOperandInstruction,
    'xor':      assembleDoubleOperandInstruction,
    #single operand instructions
    'rrc':      assembleSingleOperandInstruction,
    'swpb':     assembleSingleOperandInstruction,
    'rra':      assembleSingleOperandInstruction,
    'sxt':      assembleSingleOperandInstruction,
    'push':     assembleSingleOperandInstruction,
    'call':     assembleSingleOperandInstruction,
    'reti':     assembleSingleOperandInstruction,
    #jump instructions
    'jne':      assembleJumpInstruction,
    'jnz':      assembleJumpInstruction,
    'jeq':      assembleJumpInstruction,
    'jz':       assembleJumpInstruction,
    'jnc':      assembleJumpInstruction,
    'jlo':      assembleJumpInstruction,
    'jc':       assembleJumpInstruction,
    'jhs':      assembleJumpInstruction,
    'jl':       assembleJumpInstruction,
    'jn':       assembleJumpInstruction,
    'jge':      assembleJumpInstruction,
    'jl':       assembleJumpInstruction,
    'jmp':      assembleJumpInstruction,
    #special
    'reti':     assembleRETI,
    }

class AssembleException(Exception): pass
    
#build a list of pseudo instructions from the source
def assemble(line):
    """returns list of tuples: input, cycles, [(mode, word), ...]"""
    g = asmstatement.search(line)
    if g:
        #print g.groups()
        insn  = g.group(2)
        arg1  = g.group(4)
        arg2  = g.group(6)
        #print "%r %r %r" % (insn, arg1, arg2)
        if insn:
            minsn, bytemode = _insnMode(insn)
            if minsn in instructions.keys():
                iop, comment, cycles = apply(instructions[minsn], (string.lower(insn), arg1, arg2))
                return iop, comment, cycles
            else:
                raise AssembleException('Syntax Error: unknown instruction "%s"' % (insn))
    raise AssembleException('No MSP430 assembler instruction found')

def format(iop):
    result = ''
    for mode, code in iop:
        if result:
            result = result + '\n'
        if type(code) == type(0):
            result = result + "%s\t0x%04x" % (mode, code&0xffff)
        else:
            result = result + "%s\t%s" % (mode, code)
    return result

if __name__ == '__main__':
    if len(sys.argv) > 1:
                try:
                    iop, comment, cycles = assemble(sys.argv[1])
                    print "\t%s (%d cycle%s)" % (comment, cycles, cycles > 1 and 's' or '')
                    print format(iop)
                except AssembleException, e:
                    print "*** %s" % e
    else:
        import cgi, os
        #cgitb is not available in py 1.5.2
        #import cgitb
        #cgitb.enable()

        try:
            print "Content-Type: text/html"
            print
            fields = cgi.FieldStorage()
            if fields.has_key("line"):
                line = fields['line'].value
                print "<H3>Assembling the following line:</H3>"
                print "<pre>", line, "</pre>"
                print "<H3>Results in the following machine code:</H3>"
                print "<pre>"
                try:
                    iop, comment, cycles = assemble(line)
                    print "\t%s (%d cycle%s)" % (comment, cycles, cycles > 1 and 's' or '')
                    print format(iop)
                except (AssembleException, ValueError, TypeError), e:
                    print "*** %s" % e
                print "</pre>"
            else:
                print "please go to the correct form to enter the data for this CGI!"
        except:
            print "there was an error :-("
            import traceback
            traceback.print_exc(file=sys.stdout)


