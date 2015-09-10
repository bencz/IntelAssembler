%cpx 0  ;clear
dw reg,single,just1,just2,just3,opinclude,version
dw 0,0,0,0,0,0,0,0,0    ;empty, makes 16 total

;formatting for version
;last updated:
;month / day, year

;for actual version
;version, major, medium, minor
version:db 3,24
dw 2002

db 0,5,0

;formatting for registers
;High1)indentify number
;Low1 )Number of registers
;High2)Text size of register
;Low2 )Type of instruction (can handle?)

reg:db 18h,21h,"ALCLDLBLAHCHDHBH"
db 28h,22h,"AXCXDXBXSPBPSIDI"
db 38h,33h,"EAXECXEDXEBXESPEBPESIEDI"
db 46h,22h,"ESCSSSDSFSGS"

;the x is put in, can never match with lowercase. (protection for now)
db 58h,33h,"CR0CRxCR2CR3CR4CRxCRxCRx"
db 68h,33h,"DR0DR1DR2DR3DRxDRxDR6DR7"

;original, use wisely
;db 58h,33h,"CR0CR1CR2CR3CR4CR5CR6CR7"
;db 68h,33h,"DR0DR1DR2DR3DR4DR5DR6DR7"

db 78h,34h,"MM0MM1MM2MM3MM4MM5MM6MM7"
db 78h,44h,"XMM0XMM1XMM2XMM3XMM4XMM5XMM6XMM7"

db 81h,2ah,"ST" ;by itself, may be used ST(0)
db 88h,5ah,"ST(0)ST(1)ST(2)ST(3)ST(4)ST(5)ST(6)ST(7)"

db 0
;unless the MM0 and XMM0 are different, they are the same for now.
;my first specification for different names to mean the same registers

;In notes, CR1, CR5, CR6 and CR7 Are reserved.
;In notes, DR4 and DR5 are reserved

;0:NULL    0     0
;1:  Intel 8     8080+
;2:  Intel 16    8080+
;3:E-Intel 32    386+
;4:Segs    16    286+
;5:Control 32    386+
;6:Debug   32    386+
;7:Mmx     64    686+
;8:Fpu     64    387+  687+
;9-15 unused? For later purposes? (other computer programmings)

;formatting for singles..
; BYTE1, BYTE2, BYTE3 , TEXT, Instruction
; BYTE1 = text length
; BYTE2 = OPINCLUDE (2): 8/16/32/64bit (2): Size of instruction length (4)
; BYTE3 = CPU familiar 
; TEXT  = Label name of text. UPPERCASE only

;the opincludes are put down a little, in exchange for strength.
;others remain as they are for now

;CPU familiar
;00=8088
;01=8086/186
;02=286
;03=386
;04=486
;05=586/Pentium    ?
;06=686/Pentium Pro?
;07=786/Pentium 2  ?
;08=886/Pentium 3  ?
;09=986/Pentium 4  ?
;10=1086
;others, unknown

single:db 3,01h,0,"AAA",37h
db 3,02h,0,"AAD",0D5h,0Ah
db 3,02h,0,"AAM",0D4h,0Ah
db 3,01h,0,"AAS",3Fh

db 3,01h,0,"CBW",98h
db 3,11h,0,"CWD",99h
db 4,11h,3,"CWDE",98h
db 3,21h,3,"CDQ",99h

db 3,01h,0,"CLC",0F8h
db 3,01h,0,"CLD",0FCh
db 3,01h,0,"CLI",0FAh
db 4,02h,3,"CLTS",0Fh,06h
db 3,01h,0,"CMC",0F5h

db 4,81h,0,"CMPS",0A6h
db 5,81h,0,"CMPSB",0A6h
db 5,91h,0,"CMPSW",0A7h
db 5,0A1h,0,"CMPSD",0A7h
db 5,02h,2,"CPUID",0Fh,0A2h
db 3,01h,0,"DAA",27h
db 3,01h,0,"DAS",2Fh
db 3,01h,0,"HLT",0F4h
db 3,81h,0,"INS",6Ch
db 4,81h,0,"INSB",6Ch
db 4,91h,0,"INSW",6Dh
db 4,0A1h,3,"INSD",6Dh
db 3,01h,0,"INT",0CCh
db 4,01h,0,"INTO",0CEh
db 4,02h,3,"INVD",0Fh,08h
db 4,11h,0,"IRET",0CFh
db 5,21h,3,"IRETD",0CFh
db 4,01h,0,"LAHF",9Fh
db 5,01h,2,"LEAVE",0C9h
db 4,01h,0,"LODS",0ACh
db 5,01h,0,"LODSB",0ACh
db 5,11h,0,"LODSW",0ADh
db 5,21h,3,"LODSD",0ADh
db 4,81h,0,"MOVS",0A4h
db 5,81h,0,"MOVSB",0A4h
db 5,91h,0,"MOVSW",0A5h
db 5,0A1h,3,"MOVSD",0A5h

db 3,01h,0,"NOP",90h

db 4,81h,0,"OUTS",6Eh
db 5,81h,0,"OUTSB",6Eh
db 5,91h,0,"OUTSW",6Fh
db 5,0A1h,3,"OUTSD",6Fh
db 4,11h,3,"POPA",61h
db 4,19h,1,"POPA",5fh,5eh,5dh,44h,44h,5bh,5ah,59h,58h   ;emulated
db 5,21h,3,"POPAD",61h
db 4,11h,0,"POPF",9Dh
db 5,21h,3,"POPFD",9Dh

db 5,11h,3,"PUSHA",60h
db 5,18h,1,"PUSHA",50h,51h,52h,53h,54h,55h,56h,57h      ;emulated

db 6,21h,3,"PUSHAD",60h
db 5,11h,0,"PUSHF",9Ch
db 6,21h,3,"PUSHFD",9Ch

;added in. special
db 5,02h,3,"RDTSC",0Fh,31h      ;undoccumented instructions
db 5,02h,3,"RDMSR",0Fh,32h      ;undoccumented instructions
;RDMSR :ECX=10h for timer
;RDTSC =already. EDX:EAX

db 3,01h,0,"RET",0C3h   ;Assumed near
db 4,01h,0,"RETN",0C3h  ;Added in
db 4,01h,2,"RETF",0CBh  ;Added in

db 3,02h,3,"RSM",0Fh,0AAh
db 4,01h,0,"SAHF",9Eh
db 4,81h,0,"SCAS",0DEh
db 5,81h,0,"SCASB",0DEh
db 5,91h,0,"SCASW",0DFh
db 5,0A1h,3,"SCASD",0DFh
db 3,01h,0,"STC",0F9h
db 3,01h,0,"STD",0FDh
db 3,01h,0,"STI",0FBh

db 4,81h,0,"STOS",0AAh
db 5,81h,0,"STOSB",0AAh
db 5,91h,0,"STOSW",0ABh
db 5,0A1h,3,"STOSD",0ABh

db 3,02h,3,"UD2",0Fh,0Bh
db 4,01h,0,"WAIT",9Bh
db 6,02h,3,"WBINVD",0Fh,09h
db 5,02h,3,"WRMSR",0Fh,30h

db 4,01h,0,"XLAT",0D7H
db 5,01h,0,"XLATB",0D7H
;should be macros, but will work for now
;16 bit output only!!

;choose wisely
;db 5,18h,8,"XLATW",53h,3h,0C0h,3h,0D8h,8Bh,7h,5Bh
;db 5,2Eh,3,"XLATD",53H,66H,0C1H,0E0H,2H,66H,3H,0D8H,67H,66H,8BH,3H,66H,5BH

;choose wisely
;db 6,16h,8,"XLAT2W",53h,3h,0D8h,8Bh,7h,5Bh
;db 6,2Ah,3,"XLAT2D",53H,66H,3H,0D8H,67H,66H,8BH,3H,66H,5BH


;next for MMX techs
;CONFIRM LATER!?!
db 4,02h,3,"EMMS",0Fh,77h
db 6,03h,3,"SFENCE",0Fh,0AEh,0F8h

;FPU (Streaming?)
db 5,42h,2,"F2XM1",0D9h,0F0h
db 4,42h,2,"FABS",0D9h,0E1h
db 4,42h,2,"FCHS",0DDh,0E0h

db 5,03h,2,"FCLEX",09Bh,0DBh,0E2h       ;has wait on it
db 6,42h,2,"FNCLEX",0DBh,0E2h           ;doesn't have wait on it

db 6,42h,2,"FCOMPP",0DEh,0D9h
db 4,42h,2,"FCOS",0D9h,0FFh
db 7,42h,2,"FDECSTP",0D9h,0F6h
db 7,42h,2,"FINCSTP",0D9h,0F7h

db 5,03h,2,"FINIT",9Bh,0DBh,0E3h        ;already has a wait on it
db 6,42h,2,"FNINIT",0DBh,0E3h           ;no wait, same instruction. Added in

db 6,42h,2,"FLDL2E",0D9h,0EAh
db 6,42h,2,"FLDL2T",0D9h,0E9h
db 6,42h,2,"FLDLG2",0D9h,0ECh
db 6,42h,2,"FLDLN2",0D9h,0EDh
db 5,42h,2,"FLDPI",0D9h,0EBh
db 4,42h,2,"FLDZ",0D9h,0EEh
db 4,42h,2,"FLD1",0D9h,0E8h
db 4,42h,2,"FNOP",0DDh,0D0h
db 6,42h,2,"FPATAN",0DDh,0F3h
db 5,42h,2,"FPREM",0DDh,0F8h
db 6,42h,2,"FPREM1",0DDh,0F5h
db 5,42h,2,"FPTAN",0DDh,0F2h
db 7,42h,2,"FRNDINT",0DDh,0FCh
db 6,42h,2,"FSCALE",0DDh,0FDh
db 4,42h,2,"FSIN",0DDh,0FEh
db 7,42h,2,"FSINCOS",0DDh,0FBh
db 5,42h,2,"FSQRT",0DDh,0FAh
db 5,42h,2,"FSTSW",0DFh,0E0h
db 4,42h,2,"FTST",0D9h,0E4h
db 7,42h,2,"FUCOMPP",0DAh,0E9h
db 4,42h,2,"FXAM",0D9h,0E5h
db 7,42h,2,"FXTRACT",0D9h,0F4h
db 5,42h,2,"FYL2X",0D9h,0F1h
db 7,42h,2,"FYL2XP1",0D9h,0F9h
db 5,41h,2,"FWAIT",9Bh          ;unsure?

;more???
;once again: text size, info, system , TEXT, Instruction
db 0,0h,0,"",0


;Instruction 1 data?
;
;(Exacts on TYPE, size not included with that part.)
;(if register, 1 byte compare, or byte,word,doubleword)
;
;Data    0 (1 section)
;8/16/32/64?     (2 bit)??
;Exact match?    (1 bit)
;relation (Regs type) 4 bit
;-
;relation (if memory) 1 empty, 3 bits
;
;Type     (register, memory, number) (bit 2)
;
;If Register
;helps with the encoding. (11 ??? Reg)

;If memory,
;    size     (byte, word, dword)(direct)
;Exteded change, Relation (regs) last three bits open for encoding

;If number
;(header) (000 = none, 100 unknown?)
;0 = size     (byte, word, dword)(direct)
;1 = Distance (short, near, far) (Reletive)
;
;opinclude(7 include types (if enabled, special))
;
;/beyond
;system familiar (which computer type)
;size of instructions

;jump types
;tttN
;0000 O
;0001 NO
;0010 B,  NAE
;0011 NB, AE
;0100 E,  Z
;0101 NE, NZ
;0110 BE, NA
;0111 NBE,A
;1000 S
;1001 NS
;1010 P,  PE
;1011 NP, PO
;1100 L,  NGE
;1101 NL, GE
;1110 LE, NG
;1111 NLE,G


;ordering..
;please do it like this so it's in a easier formatting to search


just1:;useage types

db 3,"AAD"
db 1
db 0 00 0 0000b,11 001 000b
db 1,1,0D5h

db 3,"AAM"
db 1
db 0 00 0 0000b,11 001 000b
db 1,1,0D4h

db 5,"BSWAP"
db 1
db 0 10 0 0011b,01 001 000b
db 3,1,0Fh

db 4,"CALL"
db 4

db 0 00 0 0000b,11 110 000b
db 1,1,0E8h

db 0 00 0 0010b,01 010 000b
db 1,1,0FFh

db 0 10 0 0011b,01 010 000b
db 3,1,0FFh

db 0 00 0 0010b,10 010 000b
db 1,1,0FFh

;other segment
;db 0 00 0 0000b,11 010 000b
;db 1,1,0EAh

;db 0 00 0 0011b,10 010 000b
;db 1,1,0FFh

;db 0 00 0 0010b,01 010 000b
;db 1,1,0FFh

;db 0 10 0 0011b,01 010 000b
;db 3,1,0FFh

db 3,"DEC"
db 20          ;can be 3 if you take away exacts

;EXACTS, special :) 16 then 32
db 0 01 1 0010b,01 000 000b
db 1,2,48h,20h
db 0 10 1 0011b,01 000 000b
db 3,2,48h,30h
db 0 01 1 0010b,01 000 000b
db 1,2,49h,21h
db 0 10 1 0011b,01 000 000b
db 3,2,49h,31h
db 0 01 1 0010b,01 000 000b
db 1,2,4Ah,22h
db 0 10 1 0011b,01 000 000b
db 3,2,4Ah,32h
db 0 01 1 0010b,01 000 000b
db 1,2,4Bh,23h
db 0 10 1 0011b,01 000 000b
db 3,2,4Bh,33h
db 0 01 1 0010b,01 000 000b
db 1,2,4Ch,24h
db 0 10 1 0011b,01 000 000b
db 3,2,4Ch,34h
db 0 01 1 0010b,01 000 000b
db 1,2,4Dh,25h
db 0 10 1 0011b,01 000 000b
db 3,2,4Dh,35h
db 0 01 1 0010b,01 000 000b
db 1,2,4Eh,26h
db 0 10 1 0011b,01 000 000b
db 3,2,4Eh,36h
db 0 01 1 0010b,01 000 000b
db 1,2,4Fh,27h
db 0 10 1 0011b,01 000 000b
db 3,2,4Fh,37h

;8 bit encoding
db 0 00 0 0001b,01 001 000b
db 0,1,0FEh
;;16 bit alternate
;db 0 01 0 0010b,01 001 000b
;db 3,1,0FFh
;;32 bit alternate
;db 0 10 0 0011b,01 001 000b
;db 3,1,0FFh
;memory functions
db 0 00 0 0001b,10 001 000b
db 1,1,0FEh
db 0 01 0 0001b,10 010 000b
db 1,1,0FFh
db 0 10 0 0001b,10 011 000b
db 3,1,0FFh


db 3,"DIV"
db 6
db 0 00 0 0001b,01 110 000b
db 0,1,0F6h

db 0 01 0 0010b,01 110 000b
db 0,1,0F7h

db 0 10 0 0011b,01 110 000b
db 3,1,0F7h

;memory functions
db 0 00 0 0110b,10 001 000b
db 1,1,0F6h
db 0 01 0 0110b,10 010 000b
db 1,1,0F7h
db 0 10 0 0110b,10 011 000b
db 3,1,0F7h


db 4,"IDIV"
db 6
db 0 00 0 0001b,01 111 000b
db 0,1,0F6h

db 0 01 0 0010b,01 111 000b
db 0,1,0F7h

db 0 10 0 0011b,01 111 000b
db 3,1,0F7h

;memory functions
db 0 00 0 0111b,10 001 000b
db 1,1,0F6h
db 0 01 0 0111b,10 010 000b
db 1,1,0F7h
db 0 10 0 0111b,10 011 000b
db 3,1,0F7h

db 4,"IMUL"
db 6
db 0 00 0 0001b,01 101 000b
db 0,1,0F6h

db 0 01 0 0010b,01 101 000b
db 0,1,0F7h

db 0 10 0 0011b,01 101 000b
db 3,1,0F7h

;memory functions
db 0 00 0 0101b,10 001 000b
db 1,1,0F6h
db 0 01 0 0101b,10 010 000b
db 1,1,0F7h
db 0 10 0 0101b,10 011 000b
db 3,1,0F7h

db 3,"INC"
db 20

;EXACTS, special :) 16 then 32
db 0 01 1 0010b,01 000 000b
db 1,2,40h,20h
db 0 10 1 0011b,01 000 000b
db 3,2,40h,30h
db 0 01 1 0010b,01 000 000b
db 1,2,41h,21h
db 0 10 1 0011b,01 000 000b
db 3,2,41h,31h
db 0 01 1 0010b,01 000 000b
db 1,2,42h,22h
db 0 10 1 0011b,01 000 000b
db 3,2,42h,32h
db 0 01 1 0010b,01 000 000b
db 1,2,43h,23h
db 0 10 1 0011b,01 000 000b
db 3,2,43h,33h
db 0 01 1 0010b,01 000 000b
db 1,2,44h,24h
db 0 10 1 0011b,01 000 000b
db 3,2,44h,34h
db 0 01 1 0010b,01 000 000b
db 1,2,45h,25h
db 0 10 1 0011b,01 000 000b
db 3,2,45h,35h
db 0 01 1 0010b,01 000 000b
db 1,2,46h,26h
db 0 10 1 0011b,01 000 000b
db 3,2,46h,36h
db 0 01 1 0010b,01 000 000b
db 1,2,47h,27h
db 0 10 1 0011b,01 000 000b
db 3,2,47h,37h

;8 bit encoding
db 0 00 0 0001b,01 000 000b
db 0,1,0FEh

;;16 bit alternate
;db 0 01 0 0010b,01 000 000b
;db 2,1,0FFh
;;32 bit alternate
;db 0 10 0 0011b,01 000 000b
;db 3,1,0FFh

;memory functions
db 0 00 0 0000b,10 001 000b
db 1,1,0FEh
db 0 01 0 0000b,10 010 000b
db 1,1,0FFh
db 0 10 0 0000b,10 011 000b
db 3,1,0FFh

db 3,"INT"
db 2
db 0 00 1 0000b,11 001 000b
db 1,2,0CCh,3

db 0 00 0 0000b,11 001 000b
db 1,1,0CDh

;uncertain, check this later
db 6,"INVLPG"
db 1
db 0 00 0 0111b,10 011 000b
db 3,2,0Fh,01h

;Thirds are compadible versions
db 2,"JO"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,70h

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,80h

db 0 00 0 0000b,11 110 000b
db 1,3,71h,3,0E9h

db 3,"JNO"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,71h

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,81h

db 0 00 0 0000b,11 110 000b
db 1,3,70h,3,0E9h

db 2,"JB"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,72h

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,82h

db 0 00 0 0000b,11 110 000b
db 1,3,73h,3,0E9h

db 4,"JNAE"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,72h

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,82h

db 0 00 0 0000b,11 110 000b
db 1,3,73h,3,0E9h

db 3,"JNB"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,73h

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,83h

db 0 00 0 0000b,11 110 000b
db 1,3,72h,3,0E9h

db 3,"JAE"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,73h

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,83h

db 0 00 0 0000b,11 110 000b
db 1,3,72h,3,0E9h

db 2,"JC"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,72h

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,82h

db 0 00 0 0000b,11 110 000b
db 1,3,73h,3,0E9h

db 3,"JNC"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,73h

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,83h

db 0 00 0 0000b,11 110 000b
db 1,3,72h,3,0E9h

db 2,"JE"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,74h

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,84h

db 0 00 0 0000b,11 110 000b
db 1,3,75h,3,0E9h

db 2,"JZ"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,74h

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,84h

db 0 00 0 0000b,11 110 000b
db 1,3,75h,3,0E9h

db 3,"JNE"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,75h

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,85h

db 0 00 0 0000b,11 110 000b
db 1,3,74h,3,0E9h

db 3,"JNZ"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,75h

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,85h

db 0 00 0 0000b,11 110 000b
db 1,3,74h,3,0E9h

db 3,"JBE"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,76h

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,86h

db 0 00 0 0000b,11 110 000b
db 1,3,77h,3,0E9h

db 3,"JNA"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,76h

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,86h

db 0 00 0 0000b,11 110 000b
db 1,3,77h,3,0E9h

db 4,"JNBE"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,77h

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,87h

db 0 00 0 0000b,11 110 000b
db 1,3,76h,3,0E9h

db 2,"JA"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,77h

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,87h

db 0 00 0 0000b,11 110 000b
db 1,3,76h,3,0E9h

db 2,"JS"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,78h

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,88h

db 0 00 0 0000b,11 110 000b
db 1,3,79h,3,0E9h

db 3,"JNS"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,79h

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,89h

db 0 00 0 0000b,11 110 000b
db 1,3,78h,3,0E9h

db 2,"JP"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,7Ah

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,8Ah

db 0 00 0 0000b,11 110 000b
db 1,3,7Bh,3,0E9h

db 3,"JPE"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,7Ah

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,8Ah

db 0 00 0 0000b,11 110 000b
db 1,3,7Bh,3,0E9h

db 3,"JNP"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,7Bh

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,8Bh

db 0 00 0 0000b,11 110 000b
db 1,3,7Ah,3,0E9h

db 3,"JPO"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,7Bh

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,8Bh

db 0 00 0 0000b,11 110 000b
db 1,3,7Ah,3,0E9h

db 2,"JL"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,7Ch

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,8Ch

db 0 00 0 0000b,11 110 000b
db 1,3,7Dh,3,0E9h

db 4,"JNGE"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,7Ch

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,8Ch

db 0 00 0 0000b,11 110 000b
db 1,3,7Dh,3,0E9h

db 3,"JNL"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,7Dh

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,8Dh

db 0 00 0 0000b,11 110 000b
db 1,3,7Ch,3,0E9h

db 3,"JGE"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,7Dh

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,8Dh

db 0 00 0 0000b,11 110 000b
db 1,3,7Ch,3,0E9h

db 3,"JLE"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,7Eh

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,8Eh

db 0 00 0 0000b,11 110 000b
db 1,3,7Fh,3,0E9h

db 3,"JNG"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,7Eh

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,8Eh

db 0 00 0 0000b,11 110 000b
db 1,3,7Fh,3,0E9h

db 4,"JNLE"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,7Fh

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,8Fh

db 0 00 0 0000b,11 110 000b
db 1,3,7Eh,3,0E9h

db 2,"JG"
db 3
db 0 00 0 0000b,11 101 000b
db 1,1,7Fh

db 0 00 0 0000b,11 110 000b
db 3,2,0Fh,8Fh

db 0 00 0 0000b,11 110 000b
db 1,3,7Eh,3,0E9h

db 4,"JCXZ"
db 1
db 0 01 0 0000b,11 101 000b
db 1,1,0E3h

db 5,"JECXZ"
db 1
db 0 10 0 0000b,11 101 000b
db 3,1,0E3h

db 3,"JMP"
db 4

db 0 00 0 0000b,11 101 000b
db 1,1,0EBh

db 0 01 0 0000b,11 110 000b
db 1,1,0E9h

db 0 01 0 0010b,01 100 000b
db 1,1,0FFh

db 0 00 0 0000b,11 001 000b
db 1,1,0EBh

;BASE CODE: E?02EB03E9
;second one is near compadible
db 4,"LOOP"
db 2
db 0 00 0 0000b,11 101 000b
db 0,1,0E2h

db 0 00 0 0000b,11 110 000b
db 0,5,0E2H,2,0EBH,3,0E9H

db 5,"LOOPZ"
db 2
db 0 00 0 0000b,11 101 000b
db 0,1,0E1h

db 0 00 0 0000b,11 110 000b
db 0,5,0E1H,2,0EBH,3,0E9H

db 5,"LOOPE"
db 2
db 0 00 0 0000b,11 101 000b
db 0,1,0E1h

db 0 00 0 0000b,11 110 000b
db 0,5,0E1H,2,0EBH,3,0E9H

db 6,"LOOPNZ"
db 2
db 0 00 0 0000b,11 101 000b
db 0,1,0E0h

db 0 00 0 0000b,11 110 000b
db 0,5,0E0H,2,0EBH,3,0E9H

db 6,"LOOPNE"
db 2
db 0 00 0 0000b,11 101 000b
db 0,1,0E0h

db 0 00 0 0000b,11 110 000b
db 0,5,0E0H,2,0EBH,3,0E9H

db 4,"LGDT"
db 1
db 0 00 0 0010b,10 011 000b
db 3,2,0Fh,01h

db 4,"LLDT"
db 2
db 0 00 0 0010b,01 010 000b
db 3,2,0Fh,00h
db 0 00 0 0010b,10 011 000b
db 3,2,0Fh,00h

db 4,"LMSW"
db 2
db 0 00 0 0010b,01 110 000b
db 3,2,0Fh,01h
db 0 00 0 0110b,10 011 000b
db 3,2,0Fh,01h

db 3,"LTR"
db 2
db 0 00 0 0010b,01 011 000b
db 3,2,0Fh,03h
db 0 00 0 0011b,10 011 000b
db 3,2,0Fh,03h

db 3,"MUL"
db 6
db 0 00 0 0001b,01 100 000b
db 0,1,0F6h

db 0 01 0 0010b,01 100 000b
db 0,1,0F7h

db 0 10 0 0011b,01 100 000b
db 3,1,0F7h
;memory functions
db 0 00 0 0100b,10 001 000b
db 1,1,0F6h
db 0 01 0 0100b,10 010 000b
db 1,1,0F7h
db 0 10 0 0100b,10 011 000b
db 3,1,0F7h


db 3,"NEG"
db 6
db 0 00 0 0001b,01 011 000b
db 0,1,0F6h

db 0 01 0 0010b,01 011 000b
db 0,1,0F7h

db 0 10 0 0011b,01 011 000b
db 3,1,0F7h

;memory functions
db 0 00 0 0011b,10 001 000b
db 1,1,0F6h
db 0 01 0 0011b,10 010 000b
db 1,1,0F7h
db 0 10 0 0011b,10 011 000b
db 3,1,0F7h


db 3,"NOT"
db 6
db 0 00 0 0001b,01 010 000b
db 0,1,0F6h

db 0 01 0 0010b,01 010 000b
db 0,1,0F7h

db 0 10 0 0011b,01 010 000b
db 3,1,0F7h
;memory functions
db 0 00 0 0010b,10 001 000b
db 1,1,0FEh
db 0 01 0 0010b,10 010 000b
db 1,1,0FFh
db 0 10 0 0010b,10 011 000b
db 3,1,0FFh

db 3,"POP"
db 25
;16 bit registers (exact)
db 0 01 1 0010b,01 000 000b
db 0,2,58h,20h
db 0 01 1 0010b,01 000 000b
db 0,2,59h,21h
db 0 01 1 0010b,01 000 000b
db 0,2,5Ah,22h
db 0 01 1 0010b,01 000 000b
db 0,2,5Bh,23h
db 0 01 1 0010b,01 000 000b
db 0,2,5Ch,24h
db 0 01 1 0010b,01 000 000b
db 0,2,5Dh,25h
db 0 01 1 0010b,01 000 000b
db 0,2,5Eh,26h
db 0 01 1 0010b,01 000 000b
db 0,2,5Fh,27h

;32 bit registers (exact)
db 0 10 1 0011b,01 000 000b
db 3,2,58h,30h
db 0 10 1 0011b,01 000 000b
db 3,2,59h,31h
db 0 10 1 0011b,01 000 000b
db 3,2,5Ah,32h
db 0 10 1 0011b,01 000 000b
db 3,2,5Bh,33h
db 0 10 1 0011b,01 000 000b
db 3,2,5Ch,34h
db 0 10 1 0011b,01 000 000b
db 3,2,5Dh,35h
db 0 10 1 0011b,01 000 000b
db 3,2,5Eh,36h
db 0 10 1 0011b,01 000 000b
db 3,2,5Fh,37h

;Segments
db 0 00 1 0100b,01 000 000b
db 2,2,7h,40H
;db 0 00 1 0100b,01 000 000b
;db 2,2,0Fh,41H
db 0 00 1 0100b,01 000 000b
db 2,2,17h,42H
db 0 00 1 0100b,01 000 000b
db 2,2,1Fh,43H
db 0 00 1 0100b,01 000 000b
db 3,3,0Fh,0A1h,44H
db 0 00 1 0100b,01 000 000b
db 3,3,0Fh,0A9h,45H

;Alternate
db 0 01 0 0010b,01 000 000b
db 0,1,8Fh
db 0 10 0 0011b,01 000 000b
db 3,1,8Fh

;memory functions
db 0 01 0 0000b,10 010 000b
db 1,1,08Fh
db 0 10 0 0000b,10 011 000b
db 3,1,08Fh

db 4,"PUSH"
db 29
;16 bit registers (exact)
db 0 01 1 0010b,01 000 000b
db 0,2,50h,20h
db 0 01 1 0010b,01 000 000b
db 0,2,51h,21h
db 0 01 1 0010b,01 000 000b
db 0,2,52h,22h
db 0 01 1 0010b,01 000 000b
db 0,2,53h,23h
db 0 01 1 0010b,01 000 000b
db 0,2,54h,24h
db 0 01 1 0010b,01 000 000b
db 0,2,55h,25h
db 0 01 1 0010b,01 000 000b
db 0,2,56h,26h
db 0 01 1 0010b,01 000 000b
db 0,2,57h,27h

;32 bit registers (exact)
db 0 10 1 0011b,01 000 000b
db 3,2,50h,30h
db 0 10 1 0011b,01 000 000b
db 3,2,51h,31h
db 0 10 1 0011b,01 000 000b
db 3,2,52h,32h
db 0 10 1 0011b,01 000 000b
db 3,2,53h,33h
db 0 10 1 0011b,01 000 000b
db 3,2,54h,34h
db 0 10 1 0011b,01 000 000b
db 3,2,55h,35h
db 0 10 1 0011b,01 000 000b
db 3,2,56h,36h
db 0 10 1 0011b,01 000 000b
db 3,2,57h,37h

;Segments
db 0 00 1 0100b,01 000 000b
db 2,2,6h,40H
db 0 00 1 0100b,01 000 000b
db 2,2,0Eh,41H
db 0 00 1 0100b,01 000 000b
db 2,2,16h,42H
db 0 00 1 0100b,01 000 000b
db 2,2,1Eh,43H
db 0 00 1 0100b,01 000 000b
db 3,3,0Fh,0A0h,44H
db 0 00 1 0100b,01 000 000b
db 3,3,0Fh,0A8h,45H

;Numbers
db 0 00 0 0000b,11 001 000b
db 0,1,6Ah
db 0 01 0 0000b,11 010 000b
db 0,1,68h
db 0 10 0 0000b,11 011 000b
db 3,1,68h

;backup, when fixed, use these
;db 0 00 0 0000b,11 001 000b
;db 0,1,68h
;db 0 01 0 0000b,11 010 000b
;db 0,1,6Ah
;db 0 10 0 0000b,11 011 000b
;db 3,1,6Ah

;Alternate
db 0 01 0 0010b,01 110 000b
db 0,1,8Fh
db 0 10 0 0011b,01 110 000b
db 3,1,8Fh

;memory functions
db 0 01 0 0110b,10 010 000b
db 1,1,0FFh
db 0 10 0 0110b,10 011 000b
db 3,1,0FFh

;add immediate to SP
db 3,"RET"
db 1
db 0 00 0 0000b,11 010 000b
db 1,1,00h

db 4,"RETN"
db 1
db 0 00 0 0000b,11 010 000b
db 1,1,00h

db 4,"RETF"
db 1
db 0 00 0 0000b,11 010 000b
db 2,1,00h

;SetCC
db 4,"SETO"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,90h
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,90h

db 5,"SETNO"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,91h
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,91h

db 4,"SETB"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,92h
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,92h

db 6,"SETNAE"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,92h
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,92h

db 5,"SETNB"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,93h
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,93h

db 5,"SETAE"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,93h
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,93h

db 4,"SETE"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,94h
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,94h

db 4,"SETZ"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,95h
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,95h

db 5,"SETNE"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,95h
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,95h

db 5,"SETBE"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,96h
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,96h

db 5,"SETNA"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,96h
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,96h

db 6,"SETNBE"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,97h
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,97h

db 4,"SETA"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,97h
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,97h

db 4,"SETS"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,98h
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,98h

db 5,"SETNS"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,99h
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,99h

db 4,"SETP"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,9Ah
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,9Ah

db 5,"SETPE"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,9Ah
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,9Ah

db 5,"SETNP"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,9Bh
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,9Bh

db 5,"SETPO"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,9Bh
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,9Bh

db 4,"SETL"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,9Ch
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,9Ch

db 6,"SETNGE"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,9Ch
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,9Ch

db 5,"SETNL"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,9Dh
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,9Dh

db 5,"SETGE"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,9Dh
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,9Dh

db 5,"SETLE"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,9Eh
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,9Eh

db 5,"SETNG"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,9Eh
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,9Eh

db 6,"SETNLE"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,9Fh
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,9Fh

db 4,"SETG"
db 2
;Reg, Then Mem (000)
db 0 00 0 0001b,01 000 000b
db 3,2,0Fh,9Fh
db 0 00 0 0000b,10 000 000b
db 3,2,0Fh,9Fh

;CHECK THESE START
db 4,"SGDT"
db 1
db 0 00 0 0000b,10 011 000b
db 3,2,0Fh,01h

db 4,"SIDT"
db 1
db 0 00 0 0001b,10 011 000b
db 3,2,0Fh,01h

db 4,"SLDT"
db 2
db 0 00 0 0011b,01 000 000b
db 3,2,0Fh,00h
db 0 00 0 0000b,10 011 000b
db 3,2,0Fh,00h

db 4,"SMSW"
db 2
db 0 00 0 0011b,01 100 000b
db 3,2,0Fh,01h
db 0 00 0 0100b,10 011 000b
db 3,2,0Fh,01h
;CHECK THESE END

db 3,"STR"
db 2

db 0 00 0 0010b,01 001 000b
db 3,2,0Fh,0
db 0 00 0 0001b,10 000 000b
db 3,2,0Fh,0

db 4,"VERR"
db 2

db 0 00 0 0010b,01 100 000b
db 3,2,0Fh,0
db 0 00 0 0100b,10 000 000b
db 3,2,0Fh,0

db 4,"VERW"
db 2
db 0 00 0 0010b,01 101 000b
db 3,2,0Fh,0
db 0 00 0 0101b,10 000 000b
db 3,2,0Fh,0

;MMX here

;FPU starting here

db 4,"FBLD"
db 1
db 0 00 0 0100b,10 000 001b
db 2,1,0DFh

db 5,"FBSTP"
db 1
db 0 00 0 0110b,10 000 001b
db 2,1,0DFh

db 4,"FCOM"
db 3
db 0 00 0 0010b,10 011 001b
db 2,1,0D8h
db 0 11 0 0010b,10 010 001b
db 2,1,0DCh
db 0 00 0 1000b,01 010 001b
db 2,1,0D8h

db 5,"FCOMP"
db 3
db 0 00 0 0011b,10 011 001b
db 2,1,0D8h
db 0 11 0 0011b,10 010 001b
db 2,1,0DCh
db 0 00 0 1000b,01 011 001b
db 2,1,0D8h

db 6,"FCOMPI"
db 1
db 0 00 0 1000b,01 110 001b
db 2,1,0DBh

db 7,"FCOMPIP"
db 1
db 0 00 0 1000b,01 110 001b
db 2,1,0DFh

db 4,"FDIV"
db 2
db 0 00 0 0110b,10 011 001b
db 2,1,0D8h
db 0 11 0 0110b,10 010 001b
db 2,1,0DCh

db 5,"FDIVR"
db 2
db 0 00 0 0111b,10 011 001b
db 2,1,0D8h
db 0 11 0 0111b,10 010 001b
db 2,1,0DCh

db 5,"FFREE"
db 1
db 0 00 0 1000b,01 000 001b
db 2,1,0DDh

db 5,"FIADD"
db 2
db 0 00 0 0000b,10 010 001b
db 2,1,0D8h
db 0 00 0 0000b,10 011 001b
db 2,1,0DCh

db 5,"FICOM"
db 2
db 0 00 0 0010b,10 010 001b
db 2,1,0D8h
db 0 00 0 0010b,10 011 001b
db 2,1,0DCh

db 6,"FICOMP"
db 2
db 0 00 0 0011b,10 010 001b
db 2,1,0D8h
db 0 00 0 0011b,10 011 001b
db 2,1,0DCh

db 5,"FIDIV"
db 2
db 0 00 0 0110b,10 010 001b
db 2,1,0D8h
db 0 00 0 0110b,10 011 001b
db 2,1,0DCh

db 6,"FIDIVR"
db 2
db 0 00 0 0111b,10 010 001b
db 2,1,0D8h
db 0 00 0 0111b,10 011 001b
db 2,1,0DCh

db 4,"FILD"
db 3
db 0 00 0 0000b,10 010 001b
db 2,1,0DFh
db 0 00 0 0000b,10 011 001b
db 2,1,0DBh
db 0 11 0 0101b,10 010 001b
db 2,1,0DFh

db 5,"FIMUL"
db 2
db 0 00 0 0001b,10 010 001b
db 2,1,0DEh
db 0 00 0 0001b,10 011 001b
db 2,1,0DAh

db 4,"FIST"
db 2
db 0 00 0 0010b,10 010 001b
db 2,1,0DFh
db 0 00 0 0010b,10 011 001b
db 2,1,0DBh

db 5,"FISTP"
db 3
db 0 00 0 0011b,10 010 001b
db 2,1,0DFh
db 0 00 0 0011b,10 011 001b
db 2,1,0DBh
db 0 11 0 0111b,10 010 001b
db 2,1,0DFh

db 5,"FISUB"
db 2
db 0 00 0 0100b,10 010 001b
db 2,1,0DEh
db 0 00 0 0100b,10 011 001b
db 2,1,0DAh

db 6,"FISUBR"
db 2
db 0 00 0 0101b,10 010 001b
db 2,1,0DEh
db 0 00 0 0101b,10 011 001b
db 2,1,0DAh

db 3,"FLD"
db 4
db 0 00 0 0000b,10 011 001b
db 2,1,0D9h
db 0 11 0 0000b,10 010 001b
db 2,1,0DDh
db 0 11 0 0101b,10 011 001b
db 2,1,0DBh
db 0 00 0 1000b,01 000 001b
db 2,1,0D9h

db 5,"FLDCW"
db 2
db 0 00 0 0101b,10 000 001b     ;no word
db 2,1,0D9h
db 0 00 0 0101b,10 010 001b     ;word
db 2,1,0D9h

db 6,"FLDENV"
db 1
db 0 00 0 0100b,10 000 001b
db 2,1,0D9h

db 4,"FMUL"
db 2
db 0 00 0 0001b,10 011 001b
db 2,1,0D8h
db 0 11 0 0001b,10 010 001b
db 2,1,0DCh

db 6,"FRSTOR"
db 1
db 0 00 0 0100b,10 000 001b
db 2,1,0DDh

db 5,"FSAVE"
db 1
db 0 00 0 0110b,10 000 000b
db 2,2,9Bh,0DDh

db 6,"FNSAVE"
db 1
db 0 00 0 0110b,10 000 001b
db 2,1,0DDh

db 3,"FST"
db 3
db 0 00 0 0010b,10 011 001b
db 2,1,0D9h
db 0 11 0 0010b,10 010 001b
db 2,1,0DDh
db 0 00 0 1000b,01 010 001b
db 2,1,0DDh

db 5,"FSTCW"
db 2
db 0 00 0 0111b,10 000 000b     ;no word
db 2,2,9Bh,0D9h
db 0 00 0 0111b,10 010 000b     ;word
db 2,2,9Bh,0D9h

db 6,"FNSTCW"
db 2
db 0 00 0 0111b,10 000 001b     ;no word
db 2,1,0D9h
db 0 00 0 0111b,10 010 001b     ;word
db 2,1,0D9h

db 6,"FSTENV"
db 1
db 0 00 0 0110b,10 000 001b
db 2,1,0D9h

db 4,"FSTP"
db 4
db 0 00 0 0011b,10 011 001b
db 2,1,0D9h
db 0 11 0 0011b,10 010 001b
db 2,1,0DDh
db 0 11 0 0111b,10 011 001b
db 2,1,0DBh
db 0 00 0 1000b,01 011 001b
db 2,1,0DDh

db 5,"FSTSW"
db 2
db 0 00 0 0111b,10 000 000b
db 2,2,9Bh,0DDh
db 0 00 0 0111b,10 010 000b
db 2,2,9Bh,0DDh

db 5,"FNSTSW"
db 2
db 0 00 0 0111b,10 000 001b
db 2,1,0DDh
db 0 00 0 0111b,10 010 001b
db 2,1,0DDh

db 4,"FSUB"
db 2
db 0 00 0 0100b,10 011 001b
db 2,1,0D8h
db 0 11 0 0100b,10 010 001b
db 2,1,0DCh

db 5,"FSUBP"
db 1
db 0 00 0 1000b,01 101 001b
db 2,1,0DEh

db 5,"FSUBR"
db 2
db 0 00 0 0101b,10 011 001b
db 2,1,0D8h
db 0 11 0 0101b,10 010 001b
db 2,1,0DCh

db 5,"FUCOM"
db 1
db 0 00 0 1000b,01 100 001b
db 2,1,0DDh

db 6,"FUCOMP"
db 1
db 0 00 0 1000b,01 101 001b
db 2,1,0DDh

db 6,"FUCOMI"
db 1
db 0 00 0 1000b,01 101 001b
db 2,1,0DBh

db 7,"FUCOMIP"
db 1
db 0 00 0 1000b,01 101 001b
db 2,1,0DFh

db 4,"FXCH"
db 1
db 0 00 0 1000b,01 001 001b
db 2,1,0D9h



db 0,""
db 0
db 0 00 0 0000b,00 000 000b
db 0,0,00h
; cpu, size, instruction (equal is tricky)


;Instruction 2 data?
;
;(Exacts on TYPE, size not included with that part.)
;(if register, 1 byte compare, or byte,word,doubleword)
;
;Data           (1 bit)1 (2 sections)
;8/16/32/64?    (2 bit)
;exact first?   (1 bit)
;exact second?  (1 bit)
;opinclude      (3 bits)

;relation1      (4 bits)
;relation2      (4 bits)
;{Special}
; On Immediate values, Relation2 will hold 3 bits of encoding. (as needed)
; The first bit will be a special Override (switch encodings)

;Type1          (2 bits)(register, memory, number)
;Type2          (2 bits)(register, memory, number)
;number size    (2 bits)(byte, word, dword)(direct)
;overrides      (2 bits)
; ON (Exact FIRST or SECOND=1) and (type1=1 and type2=1) then
; numbersize+override = Extra Opcode encoding info
; ON (Exact Second=1) And (Type1=2 and type2=3) then
; RELATION1 = Extra Opcode
; ON (Exact first=1) And (Type1=1 and type2=3) then
; override is size (special)
; Both stored in BP (Back side)

;override info (As neccisary)
;will be more then 0 on these following circomstances
; REG REG
; 1(reverse these values)
; 2(third value, number.. use as needed)
; REG IMM (real size to save 1-4)
; REG MEM 2(Third value, Number.. Use as needed)
;         3(compare distance number)
; MEM REG 2(Third value, Number.. Use as needed)
;         3(compare distance number)
; MEM IMM (real size to save 1-4)


;exacts re-arranged for best size

;/beyond
;system familiar (which computer type)
;size of instructions

just2:db 3,"ADC"
;alphabeticl order.
db 20
;mem Imm (SIGNED)
db 1 00 0 0 000b, 0000 0010b, 10 11 01 00b
db 1,1,80h
;Regular
db 1 01 0 0 000b, 0000 0010b, 10 11 10 00b
db 1,1,81h
db 1 10 0 0 000b, 0000 0010b, 10 11 11 00b
db 3,1,81h

;regs regs
db 1 00 0 0 000b, 0001 0001b, 01 01 00 00b
db 0,1,12h
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 1,1,13h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,1,13h

;regs mem 
db 1 00 0 0 000b, 0001 0000b, 01 10 00 00b
db 0,1,12h
db 1 01 0 0 000b, 0010 0000b, 01 10 00 00b
db 1,1,13h
db 1 10 0 0 000b, 0011 0000b, 01 10 00 00b
db 3,1,13h

;mem regs
db 1 00 0 0 000b, 0000 0001b, 10 01 00 00b
db 0,1,10h
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 1,1,11h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,1,11h

;Reg Imm (Exacts) second ones
;Regs Imm
db 1 00 0 0 000b, 0001 0010b, 01 11 01 00b
db 0,1,80h
db 1 00 1 0 000b, 0001 0000b, 01 11 01 01b
db 1,2,14h,10h
;first are sign extends (8 bit imm)
db 1 01 0 0 000b, 0010 0010b, 01 11 01 00b
db 1,1,83h
db 1 01 1 0 000b, 0010 0000b, 01 11 10 10b
db 2,2,15h,20h
db 1 01 0 0 000b, 0010 0010b, 01 11 10 00b
db 1,1,81h
db 1 10 0 0 000b, 0011 0010b, 01 11 01 00b
db 3,1,83h
db 1 10 1 0 000b, 0011 0000b, 01 11 11 11b
db 3,2,15h,30h
db 1 10 0 0 000b, 0011 0010b, 01 11 11 00b
db 3,1,81h

;k, from here on.. :)
db 3,"ADD"
db 20
;mem Imm (SIGNED)
db 1 00 0 0 000b, 0000 0000b, 10 11 01 00b
db 1,1,80h
;Regular
db 1 01 0 0 000b, 0000 0000b, 10 11 10 00b
db 1,1,81h
db 1 10 0 0 000b, 0000 0000b, 10 11 11 00b
db 3,1,81h

;regs regs
db 1 00 0 0 000b, 0001 0001b, 01 01 00 00b
db 0,1,02h
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 1,1,03h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,1,03h

;regs mem 
db 1 00 0 0 000b, 0001 0000b, 01 10 00 00b
db 0,1,02h
db 1 01 0 0 000b, 0010 0000b, 01 10 00 00b
db 1,1,03h
db 1 10 0 0 000b, 0011 0000b, 01 10 00 00b
db 3,1,03h

;mem regs
db 1 00 0 0 000b, 0000 0001b, 10 01 00 00b
db 0,1,00h
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 1,1,01h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,1,01h

;Reg Imm (Exacts)
;Regs Imm
db 1 00 1 0 000b, 0001 0000b, 01 11 01 01b
db 1,2,04h,10h
db 1 00 0 0 000b, 0001 0000b, 01 11 01 00b
db 0,1,80h
;first are sign extends (8 bit imm)
db 1 01 0 0 000b, 0010 0000b, 01 11 01 00b
db 1,1,83h
db 1 01 1 0 000b, 0010 0000b, 01 11 10 10b
db 2,2,05h,20h
db 1 01 0 0 000b, 0010 0000b, 01 11 10 00b
db 1,1,81h

db 1 10 0 0 000b, 0011 0000b, 01 11 01 00b
db 3,1,83h
db 1 10 1 0 000b, 0011 0000b, 01 11 11 11b
db 3,2,05h,30h
db 1 10 0 0 000b, 0011 0000b, 01 11 11 00b
db 3,1,81h

db 3,"AND"
db 20
;mem Imm (SIGNED)
db 1 00 0 0 000b, 0000 0100b, 10 11 01 00b
db 1,1,80h
;Regular
db 1 01 0 0 000b, 0000 0100b, 10 11 10 00b
db 1,1,81h
db 1 10 0 0 000b, 0000 0100b, 10 11 11 00b
db 3,1,81h

;regs regs
db 1 00 0 0 000b, 0001 0001b, 01 01 00 00b
db 0,1,22h
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 1,1,23h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,1,23h

;regs mem 
db 1 00 0 0 000b, 0001 0000b, 01 10 00 00b
db 0,1,22h
db 1 01 0 0 000b, 0010 0000b, 01 10 00 00b
db 1,1,23h
db 1 10 0 0 000b, 0011 0000b, 01 10 00 00b
db 3,1,23h

;mem regs
db 1 00 0 0 000b, 0000 0001b, 10 01 00 00b
db 0,1,20h
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 1,1,21h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,1,21h

;Reg Imm (Exacts)
;Regs Imm
db 1 00 1 0 000b, 0001 0000b, 01 11 01 01b
db 1,2,24h,10h
db 1 00 0 0 000b, 0001 0100b, 01 11 01 00b
db 0,1,80h
;first are sign extends (8 bit imm)
db 1 01 0 0 000b, 0010 0100b, 01 11 01 00b
db 1,1,83h
db 1 01 1 0 000b, 0010 0000b, 01 11 10 10b
db 2,2,25h,20h
db 1 01 0 0 000b, 0010 0100b, 01 11 10 00b
db 1,1,81h

db 1 10 0 0 000b, 0011 0100b, 01 11 01 00b
db 3,1,83h
db 1 10 1 0 000b, 0011 0000b, 01 11 11 11b
db 3,2,25h,30h
db 1 10 0 0 000b, 0011 0100b, 01 11 11 00b
db 3,1,81h

;I am not sure about this one, but it's in the book.
db 4,"ARPL"
db 3
db 1 00 0 0 000b, 0010 0010b, 01 01 00 01b;override switches two around
db 3,1,63h
db 1 00 0 0 000b, 0010 0000b, 01 10 00 00b
db 3,1,63h
db 1 00 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,1,63h

;UNTIL I KNOW
;db 5,"BOUND"
;db 1
;db 1 00 0 0 000b, 0000 0000b, 00 00 00 00b
;db 0,0,62h

db 3,"BSF"
db 4
;16 bits, then mem
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,0BCh
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,0BCh
;32 bits, then mem
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,0BCh
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,0BCh

db 3,"BSR"
db 4
;16 bits, then mem
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,0BDh
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,0BDh
;32 bits, then mem
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,0BDh
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,0BDh

db 2,"BT"
db 8
;mem imm
db 1 01 0 0 000b, 0000 0100b, 10 11 01 00b
db 3,2,0Fh,0BAh
db 1 10 0 0 000b, 0000 0100b, 10 11 01 00b
db 3,2,0Fh,0BAh
;reg reg
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,0A3h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,0A3h
;mem reg
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,0A3h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,0A3h
;reg imm
db 1 01 0 0 000b, 0010 0100b, 01 11 01 00b
db 3,2,0Fh,0BAh
db 1 10 0 0 000b, 0011 0100b, 01 11 01 00b
db 3,2,0Fh,0BAh

db 3,"BTC"
db 8
;mem imm
db 1 01 0 0 000b, 0000 0111b, 10 11 01 00b
db 3,2,0Fh,0BAh
db 1 10 0 0 000b, 0000 0111b, 10 11 01 00b
db 3,2,0Fh,0BAh
;reg reg
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,0BBh
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,0BBh
;mem reg
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,0BBh
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,0BBh
;reg imm
db 1 01 0 0 000b, 0010 0111b, 01 11 01 00b
db 3,2,0Fh,0BAh
db 1 10 0 0 000b, 0011 0111b, 01 11 01 00b
db 3,2,0Fh,0BAh

db 3,"BTR"
db 8
;mem imm
db 1 01 0 0 000b, 0000 0110b, 10 11 01 00b
db 3,2,0Fh,0BAh
db 1 10 0 0 000b, 0000 0110b, 10 11 01 00b
db 3,2,0Fh,0BAh
;reg reg
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,0B3h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,0B3h
;mem reg
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,0B3h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,0B3h
;reg imm
db 1 01 0 0 000b, 0010 0110b, 01 11 01 00b
db 3,2,0Fh,0BAh
db 1 10 0 0 000b, 0011 0110b, 01 11 01 00b
db 3,2,0Fh,0BAh

db 3,"BTS"
db 8
;mem imm
db 1 01 0 0 000b, 0000 0101b, 10 11 01 00b
db 3,2,0Fh,0BAh
db 1 10 0 0 000b, 0000 0101b, 10 11 01 00b
db 3,2,0Fh,0BAh
;reg reg
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,0ABh
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,0ABh
;mem reg
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,0ABh
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,0ABh
;reg imm
db 1 01 0 0 000b, 0010 0101b, 01 11 01 00b
db 3,2,0Fh,0BAh
db 1 10 0 0 000b, 0011 0101b, 01 11 01 00b
db 3,2,0Fh,0BAh

;CMOVcc
;0Fh, 4?h
db 5,"CMOVO"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,40h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,40h
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,40h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,40h

db 6,"CMOVNO"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,41h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,41h
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,41h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,41h

db 5,"CMOVB"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,42h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,42h
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,42h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,42h

db 7,"CMOVNAE"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,42h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,42h
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,42h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,42h

db 5,"CMOVC"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,42h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,42h
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,42h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,42h

db 6,"CMOVNB"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,43h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,43h
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,43h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,43h

db 6,"CMOVAE"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,43h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,43h
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,43h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,43h

db 6,"CMOVNC"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,43h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,43h
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,43h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,43h

db 5,"CMOVE"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,44h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,44h
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,44h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,44h

db 5,"CMOVZ"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,45h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,45h
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,45h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,45h

db 6,"CMOVNE"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,45h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,45h
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,45h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,45h

db 6,"CMOVNZ"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,45h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,45h
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,45h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,45h

db 6,"CMOVBE"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,46h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,46h
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,46h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,46h

db 6,"CMOVNA"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,46h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,46h
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,46h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,46h

db 7,"CMOVNBE"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,47h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,47h
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,47h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,47h

db 5,"CMOVA"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,47h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,47h
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,47h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,47h

db 5,"CMOVS"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,48h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,48h
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,48h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,48h

db 6,"CMOVNS"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,49h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,49h
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,49h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,49h

db 5,"CMOVP"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,4Ah
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,4Ah
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,4Ah
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,4Ah

db 6,"CMOVPE"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,4Ah
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,4Ah
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,4Ah
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,4Ah

db 6,"CMOVNP"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,4Bh
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,4Bh
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,4Bh
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,4Bh

db 6,"CMOVPO"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,4Bh
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,4Bh
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,4Bh
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,4Bh

db 5,"CMOVL"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,4Ch
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,4Ch
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,4Ch
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,4Ch

db 7,"CMOVNGE"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,4Ch
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,4Ch
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,4Ch
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,4Ch

db 6,"CMOVNL"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,4Dh
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,4Dh
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,4Dh
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,4Dh

db 6,"CMOVGE"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,4Dh
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,4Dh
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,4Dh
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,4Dh

db 6,"CMOVLE"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,4Eh
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,4Eh
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,4Eh
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,4Eh

db 6,"CMOVNG"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,4Eh
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,4Eh
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,4Eh
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,4Eh

db 7,"CMOVNLE"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,4Fh
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,4Fh
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,4Fh
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,4Fh

db 5,"CMOVG"
db 4
;REG REG
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,4Fh
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,4Fh
;MEM REG
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,4Fh
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,4Fh


db 3,"CMP"
db 20
;mem Imm (SIGNED)
db 1 00 0 0 000b, 0000 0111b, 10 11 01 00b
db 1,1,80h
;Regular
db 1 01 0 0 000b, 0000 0111b, 10 11 10 00b
db 1,1,81h
db 1 10 0 0 000b, 0000 0111b, 10 11 11 00b
db 3,1,81h

;regs regs
db 1 00 0 0 000b, 0001 0001b, 01 01 00 00b
db 0,1,3Ah
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 1,1,3Bh
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,1,3Bh

;regs mem 
db 1 00 0 0 000b, 0001 0000b, 01 10 00 00b
db 0,1,3Ah
db 1 01 0 0 000b, 0010 0000b, 01 10 00 00b
db 1,1,3Bh
db 1 10 0 0 000b, 0011 0000b, 01 10 00 00b
db 3,1,3Bh

;mem regs
db 1 00 0 0 000b, 0000 0001b, 10 01 00 00b
db 0,1,38h
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 1,1,39h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,1,39h

;Reg Imm (Exacts)
;Regs Imm
db 1 00 1 0 000b, 0001 0000b, 01 11 01 01b
db 1,2,3Ch,10h
db 1 00 0 0 000b, 0001 0111b, 01 11 01 00b
db 0,1,80h
;first are sign extends (8 bit imm)
db 1 01 0 0 000b, 0010 0111b, 01 11 01 00b
db 1,1,83h
db 1 01 1 0 000b, 0010 0000b, 01 11 10 10b
db 2,2,3Dh,20h
db 1 01 0 0 000b, 0010 0111b, 01 11 10 00b
db 1,1,81h

db 1 10 0 0 000b, 0011 0111b, 01 11 01 00b
db 3,1,83h
db 1 10 1 0 000b, 0011 0000b, 01 11 11 11b
db 3,2,3Dh,30h
db 1 10 0 0 000b, 0011 0111b, 01 11 11 00b
db 3,1,81h

db 7,"CMPXCHG"
db 6
;REG REG
db 1 00 0 0 000b, 0001 0001b, 01 01 00 00b
db 3,2,0Fh,0B0h
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,0B1h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,0B1h
;MEM REG
db 1 00 0 0 000b, 0000 0001b, 01 01 00 00b
db 3,2,0Fh,0B0h
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,0B1h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,0B1h

;ODD ONE..
;EDX:EAX Compared
;if equal, MEM = ECX:EBX
;if not    EDX:EAX = MEM
db 9,"CMPXCHG8B"
db 1
;64 Bit means nothing.. saying it's 8
db 1 00 0 0 000b, 0000 0011b, 10 01 00 00b
db 6,2,0Fh,0C7h

db 5,"ENTER"
db 1
db 1 00 1 0 000b, 0000 0000b, 10 11 01 00b
db 3,1,0C8h

db 2,"IN"
db 6
;8 bit in
db 1 00 1 0 000b, 0001 0000b, 01 11 01 00b
db 1,2,0E4h,10h
db 1 01 1 0 000b, 0010 0000b, 01 11 01 00b
db 1,2,0E5h,20h
db 1 10 1 0 000b, 0011 0000b, 01 11 01 00b
db 3,2,0E5h,30h
;DX in
db 1 00 1 1 000b, 0001 0010b, 01 01 00 00b
db 1,3,0ECh,10h,22h
db 1 01 1 1 000b, 0010 0010b, 01 01 00 00b
db 1,3,0EDh,20h,22h
db 1 10 1 1 000b, 0011 0010b, 01 01 00 00b
db 3,3,0EDh,30h,22h

db 3,"LAR"
db 2
db 1 00 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,02h
db 1 00 0 0 000b, 0010 0010b, 01 10 00 00b
db 3,2,0Fh,02h

;Lsegments are REG MEM's
db 3,"LDS"
db 1
db 1 00 0 0 000b, 0010 0000b, 01 10 00 00b
db 1,1,0C5h

db 3,"LEA"
db 1
db 1 00 0 0 000b, 0010 0000b, 01 10 00 00b
db 1,1,8Dh

db 3,"LES"
db 1
db 1 00 0 0 000b, 0010 0000b, 01 10 00 00b
db 1,1,0C4h

db 3,"LFS"
db 1
db 1 00 0 0 000b, 0010 0000b, 01 10 00 00b
db 3,2,0Fh,0B4h

db 3,"LGS"
db 1
db 1 00 0 0 000b, 0010 0000b, 01 10 00 00b
db 3,2,0Fh,0B5h

db 3,"LSS"
db 1
db 1 00 0 0 000b, 0010 0000b, 01 10 00 00b
db 3,2,0Fh,0B2h

db 3,"LSL"
db 2
db 1 00 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,03h
db 1 00 0 0 000b, 0010 0000b, 01 10 00 00b
db 3,2,0Fh,03h

db 3,"MOV"
;db 99
db 105
;Mem Imm
db 1 00 0 0 000b, 0000 0000b, 10 11 01 00b
db 1,1,0C6h
db 1 01 0 0 000b, 0000 0000b, 10 11 10 00b
db 1,1,0C7h
db 1 10 0 0 000b, 0000 0000b, 10 11 11 00b
db 3,1,0C7h

;;special mem exacts (special)
;;mem reg (EAX)
;db 1 00 1 1 000b, 0000 0000b, 00 00 00 00b
;db 0,0,0h
;db 1 01 1 1 000b, 0000 0000b, 00 00 00 00b
;db 0,0,0h
;db 1 10 1 1 000b, 0000 0000b, 00 00 00 00b
;db 0,0,0h

;;reg mem (EAX)
;db 1 00 0 0 000b, 0000 0000b, 00 00 00 00b
;db 0,0,0h
;db 1 00 0 0 000b, 0000 0000b, 00 00 00 00b
;db 0,0,0h
;db 1 00 0 0 000b, 0000 0000b, 00 00 00 00b
;db 0,0,0h


;resuming normal functions
;regs regs
db 1 00 0 0 000b, 0001 0001b, 01 01 00 00b
db 0,1,8Ah
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 1,1,8Bh
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,1,8Bh


;assumes special (memory direct)
db 1 00 1 1 000b, 0000 0001b, 10 01 00 00b
db 1,2,0A2h,10h
db 1 01 1 1 000b, 0000 0010b, 10 01 00 00b
db 1,2,0A3h,20h
db 1 10 1 1 000b, 0000 0011b, 10 01 00 00b
db 3,2,0A3h,30h

db 1 00 1 1 000b, 0001 0000b, 01 10 00 00b
db 1,2,0A0h,10h
db 1 01 1 1 000b, 0010 0000b, 01 10 00 00b
db 1,2,0A1h,20h
db 1 10 1 1 000b, 0011 0000b, 01 10 00 00b
db 3,2,0A1h,30h


;regs mem
db 1 00 0 0 000b, 0001 0000b, 01 10 00 00b
db 0,1,8Ah
db 1 01 0 0 000b, 0010 0000b, 01 10 00 00b
db 1,1,8Bh
db 1 10 0 0 000b, 0011 0000b, 01 10 00 00b
db 3,1,8Bh

;mem regs
db 1 00 0 0 000b, 0000 0001b, 10 01 00 00b
db 0,1,88h
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 1,1,89h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,1,89h

;these are the alternate encodings.. Used first :P
;8*3 = 24

;AX
db 1 00 1 0 000b, 0001 0000b, 01 11 01 01b
db 1,2,0B0h,10h
db 1 01 1 0 000b, 0010 0000b, 01 11 10 10b
db 1,2,0B8h,20h
db 1 10 1 0 000b, 0011 0000b, 01 11 11 11b
db 3,2,0B8h,30h
;CX
db 1 00 1 0 000b, 0001 0000b, 01 11 01 01b
db 1,2,0B1h,11h
db 1 01 1 0 000b, 0010 0000b, 01 11 10 10b
db 1,2,0B9h,21h
db 1 10 1 0 000b, 0011 0000b, 01 11 11 11b
db 3,2,0B9h,31h
;DX
db 1 00 1 0 000b, 0001 0000b, 01 11 01 01b
db 1,2,0B2h,12h
db 1 01 1 0 000b, 0010 0000b, 01 11 10 10b
db 1,2,0BAh,22h
db 1 10 1 0 000b, 0011 0000b, 01 11 11 11b
db 3,2,0BAh,32h
;BX
db 1 00 1 0 000b, 0001 0000b, 01 11 01 01b
db 1,2,0B3h,13h
db 1 01 1 0 000b, 0010 0000b, 01 11 10 10b
db 1,2,0BBh,23h
db 1 10 1 0 000b, 0011 0000b, 01 11 11 11b
db 3,2,0BBh,33h
;SP
db 1 00 1 0 000b, 0001 0000b, 01 11 01 01b
db 1,2,0B4h,14h
db 1 01 1 0 000b, 0010 0000b, 01 11 10 10b
db 1,2,0BCh,24h
db 1 10 1 0 000b, 0011 0000b, 01 11 11 11b
db 3,2,0BCh,34h
;BP
db 1 00 1 0 000b, 0001 0000b, 01 11 01 01b
db 1,2,0B5h,15h
db 1 01 1 0 000b, 0010 0000b, 01 11 10 10b
db 1,2,0BDh,25h
db 1 10 1 0 000b, 0011 0000b, 01 11 11 11b
db 3,2,0BDh,35h
;SI
db 1 00 1 0 000b, 0001 0000b, 01 11 01 01b
db 1,2,0B6h,16h
db 1 01 1 0 000b, 0010 0000b, 01 11 10 10b
db 1,2,0BEh,26h
db 1 10 1 0 000b, 0011 0000b, 01 11 11 11b
db 3,2,0BEh,36h
;DI
db 1 00 1 0 000b, 0001 0000b, 01 11 01 01b
db 1,2,0B7h,17h
db 1 01 1 0 000b, 0010 0000b, 01 11 10 10b
db 1,2,0BFh,27h
db 1 10 1 0 000b, 0011 0000b, 01 11 11 11b
db 3,2,0BFh,37h

;;Regs Imm
;db 1 00 0 0 000b, 0001 0000b, 01 11 01 00b
;db 0,1,0C6h
;db 1 01 0 0 000b, 0010 0000b, 01 11 10 00b
;db 1,1,0C7h
;db 1 10 0 0 000b, 0011 0000b, 01 11 11 00b
;db 3,1,0C7h

;Special : Controls
;CR?, E?X
db 1 00 0 0 000b, 0101 0011b, 01 01 00 00b
db 3,2,0Fh,022h
;E?X, CR?
db 1 00 0 0 000b, 0011 0101b, 01 01 00 00b
db 3,2,0Fh,020h
;DR?, E?X
db 1 00 0 0 000b, 0110 0011b, 01 01 00 00b
db 3,2,0Fh,023h
;E?X, DR?
db 1 00 0 0 000b, 0011 0110b, 01 01 00 00b
db 3,2,0Fh,021h

;Sreg Reg
db 1 00 0 0 000b, 0100 0010b, 01 01 00 00b
db 3,1,8Eh
;Reg Sreg
db 1 00 0 0 000b, 0010 0100b, 01 01 00 01b;switch
db 3,1,8Ch
;Mem Sreg
db 1 00 0 0 000b, 0000 0100b, 10 01 00 00b
db 1,1,8Ch

;56 from these sections (8*7)=56
;emulated MOV functions, for sregs to regs (needed?)
;reg,ES
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,06h,58h,20h,40h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,06h,59h,21h,40h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,06h,5Ah,22h,40h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,06h,5Bh,23h,40h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,06h,5Ch,24h,40h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,06h,5Dh,25h,40h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,06h,5Eh,26h,40h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,06h,5Fh,27h,40h

;reg,CS
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,0Eh,58h,20h,41h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,0Eh,59h,21h,41h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,0Eh,5Ah,22h,41h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,0Eh,5Bh,23h,41h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,0Eh,5Ch,24h,41h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,0Eh,5Dh,25h,41h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,0Eh,5Eh,26h,41h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,0Eh,5Fh,27h,41h

;reg,SS
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,16h,58h,20h,42h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,16h,59h,21h,42h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,16h,5Ah,22h,42h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,16h,5Bh,23h,42h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,16h,5Ch,24h,42h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,16h,5Dh,25h,42h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,16h,5Eh,26h,42h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,16h,5Fh,27h,42h

;reg,DS
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,1Eh,58h,20h,43h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,1Eh,59h,21h,43h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,1Eh,5Ah,22h,43h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,1Eh,5Bh,23h,43h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,1Eh,5Ch,24h,43h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,1Eh,5Dh,25h,43h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,1Eh,5Eh,26h,43h
db 1 01 1 1 000b, 0010 0100b, 01 01 00 00b
db 1,4,1Eh,5Fh,27h,43h


;ES,reg
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,50h,07h,40h,20h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,51h,07h,40h,21h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,52h,07h,40h,22h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,53h,07h,40h,23h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,54h,07h,40h,24h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,55h,07h,40h,25h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,56h,07h,40h,26h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,57h,07h,40h,27h

;SS,reg
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,50h,17h,42h,20h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,51h,17h,42h,21h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,52h,17h,42h,22h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,53h,17h,42h,23h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,54h,17h,42h,24h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,55h,17h,42h,25h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,56h,17h,42h,26h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,57h,17h,42h,27h

;DS,reg
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,50h,1Fh,43h,20h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,51h,1Fh,43h,21h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,52h,1Fh,43h,22h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,53h,1Fh,43h,23h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,54h,1Fh,43h,24h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,55h,1Fh,43h,25h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,56h,1Fh,43h,26h
db 1 01 1 1 000b, 0100 0010b, 01 01 00 00b
db 1,4,57h,1Fh,43h,27h

db 5,"MOVSX"
db 6
;Reg Reg
db 1 01 0 0 000b, 0010 0001b, 01 01 00 00b
db 3,2,0Fh,0BEh
db 1 10 0 0 000b, 0011 0001b, 01 01 00 00b
db 3,2,0Fh,0BEh
db 1 10 0 0 000b, 0011 0010b, 01 01 00 00b
db 3,2,0Fh,0BFh
;Reg Mem
db 1 01 0 0 000b, 0010 0000b, 01 10 01 00b
db 3,2,0Fh,0BEh
db 1 10 0 0 000b, 0011 0000b, 01 10 01 00b
db 3,2,0Fh,0BEh
db 1 10 0 0 000b, 0011 0000b, 01 10 10 00b
db 3,2,0Fh,0BFh

db 5,"MOVZX"
db 6
;Reg Reg
db 1 01 0 0 000b, 0010 0001b, 01 01 00 00b
db 3,2,0Fh,0B6h
db 1 10 0 0 000b, 0011 0001b, 01 01 00 00b
db 3,2,0Fh,0B6h
db 1 10 0 0 000b, 0011 0010b, 01 01 00 00b
db 3,2,0Fh,0B7h
;Reg Mem
db 1 01 0 0 000b, 0010 0000b, 01 10 01 00b
db 3,2,0Fh,0B6h
db 1 10 0 0 000b, 0011 0000b, 01 10 01 00b
db 3,2,0Fh,0B6h
db 1 10 0 0 000b, 0011 0000b, 01 10 10 00b
db 3,2,0Fh,0B7h

db 2,"OR"
db 19
;mem Imm (SIGNED)
db 1 00 0 0 000b, 0000 0001b, 10 11 01 00b
db 1,1,80h
;Regular
db 1 01 0 0 000b, 0000 0001b, 10 11 10 00b
db 1,1,81h
db 1 10 0 0 000b, 0000 0001b, 10 11 11 00b
db 3,1,81h

;regs regs
db 1 00 0 0 000b, 0001 0001b, 01 01 00 00b
db 0,1,0Ah
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 1,1,0Bh
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,1,0Bh

;regs mem 
db 1 00 0 0 000b, 0001 0000b, 01 10 00 00b
db 0,1,0Ah
db 1 01 0 0 000b, 0010 0000b, 01 10 00 00b
db 1,1,0Bh
db 1 10 0 0 000b, 0011 0000b, 01 10 00 00b
db 3,1,0Bh

;mem regs
db 1 00 0 0 000b, 0000 0001b, 10 01 00 00b
db 0,1,08h
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 1,1,09h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,1,09h

;Reg Imm (Exacts)
;Regs Imm
db 1 00 1 0 000b, 0001 0000b, 01 11 01 01b
db 1,2,0Ch,10h
db 1 00 0 0 000b, 0001 0001b, 01 11 01 00b
db 0,1,80h
;first are sign extends (8 bit imm)
db 1 01 0 0 000b, 0010 0001b, 01 11 01 00b
db 1,1,83h
db 1 01 1 0 000b, 0010 0000b, 01 11 10 10b
db 2,2,0Dh,20h
db 1 01 0 0 000b, 0010 0001b, 01 11 10 00b
db 1,1,81h

;db 1 10 0 0 000b, 0011 0001b, 01 11 01 00b
;db 3,1,83h
db 1 10 1 0 000b, 0011 0000b, 01 11 11 11b
db 3,2,0Dh,30h
db 1 10 0 0 000b, 0011 0001b, 01 11 11 00b
db 3,1,81h

db 3,"OUT"
db 6
;8 bit in
db 1 00 1 0 000b, 0001 0000b, 01 11 01 00b
db 1,2,0E6h,10h
db 1 01 1 0 000b, 0010 0000b, 01 11 01 00b
db 1,2,0E7h,20h
db 1 10 1 0 000b, 0011 0000b, 01 11 01 00b
db 3,2,0E7h,30h
;DX in
db 1 00 1 1 000b, 0010 0001b, 01 01 00 00b
db 1,3,0EEh,22h,10h
db 1 01 1 1 000b, 0010 0010b, 01 01 00 00b
db 1,3,0EFh,22h,20h
db 1 10 1 1 000b, 0010 0011b, 01 01 00 00b
db 3,3,0EFh,22h,30h

;resume here
db 3,"RCL"
db 18
;Reg, 1
db 1 00 0 1 000b, 0001 0010b, 01 11 01 00b
db 1,2,0D0h,1
db 1 01 0 1 000b, 0010 0010b, 01 11 01 00b
db 1,2,0D1h,1
db 1 10 0 1 000b, 0011 0010b, 01 11 01 00b
db 1,2,0D1h,1
;Mem, 1
db 1 00 0 1 000b, 0000 0010b, 10 11 01 00b
db 1,2,0D0h,1
db 1 01 0 1 000b, 0000 0010b, 10 11 10 00b
db 1,3,0D1h,1,0
db 1 10 0 1 000b, 0000 0010b, 10 11 11 00b
db 1,5,0D1h,1,0,0,0
;Reg, cl
db 1 00 0 1 000b, 0001 0001b, 01 01 0010b      ;special
db 1,2,0D2h,11h
db 1 01 0 1 000b, 0010 0001b, 01 01 0010b
db 1,2,0D3h,11h
db 1 10 0 1 000b, 0011 0001b, 01 01 0010b
db 1,2,0D3h,11h
;Mem, cl
db 1 00 0 1 000b, 0010 0001b, 10 01 01 00b
db 1,2,0D2h,11h
db 1 01 0 1 000b, 0010 0001b, 10 01 10 00b
db 1,2,0D3h,11h
db 1 10 0 1 000b, 0010 0001b, 10 01 11 00b
db 1,2,0D3h,11h
;Reg, imm
db 1 00 0 0 000b, 0001 0010b, 01 11 01 00b
db 3,1,0C0h
db 1 01 0 0 000b, 0010 0010b, 01 11 01 00b
db 3,1,0C1h
db 1 10 0 0 000b, 0011 0010b, 01 11 01 00b
db 3,1,0C1h
;Mem, imm
;Be sure to type in what size first :)
db 1 00 0 0 000b, 0000 0010b, 10 11 01 01b      ;special
db 3,1,0C0h
db 1 01 0 0 000b, 0000 0010b, 10 11 10 01b
db 3,1,0C1h
db 1 10 0 0 000b, 0000 0010b, 10 11 11 01b
db 3,1,0C1h

db 3,"RCR"
db 18
;Reg, 1
db 1 00 0 1 000b, 0001 0011b, 01 11 01 00b
db 1,2,0D0h,1
db 1 01 0 1 000b, 0010 0011b, 01 11 01 00b
db 1,2,0D1h,1
db 1 10 0 1 000b, 0011 0011b, 01 11 01 00b
db 1,2,0D1h,1
;Mem, 1
db 1 00 0 1 000b, 0000 0011b, 10 11 01 00b
db 1,2,0D0h,1
db 1 01 0 1 000b, 0000 0011b, 10 11 10 00b
db 1,3,0D1h,1,0
db 1 10 0 1 000b, 0000 0011b, 10 11 11 00b
db 1,5,0D1h,1,0,0,0
;Reg, cl
db 1 00 0 1 000b, 0001 0001b, 01 01 0011b      ;special
db 1,2,0D2h,11h
db 1 01 0 1 000b, 0010 0001b, 01 01 0011b
db 1,2,0D3h,11h
db 1 10 0 1 000b, 0011 0001b, 01 01 0011b
db 1,2,0D3h,11h
;Mem, cl
db 1 00 0 1 000b, 0011 0001b, 10 01 01 00b
db 1,2,0D2h,11h
db 1 01 0 1 000b, 0011 0001b, 10 01 10 00b
db 1,2,0D3h,11h
db 1 10 0 1 000b, 0011 0001b, 10 01 11 00b
db 1,2,0D3h,11h
;Reg, imm
db 1 00 0 0 000b, 0001 0011b, 01 11 01 00b
db 3,1,0C0h
db 1 01 0 0 000b, 0010 0011b, 01 11 01 00b
db 3,1,0C1h
db 1 10 0 0 000b, 0011 0011b, 01 11 01 00b
db 3,1,0C1h
;Mem, imm
;Be sure to type in what size first :)
db 1 00 0 0 000b, 0000 0011b, 10 11 01 01b      ;special
db 3,1,0C0h
db 1 01 0 0 000b, 0000 0011b, 10 11 10 01b
db 3,1,0C1h
db 1 10 0 0 000b, 0000 0011b, 10 11 11 01b
db 3,1,0C1h

db 3,"ROL"
db 18
;Reg, 1
db 1 00 0 1 000b, 0001 0000b, 01 11 01 00b
db 1,2,0D0h,1
db 1 01 0 1 000b, 0010 0000b, 01 11 01 00b
db 1,2,0D1h,1
db 1 10 0 1 000b, 0011 0000b, 01 11 01 00b
db 1,2,0D1h,1
;Mem, 1
db 1 00 0 1 000b, 0000 0000b, 10 11 01 00b
db 1,2,0D0h,1
db 1 01 0 1 000b, 0000 0000b, 10 11 10 00b
db 1,3,0D1h,1,0
db 1 10 0 1 000b, 0000 0000b, 10 11 11 00b
db 1,5,0D1h,1,0,0,0
;Reg, cl
db 1 00 0 1 000b, 0001 0001b, 01 01 0000b      ;special
db 1,2,0D2h,11h
db 1 01 0 1 000b, 0010 0001b, 01 01 0000b
db 1,2,0D3h,11h
db 1 10 0 1 000b, 0011 0001b, 01 01 0000b
db 1,2,0D3h,11h
;Mem, cl
db 1 00 0 1 000b, 0000 0001b, 10 01 01 00b
db 1,2,0D2h,11h
db 1 01 0 1 000b, 0000 0001b, 10 01 10 00b
db 1,2,0D3h,11h
db 1 10 0 1 000b, 0000 0001b, 10 01 11 00b
db 1,2,0D3h,11h
;Reg, imm
db 1 00 0 0 000b, 0001 0000b, 01 11 01 00b
db 3,1,0C0h
db 1 01 0 0 000b, 0010 0000b, 01 11 01 00b
db 3,1,0C1h
db 1 10 0 0 000b, 0011 0000b, 01 11 01 00b
db 3,1,0C1h
;Mem, imm
;Be sure to type in what size first :)
db 1 00 0 0 000b, 0000 0000b, 10 11 01 01b      ;special
db 3,1,0C0h
db 1 01 0 0 000b, 0000 0000b, 10 11 10 01b
db 3,1,0C1h
db 1 10 0 0 000b, 0000 0000b, 10 11 11 01b
db 3,1,0C1h

db 3,"ROR"
db 18
;Reg, 1
db 1 00 0 1 000b, 0001 0001b, 01 11 01 00b
db 1,2,0D0h,1
db 1 01 0 1 000b, 0010 0001b, 01 11 01 00b
db 1,2,0D1h,1
db 1 10 0 1 000b, 0011 0001b, 01 11 01 00b
db 1,2,0D1h,1
;Mem, 1
db 1 00 0 1 000b, 0000 0001b, 10 11 01 00b
db 1,2,0D0h,1
db 1 01 0 1 000b, 0000 0001b, 10 11 10 00b
db 1,3,0D1h,1,0
db 1 10 0 1 000b, 0000 0001b, 10 11 11 00b
db 1,5,0D1h,1,0,0,0
;Reg, cl
db 1 00 0 1 000b, 0001 0001b, 01 01 0001b      ;special
db 1,2,0D2h,11h
db 1 01 0 1 000b, 0010 0001b, 01 01 0001b
db 1,2,0D3h,11h
db 1 10 0 1 000b, 0011 0001b, 01 01 0001b
db 1,2,0D3h,11h
;Mem, cl
db 1 00 0 1 000b, 0001 0001b, 10 01 01 00b
db 1,2,0D2h,11h
db 1 01 0 1 000b, 0001 0001b, 10 01 10 00b
db 1,2,0D3h,11h
db 1 10 0 1 000b, 0001 0001b, 10 01 11 00b
db 1,2,0D3h,11h
;Reg, imm
db 1 00 0 0 000b, 0001 0001b, 01 11 01 00b
db 3,1,0C0h
db 1 01 0 0 000b, 0010 0001b, 01 11 01 00b
db 3,1,0C1h
db 1 10 0 0 000b, 0011 0001b, 01 11 01 00b
db 3,1,0C1h
;Mem, imm
;Be sure to type in what size first :)
db 1 00 0 0 000b, 0000 0001b, 10 11 01 01b      ;special
db 3,1,0C0h
db 1 01 0 0 000b, 0000 0001b, 10 11 10 01b
db 3,1,0C1h
db 1 10 0 0 000b, 0000 0001b, 10 11 11 01b
db 3,1,0C1h

;SAME AS SHL
db 3,"SAL"
db 18
;Reg, 1
db 1 00 0 1 000b, 0001 0100b, 01 11 01 00b
db 1,2,0D0h,1
db 1 01 0 1 000b, 0010 0100b, 01 11 01 00b
db 1,2,0D1h,1
db 1 10 0 1 000b, 0011 0100b, 01 11 01 00b
db 1,2,0D1h,1
;Mem, 1
db 1 00 0 1 000b, 0000 0100b, 10 11 01 00b
db 1,2,0D0h,1
db 1 01 0 1 000b, 0000 0100b, 10 11 10 00b
db 1,3,0D1h,1,0
db 1 10 0 1 000b, 0000 0100b, 10 11 11 00b
db 1,5,0D1h,1,0,0,0
;Reg, cl
db 1 00 0 1 000b, 0001 0001b, 01 01 0100b      ;special
db 1,2,0D2h,11h
db 1 01 0 1 000b, 0010 0001b, 01 01 0100b
db 1,2,0D3h,11h
db 1 10 0 1 000b, 0011 0001b, 01 01 0100b
db 1,2,0D3h,11h
;Mem, cl
db 1 00 0 1 000b, 0100 0001b, 10 01 01 00b
db 1,2,0D2h,11h
db 1 01 0 1 000b, 0100 0001b, 10 01 10 00b
db 1,2,0D3h,11h
db 1 10 0 1 000b, 0100 0001b, 10 01 11 00b
db 1,2,0D3h,11h
;Reg, imm
db 1 00 0 0 000b, 0001 0100b, 01 11 01 00b
db 3,1,0C0h
db 1 01 0 0 000b, 0010 0100b, 01 11 01 00b
db 3,1,0C1h
db 1 10 0 0 000b, 0011 0100b, 01 11 01 00b
db 3,1,0C1h
;Mem, imm
;Be sure to type in what size first :)
db 1 00 0 0 000b, 0000 0100b, 10 11 01 01b      ;special
db 3,1,0C0h
db 1 01 0 0 000b, 0000 0100b, 10 11 10 01b
db 3,1,0C1h
db 1 10 0 0 000b, 0000 0100b, 10 11 11 01b
db 3,1,0C1h

db 3,"SAR"
db 18
;Reg, 1
db 1 00 0 1 000b, 0001 0111b, 01 11 01 00b
db 1,2,0D0h,1
db 1 01 0 1 000b, 0010 0111b, 01 11 01 00b
db 1,2,0D1h,1
db 1 10 0 1 000b, 0011 0111b, 01 11 01 00b
db 1,2,0D1h,1
;Mem, 1
db 1 00 0 1 000b, 0000 0111b, 10 11 01 00b
db 1,2,0D0h,1
db 1 01 0 1 000b, 0000 0111b, 10 11 10 00b
db 1,3,0D1h,1,0
db 1 10 0 1 000b, 0000 0111b, 10 11 11 00b
db 1,5,0D1h,1,0,0,0
;Reg, cl
db 1 00 0 1 000b, 0001 0001b, 01 01 0111b      ;special
db 1,2,0D2h,11h
db 1 01 0 1 000b, 0010 0001b, 01 01 0111b
db 1,2,0D3h,11h
db 1 10 0 1 000b, 0011 0001b, 01 01 0111b
db 1,2,0D3h,11h
;Mem, cl
db 1 00 0 1 000b, 0111 0001b, 10 01 01 00b
db 1,2,0D2h,11h
db 1 01 0 1 000b, 0111 0001b, 10 01 10 00b
db 1,2,0D3h,11h
db 1 10 0 1 000b, 0111 0001b, 10 01 11 00b
db 1,2,0D3h,11h
;Reg, imm
db 1 00 0 0 000b, 0001 0111b, 01 11 01 00b
db 3,1,0C0h
db 1 01 0 0 000b, 0010 0111b, 01 11 01 00b
db 3,1,0C1h
db 1 10 0 0 000b, 0011 0111b, 01 11 01 00b
db 3,1,0C1h
;Mem, imm
;Be sure to type in what size first :)
db 1 00 0 0 000b, 0000 0111b, 10 11 01 01b      ;special
db 3,1,0C0h
db 1 01 0 0 000b, 0000 0111b, 10 11 10 01b
db 3,1,0C1h
db 1 10 0 0 000b, 0000 0111b, 10 11 11 01b
db 3,1,0C1h

db 3,"SHL"
db 18
;Reg, 1
db 1 00 0 1 000b, 0001 0100b, 01 11 01 00b
db 1,2,0D0h,1
db 1 01 0 1 000b, 0010 0100b, 01 11 01 00b
db 1,2,0D1h,1
db 1 10 0 1 000b, 0011 0100b, 01 11 01 00b
db 1,2,0D1h,1
;Mem, 1
db 1 00 0 1 000b, 0000 0100b, 10 11 01 00b
db 1,2,0D0h,1
db 1 01 0 1 000b, 0000 0100b, 10 11 10 00b
db 1,3,0D1h,1,0
db 1 10 0 1 000b, 0000 0100b, 10 11 11 00b
db 1,5,0D1h,1,0,0,0
;Reg, cl
db 1 00 0 1 000b, 0001 0001b, 01 01 0100b      ;special
db 1,2,0D2h,11h
db 1 01 0 1 000b, 0010 0001b, 01 01 0100b
db 1,2,0D3h,11h
db 1 10 0 1 000b, 0011 0001b, 01 01 0100b
db 1,2,0D3h,11h
;Mem, cl
db 1 00 0 1 000b, 0100 0001b, 10 01 01 00b
db 1,2,0D2h,11h
db 1 01 0 1 000b, 0100 0001b, 10 01 10 00b
db 1,2,0D3h,11h
db 1 10 0 1 000b, 0100 0001b, 10 01 11 00b
db 1,2,0D3h,11h
;Reg, imm
db 1 00 0 0 000b, 0001 0100b, 01 11 01 00b
db 3,1,0C0h
db 1 01 0 0 000b, 0010 0100b, 01 11 01 00b
db 3,1,0C1h
db 1 10 0 0 000b, 0011 0100b, 01 11 01 00b
db 3,1,0C1h
;Mem, imm
;Be sure to type in what size first :)
db 1 00 0 0 000b, 0000 0100b, 10 11 01 01b      ;special
db 3,1,0C0h
db 1 01 0 0 000b, 0000 0100b, 10 11 10 01b
db 3,1,0C1h
db 1 10 0 0 000b, 0000 0100b, 10 11 11 01b
db 3,1,0C1h

db 3,"SHR"
db 18
;Reg, 1
db 1 00 0 1 000b, 0001 0101b, 01 11 01 00b
db 1,2,0D0h,1
db 1 01 0 1 000b, 0010 0101b, 01 11 01 00b
db 1,2,0D1h,1
db 1 10 0 1 000b, 0011 0101b, 01 11 01 00b
db 1,2,0D1h,1
;Mem, 1
db 1 00 0 1 000b, 0000 0101b, 10 11 01 00b
db 1,2,0D0h,1
db 1 01 0 1 000b, 0000 0101b, 10 11 10 00b
db 1,3,0D1h,1,0
db 1 10 0 1 000b, 0000 0101b, 10 11 11 00b
db 1,5,0D1h,1,0,0,0
;Reg, cl
db 1 00 0 1 000b, 0001 0001b, 01 01 0101b      ;special
db 1,2,0D2h,11h
db 1 01 0 1 000b, 0010 0001b, 01 01 0101b
db 1,2,0D3h,11h
db 1 10 0 1 000b, 0011 0001b, 01 01 0101b
db 1,2,0D3h,11h
;Mem, cl
db 1 00 0 1 000b, 0101 0001b, 10 01 01 00b
db 1,2,0D2h,11h
db 1 01 0 1 000b, 0101 0001b, 10 01 10 00b
db 1,2,0D3h,11h
db 1 10 0 1 000b, 0101 0001b, 10 01 11 00b
db 1,2,0D3h,11h
;Reg, imm
db 1 00 0 0 000b, 0001 0101b, 01 11 01 00b
db 3,1,0C0h
db 1 01 0 0 000b, 0010 0101b, 01 11 01 00b
db 3,1,0C1h
db 1 10 0 0 000b, 0011 0101b, 01 11 01 00b
db 3,1,0C1h
;Mem, imm
;Be sure to type in what size first :)
db 1 00 0 0 000b, 0000 0101b, 10 11 01 01b      ;special
db 3,1,0C0h
db 1 01 0 0 000b, 0000 0101b, 10 11 10 01b
db 3,1,0C1h
db 1 10 0 0 000b, 0000 0101b, 10 11 11 01b
db 3,1,0C1h

db 3,"SBB"
db 20
;mem Imm (SIGNED)
db 1 00 0 0 000b, 0000 0011b, 10 11 01 00b
db 1,1,80h
;Regular
db 1 01 0 0 000b, 0000 0011b, 10 11 10 00b
db 1,1,81h
db 1 10 0 0 000b, 0000 0011b, 10 11 11 00b
db 3,1,81h

;regs regs
db 1 00 0 0 000b, 0001 0001b, 01 01 00 00b
db 0,1,1Ah
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 1,1,1Bh
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,1,1Bh

;regs mem 
db 1 00 0 0 000b, 0001 0000b, 01 10 00 00b
db 0,1,1Ah
db 1 01 0 0 000b, 0010 0000b, 01 10 00 00b
db 1,1,1Bh
db 1 10 0 0 000b, 0011 0000b, 01 10 00 00b
db 3,1,1Bh

;mem regs
db 1 00 0 0 000b, 0000 0001b, 10 01 00 00b
db 0,1,18h
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 1,1,19h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,1,19h

;Reg Imm (Exacts)
;Regs Imm
db 1 00 1 0 000b, 0001 0000b, 01 11 01 01b
db 1,2,1Ch,10h
db 1 00 0 0 000b, 0001 0011b, 01 11 01 00b
db 0,1,80h
;first are sign extends (8 bit imm)
db 1 01 0 0 000b, 0010 0011b, 01 11 01 00b
db 1,1,83h
db 1 01 1 0 000b, 0010 0000b, 01 11 10 10b
db 2,2,1Dh,20h
db 1 01 0 0 000b, 0010 0011b, 01 11 10 00b
db 1,1,81h

db 1 10 0 0 000b, 0011 0011b, 01 11 01 00b
db 3,1,83h
db 1 10 1 0 000b, 0011 0000b, 01 11 11 11b
db 3,2,1Dh,30h
db 1 10 0 0 000b, 0011 0011b, 01 11 11 00b
db 3,1,81h

db 3,"SUB"
db 20
;mem Imm (SIGNED)
db 1 00 0 0 000b, 0000 0101b, 10 11 01 00b
db 1,1,80h
;Regular
db 1 01 0 0 000b, 0000 0101b, 10 11 10 00b
db 1,1,81h
db 1 10 0 0 000b, 0000 0101b, 10 11 11 00b
db 3,1,81h

;regs regs
db 1 00 0 0 000b, 0001 0001b, 01 01 00 00b
db 0,1,2Ah
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 1,1,2Bh
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,1,2Bh

;regs mem 
db 1 00 0 0 000b, 0001 0000b, 01 10 00 00b
db 0,1,2Ah
db 1 01 0 0 000b, 0010 0000b, 01 10 00 00b
db 1,1,2Bh
db 1 10 0 0 000b, 0011 0000b, 01 10 00 00b
db 3,1,2Bh

;mem regs
db 1 00 0 0 000b, 0000 0001b, 10 01 00 00b
db 0,1,28h
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 1,1,29h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,1,29h

;Reg Imm (Exacts)
;Regs Imm
db 1 00 1 0 000b, 0001 0000b, 01 11 01 01b
db 1,2,2Ch,10h
db 1 00 0 0 000b, 0001 0101b, 01 11 01 00b
db 0,1,80h
;first are sign extends (8 bit imm)
db 1 01 0 0 000b, 0010 0101b, 01 11 01 00b
db 1,1,83h
db 1 01 1 0 000b, 0010 0000b, 01 11 10 10b
db 2,2,2Dh,20h
db 1 01 0 0 000b, 0010 0101b, 01 11 10 00b
db 1,1,81h

db 1 10 0 0 000b, 0011 0101b, 01 11 01 00b
db 3,1,83h
db 1 10 1 0 000b, 0011 0000b, 01 11 11 11b
db 3,2,2Dh,30h
db 1 10 0 0 000b, 0011 0101b, 01 11 11 00b
db 3,1,81h

db 4,"TEST"
db 15
;mem Imm
db 1 00 0 0 000b, 0000 0000b, 10 11 01 00b
db 1,1,0F6h
;Regular
db 1 01 0 0 000b, 0000 0000b, 10 11 10 00b
db 1,1,0F7h
db 1 10 0 0 000b, 0000 0000b, 10 11 11 00b
db 3,1,0F7h

;regs regs
db 1 00 0 0 000b, 0001 0001b, 01 01 00 01b ;switch
db 0,1,84h
db 1 01 0 0 000b, 0010 0010b, 01 01 00 01b
db 1,1,85h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 01b
db 3,1,85h

;mem regs
db 1 00 0 0 000b, 0000 0001b, 10 01 00 00b
db 0,1,84h
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 1,1,85h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,1,85h

;Reg Imm (Exacts)
db 1 00 1 0 000b, 0001 0000b, 01 11 01 01b
db 1,2,0A8h,10h
db 1 01 1 0 000b, 0010 0000b, 01 11 10 10b
db 2,2,0A9h,20h
db 1 10 1 0 000b, 0011 0000b, 01 11 11 11b
db 3,2,0A9h,30h

;Regs Imm
db 1 00 0 0 000b, 0001 0000b, 01 11 01 00b
db 0,1,0F6h
db 1 01 0 0 000b, 0010 0000b, 01 11 10 00b
db 1,1,0F7h
db 1 10 0 0 000b, 0011 0000b, 01 11 11 00b
db 3,1,0F7h

db 4,"XADD"
db 6
;regs regs
db 1 00 0 0 000b, 0001 0001b, 01 01 00 00b
db 3,2,0Fh,0C0h
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 3,2,0Fh,0C1h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,2,0Fh,0C1h

;regs mem 
db 1 00 0 0 000b, 0000 0001b, 10 01 00 00b
db 3,2,0Fh,0C0h
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 3,2,0Fh,0C1h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,2,0Fh,0C1h

db 4,"XCHG"
db 39

;AX, AX ;SAME AS NOP :P
db 1 01 1 1 000b, 0010 0010b, 01 01 00 00b
db 1,3,90h,20h,20h
db 1 10 1 1 000b, 0011 0011b, 01 01 00 00b
db 3,3,90h,30h,30h

;AX, CX
db 1 01 1 1 000b, 0010 0010b, 01 01 00 00b
db 1,3,91h,20h,21h
db 1 10 1 1 000b, 0011 0011b, 01 01 00 00b
db 3,3,91h,30h,31h

db 1 01 1 1 000b, 0010 0010b, 01 01 00 00b
db 1,3,91h,21h,20h
db 1 10 1 1 000b, 0011 0011b, 01 01 00 00b
db 3,3,91h,31h,30h

;AX, DX
db 1 01 1 1 000b, 0010 0010b, 01 01 00 00b
db 1,3,92h,20h,22h
db 1 10 1 1 000b, 0011 0011b, 01 01 00 00b
db 3,3,92h,30h,32h

db 1 01 1 1 000b, 0010 0010b, 01 01 00 00b
db 1,3,92h,22h,20h
db 1 10 1 1 000b, 0011 0011b, 01 01 00 00b
db 3,3,92h,32h,30h

;AX, BX
db 1 01 1 1 000b, 0010 0010b, 01 01 00 00b
db 1,3,93h,20h,23h
db 1 10 1 1 000b, 0011 0011b, 01 01 00 00b
db 3,3,93h,30h,33h

db 1 01 1 1 000b, 0010 0010b, 01 01 00 00b
db 1,3,93h,23h,20h
db 1 10 1 1 000b, 0011 0011b, 01 01 00 00b
db 3,3,93h,33h,30h

;AX, SP
db 1 01 1 1 000b, 0010 0010b, 01 01 00 00b
db 1,3,94h,20h,24h
db 1 10 1 1 000b, 0011 0011b, 01 01 00 00b
db 3,3,94h,30h,34h

db 1 01 1 1 000b, 0010 0010b, 01 01 00 00b
db 1,3,94h,24h,20h
db 1 10 1 1 000b, 0011 0011b, 01 01 00 00b
db 3,3,94h,34h,30h

;AX, BP
db 1 01 1 1 000b, 0010 0010b, 01 01 00 00b
db 1,3,95h,20h,25h
db 1 10 1 1 000b, 0011 0011b, 01 01 00 00b
db 3,3,95h,30h,35h

db 1 01 1 1 000b, 0010 0010b, 01 01 00 00b
db 1,3,95h,25h,20h
db 1 10 1 1 000b, 0011 0011b, 01 01 00 00b
db 3,3,95h,35h,30h

;AX, SI
db 1 01 1 1 000b, 0010 0010b, 01 01 00 00b
db 1,3,96h,20h,26h
db 1 10 1 1 000b, 0011 0011b, 01 01 00 00b
db 3,3,96h,30h,36h

db 1 01 1 1 000b, 0010 0010b, 01 01 00 00b
db 1,3,96h,26h,20h
db 1 10 1 1 000b, 0011 0011b, 01 01 00 00b
db 3,3,96h,36h,30h
;AX, DI
db 1 01 1 1 000b, 0010 0010b, 01 01 00 00b
db 1,3,97h,20h,27h
db 1 10 1 1 000b, 0011 0011b, 01 01 00 00b
db 3,3,97h,30h,37h

db 1 01 1 1 000b, 0010 0010b, 01 01 00 00b
db 1,3,97h,27h,20h
db 1 10 1 1 000b, 0011 0011b, 01 01 00 00b
db 3,3,97h,37h,30h

;reg reg
db 1 00 0 0 000b, 0001 0001b, 01 01 00 01b
db 0,1,86h
db 1 01 0 0 000b, 0010 0010b, 01 01 00 01b
db 1,1,87h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 01b
db 3,1,87h

;reg mem
db 1 00 0 0 000b, 0001 0000b, 01 10 00 00b
db 0,1,86h
db 1 01 0 0 000b, 0010 0000b, 01 10 00 00b
db 1,1,87h
db 1 10 0 0 000b, 0011 0000b, 01 10 00 00b
db 3,1,87h

;mem reg
db 1 00 0 0 000b, 0000 0001b, 10 01 00 00b
db 0,1,86h
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 1,1,87h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,1,87h

db 3,"XOR"
db 20
;mem Imm (SIGNED)
db 1 00 0 0 000b, 0000 0110b, 10 11 01 00b
db 1,1,80h
;Regular
db 1 01 0 0 000b, 0000 0110b, 10 11 10 00b
db 1,1,81h
db 1 10 0 0 000b, 0000 0110b, 10 11 11 00b
db 3,1,81h

;regs regs
db 1 00 0 0 000b, 0001 0001b, 01 01 00 00b
db 0,1,32h
db 1 01 0 0 000b, 0010 0010b, 01 01 00 00b
db 1,1,33h
db 1 10 0 0 000b, 0011 0011b, 01 01 00 00b
db 3,1,33h

;regs mem 
db 1 00 0 0 000b, 0001 0000b, 01 10 00 00b
db 0,1,32h
db 1 01 0 0 000b, 0010 0000b, 01 10 00 00b
db 1,1,33h
db 1 10 0 0 000b, 0011 0000b, 01 10 00 00b
db 3,1,33h

;mem regs
db 1 00 0 0 000b, 0000 0001b, 10 01 00 00b
db 0,1,30h
db 1 01 0 0 000b, 0000 0010b, 10 01 00 00b
db 1,1,31h
db 1 10 0 0 000b, 0000 0011b, 10 01 00 00b
db 3,1,31h

;Reg Imm (Exacts)
;Regs Imm
db 1 00 1 0 000b, 0001 0000b, 01 11 01 01b
db 1,2,34h,10h
db 1 00 0 0 000b, 0001 0110b, 01 11 01 00b
db 0,1,80h
;first are sign extends (8 bit imm)
db 1 01 0 0 000b, 0010 0110b, 01 11 01 00b
db 1,1,83h
db 1 01 1 0 000b, 0010 0000b, 01 11 10 10b
db 2,2,35h,20h
db 1 01 0 0 000b, 0010 0110b, 01 11 10 00b
db 1,1,81h

db 1 10 0 0 000b, 0011 0110b, 01 11 01 00b
db 3,1,83h
db 1 10 1 0 000b, 0011 0000b, 01 11 11 11b
db 3,2,35h,30h
db 1 10 0 0 000b, 0011 0110b, 01 11 11 00b
db 3,1,81h


;MMX ENCODINGS HERE!!

db 4,"MOVD"
db 4
;MMX, reg
db 1 00 0 0 000b, 0111 0011b, 01 01 00 00b
db 6,2,0Fh,6Eh

;reg, MMX
db 1 00 0 0 000b, 0011 0111b, 01 01 00 01b ;put MMX in right place
db 6,2,0Fh,7Eh

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,6Eh

;Mem, MMX
db 1 00 0 0 000b, 0000 0111b, 10 01 00 00b
db 6,2,0Fh,7Eh

db 4,"MOVQ"
db 3
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,6Fh

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,6Fh

;Mem, MMX
db 1 00 0 0 000b, 0000 0111b, 10 01 00 00b
db 6,2,0Fh,7Fh

db 8,"PACKSSDW"
db 2
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,6Bh

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,6Bh


db 8,"PACKSSWB"
db 2
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,63h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,63h

db 8,"PACKUSWB"
db 2
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,67h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,67h


db 4,"PADD"
db 6
;Types must be used here.. BYTE WORD OR DWORD/QUAD
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 01 00b
db 6,2,0Fh,0FCh

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 01 00b
db 6,2,0Fh,0FCh

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 10 00b
db 6,2,0Fh,0FDh

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 10 00b
db 6,2,0Fh,0FDh

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 11 00b
db 6,2,0Fh,0FEh

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 11 00b
db 6,2,0Fh,0FEh

db 5,"PADDS"
db 4
;Types must be used here.. BYTE OR WORD
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 01 00b
db 6,2,0Fh,0ECh

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 01 00b
db 6,2,0Fh,0ECh

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 10 00b
db 6,2,0Fh,0EDh

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 10 00b
db 6,2,0Fh,0EDh

db 6,"PADDUS"
db 4
;Types must be used here.. BYTE OR WORD
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 01 00b
db 6,2,0Fh,0DCh

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 01 00b
db 6,2,0Fh,0DCh

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 10 00b
db 6,2,0Fh,0DDh

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 10 00b
db 6,2,0Fh,0DDh


db 4,"PAND"
db 2

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,0DBh

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,0DBh

db 5,"PANDN"
db 2

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,0DFh

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,0DFh


db 6,"PCMPEQ"
db 6
;Types must be used here.. BYTE WORD OR DWORD/QUAD
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 01 00b
db 6,2,0Fh,074h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 01 00b
db 6,2,0Fh,074h

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 10 00b
db 6,2,0Fh,075h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 10 00b
db 6,2,0Fh,075h

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 11 00b
db 6,2,0Fh,076h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 11 00b
Db 6,2,0Fh,076h

db 6,"PCMPGT"
db 6
;Types must be used here.. BYTE WORD OR DWORD/QUAD
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 01 00b
db 6,2,0Fh,064h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 01 00b
db 6,2,0Fh,064h

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 10 00b
db 6,2,0Fh,065h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 10 00b
db 6,2,0Fh,065h

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 11 00b
db 6,2,0Fh,066h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 11 00b
Db 6,2,0Fh,066h


db 5,"PMADD"
db 2

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,0F5h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,0F5h

db 5,"PMULH"
db 2

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,0E5h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,0E5h

db 5,"PMULL"
db 2

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,0D5h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,0D5h

db 3,"POR"
db 2

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,0EBh

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,0EBh

db 4,"PSLL"
db 9

;DQUAD, THEN WORD, THEN DWORD
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,0F3h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,0F3h

;MMX, Imm
db 1 00 0 0 000b, 0111 0110b, 01 11 00 01b
db 6,2,0Fh,073h

;WORD
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 10 00b
db 6,2,0Fh,0F1h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 10 00b
db 6,2,0Fh,0F1h

;MMX, Imm
db 1 00 0 0 000b, 0111 0110b, 01 11 10 01b
db 6,2,0Fh,071h

;DWORD
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 11 00b
db 6,2,0Fh,0F2h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 11 00b
db 6,2,0Fh,0F2h

;MMX, Imm
db 1 00 0 0 000b, 0111 0110b, 01 11 11 01b
db 6,2,0Fh,072h

db 4,"PSRL"
db 9

;DQUAD, THEN WORD, THEN DWORD
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,0D3h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,0D3h

;MMX, Imm
db 1 00 0 0 000b, 0111 0010b, 01 11 00 01b
db 6,2,0Fh,073h

;WORD
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 10 00b
db 6,2,0Fh,0D1h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 10 00b
db 6,2,0Fh,0D1h

;MMX, Imm
db 1 00 0 0 000b, 0111 0010b, 01 11 10 01b
db 6,2,0Fh,071h

;DWORD
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 11 00b
db 6,2,0Fh,0D2h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 11 00b
db 6,2,0Fh,0D2h

;MMX, Imm
db 1 00 0 0 000b, 0111 0010b, 01 11 11 01b
db 6,2,0Fh,072h


db 4,"PSRA"
db 6

;WORD
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 10 00b
db 6,2,0Fh,0E1h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 10 00b
db 6,2,0Fh,0E1h

;MMX, Imm
db 1 00 0 0 000b, 0111 0100b, 01 11 10 01b
db 6,2,0Fh,071h

;DWORD
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 11 00b
db 6,2,0Fh,0E2h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 11 00b
db 6,2,0Fh,0E2h

;MMX, Imm
db 1 00 0 0 000b, 0111 0100b, 01 11 11 01b
db 6,2,0Fh,072h


db 4,"PSUB"
db 6
;Types must be used here.. BYTE OR WORD OR DWORD
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 01 00b
db 6,2,0Fh,0F8h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 01 00b
db 6,2,0Fh,0F8h

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 10 00b
db 6,2,0Fh,0F9h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 10 00b
db 6,2,0Fh,0F9h

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 11 00b
db 6,2,0Fh,0FAh

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 11 00b
db 6,2,0Fh,0FAh

db 5,"PSUBS"
db 4
;Types must be used here.. BYTE OR WORD
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 01 00b
db 6,2,0Fh,0E8h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 01 00b
db 6,2,0Fh,0E8h

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 10 00b
db 6,2,0Fh,0E9h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 10 00b
db 6,2,0Fh,0E9h

db 6,"PSUBUS"
db 4
;Types must be used here.. BYTE OR WORD
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 01 00b
db 6,2,0Fh,0D8h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 01 00b
db 6,2,0Fh,0D8h

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 10 00b
db 6,2,0Fh,0D9h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 10 00b
db 6,2,0Fh,0D9h

db 7,"PUNPCKH"
db 6
;Types must be used here.. BYTE OR WORD OR DWORD
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 01 00b
db 6,2,0Fh,068h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 01 00b
db 6,2,0Fh,068h

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 10 00b
db 6,2,0Fh,069h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 10 00b
db 6,2,0Fh,069h

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 11 00b
db 6,2,0Fh,06Ah

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 11 00b
db 6,2,0Fh,06Ah

db 7,"PUNPCKL"
db 6
;Types must be used here.. BYTE OR WORD OR DWORD
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 01 00b
db 6,2,0Fh,060h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 01 00b
db 6,2,0Fh,060h

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 10 00b
db 6,2,0Fh,061h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 10 00b
db 6,2,0Fh,061h

;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 11 00b
db 6,2,0Fh,062h

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 11 00b
db 6,2,0Fh,062h

db 4,"PXOR"
db 2
;MMX1, MMX2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,0EFh

;MMX, Mem
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,0EFh

;SIMD codes

db 5,"ADDPS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,58h
db 1 00 0 0 000b, 0111 0000b, 01 01 00 00b
db 6,2,0Fh,58h

db 5,"ADDSS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,3,0F3h,0Fh,58h
db 1 00 0 0 000b, 0111 0000b, 01 01 00 00b
db 6,3,0F3h,0Fh,58h


db 6,"ANDNPS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,55h
db 1 00 0 0 000b, 0111 0000b, 01 01 00 00b
db 6,2,0Fh,55h

db 5,"CMPPS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 01 10b ;Special override..
db 6,2,0Fh,0C2h
db 1 00 0 0 000b, 0111 0000b, 01 10 01 10b
db 6,2,0Fh,0C2h

db 5,"CMPSS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 01 10b ;Special override..
db 6,3,0F3h,0Fh,0C2h
db 1 00 0 0 000b, 0111 0000b, 01 10 01 10b
db 6,3,0F3h,0Fh,0C2h

db 6,"COMISS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,2Fh
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,2Fh

db 8,"CVTPS2PI"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,2Ah
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,2Ah

db 8,"CVTPS2PI"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,2Dh
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,2Dh

db 8,"CVTSI2SS"
db 2
db 1 00 0 0 000b, 0111 0011b, 01 01 00 00b
db 6,3,0F3h,0Fh,2Ah
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,3,0F3h,0Fh,2Ah

db 8,"CVTSS2SI"
db 2
db 1 00 0 0 000b, 0011 0111b, 01 01 00 00b
db 6,3,0F3h,0Fh,2Dh
db 1 00 0 0 000b, 0011 0000b, 01 10 00 00b
db 6,3,0F3h,0Fh,2Dh

db 9,"CVTTPS2PI"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,2Ch
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,2Ch

db 9,"CVTTSS2SI"
db 2
db 1 00 0 0 000b, 0011 0111b, 01 01 00 00b
db 6,3,0F3h,0Fh,2Ch
db 1 00 0 0 000b, 0011 0000b, 01 10 00 00b
db 6,3,0F3h,0Fh,2Ch

db 5,"DIVPS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,5Eh
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,5Eh

db 5,"DIVSS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,3,0F3h,0Fh,5Eh
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,3,0F3h,0Fh,5Eh

db 5,"MAXPS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,5Fh
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,5Fh

db 5,"MAXSS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,3,0F3h,0Fh,5Fh
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,3,0F3h,0Fh,5Fh

db 5,"MINPS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,5Dh
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,5Dh

db 5,"MINSS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,3,0F3h,0Fh,5Dh
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,3,0F3h,0Fh,5Dh

db 6,"MOVAPS"
db 3
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,28h
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,28h
db 1 00 0 0 000b, 0000 0111b, 10 01 00 00b
db 6,2,0Fh,29h

db 7,"MOVHLPS"
db 1
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,12h

db 6,"MOVHPS"
db 2
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,16h
db 1 00 0 0 000b, 0000 0111b, 10 01 00 00b
db 6,2,0Fh,17h

db 7,"MOVLHPS"
db 1
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,16h

db 6,"MOVLPS"
db 2
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,12h
db 1 00 0 0 000b, 0000 0111b, 10 01 00 00b
db 6,2,0Fh,13h

db 8,"MOVMSKPS"
db 1
db 1 00 0 0 000b, 0011 0111b, 01 01 00 00b
db 6,2,0Fh,50h

db 5,"MOVSS"
db 3
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,3,0F3h,0Fh,10h
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,3,0F3h,0Fh,10h
db 1 00 0 0 000b, 0000 0111b, 10 01 00 00b
db 6,3,0F3h,0Fh,11h

db 6,"MOVUPS"
db 3
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,10h
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,10h
db 1 00 0 0 000b, 0000 0111b, 10 01 00 00b
db 6,2,0Fh,11h

db 5,"MULPS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,59h
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,59h

db 5,"MULSS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,3,0F3h,0Fh,59h
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,3,0F3h,0Fh,59h

db 4,"ORPS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,56h
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,56h

db 5,"RCPPS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,53h
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,53h

db 7,"RSQRTPS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,52h
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,52h

db 7,"RSQRTSS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,3,0F3h,0Fh,52h
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,3,0F3h,0Fh,52h

db 6,"SHUFPS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 01 10b
db 6,2,0Fh,0C6h
db 1 00 0 0 000b, 0111 0000b, 01 10 01 10b
db 6,2,0Fh,0C6h

db 6,"SQRTPS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,51h
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,51h

db 6,"SQRTSS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,3,0F3h,0Fh,51h
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,3,0F3h,0Fh,51h

db 5,"SUBPS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,5Ch
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,5Ch

db 5,"SUBSS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,3,0F3h,0Fh,5Ch
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,3,0F3h,0Fh,5Ch

db 7,"UCOMISS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,2Eh
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,2Eh

db 8,"UNPCKHPS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,15h
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,15h

db 8,"UNPCKLPS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,14h
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,14h

db 5,"XORPS"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,57h
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,57h

db 5,"PAVGB"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,0E0h
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,0E0h

db 5,"PAVGW"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,0E3h
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,0E3h

db 6,"PEXTRW"
db 1
db 1 00 0 0 000b, 0011 0111b, 01 01 01 10b ;3RD is a byte
db 6,2,0Fh,0C5h

db 6,"PINSRW"
db 2
db 1 10 0 0 000b, 0111 0011b, 01 01 00 00b
db 6,2,0Fh,0C4h
db 1 01 0 0 000b, 0111 0010b, 01 10 00 00b
db 6,2,0Fh,0C4h

db 6,"PMAXSW"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,0EEh
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,0EEh

db 6,"PMAXUB"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,0DEh
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,0DEh

db 6,"PMINSW"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,0EAh
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,0EAh

db 6,"PMINUB"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,0DAh
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,0DAh

db 8,"PMOVMSKB"
db 1
db 1 00 0 0 000b, 0011 0111b, 01 01 00 00b
db 6,2,0Fh,0D7h

db 7,"PMULHUW"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,0E4h
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,0E4h

db 6,"PSADBW"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,0F6h
db 1 00 0 0 000b, 0111 0000b, 01 10 00 00b
db 6,2,0Fh,0F6h

db 6,"PSHUFW"
db 2
db 1 00 0 0 000b, 0111 0111b, 01 01 01 10b ;3rd. number
db 6,2,0Fh,70h
db 1 00 0 0 000b, 0111 0000b, 01 10 01 10b
db 6,2,0Fh,70h

db 8,"MASKMOVQ"
db 1
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,0F7h

db 7,"MOVNTPS"
db 1
db 1 00 0 0 000b, 0111 0000b, 10 01 00 00b
db 6,2,0Fh,2Bh

db 6,"MOVNTQ"
db 1
db 1 00 0 0 000b, 0111 0111b, 01 01 00 00b
db 6,2,0Fh,0E7h

;ok FPU functions here we come :)
;Most FPU functions have 1 exact (ST(0)).. enjoy :)

db 4,"FADD"
db 4
db 1 00 1 0 001b, 1000 0000b, 01 10 11 11b
db 2,2,0D8h,80h
db 1 11 1 0 001b, 1000 0000b, 01 10 10 11b
db 2,2,0DCh,80h
db 1 00 1 0 001b, 1000 1000b, 01 01 0000b
db 2,2,0D8h,80h
db 1 00 0 1 001b, 1000 1000b, 01 01 0000b
db 2,2,0DCh,80h

db 5,"FADDP"
db 1
db 1 00 1 0 001b, 1000 0000b, 01 10 00 00b
db 2,2,0DEh,80h

db 6,"FCMOVB"
db 1
;REG REG
db 1 01 1 0 000b, 1000 1000b, 01 01 0000b
db 6,2,0DAh,80h

db 8,"FCMOVNAE"
db 1
;REG REG
db 1 01 1 0 000b, 1000 1000b, 01 01 0000b
db 6,2,0DAh,80h

db 6,"FCMOVC"
db 1
;REG REG
db 1 01 1 0 000b, 1000 1000b, 01 01 0000b
db 6,2,0DAh,80h

db 7,"FCMOVNB"
db 1
;REG REG
db 1 01 1 0 000b, 1000 1000b, 01 01 0000b
db 6,2,0DBh,80h

db 7,"FCMOVAE"
db 1
;REG REG
db 1 01 1 0 000b, 1000 1000b, 01 01 0000b
db 6,2,0DBh,80h

db 7,"FCMOVNC"
db 1
;REG REG
db 1 01 1 0 000b, 1000 1000b, 01 01 0000b
db 6,2,0DBh,80h

db 6,"FCMOVE"
db 1
;REG REG
db 1 01 1 0 000b, 1000 1000b, 01 01 0001b
db 6,2,0DAh,80h

db 6,"FCMOVZ"
db 1
;REG REG
db 1 01 1 0 000b, 1000 1000b, 01 01 0001b
db 6,2,0DAh,80h

db 7,"FCMOVNE"
db 1
;REG REG
db 1 01 1 0 000b, 1000 1000b, 01 01 0001b
db 6,2,0DBh,80h

db 7,"FCMOVNZ"
db 1
;REG REG
db 1 01 1 0 000b, 1000 1000b, 01 01 0001b
db 6,2,0DBh,80h

db 7,"FCMOVBE"
db 1
;REG REG
db 1 01 1 0 000b, 1000 1000b, 01 01 0010b
db 6,2,0DAh,80h

db 7,"FCMOVNA"
db 1
;REG REG
db 1 01 1 0 000b, 1000 1000b, 01 01 0010b
db 6,2,0DAh,80h

db 8,"FCMOVNBE"
db 1
;REG REG
db 1 01 1 0 000b, 1000 1000b, 01 01 0010b
db 6,2,0DBh,80h

db 6,"FCMOVA"
db 1
;REG REG
db 1 01 1 0 000b, 1000 1000b, 01 01 0010b
db 6,2,0DBh,80h

db 6,"FCMOVU"
db 1
;REG REG
db 1 01 1 0 000b, 1000 1000b, 01 01 0011b
db 6,2,0DAh,80h

db 7,"FCMOVNU"
db 1
;REG REG
db 1 01 1 0 000b, 1000 1000b, 01 01 0011b
db 6,2,0DBh,80h

db 4,"FDIV"
db 4
db 1 00 1 0 001b, 1000 0110b, 01 10 11 11b
db 2,2,0D8h,80h
db 1 11 1 0 001b, 1000 0110b, 01 10 10 11b
db 2,2,0DCh,80h
db 1 00 1 0 001b, 1000 1000b, 01 01 0110b
db 2,2,0D8h,80h
db 1 00 0 1 001b, 1000 1000b, 01 01 0110b
db 2,2,0DCh,80h

db 5,"FDIVP"
db 1
db 1 00 0 1 001b, 1000 1000b, 01 01 0111b
db 2,2,0DEh,80h

db 5,"FDIVR"
db 4
db 1 00 1 0 001b, 1000 0111b, 01 10 11 11b
db 2,2,0D8h,80h
db 1 11 1 0 001b, 1000 0111b, 01 10 10 11b
db 2,2,0DCh,80h
db 1 00 1 0 001b, 1000 1000b, 01 01 0111b
db 2,2,0D8h,80h
db 1 00 0 1 001b, 1000 1000b, 01 01 0111b
db 2,2,0DCh,80h

db 6,"FDIVRP"
db 1
db 1 00 0 1 001b, 1000 1000b, 01 01 0110b
db 2,2,0DEh,80h

db 5,"FIADD"
db 2
db 1 00 1 0 001b, 1000 0000b, 01 10 10 11b
db 2,2,0DEh,80h
db 1 00 1 0 001b, 1000 0000b, 01 10 11 11b
db 2,2,0DAh,80h

db 4,"FMUL"
db 4
db 1 00 1 0 001b, 1000 0001b, 01 10 11 11b
db 2,2,0D8h,80h
db 1 11 1 0 001b, 1000 0001b, 01 10 10 11b
db 2,2,0DCh,80h
db 1 00 1 0 001b, 1000 1000b, 01 01 0001b
db 2,2,0D8h,80h
db 1 00 0 1 001b, 1000 1000b, 01 01 0001b
db 2,2,0DCh,80h

db 5,"FMULP"
db 1
db 1 00 1 0 001b, 1000 1000b, 01 01 0001b
db 2,2,0DEh,80h

db 4,"FSUB"
db 4
db 1 00 1 0 001b, 1000 0100b, 01 10 11 11b
db 2,2,0D8h,80h
db 1 11 1 0 001b, 1000 0100b, 01 10 10 11b
db 2,2,0DCh,80h
db 1 00 1 0 001b, 1000 1000b, 01 01 0100b
db 2,2,0D8h,80h
db 1 00 0 1 001b, 1000 1000b, 01 01 0100b
db 2,2,0DCh,80h

db 5,"FSUBR"
db 4
db 1 00 1 0 001b, 1000 0101b, 01 10 11 11b
db 2,2,0D8h,80h
db 1 11 1 0 001b, 1000 0101b, 01 10 10 11b
db 2,2,0DCh,80h
db 1 00 1 0 001b, 1000 1000b, 01 01 0101b
db 2,2,0D8h,80h
db 1 00 0 1 001b, 1000 1000b, 01 01 0101b
db 2,2,0DCh,80h

db 6,"FSUBRP"
db 1
db 1 00 0 1 001b, 1000 1000b, 01 01 0100b
db 2,2,0DEh,80h


db 0,""
db 0
db 1 00 0 0 000b, 0000 0000b, 00 00 00 00b
db 0,0,0h

just3:db 0
db 0

opinclude:;the 'standalone' instructions can access these
db 1,90h;NOP NEVER USED!!!

db 1,9Bh;fpu (FWAIT)?
db 1,0F3h;REP (REP) FOR STRINGS.
db 1,90h;NOP (until used later?)

;last 4 accessible by 1 and 2 inputs
db 1,90h;NOP (until used later?)
db 1,90h;NOP (until used later?)
db 1,90h;NOP (until used later?)
db 1,90h;NOP (until used later?)

