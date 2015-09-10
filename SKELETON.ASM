%cpx 100h
                pushad
                mov ah,ptext    ;introduction text
                mov dx,intro
                mov bx,1
                int 21h

                mov bx,cs       ;get size
                mov ds,bx       ;prepair
                mov ax,cs:[2]   ;get exact
                sub ax,bx
                dec ax
                cmp ax,4000h    ;256K (64 program,64 insts,64 labels,64 data)
                jae short memgood
                mov ah,ptext
                mov dx,memerr
                int 21h
                jmp near ending
memgood:        mov ax,cs
                mov ds,ax       ;make sure..
                add ax,1000h
                mov fs,ax       ;create inst
                add ax,1000h
                mov es,ax       ;create labels
                add ax,1000h
                mov gs,ax       ;set up data area

                cmp byte ptr [129],0    ;anything in the line segment?
                je near proguse2
                cmp byte ptr [129],13   ;anything in the line segment?
                je near proguse2

                mov si,files            ;prepair output
                mov bx,130              ;get location start
                xor cx,cx               ;clear
                jmp short chalk_cheat   ;start in the middle

chalkboard_0:   inc bx                  ;start (at the next letter)
                cmp byte ptr [bx]," "   ;space?
                jne short chalkboard_1
                mov byte ptr [bx],0     ;set up null (ending)
                inc bx                  ;fix, first letter, not null
chalk_cheat:    mov [si],bx             ;save (also middle)
                dec bx
                add si,2                ;move up 2
                inc cx                  ;number count
chalkboard_1:   cmp byte ptr [bx],13    ;ending
                je short chalkboard_2   ;see if ending
                jmp short chalkboard_0  ;no? Start
chalkboard_2:   mov byte ptr [bx],0     ;set up null (ending)
                cmp cx,2
                jb near proguse2        ;see if enough parts

                cmp cx,3
                jb short chalkboard_7

                mov bx,files    ;prepair for working
                mov si,[bx+4]   ;get start

chalkboard_7:

closeboth:mov ah,close
        mov bx,cs:[file2]
        int 21h

closefirst:mov ah,close
        mov bx,cs:[file1]
        int 21h
        ret

proguse2:mov ah,ptext   ;say program use
        mov dx,proguse
        mov bx,1
        int 21h
ending: mov ah,ptext    ;ending text
        mov dx,exit
        mov bx,1
        int 21h
        popad
        ret


Intro:
db "An Intro Text V0.0",13,10
db ":)$"

Proguse:
db "Program Usage: Assemble [Input.asm] [Output.com] [DMF]",13,10
db "Other options:",13,10,13,10
db "                             ",13,10,13,10,"$"

Exit: db "Thanks for using my Program",13,10,"$"

Err: db "An error has occured, while trying to access a file",13,10
     db "Please check over to make sure you typed it all in correctly",13,10,13,10,"$"
memerr: db "Not Enough Memory to Run Program",13,10,"$"

file1: dw 0
file2: dw 0
file3: dw 0   ;debug.txt
files: dw 0,0,0,0,0,0,0,0,0,0 ;junk area, just in case, only 3 needed

^ptext 9
