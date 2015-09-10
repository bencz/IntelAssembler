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
                cmp ax,1000h    ;64K
                jae short memgood
                mov ah,ptext
                mov dx,memerr
                int 21h
                jmp near ending
memgood:        mov ax,cs
                mov ds,ax       ;make sure..

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

                mov bx,files    ;prepair for working
                mov dx,[bx]     ;get start

                mov ah,open     ;open the file?
                mov al,readonly
                int 21h
                jnc short noerr1

                mov ah,ptext
                mov dx,err
                int 21h         ;write
                jmp near ending

noerr1:         mov [file1],ax  ;write handle
                mov ah,create
                mov dx,[bx+2]   ;ok, get second part
                mov cx,0
                moc bx,0
                int 21h         ;ok, make second file
                jnc short noerr2

                call closefirst ;close the open file
                mov ah,ptext
                mov dx,err
                int 21h         ;write
                jmp near ending

noerr2:         mov [file2],ax  ;write handle

;ok, at this point we get a few K at a time, and write it out in the file.
;hexes only, skip out all the rest.

starting:mov bx,[file1] ;read handle
        mov cx,datasize ;for the fixed size
        mov dx,readhere ;to location
        mov ah,read
        int 21h         ;k, grab section

;horray, now that we have the information.. do we quit?

        cmp ax,0        ;anything left?
        je near quit    ;quitting now

        mov si,readhere
        mov di,writehere
        mov dx,ax       ;get bytes for it.

start2: mov bx,firstout
        mov cx,firstout_l
        call copying    ;ok, copy this over

        mov cx,numcount ;number of characters per line
        cmp dx,cx
        ja short start2_1
        mov cx,dx       ;fix to size
start2_1:sub dx,cx      ;adjust amount left
start3: push cx
        mov al,[si]     ;grab character
        mov ah,0
        shl ax,4        ;split
        shr al,4        ;split
        mov bx,dbtrans  ;translation table, Hex
        cmp ah,10
        jae short notop
        dec di          ;0 isn't needed

notop:  xlat
        xchg ah,al
        xlat
        mov [di],al
        mov [di+1],ah
        add di,2

        cmp cx,1
        je short start3_1
        mov bx,between
        mov cx,between_l
        call copying

start3_1:pop cx
        inc si
        loop short start3

        mov bx,atend
        mov cx,atend_l
        call copying

        cmp dx,0
        ja short start2 ;go through loop again

        mov cx,di       ;prepair length
        sub cx,writehere;find it
        mov dx,writehere
        mov ah,write
        mov bx,[file2]
        int 21h         ;put it out
        jmp short starting



quit:   call closeboth  ;very ending
        jmp near ending


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

;bx to di, for the length of cx
copying:pusha
copying2:mov al,[bx]    ;get
        mov [di],al     ;write
        inc bx
        inc di
        loop short copying2
        popa
        add di,cx       ;fix location?
        ret             ;return

Intro:
db "DB- Changing a code to pure HEX's",13,10
db ":)$"

Proguse:
db "Program Usage: DB [Input] [Output]",13,10
db 13,10,13,10,"$"

Exit: db "Thanks for using my Program",13,10,"$"

Err: db "An error has occured, while trying to access a file",13,10
     db "Please check over to make sure you typed it all in correctly",13,10,13,10,"$"
memerr: db "Not Enough Memory to Run Program",13,10,"$"

file1: dw 0
file2: dw 0
files: dw 0,0,0,0,0,0,0,0,0,0 ;junk area, just in case, only 3 needed

firstout: db "DB 0"
$firstout_L
between: db "h,0"
$between_L
atend: db "h",13,10
$atend_L

dbtrans: db "0123456789ABCDEF"
;this is a quick translator using Xlat, cheap, and all that we need.
;nothing fancy

dw datasize,datasize*5
;the data will be 5 times bigger.
@datasize 3000
readhere:
+cpx datasize
writehere:

^numcount 15
;this counter is for how many on one line. (single numbers)

^ptext 9

^Open 3Dh
^Create 3Ch
^OpenCreate 6Ch
^Close 3Eh
^Read 3Fh
^Write 40h

^ReadOnly 0
^WriteOnly 1
^Random 2

@PointerSof 4200h
@PointerCur 4201h
@PointerEof 4202h
