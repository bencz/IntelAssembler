%cpx 0100h
@buffs 2000  ;more then needed, but general use
@bits 2000   ;to print
                pusha
                mov cx,buffs                ;size of buffer to clear
                mov si,buff                 ;location of buffer (start)
                dec cx                      ;skip last one
buff_clear:     mov byte ptr cs:[si],"0"    ;clear to 0
                inc si                      ;move up one
                loop short buff_clear       ;loop
                mov byte ptr cs:[si],"2"    ;on last byte of buffer, save a 2
                mov word ptr cs:[si+1],0a0dh;Save Next line information
                mov [si_save],si            ;save on last(first) number
;start here
                mov cx,bits                 ;set from top
                mov dx,si                   ;assigns dx
;in the first loop, 1 is lost.. this is the first bit size.. watch it grow :)
start:          push cx                     ;save current double
                mov bx,1                    ;state video
                mov cx,[si_save]            ;grab size
                sub cx,dx                   ;get exact
                add cx,3                    ;add 3
                mov ah,40h                  ;output
                int 21h                     ;dos function

                mov cx,buffs                ;grab size
                mov si,[si_save]            ;grab save
                mov ah,0                    ;never needed
                mov bl,0                    ;clear carry
someloop:       mov al,cs:[si]              ;grab number
                sub al,"0"                  ;make number
                rol ax,1                    ;quick double
                add al,bl                    ;add the carry (bl)
                mov bl,0                     ;clear carry
                cmp al,10                   ;see if 10 or more
                jb short numskip            ;if not, skip
                sub al,10                    ;take away 10,
                inc bl                       ;set carry (bl)
numskip:        cmp al,0                    ;see if 0
                je short zero               ;if yes, skip
                mov dx,si                   ;save start data
zero:           add al,"0"                  ;make text again
                mov cs:[si],al              ;save
                dec si                      ;move down
                loop short someloop
                pop cx                      ;return bits counter for double
                loop short start
                popa                        ;restore values
                ret                         ;return to dos
si_save:        dw 0                        ;save position..
buff:
