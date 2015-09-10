;comptest
%cpx 0100h
                pusha
                mov ax,cs
                mov ds,ax
                cmp byte ptr [129],0dh  ;quick check, see if any files?
                je skiptomessage        ;no files
                mov bx,130              ;base line scanning
                mov cx,bx               ;set up 'last' file
                mov si,files            ;set up pointer (PSP)
scanhere:       mov al,[bx]             ;get letter
                cmp al," "              ;see if space
                jne short notspace
                mov al,0                ;make null
                mov [bx],al             ;save null
                mov [si],cl             ;save files
                mov cx,bx               ;prepair next filename
                inc cx                  ;prepairing the file name
                inc si
notspace:       cmp al,0dh              ;see if at end?
                je short donescan
                inc bx                  ;if not done, move up 1
                jmp short scanhere

donescan:       mov al,0                ;make null
                mov [bx],al             ;save null
                mov [si],cl             ;save files
                mov bx,files            ;get location
                cmp byte ptr [bx],0     ;see if anything was found
                je skiptomessage        ;nothing here
                cmp byte ptr [bx+1],0   ;see if anything was found
                je skiptomessage        ;nothing here
;now the fun begins :)

                cmp byte ptr [bx+2],0   ;see if anything was found
                je short Autocompress   ;nothing here
                mov ah,0
                mov al,[bx+2]           ;grab location
                mov si,ax

                cmp word ptr [si],"-s"  ;see if decompess
                je short auto1          ;change stat to work
                cmp word ptr [si],"-S"  ;see if decompress
                jne short auto2         ;don't change stat
auto1:          mov bp,2
                mov ax,0                ;clear numbers
                mov dx,ax
                mov bx,dx
                mov cx,10               ;set multiply

auto3:          cmp byte ptr [bp+si],00 ;see if at end
                je short auto4
                mul cx                  ;multiply by 10
                mov bl,[bp+si]          ;grab new digit
                sub bl,"0"              ;kill to real number
                add ax,bx               ;add number on
                inc bp                  ;move up one character
                jmp short auto3

auto4:          cmp dx,0                ;see if exceeded limit
                jne short nobuffch
                cmp ax,maxbuff          ;see if in limit?
                ja short nobuffch
                mov [buffer],ax
                jmp short auto2

nobuffch:       mov bx,1
                mov ah,ptext
                mov dx,bufferr
                int 21h

auto2:          cmp word ptr [si],"-d"  ;see if decompess
                je short statch         ;change stat to work
                cmp word ptr [si],"-D"  ;see if decompress
                jne short autocompress  ;don't change stat

statch:         mov byte ptr [status],1 ;change stat

autocompress:   mov bx,files            ;get location
                mov al,readonly
                mov ah,open
                mov dl,[bx]             ;Name
                mov dh,0
                int 21h                 ;access

                jnc short nofilerr1
                jmp errmess

nofilerr1:      mov [file1],ax          ;save
                mov ah,Create
                mov cx,0                ;No attributes
                mov dl,[bx+1]           ;Name
                mov dh,0
                int 21h                 ;access

                jnc short nofilerr2
                mov ah,close
                mov bx,[file1]
                int 21h
                jmp errmess

nofilerr2:      mov [file2],ax          ;save
                mov ah,Ptext            ;print starting message
                mov bx,1
                mov dx,base2
                int 21h

                mov ax,PointerEof       ;mark for pointer
                mov bx,[file1]
                mov cx,0
                mov dx,cx               ;say nothing
                int 21h

                mov bx,filesize         ;save Length Of File
                mov [bx],dx
                mov [bx+2],ax
                
                mov ax,PointerSof       ;return to beginning
                mov bx,[file1]
                mov cx,0
                mov dx,0
                int 21h

                cmp byte ptr [status],1 ;see if decompressing
                jne compress            ;compressing

Decompress:     mov ah,read
                mov cx,title1           ;title size
                add cx,2                ;buffer
                add cx,4                ;Filesize info
                mov dx,filepart1
                mov bx,[file1]
                int 21h

                mov si,title0
                mov ax,[si]             ;grab title
                mov bx,[si+2]
                mov cx,[si+4]           ;grab header
                mov dx,[si+6]
                cmp ax,cx               ;see if they equal
                je short good1

bad1:           mov ah,Ptext
                mov dx,Badfile          ;display message (not compressed?)
                mov bx,1
                int 21h
                jmp closefiles

good1:          cmp bx,dx               ;worked, now the second half
                je short good2
                jmp short bad1

good2:          mov cx,[si+8]           ;grab buffer size
                mov [buffer],cx         ;save buffer size

                mov dx,[si+10]          ;getting
                mov ax,[si+12]          ;file size

                div cx                  ;get differences
                mov [filesize],dx       ;save remainder

StDecomp:       mov al,0
                mov bx,maps1            ;set up for maps (identifyers)
                mov cx,288              ;size of maps and compressed maps
clearloop4:     mov [bx],al             ;save empty
                inc bx
                loop short clearloop4

                mov cx,32               ;read maps data
                mov dx,maps2
                mov bx,[file1]          ;from compressed file
                mov ah,read
                int 21h

                cmp ax,0
                je closefiles

                mov al,0                ;prepair empty
                mov cx,[buffer]         ;get size
                inc cx                  ;just in case one more
                mov bx,filepart2        ;clear compressed side
clearloop3:     mov [bx],al             ;write
                inc bx                  ;move up one
                loop short clearloop3   ;continue

                mov cx,32               ;prepair size
                mov si,maps2            ;prepair compressed
                mov di,maps1            ;to uncompress
                mov al,0

Xtract1:        mov ah,8                ;set byte size
                xchg cl,ah              ;set up loop
                mov bl,[si]             ;get first byte
Xtract2:        rcl bl,1                ;pull to left (use CARRY)
                mov [di],al             ;clear first
                adc [di],al             ;Make either 1 or 0
                inc di                  ;next byte
                loop short Xtract2      ;go through byte
                inc si                  ;move up compressed
                xchg ah,cl              ;get size
                loop short Xtract1      ;see if anymore?

                mov cx,256              ;set map size
                mov bx,maps1            ;set for starting
                mov si,0                ;clear numbers
                mov di,si
                mov dx,si

search3:        mov al,[bx+si]          ;get map part
                cmp al,1                ;see if 1
                jne short search3_no    ;if not 1, no count
                mov dx,si               ;get number
                mov [bx+di],dl          ;save as 'replace' number
                inc di                  ;add to count
search3_no:     inc si                  ;add to position
                loop short search3

                cmp di,1                ;if only 1 character,
                jne short Decomp        ;not worth De-compressing

                push dx                 ;save letter
                mov ax,PointerCur       ;mark for pointer
                mov bx,[file1]
                mov cx,0
                mov dx,cx               ;say nothing
                int 21h

                push ax                 ;back up current
                push dx

                mov ax,PointerEof       ;mark for pointer
                mov bx,[file1]
                mov cx,0
                mov dx,cx               ;say nothing
                int 21h

                pop cx                  ;get current for compare
                pop bx

                cmp dx,cx               ;check main part 1
                jne short decomp_end2   ;not at end
                cmp ax,bx               ;sec secondary
                je short decomp_end     ;at end

decomp_end2:    mov dx,bx               ;put in place
                mov ax,PointerSof       ;mark for pointer
                mov bx,[file1]
                int 21h
                jmp short decomp_end1   ;not at end

decomp_end:     mov byte ptr [status],2 ;save new status
decomp_end1:    pop dx                  ;get back letter
                mov cx,[buffer]         ;size
                mov bx,filepart2        ;set up location
                mov al,dl               ;save the only used number
no_uncomp:      mov [bx],al             ;write only number up
                inc bx                  ;move up one
                loop short no_uncomp    ;write whole buffer
                jmp skip_decomp

Decomp:         mov dx,di
                dec dx                  ;fix max count
                mov dh,0                ;clear value
                mov ax,0                ;prepair count
                mov cx,8                ;counter (in case failed?)
bitstart2:      rol dx,1                ;start count
                cmp dh,1                ;see if anything changed
                je short bitsdone2      ;if it has, size completed
                inc ax                  ;if not, add one to bits saved
                loop short bitstart2    ;max out count (not stuck :) )
bitsdone2:      cmp ax,0                ;see if ANY bits were saved
                jne short bitsgood2

                mov cx,[buffer]         ;get information
                mov ah,read             ;since no space was saved
                mov dx,filepart2        ;get buffered size
                mov bx,[file1]          ;and begin changing to normal
                int 21h                 ;hehe :)
                jmp buttercup           ;the fun part :)

bitsgood2:      push ax
                mov dx,0                ;clear extended
                mov ax,cx
                mov bx,[buffer]         ;get buffer size
                mul bx                  ;multiply
                mov bx,8                ;set up divide
                div bx                  ;divide (find size)
                cmp dx,0
                je short sizefind2      ;no remainder
                inc ax                  ;increase (extra in there?)

sizefind2:      mov cx,ax               ;get size
                mov bx,[file1]          ;prepair read
                mov dx,filepart1        ;first file block
                mov ah,read             ;read
                int 21h                 ;access

                cmp ax,cx               ;see if got all the information
                je short no_sizzle      ;check if fail
                mov byte ptr [status],2 ;failed, so update status

no_sizzle:      mov si,filepart1        ;prepair
                mov di,filepart2        ;prepair
                mov bp,[buffer]         ;prepair end
                add bp,di               ;check with BP
                add bp,8                ;one byte's more in case (bits)

                pop dx                  ;dh=8 dl=bits
                mov cx,0800h            ;ch=bits, cl=left

                mov dh,ch               ;get true size
                sub dh,dl               ;see it
                mov dl,ch               ;set up again
                mov ch,dh               ;set up prebyte
                mov ax,0                ;clear (no reason)?

DeeDee1:        cmp cl,0                ;see if any bits remain
                ja short Dextor

                cmp di,bp               ;check at end?
                je short buttercup      ;at end, leave area

                mov al,[si]             ;grab new bits
                inc si                  ;move up one
                mov cl,dl               ;save size
Dextor:         cmp ch,cl               ;enough for full change?
                jna short Bubbles

                shl ax,cl               ;move bits
                sub ch,cl               ;save byte info
                mov cl,0                ;clear cl
                jmp short DeeDee1       ;start loop again

Bubbles:        xchg cl,ch
                shl ax,cl               ;move bits
                sub ch,cl               ;get change
                xchg cl,ch              ;return

                mov ch,dh               ;set for next byte
                mov [di],ah             ;save byte
                mov ah,0                ;clear ah
                inc di                  ;move up 1 byte (saves)
                jmp short DeeDee1

ButterCup:      mov bx,filepart2        ;prepair
                mov si,maps1            ;prepair converting
                mov ah,0                ;0
                mov cx,[buffer]         ;prepair size

change3:        mov al,[bx]             ;read
                mov bp,ax               ;prepair pointer
                mov al,[bp+si]          ;check with list
                mov [bx],al             ;replace
                inc bx                  ;move up 1
                loop short change3      ;continue

skip_decomp:    mov cx,[buffer]         ;get size

                cmp byte ptr [status],1 ;check status
                je short sizzle         ;if the same, don't change
                mov cx,[filesize]       ;change to right size
sizzle:         mov ah,write            ;output
                mov dx,filepart2
                mov bx,[file2]
                int 21h                 ;access
                jmp stdecomp

compress:       mov ah,write            ;write identifyer
                mov bx,[file2]          ;to the second file
                mov cx,title1           ;size (may change? :) )
                mov dx,title0           ;location of what to write 
                int 21h

                mov ah,write            ;write identifyer
                mov bx,[file2]          ;to the second file
                mov cx,2
                mov dx,buffer           ;buffer size to write (changes later)
                int 21h

                mov ah,write            ;write identifyer
                mov bx,[file2]          ;to the second file
                mov cx,4
                mov dx,filesize         ;buffer size to write (changes later)
                int 21h


compress1:      clc                     ;clear carry
                mov ah,read             ;prepair read
                mov bx,[file1]          ;from the first file
                mov cx,[buffer]         ;size of specified buffer
                mov dx,filepart1        ;to the first part
                int 21h
                jnc short comp_noerrs   ;if error, close
2jmps:          jmp closefiles
comp_noerrs:    mov [buffer],ax         ;if changes size, it does too
                cmp ax,0
                je short 2jmps          ;see if ANYthing was captured

                mov al,0                ;prepair empty
                mov cx,[buffer]         ;get size
                inc cx                  ;just in case one more
                mov bx,filepart2        ;clear compressed side
clearloop1:     mov [bx],al             ;write
                inc bx                  ;move up one
                loop short clearloop1   ;contine

                mov bx,maps1            ;set up for maps (identifyers)
                mov cx,288              ;size of maps and compressed maps
clearloop2:     mov [bx],al             ;save empty
                inc bx
                loop short clearloop2

                mov cx,[buffer]         ;declair size
                mov bx,filepart1        ;location (search)
                mov bp,maps1            ;declair map1
                mov ah,0
search1:        mov al,[bx]             ;grab letter
                mov si,ax               ;put on add
                mov byte ptr [bp+si],1  ;mark as used
                inc bx                  ;move up 1
                loop short search1      ;loop out section

                mov cx,32               ;bytes
                mov si,maps2
mapcomp:        mov ax,0                ;clear
                mov ah,[bp+7]           ;compress 1 byte
                ror ax,1
                mov ah,[bp+6]
                ror ax,1
                mov ah,[bp+5]
                ror ax,1
                mov ah,[bp+4]
                ror ax,1
                mov ah,[bp+3]
                ror ax,1
                mov ah,[bp+2]
                ror ax,1
                mov ah,[bp+1]
                ror ax,1
                mov ah,[bp]
                ror ax,1
                mov [si],al             ;save map codes
                add bp,8                ;prepair next compress
                inc si                  ;set up next code
                loop short mapcomp      ;loop

                mov dx,maps2            ;write section
                mov bx,[file2]          ;write to second file
                mov cx,32               ;of the compressed map
                mov ah,write
                int 21h

                mov cx,256              ;set map size
                mov si,maps1            ;set for starting
                mov dx,0                ;set for... count
search2:        mov al,[si]             ;get map part
                cmp al,1                ;see if 1
                jne short search2_no    ;if not 1, no count
                mov [si],dl             ;save as new number
                inc dx                  ;add to count
search2_no:     inc si                  ;add to position
                loop short search2
                mov cx,[buffer]         ;size
                cmp dl,1                ;if only 1 character,
                je compress1            ;not worth compressing
                mov bx,filepart1        ;prepair
                mov si,maps1            ;prepair converting
                mov ah,0                ;0

change1:        mov al,[bx]             ;read
                mov bp,ax               ;prepair pointer
                mov al,[bp+si]          ;check with list
                mov [bx],al             ;replace
                inc bx                  ;move up 1
                loop short change1      ;continue

                dec dx                  ;fix max count
                mov dh,0                ;clear value
                mov ax,0                ;prepair count
                mov cx,8                ;counter (in case failed?)
bitstart:
                rol dx,1                ;start count
                cmp dh,1                ;see if anything changed
                je short bitsdone       ;if it has, size completed
                inc ax                  ;if not, add one to bits saved
                loop short bitstart     ;max out count (not stuck :) )

bitsdone:       cmp ax,0                ;see if ANY bits were saved
                jne short bitsgood
                mov bx,[file2]          ;set up write
                mov cx,[buffer]         ;set up size?
                mov dx,filepart1        ;set up location
                mov ah,write            ;write
                int 21h                 ;access
                jmp compress1           ;go on to next section

bitsgood:       mov ah,8                ;byte size (8)
                sub ah,al               ;see bits needed
                mov dx,ax
                xchg dl,dh              ;(DL bits size, DH saved)
                mov si,filepart1        ;set up locators
                mov di,filepart2

                mov ax,0                ;prepair
                mov bp,filepart1
                add bp,[buffer]
                add bp,8                ;1 extra byte in case
comp1:          mov al,8                ;set up byte (starting point)
comp2:          cmp ah,0                ;see if need a new character
                jne short notnew        ;no new character

                cmp bp,si               ;see if at max
                je short comp3          ;if is, skip to ending

                mov bl,[si]             ;grab character
                inc si                  ;increase location
                xchg cl,dh              ;adjust to the right
                shl bl,cl
                xchg cl,dh

                mov ah,cl               ;grab character bit size
notnew:         cmp al,ah
                jb short comp_small

                xchg ah,cl
                shl bx,cl               ;grab bits
                sub al,cl               ;take out as neccesary
                mov cl,0                ;take out bits
                xchg ah,cl
                jmp short comp2         ;go through bits again
comp_small:     xchg cx,ax
                shl bx,cl               ;move over as nessisary
                xchg cx,ax
                sub ah,al               ;take out bits used
                mov [di],bh             ;save new byte
                inc di                  ;move up 1
                jmp short comp1         ;start at new character

comp3:          mov ax,[buffer]         ;to this size
                mov bx,dx               ;prepair numbers
                mov dx,0
                mov bh,dh
                mul bx                  ;find size so far

                mov bl,8                ;make byte
                div bx                  ;see true size
                cmp dx,0                ;see if any remainder
                je short sizefind1      ;no remainder
                inc ax                  ;add one for the remainder (ending?)
sizefind1:      mov cx,ax               ;get size

skipcomp:       mov ah,write            ;set up write
                mov bx,[file2]          ;to the second file
                mov dx,filepart2        ;as the compressed part
                int 21h                 ;write
                jmp compress1           ;start at reading again

Closefiles:     mov ah,close
                mov bx,[file1]
                int 21h
                mov ah,close
                mov bx,[file2]
                int 21h
                jmp short ending
errmess:        mov dx,err              ;display error message
                mov bx,1
                mov ah,Ptext
                int 21h

ending:         mov dx,base1            ;and how used
                mov bx,1
                mov ah,Ptext
                int 21h
                popa
                ret

skiptomessage:  mov dx,base0            ;and how used
                mov bx,1
                mov ah,Ptext
                int 21h
                jmp short ending        ;goto ending

base0:   db "Comptest [Input] [Output] -D -S(1024 auto size)",13,10
         db "    -D De-compress, Rather then Compress",13,10
         db "    -S Buffersize for Compress",13,10
         db "$"

base1:   db 13,10
         db "Thank you for using my Compression program V2.3",13,10, "$"

base2:   db " Comptest Compression",13,10
         db " - Assembly, fast, quick",13,10
         db " - Compression Sucks, Simple",13,10
         db " - But it's cool!! :)",13,10,"$"

Err:     db "An error has occured, while trying to access a file",13,10
         db "Please check over to make sure you typed it all in correctly",13,10,13,10,"$"

bufferr: db "Sorry, but that is above this program's buffer limits",13,10,"$"

badfile: db "Sorry, this is an INVALID file format.",13,10,"$"

files:   db 0,0,0,0,0,0,0 ;file locators? (for the algorythmn)
file1:   dw 0
file2:   dw 0
Status:  db 0
Buffer:  dw 1024
filesize: dw 0,0

maps1:  db dup(256) 0
maps2:  db dup(32) 0

title0: db "Era2"
$title1

filepart1:
+cpx Maxbuff

filepart2:

;variables to use
^Open 3Dh
^Create 3Ch
^Close 3Eh
^Read 3Fh
^Write 40h

^ReadOnly 0
^WriteOnly 1
^Random 2

^Ptext 9

@Maxbuff 30000

^Pointer 42h
@PointerSof 4200h
@PointerCur 4201h
@PointerEof 4202h
