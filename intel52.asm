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
                cmp ax,5000h    ;320K (64 program,64 insts,64 labels,64 data)
                jae short memgood ;64K direct
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
                add ax,1000h
                mov [direct_segment],ax ;segment for Direct information

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

chalkboard_3:   cmp byte ptr [si],"M"
                jne short chalkboard_4
                mov byte ptr [fst],2    ;make?

chalkboard_4:   cmp byte ptr [si],"F"
                jne short chalkboard_5
                mov byte ptr [fst],1    ;use?

chalkboard_5:   cmp byte ptr [si],"D"
                jne short chalkboard_5A
                mov byte ptr [dbg],1    ;debug text (screen)?

chalkboard_5A:  cmp byte ptr [si],"O"
                jne short chalkboard_5B
                mov byte ptr [optimize],1    ;optimize?

chalkboard_5B:  cmp byte ptr [si],"Q"
                jne short chalkboard_5C
                mov byte ptr [optimize],2    ;Quick?
                mov byte ptr [loopback],3    ;skip out, make smaller loop

chalkboard_5C:  cmp byte ptr [si],"T"
                jne short chalkboard_6
                mov byte ptr [tmpkeep],1     ;optimize?

chalkboard_6:   cmp byte ptr [si],0     ;ending?
                je short chalkboard_7

                inc si
                jmp short chalkboard_3

chalkboard_7:
;enough parts.. start here

        mov dx,gs               ;set up
        call near getline_clear ;prepairs 'getline' as required (1 segment)

        mov dx,es               ;set up
        call near getline_clear ;prepairs 'getline' using LABEL segment
;until the includes are complete..

        mov ah,open             ;paste in for testing, need to add the rest here
        mov al,readonly         ;later on
        mov dx,Instruction_set
        int 21h
        jc short nosets         ;check for error, if so, clear
        push ax
        mov [file1],ax

        mov bx,ax               ;get file handle
        push fs                 ;set up segment
        pop ds

        mov ah,read             ;read it
        mov cx,-1               ;full size
        xor dx,dx               ;at 0
        int 21h
        push cs
        pop ds                  ;restore segments
        mov bx,[file1]          ;close instruction set
        mov ah,close            ;unneeded now
        int 21h
        jmp short issets        ;skip past the clearing.
;prepair debug window now..

nosets: mov dx,err4             ;error with set file
        mov ah,ptext            ;print it out
        int 21h

        push fs                 ;set up segment
        pop ds

        xor cx,cx               ;clear out counter
        mov ax,cx               ;clear information
preclear_set:mov si,cx
        mov ds:[si],al          ;clear it
        loop short preclear_set

        push cs
        pop ds                  ;return DS to proper segment

;check for setfiles now :)
        mov ax,internal_set_L     ;get number (if exsists)
        cmp ax,0
        je short issets

        mov ah,ptext
        mov dx,err5             ;tell redirect
        int 21h

        mov si,internal_set     ;ok, get source
        xor di,di               ;locate destination
        mov cx,internal_set_L   ;put in length

internalset_loop:mov al,cs:[si] ;get byte
        mov fs:[di],al          ;write byte
        inc si
        inc di                  ;increase
        loop short internalset_loop

;set check added here
issets: mov bx,fs:[ident_verset]        ;skip past date
        cmp byte ptr fs:[bx+4],Ver_Major;check major
        ja short issets_vererr
        cmp byte ptr fs:[bx+5],Ver_Minor;check minor
        ja short issets_vererr
        cmp byte ptr fs:[bx+6],Ver_Revision     ;check revision
        ja short issets_vererr
        jmp short issets_vernoerr

issets_vererr:mov ah,ptext      ;print on screen
        mov dx,verseterr        ;the error
        mov bx,1                ;screen? (Making sure)
        int 21h                 ;go do it

issets_vernoerr:
        mov dx,debugfile        ;file name 'debug'
        mov ah,create           ;create it
        mov al,writeonly        ;write only
        mov cx,0                ;clear attributes
        int 21h
        jnc short debug_txt
        mov byte ptr [file3],1  ;since error set up screen
        mov byte ptr [dbg],0    ;will be screened instead
debug_txt:mov [file3],ax

        mov bx,files            ;prepair for working

;kill all comments
;kill spaces

        mov ah,open             ;open source file
        mov dx,[bx]
        mov al,readonly         ;readonly
        int 21h
        jnc short beginning1    ;quit if error
        mov ah,ptext            ;tell error
        mov bx,1                ;to the screen
        mov dx,err              ;the error message
        int 21h
        jmp near ending
beginning1: mov [file1],ax      ;save handle
        mov ah,create           ;open temporary file
        xor cx,cx
        mov dx,tempfile1        ;second file
        int 21h
        jnc short beginning2    ;quit if error
        
        mov ah,ptext            ;tell error
        mov bx,1
        mov dx,err3
        int 21h
        call near closefirst
        jmp near proguse2
beginning2: mov [file2],ax              ;save handle
;ok, this is where 'includes' are placed.
;;some bug is here, so for sakes of simpleness, this SHOULD fix it
;pushad
beg_inc:mov bx,cs:[file1]               ;from read
        mov dx,gs
        call near getline               ;get 1 line

        push gs                         ;set information DATA
        pop ds

;ok, now the fun parts :)

        cmp cx,0                        ;check end of file
        je near beg_inc_found
;write it first
        xor dx,dx                       ;0 location
        mov ah,write                    ;write it out
        mov bx,cs:[file2]               ;output
        int 21h                         ;write it

;now check for anything odd :P
        xor bx,bx                        ;clear pointer

eg_inc_maybe:push bx                    ;backup start
        cmp byte [bx],"*"               ;check full
        jne short beg_inc_notfull

;ok, seek out ending of file name
        mov dx,bx
        inc dx

        mov cx,ax                       ;limit (bytes written :P)
beg_inc_full_nsp:cmp byte [bx],13       ;ending check?
        je short beg_inc_full_sp
        cmp byte [bx],10                ;ending check
        je short beg_inc_full_sp
        inc bx                          ;onwards
        loop short beg_inc_full_nsp     ;oh boy, a limit :P
        jmp short beg_inc_notfull       ;skip out

beg_inc_full_sp:mov byte [bx],0         ;put null pointer on

        mov ah,open                     ;open file
        xor cx,cx                       ;not sure
        mov al,readonly                 ;read it only (DX SET)
        int 21h
        jnc short beg_inc_fullnerr      ;on error, just skip it

        mov byte [bx],":"               ;put up basic message
        mov word [bx+1],"Mi"
        mov word [bx+3],"ss"
        mov word [bx+5],"in"
        mov word [bx+7],"g."
        mov word [bx+9],0A0Dh
        mov byte [bx+11],"$"

        mov ah,ptext
        int 21h                         ;print message
        jmp short beg_inc_notfull
beg_inc_fullnerr:mov cs:[file4],ax      ;write out handle

beg_inc_full_read:mov bx,cs:[file4]     ;from read
        mov dx,es
        call near getline               ;get 1 line

        push es                         ;set information LABEL
        pop ds                          ;for this include

        cmp cx,0                        ;check end of file
        je short beg_inc_full_done

        mov ah,write                    ;ok write to file
        mov bx,cs:[file2]               ;to temp file
        mov dx,0                        ;starting 0 (assumed)
        int 21h         ;assumed data isn't as nice to work with
        jmp short beg_inc_full_read

beg_inc_full_done:mov ah,close          ;close include file
        mov bx,cs:[file4]               ;Include handle
        int 21h                         ;do it

        push gs                         ;return now
        pop ds
beg_inc_notfull:pop bx          ;return start
;check other includes..
        cmp word [bx],"<I"      ;includes part
        je short beg_inc_found
        cmp word [bx],"<i"      ;other check (lowercase)
        jne near beg_inc

beg_inc_found:cmp byte cs:[includez],1;check if done this before
        je near beg_inc_next    ;if so, had to be through EOF executed, END

        cmp cx,0                ;check end of file?
        je short beg_inc_nosave
        push cx

        mov ax,PointerCur       ;prepair cursor save
        xor dx,dx
        mov cx,dx               ;clear pointer
        mov bx,cs:[file1]       ;get pointer
        int 21h                 ;go get it
        pop cx                  ;return count (EOF?)


;DX is low?
        mov bx,gs:[gleft]       ;get buffer size
        sub bx,gs:[gstat]       ;gone through already

        sub ax,bx               ;back up remainder in buffer
        sbb dx,0                ;minus carry

        push dx                 ;preserve pointer
        push ax

;probably want to clean it out..

        mov dx,es
        call getline_clear      ;ok, cleared this
        mov dx,gs
        call getline_clear      ;cleared that
beg_inc_nosave:push cx          ;save number (Tells later to resume, or end)

;ok, start at beginning of file again, look only for # and !'s :)

        mov ax,pointerSof
        xor cx,cx
        xor dx,dx               ;start at beginning
        mov bx,cs:[file1]
        int 21h                 ;point it

        mov word cs:[file4],0   ;clear handle, special

beg_inc_pt2:mov bx,cs:[file1]               ;from read
        mov dx,gs
        call near getline               ;get 1 line

        push gs                         ;set information DATA
        pop ds

        cmp cx,0
        je near beg_inc_pt2_end

;ok, now for 1 of 2 functions :)
        xor bx,bx                       ;clear

        cmp byte [bx],"#"               ;look for include
        jne short beg_inc_pt2_notinc

        push cx

;ok, the easy part..
        cmp word cs:[file4],0   ;check for open file
        je short beg_inc_pt2_inc1

        mov ah,close            ;close this file
        mov bx,cs:[file4]       ;since it's all done
        int 21h                 ; :)

beg_inc_pt2_inc1:pop cx                 ;limit, length
beg_inc_pt2_inc_nsp:cmp byte [bx],13    ;ending check?
        je short beg_inc_pt2_inc_sp
        cmp byte [bx],10                ;ending check
        je short beg_inc_pt2_inc_sp
        inc bx                          ;onwards
        loop short beg_inc_pt2_inc_nsp  ;oh boy, a limit :P
        jmp short beg_inc_pt2_notinc    ;skip out

beg_inc_pt2_inc_sp:mov byte [bx],0      ;put null pointer on

        mov ah,open                     ;open file
        xor cx,cx                       ;not sure
        mov al,readonly                 ;read it only (DX SET)
        mov dx,cx                       ;put up pointer
        inc dx                          ;fix and ready

        int 21h
        jnc short beg_inc_pt2_inc_nerr  ;on error, just skip it

        mov byte [bx],":"               ;put up basic message
        mov word [bx+1],"Mi"
        mov word [bx+3],"ss"
        mov word [bx+5],"in"
        mov word [bx+7],"g."
        mov word [bx+9],0A0Dh
        mov byte [bx+11],"$"

        mov ah,ptext
        int 21h                         ;print message
        jmp short beg_inc_pt2_notinc
beg_inc_pt2_inc_nerr:mov cs:[file4],ax  ;write out handle
;that was easy :)

beg_inc_pt2_notinc:cmp byte [bx],"!"    ;look for include
        jne near beg_inc_pt2_notfun
;now the HARD part..


        cmp word cs:[file4],0           ;not open
        je near beg_inc_pt2_notfun      ;and skip

        pushad                  ;save all, for simpleness

;limit the size of the compare first.. find the first space.
;bx should be 0 still

;cx is set
beg_inc_pt2_sp:cmp byte [bx],13         ;find space
        je short beg_inc_pt2_fsp
        cmp byte [bx],10                ;find space
        je short beg_inc_pt2_fsp
        cmp byte [bx]," "        ;find space
        je short beg_inc_pt2_fsp

        inc bx
        loop short beg_inc_pt2_sp
        jmp short beg_inc_pt2_done      ;skip out

beg_inc_pt2_fsp:mov si,bx               ;save counter
        mov dx,es
        call near getline_clear

        mov ax,pointersof               ;start at beginning of file
        xor cx,cx                       ;cleared
        xor dx,dx
        mov bx,cs:[file4]               ;handle of the include file
        int 21h                         ;do it

        xor di,di                       ;counter, special

beg_inc_pt2_inc2:mov bx,cs:[file4]      ;from read, label
        mov dx,es
        call near getline               ;get 1 line
        inc di                          ;line counter

        push es                         ;set information DATA
        pop ds

        cmp cx,0                        ;anything in line?
        je short beg_inc_pt2_donez      ;skip out

        mov cx,si       ;set limit
        xor bx,bx       ;clear pointer

beg_inc_pt2_inc3:mov al,es:[bx]         ;get byte
        cmp gs:[bx],al                  ;compare it
        jne short beg_inc_pt2_inc2
        inc bx                          ;increase check
        loop short beg_inc_pt2_inc3     ;loop the compare
;if you get this far, it all matched enough.
;we should be on a space.. 
        inc bx                          ;fix position
        mov dx,es                       ;put segment up

        mov si,bx                       ;put information in
        call near getnum                ;grab number

        push ax                         ;save number

beg_inc_pt2_inc4:cmp byte [bx]," "      ;find separator
        je short beg_inc_pt2_inc5
        inc bx                          ;mov pointer
        jmp short beg_inc_pt2_inc4      ;continue on

beg_inc_pt2_inc5:inc bx                 ;fix from space

        mov si,bx                       ;put information in
        call near getnum                ;grab number

        pop bp                          ;set number up
        mov si,ax                       ;set up number
;ok, bp contains start, si contains finnish

beg_inc_pt2_inc6:mov bx,cs:[file4]      ;from read, label
        mov dx,es
        call near getline               ;get 1 line
        inc di                          ;line counter

        push es                         ;set information DATA
        pop ds

        cmp cx,0                        ;anything in line?
        je short beg_inc_pt2_done       ;skip out

        cmp di,bp                       ;low enough?
        jb short beg_inc_pt2_inc6

        cmp di,si                       ;find ending
        ja short beg_inc_pt2_done

        mov bx,cs:[file2]               ;output to temp
;cx is here already
        xor dx,dx                       ;pointer
        mov ah,write                    ;write it
        int 21h
        jmp short beg_inc_pt2_inc6      ;loop till done

beg_inc_pt2_donez:mov ah,ptext          ;print text
        push gs
        pop ds

        mov byte [bx],":"               ;put up basic message
        mov word [bx+1],"Un"
        mov word [bx+3],"kn"
        mov word [bx+5],"ow"
        mov word [bx+7],"n "
        mov word [bx+9],"Fu"
        mov word [bx+11],"nc"
        mov word [bx+13],"ti"
        mov word [bx+15],"on"
        mov word [bx+17],0A0Dh
        mov byte [bx+19],"$"

        mov dx,1                        ;starting at
        int 21h                         ;print

beg_inc_pt2_done:popad          ;return all.. whatever :P
beg_inc_pt2_notfun:jmp near beg_inc_pt2 ;ok, next line

beg_inc_pt2_end:pop cx                  ;see if anything left
        cmp cx,0
        je short beg_inc_next   ;ok, skip out
;No? Ok, return to previous spot..

        pop dx
        pop cx                  ;pull out pointer info

        mov ax,pointersof       ;Start of file
        mov bx,cs:[file1]       ;of temp file
        int 21h                 ;and continue where we left off at..

        mov byte cs:[includez],1;so we don't put 2 of everything..

        jmp near beg_inc        ;back to getting lines..

;at this point, let's move onto the rest of the process (temp to temp)
beg_inc_next:push cs            ;prepair segments
        pop ds                  ;closes both files now

        call near closeboth     ;close both temp files

        cmp word cs:[file4],0   ;check for open file
        je short beg_inc_next_file

        mov ah,close            ;close this file
        mov bx,cs:[file4]       ;since it's all done
        int 21h                 ; :)

beg_inc_next_file:mov ah,open   ;open source file
        mov dx,tempfile1
        xor cx,cx
        mov al,readonly         ;readonly
        int 21h
        jnc short beg_inc_x1    ;quit if error
        mov ah,ptext            ;tell error
        mov bx,1                ;to the screen
        mov dx,err3             ;the error message
        int 21h
        jmp near ending
beg_inc_x1: mov [file1],ax      ;save handle

        cmp byte [optimize],1   ;check if the loops are to be done?
        jne short beg_inc_notop

        mov bx,ax       ;and handle
        mov ax,pointereof;get length
        xor cx,cx
        xor dx,dx       ;clear pointer
        int 21h         ;ok, has length
        push ax
        push dx

        mov ax,pointersof;start of file
        mov bx,[file1]
        xor cx,cx
        xor dx,dx
        int 21h         ;ok, at beginning again

        xor eax,eax
        pop ax          ;get high order
        shl eax,16      ;mov register
        pop ax          ;ok, saved it all
        xor edx,edx
        mov ecx,10000   ;10,000 for check (for every loop)
        div ecx

        cmp word ax,255 ;override size
        jb short beg_inc_notop1
        mov word ax,255 ;max size set
beg_inc_notop1: cmp ax,loopback_minimum ;check for minimum
                jb short beg_inc_notop  ;failed minimum, and stays where it is

beg_inc_notop2:mov [loopback],ax        ;save number for reloops

beg_inc_notop:mov ah,create             ;open temporary file
        xor cx,cx
        mov dx,tempfile2        ;second file
        int 21h
        jnc short beg_inc_x2    ;quit if error
        
        mov ah,ptext            ;tell error
        mov bx,1
        mov dx,err3
        int 21h
        call near closefirst
        jmp near proguse2

beg_inc_x2: mov [file2],ax              ;save handle

;since ucase only has to be used once, let's do it here..
;job, open up, capitalize everything (except for in "'s)
beginning3: mov bx,cs:[file1]           ;from read
        mov dx,gs
        call near getline               ;get 1 line

        push gs                         ;set information DATA
        pop ds

        cmp cx,0
        je near beginning4
        cmp cx,2
        jbe short beginning3 ;sift out empty lines

;ok, since i had trouble with this ALREADY it will find empty lines AGAIN
        xor bx,bx       ;prepair (first)
        mov dx,bx

BLANKITY0:cmp byte ptr [bx],10  ;ending check
        je short BLANKITY1
        cmp byte ptr [bx],13    ;skip
        je short BLANKITY2
        cmp byte ptr [bx]," "   ;space check
        je short BLANKITY2
        inc dx                  ;fail count
BLANKITY2:inc bx
        jmp short BLANKITY0     ;ok, skip the number, just go on
BLANKITY1:cmp dx,0
        je short beginning3     ;line SKIPPED because it's all spaces

        mov bx,0                ;check first letter
        cmp byte ptr [bx],"!"
        je short beginning3
        cmp byte ptr [bx],"#"
        je short beginning3     ;includes delete
        cmp byte ptr [bx],"*"
        je short beginning3
        cmp byte ptr [bx],59
        je short beginning3     ;skip whole lines of comments

        xor bx,bx               ;prepair (first)
        mov ax,cx
        mov dx,1
comment1:cmp byte ptr [bx],34   ;check ''
        jne short comment1_0
        neg dx                  ;reverse
comment1_0:cmp dx,1             ;see if in range?
        jne short comment1_1    ;if not in range, skip
        cmp byte ptr [bx],59    ;find comment (right side)
        je short comment2_0     ;if comment, move on
comment1_1:inc bx
        loop short comment1
        mov bx,ax               ;return
        cmp byte ptr [bx-2],13  ;is this the ending?
        jne short comment2_0
        dec bx
        mov byte ptr [bx-1],10  ;take it out
comment2_0:dec bx
        cmp byte ptr [bx],10     ;at end too far?
        jne short comment2a
        jmp short comment2_0
comment2a:mov cx,bx             ;current location as pointer
comment2:cmp byte ptr [bx]," "  ;check for space (end)
        jne short comment3
        dec bx
        loop short comment2
        jmp near beginning3
comment3:inc bx
        mov byte ptr [bx],10    ;save
        mov ax,bx               ;save
        mov cx,bx               ;length prepair
        xor bx,bx               ;beginning
comment4:cmp byte ptr [bx]," "  ;begginning space remove
        jne short comment5
        inc bx
        loop short comment4
        jmp near beginning3
comment5:mov cx,ax
        sub cx,bx       ;make new size
        push cx
        push bx

        mov dx,1
ucase_start:mov al,[bx]     ;get letter
        cmp al,34       ; check "'s
        jne short ucase_mid ;if not, move on
        neg dx
ucase_mid:cmp al,9
        jne short ucase_mid1;exchange the tabs for spaces
        mov al," "
ucase_mid1:cmp al,13
        jne short ucase_mid2;exchange the ending for space
        mov al," "
ucase_mid2:cmp dx,1         ;see if skipped?
        jne short ucase_end ;if not 1, skip

        cmp al,"a"      ;not in area, and works.
        jb short ucase_end
        cmp al,"z"      ;check in range
        ja short ucase_end
        sub al,32       ;fix
        mov [bx],al
ucase_end: inc bx
        loop short ucase_start
        pop dx          ;return starting address
        pop cx          ;size to save
        inc cx          ;fix bug
        mov bx,cs:[file2] ;output temp
        mov ah,write
        int 21h         ;write new section
        jmp near beginning3

beginning4:push cs
        pop ds  ;return data
;continue the work here
        call near closeboth

        mov ah,open             ;open source file
        mov dx,tempfile2
        mov al,readonly
        int 21h
        jnc short middle1       ;quit if error
        mov ah,ptext            ;tell error
        mov bx,1
        mov dx,err3
        int 21h
        jmp near ending
middle1:mov cs:[file1],ax       ;save handle

        mov bx,files            ;prepair for working
        mov dx,[bx+2]
        mov ah,create           ;open destination file
        xor cx,cx
        int 21h
        jnc short middle2       ;quit if error
        
        mov ah,ptext            ;tell error
        mov bx,1
        mov dx,err2
        int 21h
        call near closefirst
        jmp near ending
middle2:mov cs:[file2],ax          ;save handle
        xor ax,ax
        mov word ptr es:[lab],1 ;1 label (auto, cpx)
        mov word ptr es:[labcpx],ax;clear location of cpx?
        mov bx,labstart
        mov word ptr es:[bx],0403h ;save start

        mov word ptr es:[bx+2],"CP"     ;save fist half
        mov byte ptr es:[bx+4],"X"      ;save start
        mov word ptr es:[bx+5],ax       ;save zero
        mov word ptr es:[bx+7],ax       ;save zero

        cmp byte [fst],1                ;fastfile make
        jne short middle3               ;skip since it's not real

        mov bx,files            ;prepair for working
        mov si,[bx]             ;grab source file
        mov cx,120              ;limit checks

Makeu_search:cmp byte [si],"."   ;look for period
        je short Makeu_Ext1
        cmp byte [si],0         ;look for ending
        je short Makeu_Ext1
        inc si                  ;ok, add on
        loop short Makeu_Search
        jmp short middle3       ;skipping (Unknown)

Makeu_Ext1:mov word [si],".f"
        mov word [si+2],"st"
        mov byte [si+4],0

        mov ah,open             ;access fastfile
        mov dx,[bx]             ;first section
        mov al,readonly         ;readonly
        xor cx,cx               ;clear details
        int 21h
        jc short middle3        ;skip it all, don't worry

        push ax
        mov bx,ax               ;set up handle
        mov ah,read             ;read from file
        mov cx,-1               ;set for max size (who cares if it's enough :P)
        mov dx,0                ;set location to write to

        push ds                 ;preserve
        push es                 ;set for writing
        pop ds                  ;ready to write

        int 21h                 ;let her rip
        pop ds                  ;return DS

        pop bx                  ;get handle
        mov ah,close            ;close the file
        int 21h                 ;all done :)

;oh, as another note.. clear out the LOOPBACK since it's not needed :)
        mov word cs:[loopback],0;cleared :)
        jmp near half2_2        ;skip out

middle3:mov bx,cs:[file1]       ;from read
        mov dx,gs
        call near getline    ;get 1 line
;in this part, find and save labels..
;method suggested, move up as needed and save new part in middle (ES?)

        cmp cx,0
        je near half2

        mov bx,1        ;prepair base
        xor di,di       ;prepair di

        mov al,1        ;bytes needed for this
        cmp byte ptr gs:[di],"^"  ;1 byte, byte
        je short middle3_2

        mov al,2        ;bytes needed for this
        cmp byte ptr gs:[di],"@"  ;2 bytes, word
        je short middle3_2

        mov al,4        ;bytes needed for this
        cmp byte ptr gs:[di],"%"  ;4 bytes, doubleword (quad)
        je short middle3_2

        cmp byte ptr gs:[di],"$"  ;4 bytes, length
        je short middle3_2
;scanner for labels now
        dec bx

middle3_1:cmp byte ptr gs:[di]," "         ;instruction?
        je short middle3
        cmp byte ptr gs:[di],10
        je short middle3
        cmp byte ptr gs:[di],":"           ;label?
        je short middle4
        inc di
        jmp short middle3_1

middle3_2:cmp byte ptr gs:[di]," "         ;end of section?
        je short middle4
        cmp byte ptr gs:[di],10
        je short middle4
        inc di
        jmp short middle3_2
middle4: sub di,bx       ;find length?
        mov dx,di
        mov dh,al
        mov di,bx       ;prepair di

        mov si,labstart
        mov cx,es:[lab] ;get count

labsearch0:mov ax,es:[si]       ;get size (determine?)
        cmp al,dl               ;check
        jb near labadd          ;if smaller then text (passed)
        ja short labsearch2     ;if larger (still to go)
        push di                 ;equal, see if another match?
        push si                 ;save values
        push cx
        push ax
        xor cx,cx
        mov cl,dl

labsearch0_1:mov al,es:[si+2];get first letter
        cmp gs:[di],al  ;see if different
        je short labsearch1
        pop ax
        pop cx
        pop si
        pop di
        jmp short labsearch2
labsearch1:inc si
        inc di
        loop short labsearch0_1;continue loop
        pop ax          ;bug fix
        pop cx          ;equals, now skip
        pop si
        pop di
        jmp near middle3
labsearch2:add al,ah    ;get size
        mov ah,0        ;fix
        inc ax
        inc ax          ;header?
        add si,ax       ;prepair
        loop short labsearch0 ;continue
labadd: push di
        mov di,si       ;save location
        mov ax,dx       ;get info

        add al,ah       ;get size
        mov ah,0        ;fix
        inc ax
        inc ax          ;header?

        mov bx,ax       ;distance located
        cmp cx,0
        je short labadd2_2
labadd1:mov ax,es:[si]  ;get size (determine?)
        add al,ah       ;get size
        mov ah,0        ;fix
        inc ax
        inc ax          ;header?
        add si,ax       ;prepair
        loop short labadd1    ;continue

        mov bp,si       ;backup

        sub si,di       ;find difference
        mov cx,si
        mov si,bp
labadd2:dec si
        mov al,es:[si]  ;grab letter
        mov es:[si+bx],al;save a few spaces ahead
        loop short labadd2
labadd2_2:mov es:[si],dx;save label information
        pop di          ;location of label (0/1?) bug fix?
        mov cl,dl       ;prepair size (cx is 0 already)
labadd3:mov al,gs:[di]  ;get letter
        mov es:[si+2],al;save +2
        inc si
        inc di
        loop short labadd3

        mov cl,dh       ;prepair size (cx is 0 already)
        mov al,0        ;prepair
labadd4:mov es:[si+2],al;save +2
        inc si
        loop short labadd4    ;clear number to go by
        inc word ptr es:[lab] ;add another one to it
        jmp near middle3
half2:
;find and clarify CPX now :)
        mov si,labstart
        mov cx,es:[lab] ;get count

half2_0:mov ax,es:[si]  ;get size (determine?)
        cmp ax,0403h    ;information (CPX)
        jne short half2_1
        cmp word ptr es:[si+2],"CP"
        jne short half2_1
        cmp byte ptr es:[si+4],"X"
        jne short half2_1
        add si,5        ;fix, exact location (4 bytes)
        mov es:[labcpx],si      ;get count
        sub si,5
        jmp short half2_2       ;skip
half2_1:add al,ah       ;get size
        mov ah,0        ;fix
        inc ax
        inc ax          ;header?
        add si,ax       ;prepair
        loop short half2_0    ;continue

half2_2:mov bx,cs:[file1]       ;source
        mov ax,pointersof
        xor cx,cx
        mov dx,cx       ;clear
        int 21h

half2_3:mov byte [size_ident],0 ;size reset
        mov bx,cs:[file1]       ;from read
        mov dx,gs
        call near getline    ;get 1 line

        cmp cx,0
        je near half3_1

;set up a search and replace algorythmn here.

        cmp byte ptr gs:[0],"<"
        jne short half2_35
        call near specials
        jmp short half2_3

half2_35:call near replace

        mov bx,1        ;prepair base
        xor di,di       ;prepair di

        mov al,1        ;bytes needed for this
        cmp byte ptr gs:[di],"^"  ;1 byte, byte
        je short half2_5

        mov al,2        ;bytes needed for this
        cmp byte ptr gs:[di],"@"  ;2 bytes, word
        je short half2_5

        mov al,4        ;bytes needed for this
        cmp byte ptr gs:[di],"%"  ;4 bytes, doubleword (quad)
        je short half2_5

        cmp byte ptr gs:[di],"$"  ;4 bytes, length
        je short half2_5

        cmp byte ptr gs:[di],"-"  ;special, unknown
        je short half2_5
        cmp byte ptr gs:[di],"+"  ;Special, unknown
        je short half2_5

;scanner for labels now
        dec bx

half2_4:cmp byte ptr gs:[di]," "         ;instruction?
        je near half3_2more              ;considered an instruction
        cmp byte ptr gs:[di],10
        jne near half2_4x                ;identify here
        xor di,di
        jmp near half3
half2_4x:cmp byte ptr gs:[di],":"        ;label?
        je short half2_6                 ;save location CPX
        inc di
        jmp short half2_4

half2_5:cmp byte ptr gs:[di]," "         ;end of section?
        je short half2_6
        cmp byte ptr gs:[di],10
        je short half2_6
        inc di
        jmp short half2_5
half2_6:mov bp,di
        inc bp
        sub di,bx       ;find length?
        mov dx,di
        mov dh,al
        mov di,bx       ;prepair di

        mov si,labstart
        mov cx,es:[lab] ;get count

labs0:  mov ax,es:[si]  ;get size (determine?)
        cmp al,dl       ;check
        jb near half3   ;if smaller then text (passed)
        ja near labs2   ;if larger (still to go)
        push di         ;equal, see if another match?
        push si         ;save values
        push cx
        push ax
        xor cx,cx
        mov cl,dl

labs0_1:mov al,es:[si+2];get first letter
        cmp gs:[di],al  ;see if different
        je short labs1
        pop ax
        pop cx
        pop si
        pop di
        jmp near labs2
labs1:  inc si
        inc di
        loop short labs0_1    ;continue loop
        pop ax
        pop cx          ;equals, now skip
        pop si
        pop di
;get to number (if one?) determine if number needed? $(current lastused-CPX)
;no bugs, continue here..
        cmp di,1        ;label?
        je short labs_x0

        mov bx,dx
        mov bh,0        ;set up add
        inc bx
        inc bx          ;pointer fix
        add bx,si       ;prepair
        mov eax,[thecpx]        ;grab number
        mov es:[bx],eax         ;save label locator
        mov [cpxstart],eax      ;$
        mov di,bp
        jmp near half3  ;identify section

labs_x0:cmp byte ptr gs:[0],"$" ;see if length
        jne short labs_x1
        mov eax,[thecpx]        ;get current location
        sub eax,[cpxstart]      ;get start, and difference
        jmp short auto_bx       ;skipper (on to prepair)

labs_x1:xchg si,bp      ;prepair read
        push dx
        mov dx,gs
        call near getnum     ;get number (eax)
        pop dx
        xchg si,bp      ;return

auto_bx:push dx
        mov dh,0        ;get location
        inc dx
        inc dx
        add si,dx       ;set up
        pop dx

        cmp byte ptr gs:[0],"-"         ;remove checks
        jne short not_Subtraction
        cmp dh,1
        jne short subtract_1
        sub es:[si],al
subtract_1:cmp dh,2
        jne short subtract_2
        sub es:[si],ax
subtract_2:cmp dh,4
        jne short subtract_3
        sub es:[si],eax
subtract_3:cmp es:[labcpx],si
        jne near half2_3;little check (makes an easy skip)
        sub [thecpx],eax;save new CPX info
        jmp near half2_3;now let's go again

not_subtraction:cmp byte ptr gs:[0],"+"         ;remove checks
        jne short not_Addition
        cmp dh,1
        jne short addition_1
        add es:[si],al
addition_1:cmp dh,2
        jne short addition_2
        add es:[si],ax
addition_2:cmp dh,4
        jne short addition_3
        add es:[si],eax
addition_3:cmp es:[labcpx],si
        jne near half2_3;little check (makes an easy skip)
        add [thecpx],eax;save new CPX info
        jmp near half2_3;now let's go again

not_addition:cmp dh,1
        jne short not1
        mov es:[si],al  ;save number
not1:   cmp dh,2
        jne short not2
        mov es:[si],ax  ;save number
not2:   cmp dh,4
        jne short not4
        mov es:[si],eax ;save number
not4:   cmp es:[labcpx],si
        jne near half2_3;little check (makes an easy skip)
        mov [thecpx],eax;save new CPX info
        jmp near half2_3;now let's go again

labs2:  add al,ah       ;get size
        mov ah,0        ;fix
        inc ax
        inc ax          ;header?
        add si,ax       ;prepair
        loop near labs0 ;continue (a cheat. i will fix later)

half3_2more:xor di,di
half3:  cmp byte ptr cs:[loopback],0
        jne short still_loopback
        pusha
;the debugger set
        mov si,di
half3_debug0:cmp byte ptr gs:[di],10
        je short half3_debug1
        inc di
        jmp short half3_debug0
half3_debug1:sub di,si          ;get length
        cmp di,0
        je short still_loopbacks1;skip, for labels only?

        call near debug_thecpx       ;get number location

        mov cx,8
        sub cx,[debugcpx_needed];prepair true size?

        mov bx,[file3]  ;debug
        mov dx,debugcpx
        add dx,cx       ;size fix? (debugcpx-needed)
        mov cx,[debugcpx_needed];fix sizers
        inc cx          ;size fix2 (for print)
        mov ah,write
        int 21h

        cmp byte ptr cs:[dbg],1 ;screen?
        jne short half3_dbg_debug1

        mov cx,8
        sub cx,[debugcpx_needed];prepair true size?

        mov bx,1        ;screen
        mov dx,debugcpx
        add dx,cx       ;size fix? (debugcpx-needed)
        mov cx,[debugcpx_needed];fix sizers
        inc cx          ;size fix2 (for print)
        mov ah,write
        int 21h

half3_dbg_debug1:mov cx,di      ;set length
half_3_rmsp:cmp byte gs:[si]," ";see if byte
        jne short half3_dbg_removespaces
        inc si
        loop short half_3_rmsp
half3_dbg_removespaces:mov di,cx ;set length
        mov dx,si       ;set data

        mov bx,[file3]  ;set 'debug'
        mov ah,write
        push gs         ;set print
        pop ds
        int 21h         ;print

        cmp byte ptr cs:[dbg],1 ;debug?
        jne short still_loopbacks

        mov cx,di       ;set length
        mov dx,si       ;set data
        mov bx,1        ;set 'screen'
        mov ah,write
        int 21h         ;print

still_loopbacks:push cs ;return
        pop ds
still_loopbacks1:popa
still_loopback:call near identify            ;identify the instruction
        xor ecx,ecx
        mov cl,[size_ident]
        mov eax,[thecpx]                ;get size
        add eax,ecx                     ;add on

        mov cx,[direct_size_ident]      ;direct size, if any
        add eax,ecx                     ;add on

        mov [thecpx],eax                ;save new value
        mov bx,es:[labcpx]
        mov es:[bx],eax                 ;save cpx info

        cmp byte ptr cs:[loopback],0    ;write error comment
        jne short still_loopback1

        cmp byte gs:[di],10             ;at end?
        je short still_loopback1        ;skip out, it's a label

        pusha
        mov ax,err_ident_L
        xor cx,cx
        mov dx,cx                       ;clear paths
        mov cl,[err_ident]
        mul cx                          ;multiply
        add ax,err0_ident               ;prepair
        mov dx,ax
        mov si,dx                       ;location, find lenths
still_loopback_0:cmp byte ptr [si],10   ;check length
        je short still_loopback_1
        inc si
        jmp short still_loopback_0
still_loopback_1:mov cx,si                       ;prepair and do size
        sub cx,dx
        inc cx                          ;needed????!?!?!?!?
        mov ah,write
        mov bx,[file3]                  ;for 'debug'
        int 21h

        cmp byte ptr [dbg],1            ;see if debug to screen?
        jne short still_loopback0
        mov ax,err_ident_L
        xor cx,cx
        mov dx,cx                       ;clear paths
        mov cl,[err_ident]
        mul cx                          ;multiply
        add ax,err0_ident               ;prepair
        mov dx,ax
        mov ah,ptext
        mov bx,1                        ;screen
        int 21h
still_loopback0:popa
still_loopback1:cmp byte ptr cs:[loopback],0    ;save info
        jne short still_loopback2

        cmp byte [direct_on_ident],1    ;see if direct (special)
        jne short still_notdirect
;direct_size_ident
        pusha
        mov cx,[direct_size_ident]
        mov bx,[file2]                  ;output
        mov ah,write                    ;write it
        mov dx,[direct_segment]
        mov ds,dx                       ;set up data
        xor dx,dx                       ;beginning start

        int 21h
        mov dx,cs
        mov ds,dx                       ;return regular data
        popa
        jmp short still_loopback2
still_notdirect:pusha
        mov dx,real_ident               ;prepair write
        xor ax,ax                       ;clear
        mov al,[start_ident]            ;get start
        sub dx,ax                       ;prepair
        mov al,[size_ident]             ;get size
        mov cx,ax
        mov bx,[file2]                  ;output
        mov ah,write                    ;write it
        int 21h
        popa
still_loopback2:jmp near half2_3        ;continue half loop

half3_1:cmp byte ptr cs:[loopback],0
        je short half3_2
        dec word cs:[loopback]          ;decrease one (next)
;wait a sec, clear out CPX (override here, special :) )

        xor eax,eax                     ;clear number (0)
        mov cs:[thecpx],eax             ;CPX cleared

        jmp near half2_2                ;continue at start

half3_2:call near closeboth
;ok, write FAST FILE if needed (M)

        cmp byte [fst],2                ;Ok, check for make.. (1)
        jne short half3_3

        mov bx,files            ;prepair for working
        mov si,[bx]             ;grab source file
        mov cx,120              ;limit checks

Make_search:cmp byte [si],"."   ;look for period
        je short Make_Ext1
        cmp byte [si],0         ;look for ending
        je short Make_Ext1
        inc si                  ;ok, add on
        loop short Make_Search
        jmp short half3_3       ;skipping (Unknown)

Make_Ext1:mov word [si],".f"
        mov word [si+2],"st"
        mov byte [si+4],0

        mov ah,create           ;open source file
        mov dx,[bx]
        mov al,writeonly        ;readonly
        xor cx,cx               ;clear details
        int 21h
        jnc short Make_Cfile    ;skip if error

        mov ah,ptext            ;print to screen
        mov dx,fsterr           ;error text
        int 21h
        jmp short half3_3       ;skip to end
Make_Cfile:push ax              ;preserve handle
        mov bx,ax               ;mov handle to proper section

;mini function meant to pull size info into CX
;pulled from REPLACE

        pusha
        mov si,labstart
        mov cx,es:[lab] ;get count

make_re_1:mov ax,es:[si]  ;get size (determine?)

        add al,ah       ;get size
        mov ah,0        ;fix
        add ax,2        ;skip header
        add si,ax       ;prepair
        loop short make_re_1
        mov bp,sp       ;grab pointer
        mov [bp+pcx],si ;replace CX with new value
        popa            ;quit

        mov ah,write            ;save data
        mov dx,0
        push es                 ;labels segment prepair
        pop ds
        int 21h

;these will be restored in a second
        pop bx                  ;pull out handle
        mov ah,close            ;close file
        int 21h                 ;done :)

;Deleting the TEMPS now
half3_3:push cs                 ;ensure right
        pop ds

        cmp byte [tmpkeep],1    ;see if keep
        je short half3_4        ;skip out

        mov ah,delete           ;prepair
        mov dx,tempfile1        ;set first temp
        int 21h                 ;delete

        mov ah,delete           ;prepair
        mov dx,tempfile2        ;set first temp
        int 21h                 ;delete

half3_4:jmp near ending         ;to the ending
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

;input Dx=data location(on first run, only meant for 1 file's usage)
;output, none
getline_clear:  pusha
                push ds
                mov ds,dx
                xor si,si       ;set up location
                mov cx,si       ;prepair 64K loop
                mov al,0        ;null
getline_clear_1:mov [si],al     ;clear (null)
                inc si
                loop short getline_clear_1
                pop ds
                popa            ;return
                ret

;input DX=Data location, BX file handle
;output cx, (size) (start at 0)
;the new GETLINE will need a 64K block, and will return a 1024
;block at a time. (512 if it's set for unicode translate)
getline:pusha
        push ds ;save old DS
        mov ds,dx       ;set up data, alone now

        xor di,di       ;prepair counter (premenant)
        mov bp,di       ;save starting pointer (write)
        mov si,[gstat]  ;get location
        mov cx,[gleft]  ;get
        mov ax,cx
        sub ax,si

        cmp ax,0
        jne short getline_1done
getline_1start: mov ah,read
        mov dx,gstuff
        xor cx,cx

        dec cx          ;make 64K check
        sub cx,gsize    ;get size (manual, slower but still fast)
        sub cx,4        ;other data (location and size)
        shr cx,1
        shl cx,1        ;make sure it's even (kills the first bit (far right))

        int 21h
        xor cx,cx       ;clear?
        mov [gstat],cx
        mov [gleft],ax  ;new value save
getline_1done:cmp ax,0
        je short getline_skip

        mov si,[gstat]  ;get location
        add si,gstuff   ;get REAL location
        mov cx,ax       ;get size remaining

        cmp byte ptr cs:[unicode],1     ;unicode in?
        je short getline_unicode
        cmp byte ptr cs:[unicode],2     ;turn unicode?
        je short getline_turn_unicode

;REGULAR IN
getline_2:mov al,[si]   ;grab letter
        mov ds:[bp],al  ;save letter
        inc si
        inc bp  ;letter moving saved
        inc di  ;increase information grabbed
        cmp al,10
        je short getline_2gotit
        loop short getline_2
        jmp short getline_1start;if ran out of numbers, grab more..

getline_2gotit:sub si,gstuff
        mov [gstat],si          ;location
        jmp short getline_skip

;UNICODE IN
getline_unicode:mov ax,[si]     ;grab letter
        mov ds:[bp],ax  ;save letter
        add si,2
        add bp,2
        add di,2
        cmp ax,10
        je short getline_2unicode_gotit
        loop short getline_unicode
        jmp short getline_1start;if ran out of numbers, grab more..

getline_2unicode_gotit:sub si,gstuff
        mov [gstat],si          ;location
        jmp short getline_skip

;TURN ASCII TO UNICODE
getline_turn_unicode:mov ah,0
getline_turn_unicodex:mov al,[si]       ;grab letter
        mov ds:[bp],ax  ;save letter
        inc si  ;increase 1 byte
        add bp,2;fix new location (saved)
        add di,2;fix data entered (unicode)
        cmp al,10
        je short getline_2turn_unicode_gotit
        loop short getline_turn_unicodex
        jmp near getline_1start;if ran out of numbers, grab more..

getline_2turn_unicode_gotit:
        sub si,gstuff
        mov [gstat],si          ;location
;jmp short getline_skip  ;unneeded
getline_skip:   pop ds          ;return
                mov bp,sp
                mov [bp+pcx],di ;save value in stack (cx)
                popa    ;return
                ret     ;exit


;first search over, see if a space or 32 or 10 comes first,
;if 32 first, then do a search from after that, until 10 is come across.
;if a match is found, exhange (move as nessisary)
replace:pushad
        xor di,di
replace_s1:cmp byte ptr gs:[di]," "
        je short replace_start
        cmp byte ptr gs:[di],10
        je short replace_exit
        inc di
        jmp short replace_s1

replace_start:inc di
        cmp byte ptr gs:[di],10 ;ending?
        je short replace_exit

        mov si,labstart
        mov cx,es:[lab] ;get count

replace_1:mov ax,es:[si]  ;get size (determine?)
        push di         ;equal, see if another match?
        push si         ;save values
        push cx
        push ax
        xor cx,cx
        mov cl,al

replace_2:mov al,es:[si+2];get first letter
        cmp gs:[di],al  ;see if different
        je short replace_3
        pop ax
        pop cx
        pop si
        pop di
        jmp short replace_4
replace_3:inc si
        inc di
        loop short replace_2    ;continue loop
        jmp short replace_5     ;successful to later section

replace_4:add al,ah     ;get size
        mov ah,0        ;fix
        inc ax
        inc ax          ;header?
        add si,ax       ;prepair
        loop short replace_1
        jmp short replace_start
replace_exit:popad      ;quit
        ret

replace_5:pop ax
        pop cx          ;equals, now skip
        pop si
        pop di
        mov bx,ax
        mov bh,0        ;set up add
        inc bx
        inc bx          ;pointer fix
        add si,bx

        xor edx,edx    ;clear number
        cmp ah,1
        jne short replace_5_1
        mov dl,es:[si]  ;get number
replace_5_1:cmp ah,2
        jne short replace_5_2
        mov dx,es:[si]  ;get number
replace_5_2:cmp ah,4
        jne short replace_5_3
        mov edx,es:[si] ;get number
replace_5_3:mov si,di   ;clear region

        xor cx,cx
        mov cl,al       ;get size to clear
replace_6:mov byte ptr gs:[si]," "
        inc si
        loop short replace_6

        mov bx,ax       ;save info
        xor eax,eax     ;clear
        mov ecx,eax
        mov bh,cl       ;clear (quick)

        mov si,ax       ;clear
        xchg edx,eax    ;at a go
        inc si          ;Ending H on it?

        mov cx,16
        cmp eax,16      ;within decimal range?
        ja short replace_7_1
        mov cx,10       ;change to DECIMAL format
        dec si          ;ending H?
replace_7_1:push eax    ;save number
replace_7_2:cmp eax,0   ;see if anything left
        je short replace_8
        xor dx,dx       ;clear number
        div ecx         ;get next part
        inc si          ;add 1 to counter
        jmp short replace_7_2

replace_8:cmp dl,10     ;see if decimal (or needing an ending type)
        jnae short replace_8_1
        inc si
replace_8_1:cmp si,0
        jne short replace_8_1_2
        inc si          ;need at least one
replace_8_1_2:cmp si,bx
        je short replace_9
        jb short replace_8_5

;space too small, make room
        push si
        push di
        push cx ;save divisor

        mov cx,si       ;prepair
        sub cx,bx       ;find difference
        mov si,di

replace_8_2:cmp byte ptr gs:[si],10 ;find ending
        je short replace_8_3
        inc si
        jmp short replace_8_2
replace_8_3:mov bp,si   ;get length count
        sub bp,di
        sub bp,bx       ;take out already spaced

        mov di,si       ;prepair
        add di,cx       ;move upwards

        mov cx,bp       ;tell loop
        inc cx

replace_8_4:mov al,gs:[si]
        mov gs:[di],al
        dec si
        dec di
        loop short replace_8_4

        pop cx  ;restore divisor
        pop di
        pop si
        jmp short replace_9

;space too big,shrink
;took chunk from too small, find problems and fix them in repeat
replace_8_5:push si
        push di
        push cx ;save divisor

        mov cx,si       ;prepair
        sub bx,cx       ;find difference (to shrink it with)
        mov si,di
        add si,bx       ;spaces already

replace_8_6:cmp byte ptr gs:[si],10 ;find ending
        je short replace_8_7
        inc si
        jmp short replace_8_6
replace_8_7:mov bp,si   ;get length count
        sub bp,di
        sub bp,bx       ;remove needed space


        add di,cx       ;prepair
        mov si,di       ;prepair
        add si,bx

        mov cx,bp       ;tell loop

replace_8_8:mov al,gs:[si]
        mov gs:[di],al
        inc si
        inc di
        loop short replace_8_8

        pop cx  ;restore divisor
        pop di
        pop si

replace_9:pop eax       ;get number again
        add di,si
        dec di  ;fix?
        xor dx,dx

        cmp cx,16
        jne short replace_9_1
        mov byte ptr gs:[di],"H"
        dec di
replace_9_1:cmp eax,0   ;precheck, if already 0?
        je short replace_9_2_1  ;is 0, should get it's number

replace_9_2:cmp eax,0
        je short replace_10

replace_9_2_1:xor dx,dx
        div ecx

        cmp dl,10
        jb short replace_9_3
        add dl,7        ;little trick
replace_9_3:add dl,"0"      ;make number
        mov gs:[di],dl  ;failsafe (hex)
        dec di
        jmp short replace_9_2

replace_10:cmp dl,"9"
        jbe short replace_10_1
        mov byte ptr gs:[di],"0";failsafe (hex)
replace_10_1:popad              ;successful ending
        jmp near replace        ;start over :P


debug_thecpx:   pushad
        mov si,debugcpx
        add si,7        ;fix at end
        mov eax,[thecpx];grab current number
        mov cx,8        ;size of numbers
        mov ebx,16      ;prepair divisor

debug_thecpx_1: xor edx,edx     ;clear remainder
        div ebx         ;divide
        add dl,"0"
        cmp dl,"9"      ;hex?
        jbe short debug_thecpx_2
        add dl,7        ;make hex
debug_thecpx_2:mov cs:[si],dl   ;save
        dec si          ;move down
        loop short debug_thecpx_1
        popad           ;return
        ret


;check <?> Information
;first byte only matters
; I = Include
; . = CPU
; O = ORG (CPX)
; 3 = 32 bits
; 1 = 16 bits
; S = SET Number (Binary i hope :P)
;


specials:       pusha
int 3
                mov si,1
                mov dh,gs:[si]  ;get letter
                cmp dh,"."
                jne short specials_1

                mov dx,gs
                inc si

                call near getnum
                mov di,cpuformula
specials_0_1:cmp byte ptr [di],0
                je short specials_1
                mov cx,[di]
                mov dl,[di+2]
                cmp cx,ax
                je short specials_0_2
                add di,3
                jmp short specials_0_1
specials_0_2:   mov [compverset],dl     ;save new information
                jmp short specials_exit
specials_1:     cmp dh,"I"
                jne short specials_2
;include special..
                nop     ;ignore, another section earlier takes care of it
                jmp short specials_exit
specials_2:     cmp dh,"O"
                jne short specials_3

specials_2_0:   mov dh,gs:[si]          ;grab letter
                cmp dh," "              ;find space
                je short specials_2_1
                cmp dh,13               ;ending?
                je short specials_exit
                cmp dh,10               ;ending?
                je short specials_exit
                cmp dh,">"              ;ending?
                je short specials_exit
                inc si
                jmp short specials_2_0
specials_2_1:   inc si                  ;fix to number?

                mov dx,gs               ;segment
                call near getnum        ;get number

                mov cs:[thecpx],eax     ;save new value
                mov bx,es:[labcpx]
                mov es:[bx],eax         ;written in now.
                jmp short specials_exit

specials_3:     cmp dh,"1"
                jne short specials_4
                mov byte cs:[bit32_set],1;make sure it's 16 bit
                jmp short specials_exit
specials_4:     cmp dh,"3"
                jne short specials_5
                mov byte cs:[bit32_set],2;make sure it's 32 bit
                jmp short specials_exit
specials_5:     cmp dh,"S"
                jne short specials_6

specials_5_0:   mov dh,gs:[si]          ;grab letter
                cmp dh," "              ;find space
                je short specials_5_1
                cmp dh,13               ;ending?
                je short specials_exit
                cmp dh,10               ;ending?
                je short specials_exit
                cmp dh,">"              ;ending?
                je short specials_exit
                inc si
                jmp short specials_5_0
specials_5_1:   inc si                  ;fix to number?

                mov dx,gs               ;segment
                call near getnum        ;get number
                mov cs:[opinclude_set],al;save new data
                jmp short specials_exit

specials_6:    ;cmp dh,"I"
               ;jne short specials_3

specials_exit:  popa
                ret

;for certain parts :P
;INPUT dx:si
;output EAX
getnum: pushad
        push ds
        mov ds,dx       ;prepair data (alone)

        xor eax,eax
        mov ecx,eax
        mov edx,eax
getnum_start:push edx

        xor eax,eax
        cmp byte ptr [si],0
        jne short getnum_extendly
        inc si          ;skips (was an extention)
getnum_extendly:
        cmp byte ptr [si],10    ;ending?
        je near getnum_done

        cmp byte ptr [si],13    ;ending?
        je near getnum_done

        cmp byte ptr [si],">"   ;ending (Specials)
        je near getnum_done

        cmp byte ptr [si],","   ;ending?
        je near getnum_done

        cmp byte ptr [si],"]"   ;ending?
        je near getnum_done

        cmp byte ptr [si]," "   ;ending?
        je near getnum_done

        cmp byte ptr [si],")"   ;ending? (direct)
        je near getnum_done

        mov bl,1
        cmp byte ptr [si],"+"   ;accepted types
        je short getnum_type

        mov bl,2
        cmp byte ptr [si],"-"
        je short getnum_type

        mov bl,3
        cmp byte ptr [si],"\"   ;remainder find
        je short getnum_type    

        mov bl,4
        cmp byte ptr [si],"/"   ;round up
        je short getnum_type    

        mov bl,5
        cmp byte ptr [si],"*"
        je short getnum_type    

        mov bl,1                ;regular number?
        jmp short getnum_nsign
getnum_type:inc si      ;move up from sign
getnum_nsign:push si    ;save location

getnum_fend:
        cmp byte ptr [si],"+"   ;locate next parts?
        je short getnum_next

        cmp byte ptr [si],"-"
        je short getnum_next

        cmp byte ptr [si],"/"
        je short getnum_next

        cmp byte ptr [si],"\"
        je short getnum_next

        cmp byte ptr [si],"*"
        je short getnum_next

        cmp byte ptr [si],")" ;for directs
        je short getnum_next

        cmp byte ptr [si],"]" ;for mem
        je short getnum_next

        cmp byte ptr [si],10
        je short getnum_next

        cmp byte ptr [si],13
        je short getnum_next

        cmp byte ptr [si],","
        je short getnum_next

        cmp byte ptr [si]," "
        je short getnum_next

        cmp byte ptr [si],">"
        je short getnum_next

        inc si
        jmp short getnum_fend
getnum_next:    dec si

        mov cl,2 
        cmp byte ptr [si],"B"   ;binary
        je short getnum_onward

        mov cl,4
        cmp byte ptr [si],"Q"   ;quad
        je short getnum_onward

        mov cl,6
        cmp byte ptr [si],"X"   ;heX
        je short getnum_onward

        mov cl,8
        cmp byte ptr [si],"O"   ;octal
        je short getnum_onward

        mov cl,16
        cmp byte ptr [si],"H"   ;Hexidecimal
        je short getnum_onward
        mov cl,10
        inc si
        jmp short getnum_onward1
getnum_onward:  mov byte ptr [si],0     ;null, special
getnum_onward1: mov di,si
                pop si
                sub di,si               ;find loop to follow

                cmp di,0
                je short getnum_quotesin;multiple signs..

                cmp byte [si],34        ;look for quotes
                je near getnum_quotes

                cmp byte [si],"0"       ;not number?!?
                jb near getnum_notnumber
                cmp byte [si],"9"
                ja near getnum_notnumber

                cmp word [si],"0X"      ;check for hex (other way)
                jne short getnum_not2hex;no hex
                mov cl,16               ;become hex
                inc si
                inc si
                dec di
                dec di
getnum_not2hex: mov bh,[si]     ;grab letter
                sub bh,"0"      ;get decimal number
                cmp bh,9        ;is decimal?
                jbe short getnum_isdec
                sub bh,7        ;find hex real
getnum_isdec:   xor edx,edx     ;proceed with add in
                mul ecx
                mov dl,bh
                add eax,edx

                inc si
                dec di
                cmp di,0        ;loop, special
                ja short getnum_not2hex         ;next character check


getnum_quotesin:pop edx         ;get number
                cmp bl,1        ;add?
                jne short getnum_not1
                add edx,eax

getnum_not1:    cmp bl,2        ;subtract?
                jne short getnum_not2
                sub edx,eax

getnum_not2:    cmp bl,3        ;Find Remainder
                jne short getnum_not3
                mov ecx,eax     ;get divisor
                mov eax,edx     ;prepair number
                xor edx,edx     ;prepair remainder?
                div ecx
;                mov edx,eax     ;done

getnum_not3:    cmp bl,4        ;Divide (without remainder, round down)
                jne short getnum_not4
                mov ecx,eax     ;get divisor
                mov eax,edx     ;prepair number
                xor edx,edx     ;prepair remainder?
                div ecx
                mov edx,eax     ;done

getnum_not4:    cmp bl,5        ;multiply
                jne short getnum_not5

                mov ecx,eax     ;get multiplier
                mov eax,edx     ;prepair number
                xor edx,edx     ;prepair multiply
                mul ecx
                cmp edx,0
                je short getnum_not51

;                pop eax         ;already popped?
                xor eax,eax
                mov byte ptr cs:[err_ident],5   ;number TOO big
                jmp short getnum_Skipeax
getnum_not51:   mov edx,eax

getnum_not5:    jmp near getnum_start


;ok, this is a special rule, sorry..
getnum_quotes:  xor cx,cx               ;prepair shifter
                push bx                 ;save formula (change?)
getnum_quotes0: inc si                  ;move up by one
                cmp byte [si],34        ;double quotes
                je short getnum_quotes1

                xor ebx,ebx             ;prepair
                mov bl,gs:[si]          ;grab letter
                shl ebx,cl              ;move over some bits
                add eax,ebx             ;add on
                add cl,8                ;prepair next add (good up to 4)
                jmp short getnum_quotes0
getnum_quotes1: inc si                  ;fix after quotes
                pop bx                  ;return formula
                jmp short getnum_quotesin


getnum_notnumber:pop eax
                xor eax,eax
                mov byte ptr cs:[err_ident],4   ;not number
                jmp short getnum_Skipeax

;getnum_type_err:
;                pop eax
;                xor eax,eax
;                mov byte ptr cs:[err_ident],6   ;no div/mul allowed
;                jmp short getnum_Skipeax

getnum_done:    pop eax
getnum_Skipeax: pop ds                  ;return real data
                mov bp,sp
                mov [bp+peax],eax       ;carry over
                popad
                ret

identify:       pushad

                mov si,null_ident
                mov cx,D_Data_Size      ;(whole area)
                mov al,0                ;null
ident_clear:    mov [si],al             ;clear search half
                inc si
                loop short ident_clear

                cmp byte ptr gs:[di],10 ;if empty line (few)
                je near ident_NoParErr

                cmp word ptr gs:[di],200ah ;uncertain
                je near ident_NoParErr

                cmp word ptr gs:[di],0a20h ;uncertain until certain
                je near ident_NoParErr

                push di                 ;save instruction information
                xor cx,cx               ;save text size

ident_pre:      mov si,prefix_data      ;types identity
                
ident_pre0:     cmp byte ptr gs:[di],10 ;end of text
                je short ident_pre4
                mov cl,[si]             ;get text size
                cmp cx,0
                je short ident_pre3
                push di                 ;save location
                mov ax,[si+1]           ;get extended information
                add si,3                ;locate text
                mov bx,cx               ;save backup?

ident_pre1:     mov dl,[si]             ;get letter
                cmp gs:[di],dl          ;compare
                jne short ident_pre2    ;continue?
                inc si
                inc di                  ;get other location set
                loop short ident_pre1
                pop di                  ;get start location
                mov cx,bx               ;set up count

ident_pre5:     cmp byte gs:[di],"["    ;memory headers
                je short ident_pre6     ;skip it out
                cmp byte gs:[di],","    ;comma, information separator
                je short ident_pre6     ;skip it out (leaving it)

                mov byte ptr gs:[di]," ";clear out
ident_pre6:     inc di                  ;move up one
                loop short ident_pre5   ;clear area (text)

                xor bx,bx               ;clear
                mov bl,al
                add bx,null_ident       ;prepair save
                mov [bx],ah
                jmp short ident_pre     ;jump to next location

ident_pre2:     pop di                  ;return base
                add si,cx               ;increase size
                jmp short ident_pre0    ;continue looping
ident_pre3:     inc di
                jmp short ident_pre
ident_pre4:     pop di

;check for optimize
                cmp byte cs:[optimize],1
                jnae short ident_blk

                mov byte cs:[dist_ident],0 ;clear distance information
;fix out spaces (all spaces gone, but 1, plus inside "'s

ident_blk:      cmp byte ptr gs:[di]," "
                jne short ident_blk0
                inc di                  ;move up one
                jmp short ident_blk
ident_blk0:     xor cx,cx               ;prepair count (0)
                mov ch,1
                mov si,di               ;start location
                push di                 ;save start

ident_blk1:     cmp byte ptr gs:[di],34 ;check for quotes
                jne short ident_blk2
                neg ch                  ;setting on

ident_blk2:     cmp ch,1                ;see if quote on
                je short ident_blk3     ;skip if so
                jmp short ident_blk4    ;write reguardless

ident_blk3:     cmp byte ptr gs:[di]," ";check space
                jne short ident_blk4    ;space?
                cmp cl,0                ;is space
                jne short ident_blk5
                inc cl                  ;update space
ident_blk4:     mov al,gs:[di]          ;get byte
                mov gs:[si],al          ;save byte
                inc si

ident_blk5:     cmp byte ptr gs:[di],10 ;check end of line
                je short ident_blk6     ;at end of line
                inc di                  ;move up (also space skip)
                jmp short ident_blk1

ident_blk6:     cmp word ptr gs:[si-3],0a20h     ;see if it's this?
                jne short ident_blk7
                mov byte ptr gs:[si-3],10
ident_blk7:     pop di

                mov si,di
                xor cx,cx
ident_define:   cmp byte ptr gs:[si]," ";check space (at least 1)
                jne short ident_define1
                inc cx
ident_define1:  cmp byte ptr gs:[si],",";check comma (2)
                jne short ident_define2
                inc cx
                jmp short ident_define3 ;knowing 2, skip
ident_define2:  cmp byte ptr gs:[si],10 ;check ending
                je short ident_define3
                inc si
                jmp short ident_define  ;reloop (look until satisfied)

ident_define3:  cmp byte gs:[di+2]," "  ;if qualify in the 2 byte range
;identify if db, dw, dq/dd
                jne short ident_define6

                xor ax,ax               ;clear data
                inc ax                  ;make 1
                cmp word gs:[di],"DB"   ;see if byte
                jne short ident_define611
                call near ident_direct
                jmp short ident_define62;skip ahead past idents

ident_define611:inc ax                  ;make 2
                cmp word gs:[di],"DW"   ;see if word
                jne short ident_define612
                call near ident_direct
                jmp short ident_define62;skip ahead past idents

ident_define612:inc ax                  ;make 4
                inc ax
                cmp word gs:[di],"DQ"   ;see if doubleword/quad
                jne short ident_define613
                call near ident_direct
                jmp short ident_define62;skip ahead past idents

ident_define613:cmp word gs:[di],"DD"   ;see if doubleword/quad
                jne short ident_define6 
                call near ident_direct
                jmp short ident_define62;skip ahead past idents

ident_define6:  cmp cx,0
                jne short ident_define4
                call near ident_1
ident_define4:  cmp word ptr gs:[si-1],0a20h
                jne short ident_define41
                mov byte ptr gs:[si-1],0ah
                xor cx,cx               ;null empty section
                call near ident_1
ident_define41: cmp cx,1
                jne short ident_define5
                call near ident_part
                call near ident_2
ident_define5:  cmp cx,2
                jne short ident_define62
                call near ident_part
                call near ident_3
;moved stuff around

;if there's a size, disable the 9'illegal' one
ident_define62: cmp byte cs:[bit32_ident],3     ;is it 64?
                jne short ident_3_define62a     ;if not
                mov byte cs:[bit32_ident],0     ;it is, now clear
ident_3_define62a:
                cmp byte [err_ident],9  ;see if illegal
                jne short ident_define63;skip out
                cmp byte [size_ident],0 ;check size of instruction
                je short ident_define63
                mov byte [err_ident],0  ;no longer illegal (bug somewhere)

ident_define63: mov si,real_ident       ;location of instruction write
                mov dx,[prefix_data_32] ;prefix save
                xor cx,cx
                mov ch,[size_ident]     ;get size
                mov al,[seg_ident]      ;get segment

                cmp al,0                ;segment override?
                je short ident_nosegs
                dec si
                mov [si],al
                inc cx

ident_nosegs:   mov ah,[bit32_set]      ;get 32 info
                mov al,[bit32_ident]    ;using 32 registers?

                cmp al,0                ;if 8 bit, ignore
                je short ident_no32s

                cmp al,ah
                je short ident_no32s    ;they are the same?
                dec si
                mov [si],dl     ;66h?
                inc cx
ident_no32s:    mov al,[dist32_ident]   ;get distance (32?)

                cmp al,0                ;Undeclaired?
                je short ident_nodist32

                cmp al,ah
                je short ident_nodist32
                dec si
                mov [si],dh     ;67h?
                inc cx
ident_nodist32: mov al,[rep_ident]      ;get Rep info

                cmp al,0                ;Repeat addon? (higher then 0)
                je short ident_no_addons
                dec si
                mov [si],al
                inc cx

ident_no_addons:cmp byte [opinclude_ident],0
                je short ident_noIncludes

                push cx
                xor ax,ax                       ;clear
                mov ch,al                       ;clear
                mov cl,[opinclude_ident]        ;grab number
                mov al,10000000b                ;set first bit
                shr ax,cl                       ;locate
                test [opinclude_set],al         ;check to see if enabled

                jz short ident_includes1
                mov di,fs:[ident_opinclude]     ;grab new location info

ident_opincludex_1:mov al,fs:[di]               ;get size
                inc ax                          ;fix in other data size
                add di,ax                       ;new location
                loop short ident_opincludex_1   ;locate

                mov cl,fs:[di]                  ;get include (1 byte?)
                push cx                         ;save size

                sub si,cx                       ;move by that many bytes
                inc di

ident_opincludex_2:mov al,fs:[di]               ;get byte
                mov [si],al                     ;write byte
                inc si
                inc di
                loop short ident_opincludex_2

                pop ax
                pop cx
                add cx,ax                       ;return and update
                jmp short ident_noincludes
ident_includes1: pop cx                         ;refused and continue
ident_noIncludes:add ch,cl                      ;prepair
                mov [size_ident],ch             ;save instruction size
                mov [start_ident],cl            ;save location of start

                cmp ch,16
                jbe short ident_NoParErr
                mov byte ptr [err_ident],3 ;save 'instruction too big' error
ident_NoParErr: popad
                ret


ident_1:        pusha
;determine length, and then search through library.
;also, save and get location of library fs:(2)
                mov si,fs:[ident_pt1]
                push di         ;save
ident_1_length: cmp byte ptr gs:[di],10 ;see if this is end
                je short ident_1_length1
                inc di                  ;not end, continue
                jmp short ident_1_length;look next
ident_1_length1:mov dx,di               ;get location
                pop di                  ;return value
                sub dx,di               ;find real length
;dl size ;dh Comp

ident_1_start:  mov cx,fs:[si]          ;get sizes
                mov dh,fs:[si+2]        ;grab instruction computer version
                add si,3

                cmp cl,0                ;see if there is anything
                je short ident_1_nomore ;skip it all
                cmp dl,cl               ;see if matches size
                jne short ident_1_next  ;if not skip
                push di                 ;save

ident_1_goagain:cmp cl,0        ;end of count?
                je short ident_1_found

                mov al,gs:[di]  ;get letter
                cmp fs:[si],al  ;see if match
                jne short ident_1_notfound
                inc si          ;move up one
                inc di          ;move up one
                dec cx          ;fix count
                jmp short ident_1_goagain

ident_1_found:  pop di
                cmp [CompVerSet],dh             ;check
                jae short ident_1_compauto      ;worked
                mov byte ptr [err_ident],2      ;put up 'not on model' but work anyway
                jmp short ident_1_next
ident_1_compauto:xchg ch,cl             ;get number
                mov di,real_ident       ;prepair location

                shl cx,4                ;get bit
                shr cl,4                ;return size?
;any other used stuff, shove in here

                push cx
                mov cl,0        ;clear
                ror cx,2        ;adjust
                mov [opinclude_ident],ch;save opinclude
                pop cx
                and ch,3                ;32bit save
                mov [bit32_ident],ch    ;sends 32bit used?

                mov [size_ident],cl     ;save size
                mov ch,0        ;clear
ident_1_copyinst:mov al,fs:[si]         ;get letter
                mov [di],al             ;copy
                inc si
                inc di
                loop short ident_1_copyinst ;copy whole instruction
                jmp short ident_1_gotit
ident_1_notfound:pop di                 ;return location
ident_1_next:   and ch,0fh      ;grab right half only?
                add cl,ch               ;get remainder (if any)
                mov ch,0                ;clear
                add si,cx               ;add on
                jmp short ident_1_start ;continue
ident_1_nomore: mov byte ptr [err_ident],1;put up 'not identified'
ident_1_gotit:  popa
                ret

ident_2:        pushad
;determine length, and then search through library.
;also, save and get location of library fs:(2)
                mov si,fs:[ident_pt2]
                push di
ident_2_start:  cmp byte ptr gs:[di],32         ;end of text?
                je short ident_2_flen
                inc di
                jmp short ident_2_start

ident_2_flen:   mov dx,di
                pop di          ;prepair
                sub dx,di       ;find length
                xor ax,ax       ;hmm

ident_2_ftype:  push di
                xor cx,cx       ;clear counter
                mov cl,fs:[si]  ;get text size
                cmp cl,0
                je near ident_2_nomore ;enter error code

                inc si          ;prepair
                cmp dl,cl       ;match size?
                jne short ident_2_nomatch

ident_2_fmatch: mov al,gs:[di]  ;compare text
                cmp al,fs:[si]
                jne short ident_2_nomatch       ;if not match, next text
                inc si
                inc di
                loop short ident_2_fmatch
                jmp short ident_2_match

ident_2_nomatch:pop di
                add si,cx       ;find ending of text
;got to skip to next text now
                mov cl,fs:[si]  ;get number of unders
                inc si          ;start here
ident_2_nomatch2:add si,3
                mov al,fs:[si]  ;get size of instruction
                add si,ax       ;save and update
                inc si
                loop short ident_2_nomatch2     ;go until end of area
                jmp short ident_2_ftype

ident_2_match:  mov cl,fs:[si]  ;get unders
                inc si
ident_2_under:  xor ax,ax
                mov al,fs:[si]  ;get first info (compare)

;Data    0 (1 section)
;8/16/32/64?     (2 bit)??
;Exact match?    (1 bit)
;relation (Regs type) 4 bit
;
;Type     (register, memory, number) (bit 2)
;
;If Register
;helps with the encoding. (11 ??? Reg)

;If memory,
;helps with the encoding. (?? INT ???)
;64 bit enabled will expand memory limits

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


;   0 00 0 0000b,11 001 000b
;   1, 1, D5 

        shl ax,1
        cmp ah,0        ;Data 0
        jne near ident_2_illegal

        shl ax,2        ;8/16/32/64?
        mov [null_ident],ah    ;save info

        mov ah,dh       ;cleared (i am sure)
        shl ax,1

        shr al,4        ;number (exact match in bh, and relation bl)
        mov bx,ax       ;get number prepaired

;si +1 here on
        mov al,fs:[si+1]
        mov ah,dh                       ;clear
        shl ax,2                        ;TYPE (at least one here..)

        cmp ah,0                        ;say illegal?
        je near ident_2_illegal

        cmp [type1_ident],ah            ;the kicker?
        jne near ident_2_nomatch4
        mov ah,dh                       ;clear

        shl ax,3                        ;get number qualify (size/type)
        ror al,5                        ;set up opinclude?

        mov bp,ax
        cmp byte ptr [type1_ident],3    ;see if number (related)
        jne near ident_2_nocheck

        cmp ah,4
        ja short ident_2_dcheck
;number half.. check for at least value, to qualify (that number, or less)
        cmp byte ptr [dist_ident],0
        jne near ident_2_nomatch4       ;a distance value needed?

        and ah,3                        ;save only number part
        cmp ah,3                        ;see if 4 bytes?
        jne short ident_2_xxcheck2
        inc ah                          ;fix
ident_2_xxcheck2:cmp [numtype_ident],ah ;good for number
        je short ident_2_xxcheck2_exact0;will work?
        jmp near ident_2_nomatch4       ;didn't pass check

ident_2_xxcheck2_exact0:cmp bh,1                ;check for exact
                        jne near ident_2_nocheck;continue original
                        pushad

                        mov ecx,[number_ident]  ;get number
                        xor ebx,ebx
                        mov dl,fs:[si+3]        ;get size of instruction
                        sub dl,ah               ;get real instruction size
                        add si,4
                        add si,dx       ;adjust?
                        xor edx,edx

                        cmp ah,1
                        jne short ident_2_xxcheck2_exact1
                        mov dl,fs:[si]
ident_2_xxcheck2_exact1:cmp ah,2
                        jne short ident_2_xxcheck2_exact2
                        mov dx,fs:[si]
ident_2_xxcheck2_exact2:cmp ah,4
                        jne short ident_2_xxcheck2_exact3
                        mov edx,fs:[si]
ident_2_xxcheck2_exact3:cmp ecx,edx
                        je short ident_2_xxcheck2_exact4
                        popad
                        jmp near ident_2_nomatch4

ident_2_xxcheck2_exact4:mov [size_ident_e],ah   ;extra space to remove
                        popad
                        jmp short ident_2_nocheck ;done here
;base off size, get true location, then determine if it's true or not

;SINCE of no use, it will be saved for later.. if used later..
;check distance half
ident_2_dcheck:and ah,3                 ;save only that part
        cmp ah,3
        jne short ident_2_dcheck2
        inc ah                          ;fix
ident_2_dcheck2:cmp [dist_ident],ah
        je short ident_2_nocheck
        cmp byte [dist_ident],0         ;check if no value present
        jne near ident_2_nomatch4       ;see if it is worth seeing for more

        pushad                          ;push all (easier this way)
        mov dh,ah                       ;save data

        mov eax,[number_ident]          ;get number
        mov ebx,[thecpx]                ;grab location
        sub eax,ebx                     ;number to save

        cmp eax,0                       ;check for 0
        jl short ident_2_dcheck3

        add eax,127                     ;extream UP, removes the neg?
        add eax,5                       ;equalizer
ident_2_dcheck3:sub eax,5               ;equalizer
;add eax,50              ;extream DOWN

        call near Ident_number_size
        
        cmp dl,dh                       ;see if good
        jbe short ident_2_jmp_numgood

ident_2_dcheck5:popad                   ;not good, skip out
        jmp near ident_2_nomatch4       ;out of here

ident_2_jmp_numgood:cmp byte cs:[optimize],2    ;see if Quick
        jne short ident_2_dcheck4       ;nope, quit out

        cmp dh,1
        je short ident_2_dcheck5

ident_2_dcheck4:mov [dist_ident],dh     ;write the distance out
        popad                           ;get back information


;AT THIS POINT ON, USE FAILURES NOMATCH3
ident_2_nocheck:push bp                 ;save data info (types distance)

        cmp byte [type1_ident],1;register?
        jne short ident_2_noregcheck    ;relation check

;not going to save ax for now..
        xor ax,ax       ;needed?
        mov al,[reg_ident]
        shl ax,4
        shr al,4                        ;separate
        cmp ah,bl                       ;relation check
        jne near ident_2_nomatch3       ;not in the related part, keep searching
;assume all went well? Can't prove otherwise.

        cmp bh,1                        ;check for exact
        jne short ident_2_noregcheck

        xor ax,ax
        mov al,fs:[si+3]                ;grab instruction size
        push si

        dec ax                          ;remove exact (register 1 byte)
        add si,4                        ;ajust
        add si,ax

        mov al,fs:[si]                  ;get register compare
        pop si

        cmp al,0
        je near ident_2_illegalbase     ;register isn't put in right

        mov ah,[reg_ident]
        cmp al,ah

        je short ident_2_regcheck_e1

        jmp near ident_2_nomatch3       ;not exact, so can't use

ident_2_regcheck_e1:mov byte [size_ident_E],1   ;set for register delete

ident_2_noregcheck:cmp byte [type1_ident],2
        jne short ident_2_nomemcheck

;assuming the information isn't higher then 3

        mov ax,bp               ;grab data
        cmp ah,3                ;3=4
        jne short ident_2_memch1
        inc ah                  ;fix to 4
ident_2_memch1:cmp byte [null_ident],3 ;see if 64
        jne short ident_2_memch2
        shl ah,2                ;X4
ident_2_memch2:cmp [numtype_ident],ah   ;ok, see if it's in range.
;ok, see if within range of size..
        ja near ident_2_nomatch3;skip out, don't work

        cmp bh,1                        ;ok, no matches needed to satisfy
        jne short ident_2_nomemcheck

        push bp                         ;special save
        mov bp,memdata_ident

        xor ax,ax
        mov al,fs:[si+3]                ;grab instruction size
        push si

        dec ax                          ;remove exact (memory (16 assumed))
        add si,4                        ;ajust
        add si,ax

        mov al,fs:[si]                  ;get register compare
        pop si
        cmp cs:[bp+2],al
        jne near ident_2_nomatch3
        pop bp
ident_2_nomemcheck:shl bl,4
                   shl bx,4             ;save information
        mov bl,fs:[si+2]     ;get computer version
        cmp [CompVerSet],bl
        jb near ident_2_nomatch3

        mov dl,fs:[si+3]                ;get size of instruction

        cmp dl,0
        je near ident_2_illegalbase2    ;instruction size fails (0) error

ident_2_notExact:shr bx,4               ;put information back
        shr bl,4

        mov ah,dh                       ;since it seems to match, continue
        shl ax,3
        mov [opinclude_ident],ah        ;save new includes (if any)
;successful, now add on..
        push si                         ;remove annoying bug 
        add si,4                        ;already have size
        mov di,real_ident               ;prepair write

        mov cx,dx                       ;prepair loop

        sub cl,[size_ident_e]           ;fix by means of exacts?

        mov [size_ident],cl             ;save size
;exact adjustments :P (later)
ident_2_saveit:mov al,fs:[si]   ;grab
        mov [di],al             ;save
        inc si
        inc di
        loop short ident_2_saveit
;process first letter.

        pop si                  ;annoying little bug fixed (mem)
;this pop SI is for a nasty bug in MEM, it skips a whole instruction if
;left alone. Took a while to get this one

        mov al,[null_ident]     ;grab (untouched) type
        mov [bit32_ident],al    ;save info

        mov ax,bp               ;opinclude
        mov [opinclude_ident],al

        pop ax  ;aquire information

        cmp byte ptr [type1_ident],2    ;check if memory
        jne short ident_2_no_override

;        cmp ah,3                        ;see if 4 bytes?
;        jne short ident_2_memcheck
;        inc ah                          ;fix
;ident_2_memcheck:cmp [numtype_ident],ah ;good for number
;        jne near ident_2_nomatch4       ;quit
;already checked

        cmp bh,1
        jne short ident_2_no_override   ;match out with encoded source

        mov bp,memdata_ident    ;grab required area
        xor cx,cx               ;create size looper
        mov cl,cs:[bp]          ;get data size (1-2)
        add byte [size_ident],cl;increase size accordingly
        mov eax,[memnum_ident]  ;grab number
        mov [di],eax            ;save number (may be null)

ident_2_no_override:cmp bh,1
        je near ident_2_gotit          ;if exact was used..
;ok, now for the checks..
ident_2_nochecks2:cmp byte ptr [type1_ident],1  ;register
                jne near ident_2_1go_on
;build the format.. based on, 11 XXX (unused) ???

                mov ax,bp
                shl ah,3                ;place at location (extra data)
                mov al,[reg_ident]      ;get register
                and al,7                ;grab the important bits
                add al,300o             ;put in format (300o)
                add al,ah               ;put in extra data

                add byte [size_ident],1 ;increase size accordingly
                mov [di],al
                jmp near ident_2_gotit

ident_2_1go_on: cmp byte ptr [type1_ident],2    ;memory
                jne near ident_2_2go_on
;build the format.. based on, ?? MEM enc

                mov ax,bp               ;backup, and will use AH a little
                mov bp,memdata_ident    ;grab required area
                xor cx,cx               ;create size looper
                push si
                mov si,cx               ;get cleared side counter
                mov cl,cs:[bp]          ;get data size (1-2)
                add byte [size_ident],cl;increase size accordingly
                add si,2
;prepair first pass
                mov al,cs:[bp+si]       ;get mem byte
                shl bl,3                ;fix encoding
                add al,bl               ;add encoding
                jmp short ident_2_mm2   ;skip for first run

ident_2_mm:     mov al,cs:[bp+si]       ;grab mem byte
ident_2_mm2:    mov [di],al             ;save byte (First pass)
                inc di                  ;update
                inc si
                loop short ident_2_mm

                mov cl,cs:[bp+1]        ;grab size to save (of side data)
                add byte [size_ident],cl;increase size accordingly
                mov eax,[memnum_ident]  ;grab number
                mov [di],eax            ;save number (may be null)

;                xor ax,ax
;                mov al,fs:[si-4]
;                and al,7        ;grab the 3 bits we need
;they are already SET
;                call near memory_part                ;identify and set memory
                pop si                  ;resume previous number
ident_2_2go_on: cmp byte ptr [type1_ident],3    ;numbers done
                jne near ident_2_3go_on
                cmp ah,4
                jae short ident_2_distance
                cmp ah,3
                jne short ident_2_sze
                inc ah
ident_2_sze:    add [size_ident],ah     ;increase size accordingly
                mov eax,[number_ident]  ;get number
                jmp near ident_2_numgood;SHOULD save it

ident_2_distance:and ah,3       ;sift out

                cmp ah,3        ;is it 4 bytes?
                jne short ident_2_dst
                inc ah
ident_2_dst:    xor ecx,ecx
                add [size_ident],ah     ;increase size accordingly

                mov cl,[size_ident]     ;grab size

                mov eax,[number_ident]  ;get number
                mov ebx,[thecpx]        ;grab location
                add ebx,ecx             ;real location to start.
                sub eax,ebx             ;number to save
                mov dh,[dist_ident]     ;get the specified type (followed)

                call near Ident_number_size

                cmp dl,dh
                je short ident_2_numgood
                jb short ident_2_canBsmaller

                xor eax,eax
                mov byte ptr [err_ident],7      ;Don't work
                jmp short ident_2_numgood

ident_2_canBsmaller:mov byte ptr [err_ident],8      ;can be short?

ident_2_numgood:mov [di],eax            ;save in right location
ident_2_3go_on: jmp short ident_2_gotit
;---

ident_2_nomatch3:pop bp
ident_2_nomatch4:add si,3
                mov ah,0        ;clear (anyway)
                mov al,fs:[si]  ;get size of instruction

                cmp al,0
                je short ident_2_illegalbase    ;check for size problems

                cmp al,64
                jae short ident_2_illegalbase   ;continue checking

                add si,ax       ;save and update
                inc si
                loop near ident_2_under
                pop di
                jmp near ident_2_ftype
ident_2_illegal:mov byte ptr [err_ident],9      ;put up 'Illegal'
                jmp short ident_2_gotit
ident_2_illegalbase2:pop ax                     ;fix sp location
ident_2_illegalbase:mov byte ptr [err_ident],10 ;put up 'Illegal/database'
                jmp short ident_2_gotit
ident_2_nomore: mov byte ptr [err_ident],1;put up 'not identified'
ident_2_gotit:  pop di
                popad
                ret


ident_3:        pushad
;determine length, and then search through library.
;also, save and get location of library fs:(2)
                mov si,fs:[ident_pt3]
                push di
ident_3_start:  cmp byte ptr gs:[di],32         ;end of text?
                je short ident_3_flen
                inc di
                jmp short ident_3_start

ident_3_flen:   mov dx,di
                pop di          ;prepair
                sub dx,di       ;find length
                xor ax,ax       ;hmm

ident_3_ftype:  push di
                xor cx,cx       ;clear counter
                mov cl,fs:[si]  ;get text size
                cmp cl,0
                je near ident_3_nomore ;enter error code

                inc si          ;prepair
                cmp dl,cl       ;match size?
                jne short ident_3_nomatch

ident_3_fmatch: mov al,gs:[di]  ;compare text
                cmp al,fs:[si]
                jne short ident_3_nomatch       ;if not match, next text
                inc si
                inc di
                loop short ident_3_fmatch
                jmp short ident_3_match

ident_3_nomatch:pop di
                add si,cx       ;find ending of text
;got to skip to next text now
                mov cl,fs:[si]  ;get number of unders
                inc si          ;start here
ident_3_nomatch2:add si,4
                mov al,fs:[si]  ;get size of instruction
                add si,ax       ;save and update
                inc si
                loop short ident_3_nomatch2     ;go until end of area
                jmp short ident_3_ftype

ident_3_match:  mov cl,fs:[si]  ;get unders
                inc si
ident_3_under:  xor ax,ax
                mov al,fs:[si]  ;get first info (compare)

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

;Type1          (2 bits)(register, memory, number)
;Type2          (2 bits)(register, memory, number)
;number size    (2 bits)(byte, word, dword)(direct)
;overrides      (2 bits)
; ON (Exact second=1) and (type1=1 and type2=1) then
; numbersize+unused = Extra Opcode encoding info
; ON (Exact Second=1) And (Type1=2 and type2=3) then
; RELATION1 = Extra Opcode
; ON (Exact first=1) And (Type1=1 and type2=3) then
; override is size (special)
; Both stored in BP (Back side)

;override info (As neccisary)
;will be more then 0 on these following circomstances
;WITHOUT AN EXACT!!
; REG REG (reverse these values)
; REG IMM (real size to save 1-4)
; REG MEM (UNUSED)
; MEM REG (UNUSED)
; MEM IMM (real size to save 1-4)

;/beyond
;system familiar (which computer type)
;size of instructions

        shl ax,1
        cmp ah,1        ;Data 1
        jne near ident_3_illegal

        mov ah,dh       ;clear
        shl ax,2        ;8/16/32/64?
        mov [null_ident],ah    ;save info

        mov ah,dh       ;cleared (i am sure)
        push ax         ;save for a moment
        shl ax,2
        mov al,0
        shr ax,1        ;prepair the exacts
        shr al,7        ;exacts
        bswap eax       ;extra save space
        pop ax          ;ok, opinclude
        shl ax,2
        mov ah,dh
        shl ax,3        ;ok, opcode here..
        mov bp,ax

;si +1 here on
        mov al,fs:[si+1]
        mov ah,dh                       ;clear
        shl ax,4                        ;TYPE (at least one here..)
        shr al,4
        mov bx,ax                       ;save relations

        mov al,fs:[si+2]
        mov ah,dh                       ;clear
        shl ax,2                        ;TYPE (at least one here..)

        cmp ah,0                        ;say illegal?
        je near ident_3_illegal

        cmp [type1_ident],ah            ;the kicker?
        jne near ident_3_nomatch4
        mov ah,dh                       ;clear

        shl ax,2                        ;TYPE (at least one here..)

        cmp ah,0                        ;say illegal? (2)
        je near ident_3_illegal

        cmp [type2_ident],ah            ;the kicker?
        jne near ident_3_nomatch4
        mov ah,dh                       ;clear

;addin.. For when type1 and type2, registers.. exacts? (RCL RCR Ect)
        cmp word [type1_ident],0101h    ;check for both registers first
        jne short ident_3_Not111

        bswap eax                       ;grab exacts (for a second)
        cmp ax,0                        ;any exacts?
        je short ident_3_111_no
;ok, can grab the info now
        bswap eax                       ;return
        shl ax,4                        ;grab last 3 bits (3 are encoded)
        bswap ebp
        mov bp,ax                       ;save
        bswap ebp                       ;return

        xor ax,ax                       ;no need to keep a value
        jmp short ident_3_not111        ;close out

ident_3_111_no:bswap eax                ;reverse

;addin.. For when type1 is Mem, Type 2 number, and Exact 2. (RCL RCR Ect)
ident_3_not111:cmp word [type1_ident],0302h;check for both registers first
        jne short ident_3_Not231

        bswap eax                       ;grab exacts (for a second)
        cmp ax,0                        ;any exacts?
        je short ident_3_231_no
;ok, can grab the info now

        push bx                         ;save
        mov bl,0                        ;clear upper
        xchg bh,bl                      ;switch correctly

        bswap ebp
        mov bp,bx                       ;save
        bswap ebp                       ;return
        pop bx                          ;return to normal
ident_3_231_no:bswap eax                ;reverse
ident_3_not231:shl ax,2                 ;get number qualify (size/type)

;overrides added here

        push ax                         ;saved for dad
        xchg al,ah                      ;put in order
        mov ah,0                        ;clear

        cmp al,3                        ;see if should be 4
        jne short ident_3_not3264A      ;if not
        inc ax                          ;fix size to 4 (32)

ident_3_not3264A:
                cmp byte [null_ident],3;64 bit?
                jne short ident_3_not3264B
                shl ax,2                ;X4 (1 is 32, 2 is 64, 3 is 128)
ident_3_not3264B:
        add bp,ax                       ;make bp type information
        pop ax


;start overrides
        mov ah,0                        ;clear
        shl ax,2                        ;get override

        cmp ah,3                        ;check for special override
        jne short ident_3_notveryspec   ;don't bother

        cmp word [type1_ident],0102h    ;reg and mem check
        je short ident_3_veryspec
        cmp word [type1_ident],0201h    ;reg and mem check
        jne short ident_3_notveryspec

;ok, forced check.
ident_3_veryspec:
        mov ax,bp                       ;get data (in al)
        cmp [numtype_ident],al          ;see if inside range
        ja near ident_3_nomatch4        ;ok, too big? Small enough?
        xor ax,ax                       ;clear unneeded
ident_3_notveryspec:

        bswap EDI                       ;save overrides (special)
        xor di,di
        xchg al,ah                      ;put in order (lower)
        mov di,ax
        bswap EDI                       ;(put back in order)

        mov ax,bp                       ;make ax that as well

        cmp byte ptr [type1_ident],3
        je near ident_3_illegalbase     ;first section CAN'T be a number

        cmp byte ptr [type2_ident],1    ;check for register (2)
        jne short ident_3_noregch

        mov ah,dh
        push si
        mov si,reg_ident
        mov al,[si+1]           ;get information
        pop si
        shl ax,4                ;split relation
        shr al,4                ;split exact
        cmp ah,bl
        jne near ident_3_nomatch4

ident_3_noregch:cmp byte ptr [type2_ident],2    ;check memory (2)
        jne short ident_3_nomemch

        cmp byte ptr [type1_ident],2
        je near ident_3_illegalbase
nop                             ;nothing really to check..

ident_3_nomemch:cmp byte ptr [type1_ident],1    ;check register (1)
        jne short ident_3_noregch1

        mov ah,dh
        push si
        mov si,reg_ident
        mov al,[si]             ;get information (Reg 2)
        pop si
        shl ax,4                ;split relation
        shr al,4                ;split exact
        cmp ah,bh
        jne near ident_3_nomatch4

ident_3_noregch1:cmp byte ptr [type1_ident],2   ;check memory (2)
        jne short ident_3_nomemch1

        nop                     ;there's nothing to check

ident_3_nomemch1:cmp byte ptr [type2_ident],3   ;check number size (3)
        jne short ident_3_nocheck
        mov ax,bp                               ;prepair
        cmp al,0
        je near ident_3_illegal
;ok from here we will see if it fits in the byte sizes
        cmp [numtype_ident],al  ;compare with number stored (size)
        jbe short ident_3_nocheck

        mov byte ptr [err_ident],9      ;put up 'Number Too big'
        jmp near ident_3_nomatch4

ident_3_nocheck:mov ax,bp               ;get includes
        mov [opinclude_ident],ah        ;save new includes (if any)

        push bx                         ;save vital information
        bswap ebx
        pop bx

        mov bh,bl                       ;save alternate data (bl)
        mov bl,fs:[si+3]                ;get computer version
        cmp [CompVerSet],bl
        jb near ident_3_nomatch4

        mov dl,fs:[si+4]                ;get size of instruction

        cmp dl,0
        je near ident_3_illegalbase     ;instruction size fails (0) error


ident_3_notExact:mov [size_ident],dl    ;save size (needed)
        mov ah,dh                       ;since it seems to match, continue
        shl ax,3
;successful, now add on..

;AX and CX and BX are free
;moved information, SI becomes SI+5
        BSWAP EAX                       ;AH is 1, AL is 2
        push si
        push di
        push ax
        push cx

        xor cx,cx                       ;Clear out for checks

        cmp ah,1
        jne short ident_3_e_1
        cmp byte [type1_ident],1        ;register check, 1 byte
        jne short ident_3_e_1a
        inc cx                          ;Add 1
;memory checks are invalid, for either part..
ident_3_e_1a:cmp byte [type1_ident],2   ;check for mem (illegal)
        jne short ident_3_e_1           ;found bug :)

;        pop cx
;        pop ax
;        pop di
;        pop si
;        jmp near ident_3_illegal
;        inc cx                          ;can only be 1 choice

ident_3_e_1:cmp al,1
        jne short ident_3_e_2           ;check for exact

        cmp byte [type2_ident],1        ;register check, 1 byte
        jne short ident_3_e_2a

        inc cx                          ;Add 1
ident_3_e_2a:cmp byte [type2_ident],3   ;number check
        jne short ident_3_e_2b
        add cl,[numtype_ident]          ;add size to the check.

ident_3_e_2b:cmp byte [type2_ident],2   ;check mem (illegal)
        jne short ident_3_e_2           ;found bug :)
;        pop cx
;        pop ax
;        pop di
;        pop si
;        jmp near ident_3_illegal
;        inc cx                          ;can only be one thing

ident_3_e_2:mov ch,[size_ident]         ;get size (real one) and add on
        mov [size_ident_e],cl           ;adjust

        sub ch,cl
        mov cl,dh                       ;clear
        xchg ch,cl                      ;switch..
        add si,cx

        mov di,reg_ident
        cmp ah,1                        ;ident1
        jne short ident_3_e_e1b

        cmp byte [type1_ident],1        ;check register (1)
        jne short ident_3_e_e1a
        mov ah,fs:[si+5]                ;ok, get register, and compare

        cmp [di],ah                     ;check first one
        jne near ident_3_nomatch_ex     ;special nomatch4 :)

        inc si
ident_3_e_e1a:
        cmp byte [type1_ident],2        ;check memroy (2)
        jne short ident_3_e_e1b

        cmp [memdata_ident],1
        jne near ident_3_nomatch_ex     ;doesn't work
        cmp [memdata_ident+2],6
        jne near ident_3_nomatch_ex     ;doesn't work
;        mov byte [memdata_ident],0      ;remove the header

ident_3_e_e1b:cmp al,1                  ;ident2
        jne short ident_3_e_e1c

        cmp byte [type2_ident],1        ;registers
        jne short ident_3_e_e2a
        mov ah,fs:[si+5]                ;get register info.

        cmp [di+1],ah                   ;bug here.. checks SECOND one
        jne near ident_3_nomatch_ex     ;special nomatch4 :)
ident_3_e_e2a:cmp byte [type2_ident],3  ;number check
        jne short ident_3_e_e1

;bug found, compared according to the size
        mov ecx,[number_ident]          ;extract number

        cmp byte [numtype_ident],1      ;byte check
        je short ident_3_e_e3a
        cmp byte [numtype_ident],2      ;word check
        je short ident_3_e_e3b
        cmp byte [numtype_ident],4      ;quad check
        je short ident_3_e_e3c

        pop cx
        pop ax
        pop di
        pop si
        jmp near ident_3_illegal        ;something's wrong

ident_3_e_e3a:  cmp fs:[si+5],cl        ;byte check
                jne near ident_3_nomatch_ex
                jmp short ident_3_e_e1
ident_3_e_e3b:  cmp fs:[si+5],cx        ;word check
                jne near ident_3_nomatch_ex
                jmp short ident_3_e_e1
ident_3_e_e3c:  cmp fs:[si+5],ecx       ;quad check
                jne near ident_3_nomatch_ex
ident_3_e_e1:   cmp byte [type2_ident],2        ;check memroy (2)
        jne short ident_3_e_e1c

        cmp [memdata_ident],1
        jne near ident_3_nomatch_ex     ;doesn't work
        cmp [memdata_ident+2],6
        jne near ident_3_nomatch_ex     ;doesn't work
;        mov byte [memdata_ident],0      ;remove the header
ident_3_e_e1c:  pop cx
                pop ax
                pop di
                pop si
                BSWAP EAX
;exact adjustments :P (later)

        add si,5                        ;already have size
        mov di,real_ident               ;prepair write

        mov cx,dx                       ;prepair loop

        sub cl,[size_ident_e]           ;fix by means of exacts?
        mov [size_ident],cl             ;save size



;need information here, exacts specifically, now check it out.
ident_3_saveit:mov al,fs:[si]   ;grab
        mov [di],al             ;save
        inc si
        inc di
        loop short ident_3_saveit
;process first letter.

        mov al,[null_ident]     ;grab (untouched) type
        mov [bit32_ident],al    ;save info

        mov ax,bp               ;opinclude
        mov [opinclude_ident],al


;disabled until i think it's needed again.. (Is it?)

;check for exacts, and use them accordingly

        bswap eax               ;grab information
        cmp ax,0
        jne near ident_3_final_exacts

ident_3_final_11:
        cmp byte ptr [type1_ident],1    ;reg / reg
        jne short ident_3_final_12
        cmp byte ptr [type2_ident],1
        jne short ident_3_final_12
;  11 Reg1 Reg2

        push si
        mov si,reg_ident
        mov al,[si]             ;get information (Reg 1)
        shl ax,4                ;split relation
        shr al,4                ;split exact
        mov bh,al

        mov al,[si+1]           ;get information (Reg 2)
        shl ax,4                ;split relation
        shr al,4                ;split exact
        mov bl,al
        pop si


;override added here
;this override, if higher then 1, will reverse the two data pieces (bl bh)

        bswap edi               ;pull override out of the shadows
        cmp di,1
        jne short ident_3_final_11a
        xchg bl,bh              ;reverse now
ident_3_final_11a:bswap edi     ;return to normal

        shl bl,5                ;place at end
        shl bx,3                ;converse to places
        add bh,0c0h             ;put in format (300o)

        add byte [size_ident],1 ;increase size accordingly
        mov [di],bh
        inc di                  ;move up

        bswap edi               ;pull override out of the shadows
        cmp di,2
        jne short ident_3_final_11b
;ok, found the information we needed.. add in number
        bswap edi

        mov al,[numtype_ident]  ;get data
        add [size_ident],al     ;increase size accordingly
        mov eax,[number_ident]  ;get number
        mov [di],eax            ;save in right location
        jmp short ident_3_final_11c

ident_3_final_11b:bswap edi     ;return to normal
ident_3_final_11c:jmp near ident_3_gotit

ident_3_final_12:
                cmp byte ptr [type1_ident],1    ;reg / mem
                jne short ident_3_final_21
                cmp byte ptr [type2_ident],2
                jne short ident_3_final_21
;build the format.. based on, ?? Reg ???

                mov ax,bp               ;backup, and will use AH a little
                mov bp,memdata_ident    ;grab required area
                xor cx,cx               ;create size looper
                push si

                mov si,reg_ident
                mov al,[si]             ;get information (Reg 1)
                shl ax,4                ;split relation
                shr al,4                ;split exact
                mov bl,al

ident_3_final_21_skip:

                mov si,cx               ;get cleared side counter
                mov cl,cs:[bp]          ;get data size (1-2)
                add byte [size_ident],cl;increase size accordingly
                add si,2
;prepair first pass
                cmp cx,0
                je short ident_3_mm3    ;there are no passes.

                mov al,cs:[bp+si]       ;get mem byte
                shl bl,3                ;fix encoding
                add al,bl               ;add encoding
                jmp short ident_3_mm2   ;skip for first run

ident_3_mm:     mov al,cs:[bp+si]       ;grab mem byte
ident_3_mm2:    mov [di],al             ;save byte (First pass)
                inc di                  ;update
                inc si
                loop short ident_3_mm

ident_3_mm3:    mov cl,cs:[bp+1]        ;grab size to save (of side data)
                add byte [size_ident],cl;increase size accordingly
                mov eax,[memnum_ident]  ;grab number
                mov [di],eax            ;save number (may be null)

;they are already SET
                pop si                  ;resume previous number
                jmp near ident_3_gotit
ident_3_final_21:
                cmp byte ptr [type1_ident],2    ;mem / reg
                jne short ident_3_final_13
                cmp byte ptr [type2_ident],1
                jne short ident_3_final_13

                mov ax,bp               ;backup, and will use AH a little
                mov bp,memdata_ident    ;grab required area
                xor cx,cx               ;create size looper
                push si

                mov si,reg_ident
                mov al,[si+1]           ;get information (Reg 1)
                shl ax,4                ;split relation
                shr al,4                ;split exact
                mov bl,al

                jmp short ident_3_final_21_skip

ident_3_final_13:
                cmp byte ptr [type1_ident],1    ;reg / num
                jne short ident_3_final_23
                cmp byte ptr [type2_ident],3
                jne short ident_3_final_23


; 11 Enc Reg1

        push si
        mov si,reg_ident
        mov al,[si]             ;get information (Reg 1)
        shl ax,4                ;split relation
        shr al,4                ;split exact
        mov bl,al
        pop si


        shl bh,3                ;put in place
        add bh,bl

        add bh,0c0h             ;put in format (300o)

        add byte [size_ident],1 ;increase size accordingly
        mov [di],bh

        inc di                  ;fix

        bswap edi               ;grab overrides
        cmp di,0
        je short ident_3_final_13c
        mov ax,di               ;get override data
        xchg al,ah              ;replace locations
        jmp short ident_3_final_13d;skip into the rest of it all
;grabbed from (Exacts 3)
ident_3_final_13c:mov ah,[numtype_ident]        ;grab size
ident_3_final_13d:bswap edi     ;return
        add [size_ident],ah     ;increase size accordingly
        mov eax,[number_ident]  ;get number
        mov [di],eax            ;save in right location
        jmp near ident_3_gotit


ident_3_final_23:cmp byte ptr [type1_ident],2   ;mem / num
                jne short ident_3_final_33
                cmp byte ptr [type2_ident],3
                jne short ident_3_final_33
;based on ?? Re2 ???

                mov ax,bp               ;backup, and will use AH a little
                mov bp,memdata_ident    ;grab required area
                xor cx,cx               ;create size looper
                push si

                bswap ebx       ;data is in BL
                mov bh,0        ;clear

;ident_3_final_23_skip:

                mov si,cx               ;get cleared side counter
                mov cl,cs:[bp]          ;get data size (1-2)
                add byte [size_ident],cl;increase size accordingly
                add si,2
;prepair first pass
                mov al,cs:[bp+si]       ;get mem byte

                cmp bl,8
                jb short ident_3_final_23_testenc
                and bl,3                ;ensure it doesn't interphere
                xchg al,bl              ;switch
ident_3_final_23_testenc:shl bl,3       ;fix encoding
                add al,bl               ;add encoding
                jmp short ident_3_mm223 ;skip for first run

ident_3_mm23:   mov al,cs:[bp+si]       ;grab mem byte
ident_3_mm223:  mov [di],al             ;save byte (First pass)
                inc di                  ;update
                inc si
                loop short ident_3_mm23

                mov cl,cs:[bp+1]        ;grab size to save (of side data)
                add byte [size_ident],cl;increase size accordingly
                mov eax,[memnum_ident]  ;grab number
                mov [di],eax            ;save number (may be null)

                add di,cx               ;increase for memnum

;grabbed from (Exacts 3)
                bswap edi               ;grab overrides
                cmp di,0
                je short ident_3_final_23c
                mov ax,di               ;get override data
                xchg al,ah              ;replace locations
                jmp short ident_3_final_23d;skip into the rest of it all
;grabbed from (Exacts 3)
ident_3_final_23c:mov ah,[numtype_ident];grab size
ident_3_final_23d:
                bswap edi             ;return
                add [size_ident],ah     ;increase size accordingly
                mov eax,[number_ident]  ;get number
                mov [di],eax            ;save in right location

                pop si                  ;resume previous number
                jmp near ident_3_gotit
ident_3_final_33:jmp near ident_3_gotit ;both numbers impossible

;ok here's the separator..
ident_3_final_exacts:jmp short ident_3_final_exacts_notmem

;ok, there are weak parts, let's find them and use them.
;first check mem stuff..

ident_3_final_exacts_mem:nop
;ok, we need to see what we can find here.. data suggests 3(mem) is exact
;build the format.. based on, 11 REG (unused) ??? (treat REG as the data)

                bswap ebp               ;grab saved special
                mov bx,bp               ;transfer
                bswap ebp               ;return to normal ?

                mov bp,memdata_ident    ;grab required area
                xor cx,cx               ;create size looper
                push si

                mov si,cx               ;get cleared side counter
                mov cl,cs:[bp]          ;get data size (1-2)
                add byte [size_ident],cl;increase size accordingly
                add si,2
;prepair first pass
                mov al,cs:[bp+si]       ;get mem byte
                shl bl,3                ;fix encoding
                add al,bl               ;add encoding
                jmp short ident_3_final_mm223 ;skip for first run

ident_3_final_mm23:mov al,cs:[bp+si]    ;grab mem byte
ident_3_final_mm223:mov [di],al         ;save byte (First pass)
                inc di                  ;update
                inc si
                loop short ident_3_final_mm23

                mov cl,cs:[bp+1]        ;grab size to save (of side data)
                add byte [size_ident],cl;increase size accordingly
                mov eax,[memnum_ident]  ;grab number
                mov [di],eax            ;save number (may be null)

                add di,cx               ;increase for memnum

;grabbed from (Exacts 3)

                pop si                  ;resume previous number
                jmp near ident_3_gotit

ident_3_final_exacts_notmem:

;special memcheck

                cmp ah,1                ;see if exacts
                jne short ident_3_final_exacts_spmem1
                cmp byte [type1_ident],2;see if mem
                jne short ident_3_final_exacts_spmem1
        push eax        ;fix for ENTER and other specials
        mov eax,[memnum_ident]          ;grab number
        mov [di],eax                    ;save

        mov ah,0
        mov al,[memdata_ident+1]        ;grab size
        add [size_ident],al             ;increase size accordingly
        add di,ax                       ;update position
        pop eax
ident_3_final_exacts_spmem1:
                cmp al,1                ;see if exacts
                jne short ident_3_final_exacts_spmem2
                cmp byte [type2_ident],2;see if mem
                jne short ident_3_final_exacts_spmem2
        push eax        ;fix for ENTER and other specials
        mov eax,[memnum_ident]          ;grab number
        mov [di],eax                    ;save

        mov ah,0
        mov al,[memdata_ident+1]        ;grab size
        add [size_ident],al             ;increase size accordingly
        add di,ax                       ;update position
        pop eax
ident_3_final_exacts_spmem2:


                cmp ax,0101h            ;ok, here we are.. Shall we?
                je near ident_3_gotit   ;save the data here, all done

                cmp ah,1    ;check first one
                je short ident_3_final_exacts_first

                cmp byte [type1_ident],2        ;mem?
                je near ident_3_final_exacts_mem

;build the format.. based on, 11 REG (unused) ???
;from the original set (Ident_2) changed as needed

                cmp byte [type1_ident],1        ;regs?
                jne short ident_3_final_exacts_first
;ok, check to see if qualify, if so, grab the OTHER bp

                cmp word [type1_ident],0101h
                jne short ident_3_final_exacts_first_NOTBP

                bswap ebp               ;grab saved special
                mov ax,bp               ;transfer
                bswap ebp               ;return to normal ?
                jmp short ident_3_final_exacts_first_BP

ident_3_final_exacts_first_NOTBP:mov ax,bp
                bswap ebx               ;extract relations
                mov ah,bl               ;grab second (extended data)

ident_3_final_exacts_first_BP:mov al,[reg_ident]        ;get register
                and al,7                ;grab the important bits

                shl ah,3                ;place at location xx ENC reg

                add al,0C0h             ;put in format (300o)
                add al,ah               ;put in extra data

                add byte [size_ident],1 ;increase size accordingly
                mov [di],al             ;move on
                jmp near ident_2_gotit
ident_3_final_exacts_first:cmp al,1                ;check first one
                je short ident_3_final_exacts_second

                cmp byte [type2_ident],2        ;mem?
                je near ident_3_final_exacts_mem

                cmp byte [type2_ident],1
                jne short ident_3_final_exacts_first1

;build the format.. based on, 11 REG (unused) ???
;from the original set (Ident_2) changed as needed

                cmp word [type1_ident],0101h
                jne short ident_3_final_exacts_secon_NOTBP

;ident_3_final_exacts_first_okBP:
                bswap ebp               ;grab saved special
                mov ax,bp               ;transfer
                bswap ebp               ;return to normal ?
                jmp short ident_3_final_exacts_secon_BP

ident_3_final_exacts_secon_NOTBP:mov ax,bp
ident_3_final_exacts_secon_BP:mov al,[reg_ident+1];get second register

                shl ah,3                ;place at location (extra data)
                and al,7                ;grab the important bits

                add al,300o             ;put in format (300o)
                add al,ah               ;put in extra data

                add byte [size_ident],1 ;increase size accordingly
                mov [di],al
                jmp near ident_2_gotit
ident_3_final_exacts_first1:cmp byte [type2_ident],3
                jne short ident_3_final_exacts_second

                bswap edi                       ;grab overrides
                cmp di,0
                je short ident_3_final_exacts_first2a
                mov ax,di                       ;get override data
                xchg al,ah                      ;replace locations
                jmp short ident_3_final_exacts_first2b  ;skip into the rest
;grabbed from (Exacts 3)
ident_3_final_exacts_first2a:mov ah,[numtype_ident];grab size
ident_3_final_exacts_first2b:cmp ah,3           ;safe (3=4)
                jne short ident_3_final_exacts_first2c
                inc ah
ident_3_final_exacts_first2c:bswap edi          ;return
                add [size_ident],ah             ;increase size accordingly

                mov eax,[number_ident]          ;get number
                mov [di],eax                    ;save in right location
                jmp short ident_3_gotit

ident_3_final_exacts_second:jmp near ident_3_gotit      ;last resort, end it all

ident_3_nomatch_ex:pop cx
                pop ax          ;little problem with exacts.. Fixed :)
                pop di
                pop si

ident_3_nomatch4:add si,4
                mov ah,0        ;clear (anyway)
                mov al,fs:[si]  ;get size of instruction

                cmp al,0
                je short ident_3_illegalbase    ;check for size problems

                cmp al,64
                jae short ident_3_illegalbase   ;continue checking

                add si,ax       ;save and update
                inc si
                loop near ident_3_under
                pop di
                jmp near ident_3_ftype
ident_3_illegal:mov byte ptr [err_ident],9      ;put up 'Illegal'
                jmp short ident_3_gotit
ident_3_illegalbase2:pop ax                     ;fix sp location
ident_3_illegalbase:mov byte ptr [err_ident],10 ;put up 'Illegal/database'
                jmp short ident_3_gotit
ident_3_nomore: mov byte ptr [err_ident],1;put up 'not identified'
ident_3_gotit:  pop di
                popad
                ret


ident_part:     pusha
                mov cx,3
                mov si,type1_ident
                mov byte ptr [si],0     ;clear prepair
                mov byte ptr [si+1],0
ident_part_0:   cmp byte ptr gs:[di]," ";check for space
                je short ident_part_1
                cmp byte ptr gs:[di],",";check comma (end, beginning?)
                je short ident_part_1
                cmp byte ptr gs:[di],10 ;check ending?
                je near ident_part_gotit;at end anyway
                inc di                  ;no continue
                jmp short ident_part_0
ident_part_1:   inc di                  ;move up one (worked)
;first the number (easiest)
                push di

ident_part_11:  cmp byte ptr gs:[di],"-";negative?
                jne short ident_part_2
                inc di                  ;negative dissapear?

ident_part_2:   cmp byte ptr gs:[di],34 ;ok, will qualify as number now
                je short ident_part_3

                cmp byte ptr gs:[di],"0";within range?
                jb short ident_part_notnumber
                cmp byte ptr gs:[di],"9"
                ja short ident_part_notnumber
ident_part_3:   mov byte ptr [si],3     ;save as 'number'
;not perfect
                pop di                  ;return to normal (if changed from neg)
                pushad                  ;prepair special save
                mov dx,gs               ;save segment
                mov si,di               ;get location to read
                call near getnum                ;get the number

ident_part_4:
;   mov [number_ident],eax          ;save number
                call near Ident_number_size     ;get size in DL

                cmp byte [numtype_ident],0;already set?
                je short ident_part_numberISset1 ;is not set, and will be set

;work around... if both are numbers, the first is treated like a memory
;location

                cmp quad [number_ident],0       ;save number
                je short ident_part_numberISseta

;has confirmed the information, now to use it all.

                push eax                ;save number
                mov byte [type1_ident],2;set as memory
                mov eax,[number_ident]  ;grab the 32 bit number
                mov [memnum_ident],eax  ;place it correctly
                mov byte [memdata_ident],1      ;set size as 1
                mov byte [memdata_ident+1],2    ;assume word.
                mov byte [memdata_ident+2],6    ;set as input direct
                pop eax
                jmp short ident_part_numberISset1;save it as was a 0
;size of instruction, size of memnum to accept, instruction, Junk

ident_part_numberISseta:cmp [numtype_ident],dl  ;is equal or greater already?
                 jae short ident_part_numberISset4;is set perfectly/small

ident_part_numberISset0:mov byte ptr [err_ident],5;put 'too large' error on.
                xor eax,eax                     ;clear
                jmp short ident_part_numberISset4;save 0
;too large? (if NEAR, might be ok?)

ident_part_numberISset1:mov [numtype_ident],dl  ;set number
ident_part_numberISset4:mov [number_ident],eax  ;save number (new save)
ident_part_numberISset2:popad                   ;return from
;there's a bug here, now to get rid of it.

                cmp gs:[di],34                  ;look for quotes
                jne short ident_part_numberISset3

ident_part_numberISset2b:inc di                 ;move up

                cmp gs:[di],34
                je short ident_part_numberISset3;is quotes?

                cmp gs:[di],13
                je short ident_part_numberISset3b;is end?

                cmp gs:[di],10
                je short ident_part_numberISset3b;is end?
                jmp short ident_part_numberISset2b


ident_part_numberISset3:inc di                  ;final fix
ident_part_numberISset3b:jmp near ident_part_next;ok, skip out

ident_part_notnumber:pop di                     ;return first
                cmp byte ptr gs:[di],"["        ;required as a memory
                jne short ident_part_notmem
                mov byte ptr [si],2             ;save as 'memory'
;memory stuff here, but incomplete yet
                call near memory_part

                jmp near ident_part_next
ident_part_notmem:push si                       ;needed?
                mov si,di                       ;save and checs
ident_part_fsize1:cmp byte ptr gs:[si],","
                je short ident_part_fsize2
                cmp byte ptr gs:[si],10 ;check end of part
                je short ident_part_fsize2
                inc si
                jmp short ident_part_fsize1
ident_part_fsize2:sub si,di             ;size check
                push cx         ;save loops (whole)
                mov dx,si       ;get size
                mov bp,fs:[ident_pt0] ;location of registers
ident_part_regcheck:
                mov ax,fs:[bp]
                cmp al,0
                je short ident_part_Nnext ;skip early (if nothing left)
                xor bx,bx
                xchg bh,al
                shr bx,4        ;fix out information
                shr bl,4
                shr ax,4
                shr al,4

;ah=text length al=#size bh=type bl=number of registers
;dl=text size compare

                cmp dl,ah       ;check sizes
                jne short ident_part_scheck

                mov si,bp       ;prepair
                add si,2        ;fix location
                mov cl,bl       ;prepair loops

ident_part_scheck2:push cx         ;save
                push ax
                push di         ;save location
                mov cl,ah       ;prepair texts

ident_part_scheck0:mov al,fs:[si]  ;grab letter
                cmp gs:[di],al  ;compare
                jne short ident_part_scheck1
                inc si
                inc di
                loop short ident_part_scheck0
;if equal
                pop di          ;return location
                pop ax
                pop cx
                sub bl,cl       ;number it
                shl bl,4        ;prepair
                shr bx,4        ;fit in 1 byte
                pop cx          ;count
                mov si,reg_ident
                cmp cx,3        ;see if first one
                je short ident_part_saveitall
                inc si          ;fix location (second)
ident_part_saveitall:mov [si],bl;save information on the register
                mov si,reg_ident_info
                cmp cx,3        ;see if first one
                je short ident_part_saveitall2
                inc si          ;fix location (second)
ident_part_saveitall2:mov [si],al;save data size of register

                pop si          ;location to save
                mov byte ptr [si],1;count as 'register'
                jmp short ident_part_next

ident_part_scheck1:pop di          ;return location
                pop ax          ;return information
                add si,cx       ;add remainder of register check
                pop cx          ;get loop
                loop short ident_part_scheck2
                mov bp,si       ;locate?
                jmp short ident_part_regcheck

ident_part_scheck:mov al,ah     ;move text size over
                mov ah,0        ;prepair multiply
                mul bl          ;find length skip
                add ax,2        ;fix location
                add bp,ax       ;set up :)
                jmp short ident_part_regcheck
ident_part_Nnext:pop cx         ;get original CX loops
                pop si          ;previous pop?
ident_part_next:inc si                  ;location of ident
                loop near ident_part_0  ;resumeing
ident_part_gotit:popa
                ret

;input
;DI=start of instruction location
;AL=type of direct (byte size per area (divide remainder ect))
ident_direct:   pushad          ;save for function
                mov byte [err_ident],0          ;kill other (in case)

                push fs
                mov bx,[direct_segment]
                mov fs,bx       ;pepair segment
                mov ah,0
                mov bp,ax       ;save information
                xor cx,cx       ;blank out size
                mov si,cx       ;location to start saving at

;at this point, we have GS (text) FS (data area)
;DI instruction location
;SI data location
;CX size information
;BP backup of divider/size
                add di,2        ;(start of information?)
ident_direct_lo:inc di
ident_direct_st:cmp byte ptr gs:[di],22h;look for quotes
                je near ident_direct_text

                push di
                cmp byte ptr gs:[di],"-";negative?
                jne short ident_direct_neg
                inc di                 ;negative dissapear?
ident_direct_neg:cmp byte ptr gs:[di],"0";within range?
                jb short ident_direct_other
                cmp byte ptr gs:[di],"9"
                ja short ident_direct_other

                pop di
                push si
                mov si,di       ;instruction grab
                mov dx,gs       ;segment save

                call near getnum
                pop si

                mov fs:[si],eax ;save new bytes

                add cx,bp       ;add new byte information
                add si,bp       ;next location to write at

ident_direct_fc:cmp byte gs:[di],","    ;look for next part
                je short ident_direct_lo;hmm :)
                cmp byte gs:[di],0ah
                je near ident_direct_end;done here

                inc di
                jmp short ident_direct_fc

ident_direct_other:cmp word gs:[di],"DU";time to test this..
;                cmp quad gs:[di],"DUP(";failed for now
                jne near ident_direct_errors
                cmp word gs:[di+2],"P(" ;time to test this..
                jne near ident_direct_errors

                pop di
;ok, now for the dup instruction.
                add di,4                ;(fix location)
                push si
                mov si,di
                mov dx,gs
                call near getnum
                pop si

                xor dx,dx               ;clear out for remainder
                div bp                  ;find remainder?
                cmp dx,0
                je short ident_direct_dupts     ;done here
                inc ax                  ;round up
                xor dx,dx
ident_direct_dupts:mul bp               ;fix size

                cmp eax,0               ;see if neg.. (Special fix)
                jg short ident_direct_numbugfix
                mov ax,bp               ;make minimum of count
ident_direct_numbugfix:

                cmp ax,65532
                jbe short ident_direct_numbugfix2
                mov ax,65532            ;push to max
ident_direct_numbugfix2:

                add cx,ax               ;add new size (bytes)
                push cx

                mov cx,ax               ;set for size now
;k now search quickly for the next part (comma)

ident_direct_dup0:cmp byte gs:[di],","          ;find comma
                je short ident_direct_dup1
                inc di
                jmp short ident_direct_dup0
ident_direct_dup1:inc di  ;fix location
;k, now identify if number or text (simple test)

                cmp byte gs:[di],34
                je short ident_direct_dupt
;assumed number, don't mess it up now
                mov dx,gs
                push si
                mov si,di
                call near getnum        ;get new number
                pop si

ident_direct_dupn:mov fs:[si],eax       ;write number there (fill)
                add si,bp
                sub cx,bp
                cmp cx,0
                je short ident_direct_dupn2 ;loop messes up the count
                jmp short ident_direct_dupn

ident_direct_dupn2:pop cx               ;fix
                jmp short ident_direct_fc;fix

ident_direct_dupt:inc di
                cmp byte gs:[di],34     ;test if empty
                jne short ident_direct_dupt2
                xor eax,eax
                jmp short ident_direct_dupn;empty, so null text
ident_direct_dupt1:pop di               ;hmm.. a little close :P
ident_direct_dupt2:push di              ;assumes something is here
ident_direct_dupt3:mov al,gs:[di]       ;grab text byte
                cmp al,34
                je short ident_direct_dupt1     ;restart text
                mov fs:[si],al          ;save text
                inc si
                inc di
                loop short ident_direct_dupt3   ;continue grabbing text
                pop di
                pop cx                  ;previous information size?
                jmp near ident_direct_fc;fix

ident_direct_text:mov dx,cx     ;save information (for later)
                inc di          ;skip out the quotes

ident_direct_nx:mov al,gs:[di]
                cmp al,34       ;look for new quotes
                je short ident_direct_qu
                mov fs:[si],al  ;save new byte
                inc cx          ;update size, and locations
                inc si
                inc di
                jmp short ident_direct_nx

ident_direct_qu:mov ax,cx
                sub ax,dx       ;get size
                xor dx,dx       ;clear out for remainder
                div bp          ;find remainder?
                cmp dx,0
                je near ident_direct_fc;done here

                push bp         ;save size
                sub bp,dx       ;fix

                add cx,bp       ;update
                push cx
                mov cx,bp       ;set up loop
                mov al,dh       ;empty byte
ident_direct_cl:mov fs:[si],al  ;clear
                inc si
                loop short ident_direct_cl
                pop cx
                pop bp          ;return
                jmp near ident_direct_fc

ident_direct_errors:pop di
                mov byte [err_ident],11    ;check line

ident_direct_end:pop fs

;ok, save information
                mov byte [direct_on_ident],1    ;turn on
                mov [direct_size_ident],cx      ;save size
                mov byte [size_ident],0         ;kill other (in case)
                popad
                ret

;determine size (NEC?), IN: EAX, OUT: DL
Ident_number_size:pushad
                mov dl,4        ;size auto determine

                test eax,80000000h

                jnz short Ident_number_sizeISneg ;(if it matches?)

                cmp eax,65535
                ja short Ident_number_sizeISset
                mov dl,2        ;ok 16 next

                cmp word eax,255
                ja short Ident_number_sizeISset

                mov dl,1        ;save as 1 byte qualify (barely?)

                jmp short Ident_number_sizeISset
Ident_number_sizeISneg:cmp eax,-32768
                jl short Ident_number_sizeISset
                mov dl,2        ;ok 16 next

                cmp eax,-128
                jl short Ident_number_sizeISset

                mov dl,1        ;save as 1 byte qualify (barely?)

Ident_number_sizeISset:mov bp,sp;grab stack location

                mov eax,[bp+pedx];prepair number
                mov al,dl       ;return a single value
                mov [bp+pedx],eax;save number

                popad
                ret

;simple, remove all registers from memory location, while saving their
;order, any numbers left will be dealt with.. (no special math please)
memory_part:    pushad

                push di                 ;save instruction information
                xor cx,cx               ;save text size

                mov bp,cx               ;prepair 386 count

memory_part_pre:mov si,memory_data
                
memory_part_pre0:cmp byte ptr gs:[di],"]";end of text
                je short memory_part_pre4
                mov cl,[si]             ;get text size
                cmp cx,0
                je short memory_part_pre3
                push di                 ;save location
                mov ax,[si+1]           ;get extended information
                add si,3                ;locate text
                mov bx,cx               ;save backup?

memory_part_pre1:mov dl,[si]            ;get letter
                cmp gs:[di],dl          ;compare
                jne short memory_part_pre2 ;continue?
                inc si
                inc di                  ;get other location set
                loop short memory_part_pre1

                pop di                  ;get start location
                mov cx,bx               ;set up count
memory_part_pre5:mov byte ptr gs:[di]," " ;clear out

                inc di                  ;move up one
                loop short memory_part_pre5 ;clear area (text)
                xor bx,bx               ;clear
                mov bl,al

;enter in here, extended 386 half, as well as notifyer

                cmp bx,3
                jne short memory_part_pre5_2
                add bx,bp       ;add new location
                inc bp          ;increase by one
                inc byte ptr [memdata_ident]    ;add count to 386's
memory_part_pre5_2:mov [bx+memdata_ident],ah    ;save
                jmp short memory_part_pre       ;jump to next location

memory_part_pre2:pop di                 ;return base
                add si,cx               ;increase size
                jmp short memory_part_pre0 ;continue looping
memory_part_pre3:inc di
                jmp short memory_part_pre
memory_part_pre4:pop di

;cut and paste from Ident earlier (space remove) all but 1
memory_part_blk:cmp byte ptr gs:[di]," "
                jne short memory_part_blk0
                inc di                  ;move up one
                jmp short memory_part_blk
memory_part_blk0:xor cx,cx              ;prepair count (0)
                mov ch,1
                mov si,di               ;start location
                push di                 ;save start

memory_part_blk1:cmp byte ptr gs:[di],34;check for quotes
                jne short memory_part_blk2
                neg ch                  ;setting on

memory_part_blk2:cmp ch,1               ;see if quote on
                je short memory_part_blk3       ;skip if so
                jmp short memory_part_blk4      ;write reguardless

memory_part_blk3:cmp byte ptr gs:[di]," ";check space
                je short memory_part_blk5       ;space?

memory_part_blk4:mov al,gs:[di]         ;get byte
                mov gs:[si],al          ;save byte
                inc si

memory_part_blk5:cmp byte ptr gs:[di],10;check end of line
                je short memory_part_blk6       ;at end of line
                inc di                  ;move up (also space skip)
                jmp short memory_part_blk1

memory_part_blk6:cmp word ptr gs:[si-3],0a20h   ;see if it's this?
                jne short memory_part_blk7
                mov byte ptr gs:[si-3],10
memory_part_blk7:pop di
                mov dx,gs       ;prepair
                inc di          ;fix past [
                mov si,di
                call near getnum

                mov cs:[memnum_ident],eax       ;save data
;onto tedious work
;use translation table if 16 bit is used, or resort to 32bit

;16 bit assumed first
                mov bx,memdata_ident

                cmp byte ptr cs:[bx],0
                je near memory_part_not32
;32 bit code here
                mov byte ptr cs:[dist32_ident],2;set 32bit flag
                xor dx,dx                       ;dx will be data.
                cmp eax,0
                je short memory_part_32_num
                inc dh                          ;add to number
                cmp eax,127
                jg short memory_part_32_num2
                cmp eax,-128
                jl short memory_part_32_num2
                jmp short memory_part_32_num
memory_part_32_num2:inc dh                      ;set 32 bit..
memory_part_32_num:
;ok, on to other stuff..
                cmp byte ptr cs:[bx],2
                je short memory_part_32_two     ;ok, single, this will treat as a normal formula
                mov dl,cs:[bx+3]
                mov cx,dx                       ;backup
                mov cx,dx

                cmp cl,2                        ;update 32bit
                jne short memory_part_32_one
                add cl,cl                       ;double
memory_part_32_one:
                shl dl,2                        ;prepair (right side)
                shr dx,2                        ;put in place
                mov byte ptr cs:[bx],1          ;set the size up
                mov cs:[bx+1],ch                ;size of memory addin
                mov cs:[bx+2],dl                ;put in data
                jmp near memory_part_resume
memory_part_32_two:mov dl,4                     ;sp (opcode extend)
                mov cx,dx
                cmp cl,2
                jne short memory_part_32_two2
                add cl,cl                       ;double
memory_part_32_two2:
                shl dl,2                        ;prepair (right side)
                shl dx,6                        ;put in place

                mov dl,cs:[bx+7]                ;Scale (SIB)
                shl dl,3                        ;put in place
                add dl,cs:[bx+4]                ;Index
                shl dl,3                        ;put in place
                add dl,cs:[bx+3]                ;Base

                mov byte ptr cs:[bx],2          ;set the size up
                mov cs:[bx+1],ch                ;size of memory addin
                mov cs:[bx+2],dh                ;put in data
                mov cs:[bx+3],dl                ;put in data
                jmp near memory_part_resume

memory_part_not32:mov byte ptr cs:[dist32_ident],1;set 16bit flag
                cmp word ptr cs:[bx+1],0        ;empty at all?
                je short memory_part_not16
;16 bit code here
                xor dx,dx                       ;dx will be data.
                cmp eax,0
                je short memory_part_16_num
                inc dh                          ;add to number
                cmp eax,127
                jg short memory_part_16_num2
                cmp eax,-128
                jl short memory_part_16_num2
                jmp short memory_part_16_num
memory_part_16_num2:inc dh                      ;set 16 bit..
                cmp eax,32767
                jg short memory_part_16_num3
                cmp eax,-32768
                jl short memory_part_16_num3
                jmp short memory_part_16_num
memory_part_16_num3:xor eax,eax                 ;ok, illegal, but work anyway
                mov cs:[memnum_ident],eax       ;save null
memory_part_16_num:mov ax,cs:[bx+1]             ;grab 2 byte information
                mov si,memory_data2_T
                mov cx,8                        ;max count (locked in place)
memory_part_16_fix:cmp cs:[si],ax
                jne short memory_part_16_fix1   ;if it doesn't fit
                mov ax,cs:[si+2]                ;update
                jmp short memory_part_16_fix2
memory_part_16_fix1:add si,4                    ;increase on
                loop short memory_part_16_fix
memory_part_16_fix2:cmp ah,6                    ;open part BP
                jne short memory_part_16_fix3   ;is it?
                cmp dh,0                        ;any carryover?
                jne short memory_part_16_fix3   ;if higher then 0, don't bother
                inc dh                          ;make at least +0
memory_part_16_fix3:mov byte ptr cs:[bx],1      ;set the size up
                mov cs:[bx+1],dh                ;size of memory addin

                shr ax,6                        ;move in place
                mov ah,dh                       ;get important byte size
                shr ax,2                        ;final update

                mov cs:[bx+2],al                ;put in data
                jmp short memory_part_resume
memory_part_not16:
;ok, pure memory (number in only) 32/16?

                cmp eax,65535
                jg short memory_part_dr_num
                cmp eax,-32768
                jl short memory_part_dr_num
                jmp short memory_part_dr_num1
memory_part_dr_num:mov byte ptr cs:[dist32_ident],2     ;set 32bit flag
                mov dh,4                                ;32 bits
                mov dl,4                                ;sp? (Mem)?
                jmp short memory_part_dr_num2
memory_part_dr_num1:mov dh,2                            ;16 bits
                mov dl,6                                ;bp+0 (mem)
memory_part_dr_num2:mov byte ptr cs:[bx],1
                mov byte ptr cs:[bx+1],dh               ;save size info
                mov byte ptr cs:[bx+2],dl               ;save actual half
memory_part_resume:nop
memory_part_alldone:popad
                ret

Memory_data:

;format, size of text, location to save text (plus memdata_ident), value, text
;0 is null :P
;1, 2 and 7 are fixed, 3's are open, each time used, they will increase 1
;theory
;Info, Info2, BX/BP, SI/DI, E, E, E, times

db 2,1,1,"BX"
db 2,1,2,"BP"
db 2,2,1,"SI"
db 2,2,2,"DI"
db 2,7,0,"*1"
db 2,7,1,"*2"
db 2,7,2,"*4"
db 2,7,3,"*8"
db 3,3,0,"EAX"
db 3,3,1,"ECX"
db 3,3,2,"EDX"
db 3,3,3,"EBX"
db 3,3,4,"ESP"
db 3,3,5,"EBP"
db 3,3,6,"ESI"
db 3,3,7,"EDI"
db 0
;real 1
;extended

;register 1
;register 2
;register 3
;multiplyer

memory_data2_T:
db 0,1,0,4      ;backwards checked.. SI
db 0,2,0,5      ;DI
db 1,0,0,7      ;BX
db 2,0,0,6      ;there is no BP, but BP+0 there is... need check later
db 1,1,0,0      ;SI+BX
db 2,1,0,2      ;SI+BP
db 1,2,0,1      ;DI+BX
db 2,2,0,3      ;DI+BP
db 0,0,0,0

;db 1,0,0,4      ;backwards checked.. SI
;db 2,0,0,5      ;DI
;db 0,1,0,7      ;BX
;db 0,2,0,6      ;there is no BP, but BP+0 there is... need check later
;db 1,1,0,0      ;SI+BX
;db 1,2,0,2      ;SI+BP
;db 2,1,0,1      ;DI+BX
;db 2,2,0,3      ;DI+BP
;db 0,0,0,0

direct_segment:dw 0             ;segment for Direct information
prefix_data_32:db 66h,67h       ;this is the data information (locked) prefixes
CompVerSet: db 0ffh             ;set up for system (255 = all instructions)
bit32_set: db 1                 ;16 bit by default
opinclude_set: db 0             ;turned off (contains 0???????)

null_ident: db 0                ;a null location (nothing important)
direct_on_ident:db 0            ;if set to 1, will be a direct input data
direct_size_ident:dw 0          ;size of data (starting 0 from segment)
dist_ident: db 0                ;distance identify
numtype_ident: db 0             ;number size (or larger)
type1_ident: db 0               ;identify half
type2_ident: db 0               ;identify half
type3_ident: db 0               ;identify half
;0 nothing, 1 register, 2 memory, 3 number
memnum_ident: dw 0,0            ;the memory location, if there is one
memdata_ident: dw 0,0,0,0       ;reserved for the memory funciton
;also data when memory done.
;size of instruction, size of memnum to accept, instruction, Junk
number_ident: dw 0,0            ;the number, if there is one
numberEXT_ident: dw 0,0         ;extended instructions, unused for now
seg_ident: db 0                 ;segment override?
rep_ident: db 0                 ;repeats are defined
reg_ident: db 0,0               ;the 2 sections of registers (if needed)
reg_ident_info: db 0,0          ;remainder information (size of registers)
err_ident: db 0                 ;errors encountered (only 1 allowed)
start_ident: db 0               ;minus from real
size_ident: db 0                ;instruction size
size_ident_E: db 0              ;Exact Size, subtract from Size_ident
bit32_ident: db 0               ;intruction type/size
dist32_ident: db 0              ;distance 32bit?
opinclude_ident: db 0           ;the special, optional include
prefix_ident: db 0,0,0,0,0
;allows, a segment override (1 byte) 2 type overrides (mem / register),
;and an instruction override (rep)
; 1 extra (just in case)
real_ident: dw 0,0,0,0,0,0,0,0

^D_Data_Size cpx-null_ident   ;Starting at null?

prefix_data:
;format, size of text, location to save text (plus dist_ident), value, text
;0 is null :P

db 5,dist_ident-null_ident,1,"SH","ORT"
db 4,dist_ident-null_ident,2,"NE","AR"
db 3,dist_ident-null_ident,4,"FA","R"
db 5,numtype_ident-null_ident,16,"TBY","TE"     ;for FPU
db 6,numtype_ident-null_ident,8,"DOU","BLE"     ;for FPU
db 5,numtype_ident-null_ident,8,"DQ","UAD"
db 5,numtype_ident-null_ident,8,"QW","ORD"
db 6,numtype_ident-null_ident,4,"SIN","GLE"     ;for FPU
db 5,numtype_ident-null_ident,4,"FLO","AT"      ;for FPU
db 5,numtype_ident-null_ident,4,"DW","ORD"
db 4,numtype_ident-null_ident,4,"QU","AD"
db 4,numtype_ident-null_ident,2,"WO","RD"
db 4,numtype_ident-null_ident,1,"BY","TE"
db 3,null_ident-null_ident,0,"PT","R"           ;0
db 4,seg_ident-null_ident,2eh,"[CS",":"         ;removes label possibility
db 4,seg_ident-null_ident,3eh,"[DS",":"
db 4,seg_ident-null_ident,26h,"[ES",":"
db 4,seg_ident-null_ident,64h,"[FS",":"
db 4,seg_ident-null_ident,65h,"[GS",":"
db 4,seg_ident-null_ident,36h,"[SS",":"

db 4,seg_ident-null_ident,2eh," CS",":"
db 4,seg_ident-null_ident,3eh," DS",":"
db 4,seg_ident-null_ident,26h," ES",":"
db 4,seg_ident-null_ident,64h," FS",":"
db 4,seg_ident-null_ident,65h," GS",":"
db 4,seg_ident-null_ident,36h," SS",":"

db 4,seg_ident-null_ident,2eh,",CS",":"
db 4,seg_ident-null_ident,3eh,",DS",":"
db 4,seg_ident-null_ident,26h,",ES",":"
db 4,seg_ident-null_ident,64h,",FS",":"
db 4,seg_ident-null_ident,65h,",GS",":"
db 4,seg_ident-null_ident,36h,",SS",":"

db 5,rep_ident-null_ident,0f2h,"RE","PNE"
db 4,rep_ident-null_ident,0f0h,"LO","CK"
db 4,rep_ident-null_ident,0f3h,"RE","PZ"
db 4,rep_ident-null_ident,0f3h,"RE","PE"
db 3,rep_ident-null_ident,0f2h,"RE","P"
db 0


;a cheat, this will have to do (all texts the same size)
err0_ident: db 13,10,"$ SPace, Junk Data",32,"unIMPORTant!!!!------"
$err_ident_L
db 59,"Unknown Instruction",13,10,"$                   "
db 59,"Instruction Not on That Computer Model",13,10,"$"
db 59,"Instruction Too Big (over 16 bytes)",13,10,"$   "
db 59,"Is not a Number, or Register",13,10,"$          "
db 59,"Number too large",13,10,"$                      "
db 59,"No Division or Multiplacation allowed",13,10,"$ "
db 59,"Jump Too big (No Jump) >",13,10,"$              "
db 59,"Jump Is Small enough   <=",13,10,"$             "
db 59,"Illegal Instruction",13,10,"$                   "
db 59,"Database Has Errors / Illegal",13,10,"$         "
db 59,"Please Confirm this line (Db, Dw, Dq)",13,10,"$ "
;db 59,"Error in Memory Line []",13,10,"$               "
db 59,"Unknown",13,10,"$                               "

tempfile1: db "temp.asm",0,0,0,0,0
tempfile2: db "temp1.asm",0,0,0,0
debugfile: db "debug.txt",0,0,0,0
Instruction_set: db "test.set",0,0,0,0,0

dbg: db 0
includez: db 0
fst: db 0       ;1 use, 2 make
tmpkeep: db 0   ;1 keep 0 delete
optimize: db 0  ;if 1, then short and near jumps are undefined.
unicode: db 0   ;used later, leave alone for now

file1: dw 0
file2: dw 0
file3: dw 0     ;debug.txt
file4: dw 0     ;includes
files: dw 0,0,0,0,0,0,0,0,0,0 ;junk area, just in case, only 3 needed

thecpx:  dw 0,0 ;get location for CPX
cpxstart:dw 0,0 ;save location of labels here ($)

loopback: dw loopback_minimum   ;for extra info. (more complex code?)
^loopback_minimum 7             ;new minimum.. change as needed

debugcpx: db "00000000:$"       ;information, just a cut and paste.. Locked
debugcpx_needed: dw 8           ;prepair count to keep (8 at default)

cpuformula:
dw 8088
db 0
dw 8086
db 1
dw 186
db 1
dw 286
db 2
dw 386
db 3
dw 486
db 4
dw 586
db 5
dw 686
db 6
dw 786
db 7
dw 886
db 8
dw 986
db 9
dw 1086
db 10
dw 1186
db 11
dw 1286
db 12
db 0

Intro:
db " Intel/Assembler V",13,10,"$"
db "Created by Alexandre Savelli Bencz", 13,10, "$"

Proguse:
db "Assembler Usage: Intelasm [Input.asm] [Output.com] [DMFOQT]",13,10
db "Other options:",13,10,13,10
db "D - Debug code.. (True assembly)",13,10
db "M - Make fast file [input].fst",13,10
db "F - Use Fast file  [input].fst",13,10
db "O - Optimize jumps (Make Short, Near Jumps Undeclaired)",13,10
db "Q - Quick jumps (Make all jumps at least Near (Fast))",13,10
db "T - Keep Temp Files",13,10,13,10,"$"

Exit: db "Thanks for using this :)",13,10, "$"

Err: db "Unable to access Source File",13,10,13,10,"$"
Err2: db "Unable to access Destination File",13,10,13,10,"$"
Err3: db "Unable to Create/use Temporary Files",13,10,13,10,"$"
Err4: db "Unable to Access .set File",13,10,13,10,"$"
Err5: db "Redirecting .set From Internal Presets",13,10,13,10,"$"
memerr: db "Not Enough Memory to Run Program",13,10,"$"
dbgerr: db "There was an error trying to open Debug.txt, for the output.",13,10,"$"
fsterr: db "There was an error while creating the FASTFILE.",13,10,"$"
verseterr: db "This Program is not compadible with the SET file. Please Update",13,10,13,10,"$"

internal_set:
*idtest.txt
$internal_set_L

;version settings now
^digit "0"
^Ver_Major 0
^Ver_Minor 5
^Ver_Revision 2

;month, day, year
;upper, lower (upper is decades)
^Ver_LUuM 0
^Ver_LUlM 4
^Ver_LUuD 1
^Ver_LUlD 1
^Ver_LUuY 0
^Ver_LUlY 2

%cpx 0
lab:
+cpx 2
labcpx:
+cpx 2
labstart:

;info for location structure IN the structure of the library
%cpx 0
ident_pt0:
+cpx 2
ident_pt1:
+cpx 2
ident_pt2:
+cpx 2
ident_pt3:
+cpx 2
ident_pt4:      ;don't ask, it's just the way it is.
+cpx 2
ident_opinclude:
+cpx 2
ident_verset:

;NOTE: this structure is for Getline, GLINE only.. don't use it for
;anything else.. and this is for 64K only, at lines to be expected at up to
;10240 bytes (why it would be bigger, i have no idea..)
@gsize 1024

%cpx 0
gline:
+cpx gsize

;stat, for where the text is to be read/scanned next
gstat:
+cpx 2

;left, how many bytes were read, based on position, it will be
;able to tell what is left, before it has to do a real read again
gleft:
+cpx 2
gstuff:

^Delete 41h
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

^Ptext 9
;note, these are + to bp (SS) from a PUSHA PUSHAD to carry over information
;at the POPA POPAD
^pax 14
^pcx 12
^pdx 10
^pbx 8
^psp 6
^pbp 4
^psi 2
^pdi 0
^peax 28
^pecx 24
^pedx 20
^pebx 16
^pesp 12
^pebp 8
^pesi 4
^pedi 0


;rules:
;1) fs: Is the data section. (instructions)
;2) gs: Data in/out (text, and translation area (un-needed?)
;
; 3 numbers, starts of each item, each end with 1 0



