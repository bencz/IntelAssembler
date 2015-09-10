jmp main        
nop                         

main:
    mov ax, 07C0h           
    add ax, 288            
    mov ss, ax
    mov sp, 4096

    mov ax, 07C0h           
    mov ds, ax              
    call PrintString    
    jmp LoopInfinito

    LoopInfinito:          
        jmp LoopInfinito  

PrintString:           
    mov si,OlaMundoMsg    
    call PrintStr           
    ret

PrintStr:                   
    push ax                 
    mov ah,0Eh            

    _loop_:
        lodsb               
        cmp al,0x00      
        je done
        int 10h          
        jmp _loop_
    
    done:
        pop ax             
        ret


OlaMundoMsg: db  "Ola mundo, direto do BOOT-LOADER :)", 0x0d, 0x0a, 0x00
