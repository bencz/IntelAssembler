%cpx 100h
       
  pushad   
  mov ah,Create   
  mov cx,020h        
  mov dx,filename    
  int 021h 
  mov [file1],ax  ; handle do arquivo
  
  mov ah,Write                  
  mov cx,14  
  mov bx,[file1]         
  mov dx,fileCont   
  int 021h          
         
  mov cx,10
  xor ax,ax
  
jmpTest:
  mov ah,9
  mov dx,msg
  mov bx,1
  int 21h  
  inc ax
  loop jmpTest
  
  popad
  ret 
             
msg:      db "Ola mundo :)",13,10,"$" 
fileName: db "output.txt", 0
fileCont: db "Ola mundo :)",13,10
  
file1: dw 0
^Create 3Ch
^Write 40h