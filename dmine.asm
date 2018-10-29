assume cs:codeseg,ds:dataseg,ss:stack

dataseg segment

inputdim db 0dh,0ah,'Please input the length of your checkerboard:(0-99)',0dh,0ah,'(for example:01 or08 or99)',0dh,0ah,'$'
step Dw 0   ;当前翻开的格数
dim db 1 dup(?)      ;棋盘的长度
inputnbombs db 0dh,0ah,'Please input the number of mines:(0-99)',0dh,0ah,'(for example:01 or08 or99)',0dh,0ah,'$'
nbombs db 1 dup(?)   ;雷的个数
inputtime db 0dh,0ah,'Please input the playtime:(0-999s)',0dh,0ah,'(for example:001 or008 or099)',0dh,0ah,'$'
playTime dw 1 dup(?) ;游戏时间/s
inputr db 0dh,0ah,'Please input the abscissa:(0-(dim-1))',0dh,0ah,'(for example:01 or08 or99)',0dh,0ah,'$'
multiR db 1 dup(?)   ;操作位置横坐标
inputc db 0dh,0ah,'Please input ordinates:(0-(dim-1))',0dh,0ah,'(for example:01 or08 or99)',0dh,0ah,'$'
multiC db 1 dup(?)   ;操作位置纵坐标
multiA DB 0
multiB DB 0
winmessage db 0dh,0ah,'You win!','$'     
losemessage db 0dh,0ah,'Time is over,You lose!','$'  
multimessage db 0dh,0ah,'Choosing 01 means minesweeping and choosing 02 means minemarking',0dh,0ah,'$'
STRLOSE DB 0DH,0AH,'You lose,game is over!',0DH,0AH,'$'  ;游戏失败屏幕显示字样
STRCHOOSE DB 0DH,0AH,'Please choose an operation,mark bomb is 1,sweep bomb is 2',0DH,0AH,'$';标雷扫雷操作选择
STR1 DB 0DH,0AH,'There are still bombs remained,please continue playing the game!', 0DH,0AH,'$' ;插旗个数不足雷数，继续插旗
STR2 DB 0DH,0AH,'Flags are over flow,please choose operation again!',0DH,0AH,'$' ;插旗个数大于雷数，重新选择操作
STR3 DB 0DH,0AH,'You have marked wrong position,please mark again!',0DH,0AH,'$' ;正确标记雷数不足，重新标记
STR4 DB 0DH,0AH,'You win!',0DH,0AH,'$'   ;插旗数与成功标记雷数相等，且等于生成雷数，赢得游戏，游戏结束
STR5 DB 0DH,0AH,'You have marked this position,please choose another position!',0DH,0AH,'$' ;避免重复标记
sameplace DB 0DH,0AH,'You have stepped this position,please choose another position!',0DH,0AH,'$'
flag DW 0   ;插旗数，即符号’#‘的个数,初值为0
multi db 1 dup(?)    ;用户对当前位置的操作    
sumGrid dw 0         ;棋盘总格数
mark db 0            ;当前正确标记的个数
startHour db 1 dup(?);游戏开始的小时数
startMin db 1 dup(?) ;游戏开始的分钟数
startSec db 1 dup(?) ;游戏开始的秒数
timeRemained dw 1 dup(?);剩余时间数
remainMessage db 0dh,0ah,'Time is running...','$'
showMine db 10000 dup(?)
mine db 10000 dup(?)
row db ?
column db ?
Irow db ?
Icolumn db ?
Drow db ?
Dcolumn db ?
basc dw ?
LastRand db 01011101b   ;seed
lastNum  db 0
random db 5 dup(?) 
dataseg ends
;
stack segment
   dw 200 dup(0)
stack ends
;

codeseg segment
main proc far
start:mov ax,dataseg    
      mov ds,ax
	  mov ax,stack
      mov ss,ax
	  lea dx,inputdim     ;提示输入棋盘长度
	  mov ah,09h
	  int 21h
	  call changetoD
	  mov [dim],al    	  ;将输入棋盘长度放入内存
	  
	  mul al 
	  mov [sumGrid],ax
	  
	  lea dx,inputnbombs;提示输入雷的个数
	  mov ah,09h
	  int 21h
	  call changetoD
	  mov [nbombs],al   ;将输入雷的个数放入内存
	  
	  lea dx,inputtime  ;提示输入游戏时间
	  mov ah,09h
	  int 21h
	  call changePlayTime
	  call initChessBox ;初始化棋盘
	  
	  mov ax,4c00h
	  int 21h
main endp

initChessBox proc
   push ax
   push bx
   push cx
   push dx
   push di
   mov cl,nbombs
   lea dx,mine   ;取mine的偏移地址  
setB:
   mov bx,0
   call rand           ;生成一个随机数
   ;cmp bl,lastNum      ;比较新随机数与上一随机数
   ;je setB             ;相等，重新获取随机数
   cmp bx,sumGrid
   ja setB
   mov lastNum,bl      ;不相等，将新随机数赋值给上一随机数
   add bx,dx           ;偏移地址+随机数=随机数放置的地址
   mov di,bx
   cmp byte ptr[di],2ah
   jz setB
   mov byte ptr[di],2ah
   dec cl
   jnz setB
   call count   
;换行
   mov dl,0ah
   mov ah,2
   int 21h
   mov dl,0dh
   mov ah,2
   int 21h
;调用函数以表格形式输出
   pop di
   pop dx
   pop cx
   pop bx
   pop ax
   ;call output
   call initque
   ret
initChessBox endp

changetoD proc            ;将asc码转化为二位十进制数
      mov bl,0
	  mov cx,2
con:  mov ah,01h   
	  int 21h
	  sub al,30h
	  jl con
	  cmp al,9
	  jg con
	  xchg al,bl
	  mov bh,10
	  mul bh
	  add bl,al
	  dec cx
	  jnz con
	  mov al,bl
      ret
changetoD endp

changePlayTime proc
      push ax
	  push bx
	  push cx;cx中存放总数值
	  mov bl,10
con1: mov ah,01h
	  int 21h
	  sub al,30h
	  jl con1 
	  cmp al,9
	  jg con1
	  mul bl
	  mov cl,al
con2: mov ah,01h
	  int 21h
	  sub al,30h
	  jl con2 
	  cmp al,9
	  jg con2
      add al,cl
      mul bl
      mov cx,ax
con3: mov ah,01h
	  int 21h
	  sub al,30h
	  jl con3 
	  cmp al,9
	  jg con3
	  mov ah,0
      add cx,ax
      mov [playTime],cx
      pop cx
      pop bx
      pop ax 
      ret	  
changePlayTime endp
chargeTime proc
      push cx           ;cx暂存剩余时间
	  push ax
	  push bx
	  push dx
	  mov ah,2ch        ;获取系统时间
	  int 21h
	  mov bl,60
	  mov ah,startHour
	  sub ch,startHour
	  mov al,ch
	  mul bl            ;小时差对换为分钟差
	  add al,cl
	  mov ch,startMin
	  sub al,ch          ;获得分钟差总和
	  mul bl             ;分钟差对换为秒
	  mov dl,dh
	  mov dh,0
	  mov bx,0
	  mov bl,startSec
	  add ax,dx
	  sub ax,bx
	  cmp playTime,ax
	  jg showTime
	  mov ax,0
showTime:
      mov timeRemained,ax
      pop dx
	  pop bx
	  pop ax
	  pop cx
      ret
chargeTime endp
                       ;根据用户输入显示棋盘
showBoard proc
      push ax
	  push bx
	  push cx
	  push dx
multr:lea dx,inputr
      mov ah,09h
	  int 21h
	  call changetoD
	  mov [multiR],al  ;将用户输入横坐标放入内存
	  mov [multiA],al
	  inc al
	  cmp al,dim
	  jg multr
	  
multc:lea dx,inputc
	  mov ah,09h
	  int 21h
	  call changetoD
	  mov [multiC],al  ;将用户输入纵坐标放入内存
	  mov [multiB],al
	  inc al
	  cmp al,dim
	  jg multc
	   
	  call chargeTime
      mov bx,timeRemained
	  cmp bx,0
      jnz notlose
	  lea dx,losemessage;提示用户扫雷失败，游戏结束
      mov ah,09h
	  int 21h
      mov ax,4c00h
	  int 21h
notlose:lea dx,remainMessage
	  mov ah,09h
	  int 21h
	  jmp choose
	  
choose: lea dx,STRCHOOSE   
		mov ah,09h
		int 21h
		
		mov ah,1    ;有回显的键盘输入
		int 21h
		
		cmp al,'1'
		je markBomb
		cmp al,'2'
		je activate
		
		jmp choose    ;如果输入结果既不是1也不是2则重新进行操作选择
		
;将当前位置标记为雷
markBomb:mov al,multiR
		mov bl,dim
		mul bl
		mov bl,multiC
		mov bh,0
		add ax,bx     ;计算得到当前具体位置
		push ax     ;将当前位置的信息保存进堆栈
		
		
		lea bx,showMine
		mov dx,ax
		add bx,dx
		mov ax,bx
		mov di,ax
		;and ax,00ffh   cmp byte ptr[di],2ah
		;xlat
		cmp byte ptr[di],23h
		jnz renext
    	lea dx,STR5   ;重复标记一个点时，重新选择坐标进行操作
		mov ah,09h
		int 21h
		jmp multr
		
renext: inc [flag]    ;标记为雷
        mov byte ptr[bx],'#'    ;将内存中当前位置的标记改为’#‘     
		
		lea bx,mine    ;插旗    
		pop ax
		and ax,00ffh
		xlat
		cmp al,'*'
		je nextl    ;如果确实该位置确实为雷，则成功标记的雷数加1
		jne nexti
nextl:inc [mark]

nexti:jmp markWin    ;标记完地雷后跳转到标地雷路子的是否胜利语句

activate:
		MOV AL,multiR  			;扫雷，先判断是否为雷，如果是雷则游戏结束，不是雷则智能帮扫，尽可能多的显示非雷区域
		MOV BL,dim				;棋盘长度放入bl
		MUL BL					;AX<-AX*BL
		MOV BL,multiC			;bl<-纵坐标
		MOV BH,0		
		ADD AX,BX     			;计算得到当前具体位置
		PUSH AX
		LEA BX,mine		
		POP AX
		PUSH AX
		AND AX,00FFH
		XLAT
		PUSH AX
		CMP AL,'*'				;判断是否为雷
		JE lose					;如果踩到雷
		CMP AL,'0'				;如果为0
		JE SHOW					;跳转至SHOW
		POP AX			
		POP BX
		MOV DH,mine[BX]			;把雷放入DH
		CMP DH,showMine[BX]		;与已经显示的雷比较
		JZ same					;如果相同
		INC [step]				;若不同，step++
		
same:	MOV showMine[BX],AL		;并将AL加入雷显示区域
		CALL output1  			;调用output1

lose:	LEA DX,STRLOSE
		MOV AH,09H
		INT 21H					;输出失败提示
		MOV AH,4CH  			;游戏结束，返回DOS
		INT 21H

SHOW:	INC [step]				;step++
		POP AX		
		POP BX
		MOV showMine[BX],AL		;并将AL加入雷显示区域
		MOV DL,1				;循环次数初始化为1
		JMP PS
    
PX:		POP BX					;栈顶出栈
		POP AX 					;栈顶出栈
		MOV DL,1				;循环次数初始化为1
	
PP:		POP BX					;栈顶出栈
		POP AX					;栈顶出栈
		CMP AL,[multiA]
		JNE PW
		CMP BL,[multiB]
		JE  EXITO				;回退到初始位置
		JMP PW
		
EXITO:	CMP DL,5				;判断是否超过5次
		JB PW
	 
MM:		CALL output1			;在初始位置的搜索超过四次，输出雷区

PW:		PUSH AX					;当前位置入栈
		PUSH BX
		MOV multiR,AL
		MOV multiC,BL
		JMP P1
	 
PS:		MOV AL,multiR			;AL<-multiR
		MOV BL,multiC			;BL<-multiC
		PUSH AX					;当前位置入栈
		PUSH BX;
		MOV DL,1				;循环次数初始化为1
		
P1:		INC DL					;循环数加一，
		CMP DL,2				;比较循环数与2
		JA  P2					;若大于，跳至P2
		DEC [multiR]			;当前点的横坐标减1
		JMP NEXTO				;跳至NEXTO

P2:		CMP DL,3				;比较循环数与3
		JA  P3					;若大于，跳至P3
		INC [multiC]			;当前点的纵坐标加1
		JMP NEXTO				;跳至NEXTO

P3:		CMP DL,4				;比较循环数与4
		JA  P4					;若大于，跳至P4
		INC [multiR]			;当前点的横坐标加1
		JMP NEXTO				;跳至NEXTO

P4:		CMP DL,5				;比较循环数与5
		JA  PX					;若大于，跳至PX
		DEC [multiC]			;当前点的纵坐标减1
		JMP NEXTO				;跳至NEXTO
	 
NEXTO:
		;判断坐标是否合法
		;若不合法
		MOV CH,dim
		CMP [multiR],0			;将横坐标与0比较
		JB PP					;小于0，跳至PP，判断下个点
		CMP [multiR],CH			;与棋盘规格比较
		JNB PP					;大于0，跳至PP，判断下个点
		CMP [multiC],0			;将纵坐标与0比较
		JB PP					;小于0，跳至PP，判断下个点
		CMP [multiC],CH			;与棋盘规格比较比较
		JNB PP					;大于0，跳至PP，判断下个点


		;若合法
		MOV AL,multiR			;AL<-multiR
		MOV BL,multiC			;BL<-multiC
		MOV CL,CH				;将棋盘规格至CL
		MUL CL					;AX<-AX*CL
		ADD BX,AX				;BX<-AX+BX
		MOV DH,mine[BX]			;将雷放入DH
		CMP DH,showMine[BX]		;比较mine中的内容与showMine中同一位置的值，检测当前位置是否已经翻开
		JNE OPT					;若未翻开，跳至OPT
		JMP PP					;若已翻开，跳至PP，判断下个点

OPT:
		CMP DH,'0'				;未翻开，比较0与mine当前位置的值
		JE  MOVE				;若相等，则跳至MOVE
		CMP DH,'*'				;判断当前位置是否为雷
		JNE MOVE1				;不是，跳至MOVE1
		JMP PP					;若已翻开，跳至PP，判断下个点
MOVE:
		MOV showMine[BX],DH		;将DH表示的值赋给showMine[BX]，表示这个位置已经翻开
		INC [step]				;step++
		MOV DL,1				;循环次数初始化为1
		JMP PS					;跳至PS，从当前位置开始继续搜索

MOVE1:
		MOV showMine[BX],DH		;将DH表示的至赋给showMine[BX]表示这个位置已经翻开
		INC [step]
		JMP PP					;跳至PP，继续探索下一个结点

;判断标记地雷的棋子是否胜利
markWin:xor ax,ax
		mov al,nbombs
		mov bx,flag
		cmp ax,bx
		ja again1  ;插旗数小于雷数，继续游戏
		jb again2  ;插旗数大于雷数，重新选择操作
		je isRight  ;插旗数等于雷数，此时判断成功标记的雷数是否与生成雷数相等
again1:lea dx,STR1  
		mov ah,09h
		int 21h
		jmp multr
again2:lea dx,STR2  
		mov ah,09h
		int 21h
		jmp choose
isRight:xor ax,ax
		mov al,nbombs
		mov cl,mark
		cmp ax,cx
		jne again3   ;成功标记的地雷个数不等于（含重复标记的情况）生成的地雷个数，有标记错误的，重新标记
		je over   ;成功标记完所有的地雷个数，赢得游戏，游戏结束
again3:lea dx,STR3 
		mov ah,09h
		int 21h
		jmp multr
over:lea dx,STR4  
		mov ah,09h
		int 21h
		mov ah,4ch
		int 21h
        pop dx
		pop cx
		pop bx
		pop ax
        ret
showBoard endp

   ;初始化展示棋盘
initque proc
   push ax
   push bx
   push cx
   lea bx,showMine 
   mov cx,sumGrid
begin:
   mov byte ptr[bx],'?' 
   inc bx
   dec cx
   jnz begin
   mov ah,2ch        ;获取系统时间
   int 21h
   mov [startHour],ch
   mov [startMin],cl
   mov [startSec],dh
   pop cx
   pop bx
   pop ax
   call output1
   ret
initque endp
 
 
;生成随机数的函数
rand proc
   push dx
   push cx
   push ax
   mov bl,lastRand          ;取上次随机数
   mov ah,0
   int 1ah                  ; 获取时钟计数cx:dx
;获得更加随机的数
   mov bl,dh
   xor bl,dl
   mov lastRand,bl
   xor bl,ch
   add cx,306ch   
   xor cx,dx
R10:
   add bl,cl
   loop R10
   pop ax
   pop cx
   pop dx
   ret
rand endp                    ;随机函数结束


;输出函数
output proc
   push ax
   push bx
   push cx
   push dx
   lea bx,mine
   mov ch,dim
;外层循环开始
   nxt1:
   mov cl,dim
   nxt2:                    ;内层循环输出一行
   mov ah,2
   mov dl,[bx]
   int 21h
   inc bx
nxt3: 
   dec cl
   jnz nxt2
;内层循环到此结束
;换行
   nxt4:
   mov dl,0ah
   mov ah,2
   int 21h
   mov dl,0dh
   mov ah,2
   int 21h
   dec ch
   jnz nxt1
   pop dx
   pop cx
   pop bx
   pop ax
   ret
output endp                 ;输出函数结束


;输出showMine函数
output1 proc
   push ax
   push bx
   push cx
   push dx
   mov dl,0ah
   mov ah,2
   int 21h
   mov dl,0dh
   mov ah,2
   int 21h
   lea bx,showMine
   mov ch,dim
;外层循环开始
   enxt1:
   mov cl,dim
   enxt2:                    ;内层循环输出一行
   mov ah,2
   mov dl,[bx]
   int 21h
   inc bx
enxt3: 
   dec cl
   jnz enxt2
;内层循环到此结束
;换行
   enxt4:
   mov dl,0ah
   mov ah,2
   int 21h
   mov dl,0dh
   mov ah,2
   int 21h
   dec ch
   jnz enxt1
   
   mov al,nbombs    ;所有非雷区域被翻开则获胜
   mov ah,0
   mov dx,sumGrid
   sub dx,ax
   cmp step,dx
   jnz notwin

   lea dx,winmessage;提示用户赢得游戏，游戏结束
   mov ah,09h
   int 21h    
   mov ax,4c00h
   int 21h
   pop dx
   pop cx
   pop bx
   pop ax
notwin:call showBoard
   ret
output1 endp

;统计函数
count proc
   push ax
   push bx
   push cx
   push dx
   lea bx,mine
   mov basc,bx
   sub bx,basc
restart:
   mov cx,0
   push bx                    ;bx=0-99
   mov dx,bx                  ;相对位置
   add dx,basc                ;偏移地址
   mov di,dx
   cmp byte ptr [di],'*'
   je last1
   mov ax,bx
   div dim
   mov row,al   
   mov column,ah  
   inc ah
   mov Icolumn,ah   
   inc al
   mov Irow,al     
   sub al,2
   mov Drow,al
   sub ah,2
   mov Dcolumn,ah
   jmp next
last1:
   pop bx
   inc bx
   cmp bx,sumGrid
   jb restart
   call output
   mov ah,4ch                    ;最后位置为‘*’，从此返回DOS界面
   int 21h
next:
   mov al,Drow
   cmp al,0                      ;前一行是否存在
   jl trans3                     ;转到next1   
   mov bl,Dcolumn
   cmp bl,0                      ;前一列是否存在                           
   jl trans2                     ;转到next2
;(x-1,y-1) 是否是雷  
   mul dim                       ;ax=al*dim
   add ax,bx                     ;当前统计单元的相对位置
   add ax,basc                   ;当前单元的偏移地址
   mov di,ax                     ;di,si,bx,bp才能寄存器间接寻址
   cmp byte ptr[di],'*'          ;当前内存单元的内容是否为‘*’
   jne next3
   inc cl      
next3:                           ;(x-1,y)是否是雷
   mov al,Drow
   mov bl,column
   mul dim                       ;输入参数为 al,bl,ax=al*bl
   add ax,bx                     ;相对位置
   add ax,basc                   ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne next4              
   inc cl
next4:                           ;(x-1,y+1)是否是雷
   mov al,Icolumn
   cmp al,dim                    ;y+1<dim 是否成立
   jge trans4                 
   mov al,Drow
   mov bl,Icolumn
   mul dim                       ;输入参数为 al,bl,ax=al*bl
   add ax,bx                     ;相对位置
   add ax,basc                   ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne next6
   inc cl
next6:                            ;(x,y-1)是否是雷
   mov al,row
   mov bl,Dcolumn
   mul dim                        ;输入参数为 al,bl,ax=al*bl
   add ax,bx                       ;相对位置
   add ax,basc                    ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne next7
   inc cl
   jmp next7
trans3:jmp next1
trans2:jmp trans1
next7:                             ;(x,y+1)是否是雷
   mov al,row
   mov bl,Icolumn
   mul dim                         ;输入参数为 al,bl,ax=al*bl
   add ax,bx                       ;相对位置
   add ax,basc                     ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne next8             
   inc cl
   jmp next8
trans4:jmp next5
next8:                             ;(x+1,y-1)是否是雷       
   mov al,Irow
   cmp al,dim                       ;x+1<dim 是否成立
   jge tras1          
   mov al,Irow
   mov bl,Dcolumn
   mul dim                          ;输入参数为 al,bl,ax=al*bl
   add ax,bx                        ;相对位置
   add ax,basc                      ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne next9
   inc cl
next9:                              ;(x+1,y)是否是雷
   mov al,Irow
   mov bl,column
   mul dim     
   add ax,bx                        ;相对位置
   add ax,basc                      ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne next10
   inc cl
next10:                             ;(x+1,y+1)是否是雷
   mov al,Irow
   mov bl,Icolumn
   mul dim                          ;输入参数为 al,bl,ax=al*bl
   add ax,bx                        ;相对位置
   add ax,basc                      ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne totras2                        
   inc cl
   jmp last
trans1:jmp next2
next5:                              ;(x,y-1)是否是雷    x-1>=0,y-1>=0,y+1=dim 即：最后一列的情况
   mov al,row
   mov bl,Dcolumn
   mul dim                          ;输入参数为 al,bl,ax=al*bl
   add ax,bx                        ;相对位置
   add ax,basc                       ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne next11
   inc cl
   jmp next11
totras2:jmp tras2
tras1:jmp last
next11:                               ;(x+1,y-1)是否是雷
   mov al,Irow
   cmp al,dim                           ;x+1>dim是否成立，即是否为右下角
   jge tras3               
   mov al,Irow
   mov bl,Dcolumn
   mul dim                             ;输入参数为 al,bl,ax=al*bl
   add ax,bx                           ;相对位置
   add ax,basc                         ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne next12
   inc cl
next12:                                 ;(x+1,y)是否是雷
   mov al,Irow
   mov bl,column
   mul dim                              ;输入参数为 al,bl,ax=al*bl
   add ax,bx                            ;相对位置
   add ax,basc                          ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne totras4                  
   inc cl
   jmp last
next2:                                   ;x-1>=0,y-1<0;第一列非左上角                                   ;next2
   mov al,Drow                           ;(x-1,y)是否是雷
   mov bl,column
   mul dim                               ;输入参数为 al,bl,ax=al*bl
   add ax,bx                             ;相对位置
   add ax,basc                           ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne next13
   inc cl
   jmp next13
tras2:jmp last
next13:                                   ;(x-1,y+1)是否是雷
   mov al,Drow
   mov bl,Icolumn
   mul dim                                ;输入参数为 al,bl,ax=al*bl
   add ax,bx                              ;相对位置
   add ax,basc                            ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne next14
   inc cl
   jmp next14
totras4:jmp tras4
tras3:jmp last
next14:                                   ;(x,y+1)是否是雷
   mov al,row
   mov bl,Icolumn
   mul dim                                ;输入参数为 al,bl,ax=al*bl
   add ax,bx                               ;相对位置
   add ax,basc                             ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne next15
   inc cl
   jmp next15
trs3:jmp restart
next15:                                    ;(x+1,y)是否是雷
   mov al,Irow
   cmp al,dim                              ;x+1<dim 是否成立 即：是否为左下角
   jge tras6                
   mov al,Irow
   mov bl,column
   mul dim                                 ;输入参数为 al,bl,ax=al*bl
   add ax,bx                               ;相对位置
   add ax,basc                             ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne next16
   inc cl
   jmp next16
tras4:jmp last
next16:                                    ;(x+1,y+1)是否是雷
   mov al,Irow
   mov bl,Icolumn
   mul dim                                 ;输入参数为 al,bl,ax=al*bl
   add ax,bx                               ;相对位置
   add ax,basc                             ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne tras5                     
   inc cl
   jmp last
trs2:jmp trs3
next1:                                     ;x-1<0  即：当前行为第一行                                                                    ;next1
   mov bl,Dcolumn
   cmp bl,0                                ;是否为左上角
   jl to17
   mov al,row                              ;(x,y-1)是否是雷
   mov bl,Dcolumn
   mul dim                                 ;输入参数为 al,bl,ax=al*bl
   add ax,bx                               ;相对位置
   add ax,basc                             ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne next18
   inc cl
next18:                                     ;(x+1,y-1)
   mov al,Irow
   mov bl,Dcolumn
   mul dim                                 ;输入参数为 al,bl,ax=al*bl
   add ax,bx                                ;相对位置
   add ax,basc                              ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne next19
   inc cl
   jmp next19
tras6:jmp last
next19:                                     ;(x+1,y)是否是雷
   mov al,Irow
   mov bl,column
   mul dim                                  ;输入参数为 al,bl,ax=al*bl
   add ax,bx                                ;相对位置
   add ax,basc                              ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne next20
   inc cl
   jmp next20
to17:jmp next17
tras5:jmp last
next20:     
   mov bl,Icolumn
   cmp bl,dim                               ;y+1>dim 是否成立；即是否为右上角
   jge tolast          
   mov al,row                               ;(x,y+1)是否是雷
   mov bl,Icolumn
   mul dim                                  ;输入参数为 al,bl,ax=al*bl
   add ax,bx                                ;相对位置
   add ax,basc                              ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne next23
   inc cl
next23:
   mov al,Irow                              ;(x+1,y+1)是否是雷
   mov bl,Icolumn
   mul dim                                  ;输入参数为 al,bl,ax=al*bl
   add ax,bx                                ;相对位置
   add ax,basc                               ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne last
   inc cl
   jmp last
tolast:jmp last
trs1:jmp trs2
next17:                                       ;(x,y+1)是否是雷
   mov al,row
   mov bl,Icolumn
   mul dim                                     ;输入参数为 al,bl,ax=al*bl
   add ax,bx                                    ;相对位置
   add ax,basc                                  ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne next21
   inc cl
next21:                              ;(x+1,y)是否是雷
   mov al,Irow    
   mov bl,column
   mul dim                          ;输入参数为 al,bl,ax=al*bl
   add ax,bx                         ;相对位置
   add ax,basc                       ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'
   jne next22
   inc cl
next22:                              ;(x+1,y+1)是否是雷
   mov al,Irow
   mov bl,Icolumn
   mul dim                             ;输入参数为 al,bl,ax=al*bl
   add ax,bx                          ;相对位置
   add ax,basc                         ;偏移地址
   mov di,ax  
   cmp byte ptr[di],'*'           
   jne last
   inc cl
   jmp last                    
last:                                                          
   pop bx  
   mov ax,basc   
   add ax,bx                           ;当前单元在内存中的偏移地址
   mov di,ax
   add cl,30h                          ;转ASCII码表示
   mov byte ptr[di],cl 
   inc bx    ;
   cmp bx,sumGrid
   jb trs1          
   pop dx
   pop cx
   pop bx
   pop ax
   ret
count endp
;统计函数结束
codeseg ends
end start
      
