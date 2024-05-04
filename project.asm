; Project made by:
    ; Qainat Saeed - 22L-6569
    ; Asjad Siddiqui 22L-6602

[org 0x0100]
jmp start

; max width = 320 pixels
; max height = 200 pixels

;--------------------------------------------------------------------
; subroutine to clear the screen
;--------------------------------------------------------------------
clrscr:		push es
            push ax
            push cx
            push di

            ; mov ax, 0A000h
            mov ax, 9000h
            mov es, ax					; point es to video base
            mov di, 0					; point di to top left column

            ; mov al, 0					; al is the colour attribute
            mov al, [color]					; al is the colour attribute
            mov ah, al				    ; copy colour to ah
            mov cx, 32000				; 32000 times the loop will run
                                        ; it will color 2 pixels at a time (ax - al + ah) word size
                                        ; so 64000 pixels will be colored (320 x 200 = 64000)

            rep stosw					; clear screen
                                        ; While CX ! 0 Do
                                        ; Memory[ES:DI] := AX
                                        ; DI := DI + 2
                                        ; CX := CX - 1

            ; add byte [color], 1

            pop di
            pop cx
            pop ax
            pop es
            ret


;--------------------------------------------------------------------
; subroutine for delay
;--------------------------------------------------------------------
delay:  push cx
        mov cx, 0xFFFF
delayloop1:      loop delayloop1
        pop cx
        ret


;--------------------------------------------------------------------
; subroutine for half delay
;--------------------------------------------------------------------
halfdelay:  push cx
        mov cx, 0x00FF
halfdelayloop1:      loop halfdelayloop1
        pop cx
        ret


;--------------------------------------------------------------------
; subroutine to draw an object
;--------------------------------------------------------------------
drawObj:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    ; mov ax, 0A000h
    mov ax, 9000h
    mov es, ax					; point es to video base

    mov si, [bp + 8] ; obj

    mov ax, word [width]         ; load ax with columns per row
    mov cx,  [bp + 10]        ; load cx with rows (y)
    mul cx       ; multiply by rows (y)
    add ax, word [bp + 12]        ; add x offset
    mov di, ax              ; store in di


    ; mov ch, 0 ; objHeight
    ; mov cl, 0 ; objWidth
    mov cx, 0 ; objHeight
    mov bx, 0 ; objWidth

    
    outerObjLoop:
        mov dx, [bp + 12]
        innerObjLoop:
            mov al, [si]
            cmp al, 255
            je skipObj
            cmp dx, 320
            jge overflow
            jmp noOverflow
            ; mov byte [es:di], al
            overflow:
            sub di, 320
            mov byte [es:di], al
            add di, 320
            jmp skipObj
            noOverflow:
            mov byte [es:di], al
            skipObj:
            inc di
            inc si
            inc bx
            inc dx
            cmp bx, word[bp + 6]    ; obj width
            jne innerObjLoop

        ; di = di + (actualWidth) - (objWidth)
        mov dx, [width]
        mov bx, 0
        mov word bx, [bp + 6]   ; obj width
        sub dx, bx
        add di, dx

        inc cx
        mov bx, 0
        cmp cx, word [bp + 4]   ; compare height with obj height
        jne outerObjLoop

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 10

copyBuffer:
    push bp
    mov bp,sp
    push es
    push ds
    push ax
    push bx
    push cx
    push dx
    push di
    push si

    mov ax, 0xA000   ; Set data segment to the video memory
    mov es, ax

    mov ax, 0x9000   ; Set data segment to the off-screen buffer
    mov ds, ax

    mov cx, 32000    ; Copy 32000 words (320 * 200) / 2

    mov si, 0        ; Set source index to 0
    mov di, 0        ; Set destination index to 0

    cld

    rep movsw        ; Copy 32000 words from the off-screen buffer to the video memory

    pop si
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop ds
    pop es
    pop bp
    ret 0



drawCar:
    push bp
    mov bp,sp
    push ax
    push bx
    push cx
    push dx

    mov dx, [carThrusterCycles]

    
    ; - CAR -
    ; mov ax, 240      ; x 
    mov ax, [bp + 6]      ; x 
    push ax         ; [bp + 12]
    ; mov ax, 90       ; y
    mov ax, [bp + 4]       ; y
    push ax         ; [bp + 10]
    cmp dx, 100
    jl renderCar1
    jmp renderCar2
    renderCar1:
    mov bx, car
    push bx         ; [bp + 8]
    jmp draw

    renderCar2:
    mov bx, car2
    push bx

    draw:
    mov ax, [carWidth]
    push ax         ; [bp + 6]
    mov ax, [carHeight]
    push ax         ; [bp + 4]
    call drawObj

    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 4




;---------------------

kbisr:
    push ax
    in al, 0x60 ; read
    cmp al, 0x9C
    je matchEnter
    cmp al, 0x48 ; cmp with ascii
    je matchUp
    cmp al, 0x39 ; cmp with ascii
    je matchSpace
    cmp al, 0x01 ; cmp with ascii
    je matchEsc
   
    jmp nomatch

    matchEnter:
    cmp word [isGameOver], 1
    jne nomatch
    call restartGame

    jmp nomatch

    matchUp:
    cmp word [isJumping], 1
    je nomatch
    cmp word [fallingCatto], 1
    je nomatch
    cmp word [isGameOver], 1
    je nomatch
    cmp word [welcome_done], 0
    je nomatch
    call jumpCatto
    
    jmp nomatch

    matchSpace:
    cmp word [welcome_done], 1
    je nomatch
    mov word [welcome_done], 1

    jmp nomatch

    matchEsc:

    mov word [quitGame], 1

    jmp nomatch



  
nomatch:
    mov al, 0x20
    out 0x20, al
    pop ax
    iret

    ; mov al, 0x20
    ; out 0x28, al
    ; pop ax
    
    ; jmp far[cs:keyboardinterrupt]


;--------------------------------------------------------
; timer interrupt service routine
;--------------------------------------------------------
 timer:		
			cmp word[cs:currenttask],1
			je multitasking
			
            
            pusha
            cmp word [cs:iterCount], 5800
            jg tick 
            
           incIterCount:
            inc word [cs:iterCount]
            jmp skipall

           tick:
            mov word [cs:iterCount], 0

			inc word [cs:seconds] ; increment tick count

            mov ax, word [cs:seconds] ; load number in ax

			
skipall:	popa
multitasking:
			push ds
			push bx
			push cs
			pop ds ; initialize ds to data segment
			mov bx, [currenttask] ; read index of current in bx
			shl bx,5 ; multiply by 32 for pcb start
			mov [pcb+bx+0], ax ; save ax in current pcb
			mov [pcb+bx+4], cx ; save cx in current pcb
			mov [pcb+bx+6], dx ; save dx in current pcb
			mov [pcb+bx+8], si ; save si in current pcb
			mov [pcb+bx+10], di ; save di in current pcb
			mov [pcb+bx+12], bp ; save bp in current pcb
			mov [pcb+bx+24], es ; save es in current pcb
			pop ax ; read original bx from stack
			mov [pcb+bx+2], ax ; save bx in current pcb
			pop ax ; read original ds from stack
			mov [pcb+bx+20], ax ; save ds in current pcb
			pop ax ; read original ip from stack
			mov [pcb+bx+16], ax ; save ip in current pcb
			pop ax ; read original cs from stack
			mov [pcb+bx+18], ax ; save cs in current pcb
			pop ax ; read original flags from stack
			mov [pcb+bx+26], ax ; save flags in current pcb
			mov [pcb+bx+22], ss ; save ss in current pcb
			mov [pcb+bx+14], sp ; save sp in current pcb
			mov bx, [pcb+bx+28] ; read next pcb of this pcb
			mov [currenttask], bx ; update current to new pcb
			mov cl, 5
			shl bx, cl ; multiply by 32 for pcb start
			mov cx, [pcb+bx+4] ; read cx of new process
			mov dx, [pcb+bx+6] ; read dx of new process
			mov si, [pcb+bx+8] ; read si of new process
			mov di, [pcb+bx+10] ; read di of new process
			mov bp, [pcb+bx+12] ; read bp of new process
			mov es, [pcb+bx+24] ; read es of new process
			mov ss, [pcb+bx+22] ; read ss of new process
			mov sp, [pcb+bx+14] ; read sp of new process
			push word [pcb+bx+26] ; push flags of new process
			push word [pcb+bx+18] ; push cs of new process
			push word [pcb+bx+16] ; push ip of new process
			push word [pcb+bx+20] ; push ds of new process
			mov al, 0x20
			out 0x20, al ; send EOI to PIC
			mov ax, [pcb+bx+0] ; read ax of new process
			mov bx, [pcb+bx+2] ; read bx of new process
			pop ds ; read ds of new process
			iret ; return to new process

;---------------------
playBGSound:
			mov dx,[cs:sound_buffer]
			mov es,dx
			xor si,si
            add si, 100
	sLoop:
        ; send DSP Command 10h
        mov dx, 22ch
        mov al, 10h
        out dx, al
        ; send byte audio sample
        mov al, [es:si]
        out dx, al
        mov cx, 3600
			sdelay:
			loop sdelay
		inc si
		cmp si,65500
		jne sLoop
		xor si,si
        add si, 100
        jmp sLoop
		
;---------------------
hookInter:
    pusha
	push es
    cli
    xor ax,ax
    mov es,ax
    mov ax,[es:9*4]
    mov word[keyboardinterrupt], ax
    mov ax, [es:9*4+2]
    mov word[keyboardinterrupt+2],ax
    mov word [es:9*4], kbisr				;store offset at n°4....
    mov [es:9*4+2], cs 						;store segment at n*4+2

    sti
			mov ah,3dh ; open file
			mov al,0
			lea dx,filename
			int 21h
			mov [filehandle],ax
			;read file and write bytes
			mov ah,3fh
			mov cx,[sound_size]
			mov bx,[filehandle]
			mov dx,[sound_buffer]
			mov ds,dx
			xor dx,dx
			int 21h
			;close file
			 mov ax,cs
			 mov ds,ax
			 mov ah,3eh
			 mov bx,[filehandle]
			 int 21h
			 
		    mov word[pcb+16],mainloop
			mov word[pcb+18],cs
			mov word[pcb+26],0x0200
			mov word[pcb+28],1
			
			mov word[pcb+12],bp
			mov word[pcb+14],sp
			mov word[pcb+20],ds
			mov word[pcb+22],ss
			mov word[pcb+24],es
			
			mov word[pcb+32+16],playBGSound
			mov word[pcb+32+18],cs
			mov word[pcb+32+26],0x0200
			mov word[pcb+32+28],0
			
			mov word[pcb+32+12],bp
			mov word[pcb+32+14],sp
			mov word[pcb+32+20],ds
			mov word[pcb+32+22],0x2000
			mov word[pcb+32+24],es
			
			mov word[currenttask],0
cli
			mov ax, 100
			out 0x40, al
			mov al, ah
			out 0x40, al

            mov ax,[es:8*4]
            mov word[timerinterrupt], ax
            mov ax, [es:8*4+2]
            mov word[timerinterrupt+2],ax
			
			mov word [es:8*4], timer 
			mov [es:8*4+2], cs 
sti

	pop es
    popa
    ret
;---------------------

unhookInter:
    cli ; stop
    pusha
	push es
    xor ax,ax
    mov es,ax

    mov ax, [timerinterrupt]
    mov word [es:8*4],ax
    mov ax, [timerinterrupt+2]
    mov word [es:8*4+2],ax
    
    mov ax, [keyboardinterrupt]
    mov word [es:9*4],ax
    mov ax, [keyboardinterrupt+2]
    mov word [es:9*4+2],ax

    sti ; resume interripts

	pop es
    popa
    ret
;---------------------


jumpCatto:
    pusha

    mov word [isJumping], 1
    
    popa
    ret

restartGame:
    pusha

    mov word [isGameOver], 0
    mov word [fallingCatto], 0
    mov word [showLastBrick], 1
    mov word [cattoY], 178
    mov word [brickx], 130
    mov ax, [brickx]
    add ax, 24
    mov word [cattoX], ax
    mov word [currBrick], 1
    mov word [seconds], 0
    mov word [score], 0

    cmp word [currBrokenBrick], 1
    jne skipStartBrickTimer
    mov word [isTimerRunning], 1
    
    skipStartBrickTimer:
    
    popa
    ret
    
restartGameText:db 'Press Enter to Restart'
restartGameLength: dw 22


startTextPrint:
    pusha
    mov ax,cs
    mov es,ax
    mov ah,13h;service to print string in graphic mode
    mov al,0;sub-service 0 all the characters will be in the same color(bl) and cursor position is not updated after the string is written
    mov bh,0;page number=always zero
    mov bl,00001111b;color of the text (white foreground and black background)

    mov cx, word [restartGameLength] ; length
    mov dh,20;y coordinate
    mov dl,10;x coordinate
    mov bp,restartGameText;mov bp the offset of the string
    int 10h
    popa
    ret



scoreTextPrint:
    pusha

    mov ax, [score] ; load number in ax
    mov bx, 10 ; use base 10 for division
    mov cx, 0 ; initialize count of digits
    nextdigit: mov dx, 0 ; zero upper half of dividend
    div bx ; divide by 10
    add dl, 0x30 ; convert digit into ascii value
    push dx ; save ascii value on stack
    inc cx ; increment count of values
    cmp ax, 0 ; is the quotient zero
    jnz nextdigit ; if no divide it again

    mov word [scoreStringLength], cx

    mov si, 0
    nextpos:  pop dx
    mov byte [scoreString + si], dl
    inc si
    loop nextpos

    mov ax,cs
    mov es,ax
    mov ah,13h;service to print string in graphic mode
    mov al,0;sub-service 0 all the characters will be in the same color(bl) and cursor position is not updated after the string is written
    mov bh,0;page number=always zero
    mov bl,00001111b;color of the text (white foreground and black background)

    mov cx, word [scoreStringLength] ; length
    mov dh,0;y coordinate
    mov dl,20;x coordinate
    mov bp,scoreString;mov bp the offset of the string
    int 10h
    popa
    ret



start:
 mov ax,0013h
 int 10h

; call clrscr
 mov cx, 0
 mov dx, 0 ; cycles - every 100 cycles car will change to car2

 ; brick random values
 rdtsc
 mov si,320
 div si	
 mov [brick2x],dx

 mov si,320
 div si
 mov [brick3x],dx

 add dx, 40


call hookInter
			
mainloop:
    cmp  word [quitGame] ,1
    je exit

    cmp word [welcome_done], 1
    je skipWelcome
    ; mov byte [color], 128
    mov byte [color], 7
    call clrscr

    mov ax, 105      ; x 
    push ax         ; [bp + 12]
    mov ax, 40      ; y
    push ax         ; [bp + 10]
    mov bx, start_game
    push bx         ; [bp + 8]
    mov ax, [start_game_width]
    push ax         ; [bp + 6]
    mov ax, [start_game_height]
    push ax         ; [bp + 4]
    call drawObj

    mov ax, 90      ; x 
    push ax         ; [bp + 12]
    mov ax, 70      ; y
    push ax         ; [bp + 10]
    mov bx, qainat
    push bx         ; [bp + 8]
    mov ax, [qainatWidth]
    push ax         ; [bp + 6]
    mov ax, [qainatHeight]
    push ax         ; [bp + 4]
    call drawObj

    mov ax, 80      ; x 
    push ax         ; [bp + 12]
    mov ax, 100      ; y
    push ax         ; [bp + 10]
    mov bx, asjad
    push bx         ; [bp + 8]
    mov ax, [asjadWidth]
    push ax         ; [bp + 6]
    mov ax, [asjadHeight]
    push ax         ; [bp + 4]
    call drawObj


    mov ax, 105      ; x 
    push ax         ; [bp + 12]
    mov ax, 150      ; y
    push ax         ; [bp + 10]
    mov bx, spaceToStart
    push bx         ; [bp + 8]
    mov ax, [spaceToStartWidth]
    push ax         ; [bp + 6]
    mov ax, [spaceToStartHeight]
    push ax         ; [bp + 4]
    call drawObj

    
    ; call welcomeTextPrint
    call copyBuffer

    jmp mainloop


    skipWelcome:
    mov al, byte [colorOld]
    mov byte [color], al
    call clrscr


    cmp word [isTimerRunning], 1
    jne skipTimerRunningChecks

    cmp word [seconds], 7
    jl skipTimerRunningChecks

    mov word [isTimerRunning], 0
    mov word [fallingCatto], 1 ; Make cat fall
    mov word [showLastBrick], 0
    mov word [isGameOver], 1

    skipTimerRunningChecks:

    cmp word [movePlatformsDown], 1
    je moveAllPlatformsDown
    
    cmp word [fallingCatto], 1
    je makeCattoFall
    

    cmp word [isJumping], 1
    jne skipIncJump

    cmp word [cattoY], 156
    jg skipResetJumping

    mov word [isJumping], 0
    inc word [currBrick]
    mov ax, [currBrick]
    cmp ax, [currMouseBrick]
    jne skipResetMouse
    mov word [mouseExists], 0
    inc word [score]

    skipResetMouse:

    dec word [currMouseBrick]

    jmp checkPlatform
    skipResetJumping:
    sub word [cattoY], 2

    jmp skipPlatformCheck
    
    checkPlatform:
    mov ax, [brickx]
    add ax, 24 ; catto position in ax

    mov ax, [cattoX]

    mov si, [cattoWidth]
    shr si, 1 ; divide by 2

    add ax, si ; sub half of catto width

    ; cmp word [brick2x], ax
    cmp ax, word [brick2x]
    jl fallCatto

    mov si, word [brick2x]
    add si, [brickWidth]

    cmp ax, si
    jg fallCatto

    mov word [movePlatformsDown], 1

    jmp skipSetFall

    fallCatto:
    mov word [fallingCatto], 1
    mov word [showLastBrick], 0
    mov word [isGameOver], 1
    mov word [isTimerRunning], 0
    mov word [mouseExists], 0
    mov word [mouseX], 0
    mov word [mouseY], 0
    mov word [currMouseBrick], 0



    skipSetFall:
    
    ; mov si, word [brick2x]
; 
    ; fallCatto:
    ; mov word [fallCatto], 1

    jmp skipPlatformCheck

    makeCattoFall:
    cmp word [cattoY], 200
    jg fallComplete
    add word [cattoY], 2

    jmp skipFallComplete


    fallComplete:
    mov word [fallingCatto], 0

    jmp skipMovePlatformsDown

    moveAllPlatformsDown:

    cmp word [bricky], 200
    jg moveLastPlatformUp


    moveDownAll:
    add word [bricky], 2
    add word [brick2y], 2
    add word [brick3y], 2
    add word [cattoY], 2
    add word [mouseY], 2

    cmp word [movedLastPlatformUp], 1
    jne skipMovePlatformsDown
    cmp word [bricky], 147
    jl skipMovePlatformsDown

    mov word [movePlatformsDown], 0
    mov word [movedLastPlatformUp], 0

    mov ax, [brickx]
    push ax

    mov ax, [brick2x]
    mov [brickx], ax
    mov ax, [brickflag2]
    mov [brickflag], ax
    mov word [bricky], 193

    mov ax, [brick3x]
    mov [brick2x], ax
    mov ax, [brickflag3]
    mov [brickflag2], ax
    mov word [brick2y], 170

    mov word [brick3y], 147
    pop ax
    mov [brick3x], ax

    mov word [currBrick], 1

    dec word [currBrokenBrick]
    cmp word [currBrokenBrick], 0
    je resetBrokenBrick

    cmp word [currBrokenBrick], 1
    je startTimer
    mov word [isTimerRunning], 0
    jmp skipBrokenBrickReset

    startTimer:
    mov word [isTimerRunning], 1
    mov word [seconds], 0
    mov word [iterCount], 0

    jmp skipBrokenBrickReset

    resetBrokenBrick:
    mov word [currBrokenBrick], 3
    mov word [isTimerRunning], 0

    skipBrokenBrickReset:

    dec word [currSpecialBrick]
    cmp word [currSpecialBrick], 0
    je resetSpecialBrick

    jmp skipSpecialBrickReset

    resetSpecialBrick:
    mov word [currSpecialBrick], 3

    skipSpecialBrickReset:


    


    jmp skipMovePlatformsDown

    moveLastPlatformUp:
    mov word [movedLastPlatformUp], 1
    mov word [bricky], 133

    rdtsc
    rdtsc
    mov si, 160
    div si
    add dx, 40

    mov word [brickx], dx

    cmp word [mouseExists], 0
    jne skipMovePlatformsDown

    ; Random chance that mouse comes
    rdtsc
    rdtsc
    mov si, 3
    div si
    cmp dx, 1
    jne noMouse

    ; mov [brickx],dx
    mov word [mouseExists], 1
    mov dx, [brickx]
    add dx, 28
    mov word [mouseX], dx
    mov word [mouseY], 125
    mov word [currMouseBrick], 3

    noMouse:


    skipMovePlatformsDown:
    skipFallComplete:
    skipPlatformCheck:
    skipIncJump:
    
    mov ax, [cattoX]
    push ax         ; [bp + 12]
    mov ax, [cattoY]       ; y
    push ax         ; [bp + 10]
    mov bx, catto
    push bx         ; [bp + 8]
    mov ax, [cattoWidth]
    push ax         ; [bp + 6]
    mov ax, [cattoHeight]
    push ax         ; [bp + 4]
    call drawObj

    cmp word [showLastBrick], 0
    je skipDrawBrick3
    
    ; - Brick1
    mov ax, [brickx]    ; x 
    push ax         ; [bp + 12]
    mov ax, [bricky]       ; y
    push ax         ; [bp + 10]
    cmp word [currBrokenBrick], 1
    jne normalBrick1
    mov bx, brokenBrick
    jmp pushBrick1
    normalBrick1:
    cmp word [currSpecialBrick], 1
    jne normalBrick11
    mov bx, brick_dark
    jmp pushBrick1
    normalBrick11:
    mov bx, brick
    pushBrick1:
    push bx         ; [bp + 8]
    mov ax, [brickWidth]
    push ax         ; [bp + 6]
    mov ax, [brickHeight]
    push ax         ; [bp + 4]
    call drawObj

    skipDrawBrick3:
	
    ; - Brick2
	
    mov ax,[brick2x]    ;  random x value 
    push ax         ; [bp + 12]
    mov ax, [brick2y]     ; y
    push ax         ; [bp + 10]
    cmp word [currBrokenBrick], 2
    jne normalBrick2
    mov bx, brokenBrick
    jmp pushBrick2
    normalBrick2:
    cmp word [currSpecialBrick], 2
    jne normalBrick22
    mov bx, brick_dark
    jmp pushBrick2
    normalBrick22:
    mov bx, brick
    pushBrick2:
    push bx         ; [bp + 8]
    mov ax, [brickWidth]
    push ax         ; [bp + 6]
    mov ax, [brickHeight]
    push ax         ; [bp + 4]
    call drawObj
	
    ; - Brick3

    
    mov ax,[brick3x]    ;  random x value 
    push ax         ; [bp + 12]
    mov ax, [brick3y]  ; y - 7 pixels on top
    push ax         ; [bp + 10]
    cmp word [currBrokenBrick], 3
    jne normalBrick3
    mov bx, brokenBrick
    jmp pushBrick3
    normalBrick3:
    cmp word [currSpecialBrick], 3
    jne normalBrick33
    mov bx, brick_dark
    jmp pushBrick3
    normalBrick33:
    mov bx, brick
    pushBrick3:
    push bx         ; [bp + 8]
    mov ax, [brickWidth]
    push ax         ; [bp + 6]
    mov ax, [brickHeight]
    push ax         ; [bp + 4]
    call drawObj



    ; - ! TOP PART

    ; - BG -
    mov ax, cx      ; x 
    push ax         ; [bp + 12]
    mov ax, 0       ; y
    push ax         ; [bp + 10]
    mov bx, bg
    push bx         ; [bp + 8]
    mov ax, [bgWidth]
    push ax         ; [bp + 6]
    mov ax, [bgHeight]
    push ax         ; [bp + 4]
    call drawObj
   

    push 240
    push 90
    call drawCar

    push 140
    push 90
    call drawCar

    cmp word [mouseExists], 0
    je skipDrawMouse

    ; - Mouse
    mov ax, [mouseX]
    push ax         ; [bp + 12]
    mov ax, [mouseY]       ; y
    push ax         ; [bp + 10]
    mov bx, mouse
    push bx         ; [bp + 8]
    mov ax, [mouseWidth]
    push ax         ; [bp + 6]
    mov ax, [mouseHeight]
    push ax         ; [bp + 4]
    call drawObj

    skipDrawMouse:

    cmp word [isGameOver], 1
    jne skipGameOverScreen

    ; - ! TOP PART

    gameOverScreen:
     ; - Mouse
    mov ax, 130
    push ax         ; [bp + 12]
    mov ax, 20       ; y
    push ax         ; [bp + 10]
    mov bx, gameOver
    push bx         ; [bp + 8]
    mov ax, [gameOverWidth]
    push ax         ; [bp + 6]
    mov ax, [gameOverHeight]
    push ax         ; [bp + 4]
    call drawObj


    skipGameOverScreen:


    call copyBuffer
    cmp word [isGameOver], 1
    jne skipPrintGameOverText
    call startTextPrint
    skipPrintGameOverText:
    call scoreTextPrint


 ;  moving first brick
	cmp word[brickflag],0
	je moverightbrick1
	jmp moveleftbrick1
	
	
	moverightbrick1:
    cmp word [currBrokenBrick], 1
    je skipBrick1Reset
	inc word[brickx]
    cmp word [currBrick], 1
    jne skipCattoRightInc1
    add word [cattoX], 1
    skipCattoRightInc1:
    cmp word [currMouseBrick], 1
    jne skipMouseRightInc1
    add word [mouseX], 1
    skipMouseRightInc1:
    cmp word[brickx],220
    jl skipBrick1Reset
	mov word[brickflag],1
    skipBrick1Reset:

    jmp skipBrick1MoveLeft
	
	moveleftbrick1:
    cmp word [currBrokenBrick], 1
    je skipBrick1MoveLeft
	dec word[brickx]
    cmp word [currBrick], 1
    jne skipCattoLeftInc1
    sub word [cattoX], 1
    skipCattoLeftInc1:
    cmp word [currMouseBrick], 1
    jne skipMouseLeftInc1
    sub word [mouseX], 1
    skipMouseLeftInc1:
    cmp word[brickx],40
    jg skipBrick1LeftReset
    mov word[brickflag],0
    skipBrick1LeftReset:

    skipBrick1MoveLeft:


 ;  moving second brick
	cmp word[brickflag2],0
	je moverightbrick2
	jmp moveleftbrick2
	
	
	moverightbrick2:
    cmp word [currBrokenBrick], 2
    je skipBrick2Reset
	inc word[brick2x]
    cmp word [currBrick], 2
    jne skipCattoRightInc2
    add word [cattoX], 1
    skipCattoRightInc2:

    cmp word [currMouseBrick], 2
    jne skipMouseRightInc2
    add word [mouseX], 1
    skipMouseRightInc2:

    cmp word[brick2x],220
    jl skipBrick2Reset
	mov word[brickflag2],1
    skipBrick2Reset:

    jmp skipBrick2MoveLeft
	
	moveleftbrick2:
    cmp word [currBrokenBrick], 2
    je skipBrick2MoveLeft
	dec word[brick2x]
    cmp word [currBrick], 2
    jne skipCattoLeftInc2
    sub word [cattoX], 1
    skipCattoLeftInc2:
    
    cmp word [currMouseBrick], 2
    jne skipMouseLeftInc2
    sub word [mouseX], 1
    skipMouseLeftInc2:

    cmp word[brick2x],40
    jg skipBrick2LeftReset
    mov word[brickflag2],0
    skipBrick2LeftReset:

    skipBrick2MoveLeft:


    ;  moving second brick
	cmp word[brickflag3],0
	je moverightbrick3
	jmp moveleftbrick3
	
	

	moverightbrick3:
    cmp word [currBrokenBrick], 3
    je skipBrick3Reset
	inc word[brick3x]
    cmp word [currBrick], 3
    jne skipCattoRightInc3
    add word [cattoX], 1
    skipCattoRightInc3:

    cmp word [currMouseBrick], 3
    jne skipMouseRightInc3
    add word [mouseX], 1
    skipMouseRightInc3:

    cmp word[brick3x],220
    jl skipBrick3Reset
	mov word[brickflag3],1
    skipBrick3Reset:

    jmp skipBrick3MoveLeft
	
	moveleftbrick3:
    cmp word [currBrokenBrick], 3
    je skipBrick3MoveLeft
	dec word[brick3x]
    cmp word [currBrick], 3
    jne skipCattoLeftInc3
    sub word [cattoX], 1
    skipCattoLeftInc3:

    cmp word [currMouseBrick], 3
    jne skipMouseLeftInc3
    sub word [mouseX], 1
    skipMouseLeftInc3:

    cmp word[brick3x],40
    jg skipBrick3LeftReset
    mov word[brickflag3],0
    skipBrick3LeftReset:

    skipBrick3MoveLeft:

	
    add cx, 2 ; move bg 2 pixels forward
    add word [carThrusterCycles], 10

    cmp cx, 320
    jl skipReset

    mov cx, 0
    skipReset:

    cmp word [carThrusterCycles], 200
    jl skipCycleReset
    mov word [carThrusterCycles], 0
    skipCycleReset:

     
    jmp mainloop

exit:
call unhookInter 

mov ax,0003H                                   ; restore text mode
int 10H

ending:

mov ah,09H
mov ax,4C00H
int 21H


seconds: dw 0
iterCount: dw 0
secondsLength: dw 0
showLastBrick: dw 1

welcome_done:
    dw 0


;Multitasking variables
;-------------------------------------
;	  ax,bx,cx,dx,di,si,bp,sp,ip,cs,es,ip,cs,flags
pcb: times 32*16 dw 0
currenttask:dw 0

filename: db 'bg.wav',0
sound_size:dw 0xFFFF
filehandle: dw 0
sound_buffer: dw 0x6000
score:
dw 0

scoreString:
db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

scoreStringLength:
dw 0

currBrokenBrick: dw 3
isTimerRunning: dw 0

color: db 224 ; bg color in clrscr subroutine
colorOld: db 224 ; bg color in clrscr subroutine

width:
    dw 320 ; width of the screen in pixels
height:
    dw 200 ; height of the screen in pixels


carThrusterCycles:
    dw 0

car1X:
    dw 240

car1Y:
    dw 90

car2X:
    dw 140

car2Y:
    dw 90

keyboardinterrupt: 
    dw 0,0

timerinterrupt: 
    dw 0,0

brickWidth:
    dw 60

brickHeight:
    dw 8

brickx:
    dw 80
brick2x:
    dw 0
brick3x:
    dw 0

bricky:
    dw 193
brick2y:
    dw 170
brick3y:
    dw 147


brickflag: ; if 0 moves right, otherwise moves left
	dw 0
brickflag2:
	dw 0
brickflag3:
	dw 0

currBrick:
    dw 1

currSpecialBrick:
    dw 2


brick:
db 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18
db 18, 27, 27, 27, 27, 27, 27, 8, 8, 8, 8, 8, 8, 8, 8, 18, 27, 27, 27, 27, 27, 27, 8, 8, 8, 8, 8, 8, 8, 8, 18, 27, 27, 27, 27, 27, 27, 8, 8, 8, 8, 8, 8, 8, 8, 18, 27, 27, 27, 27, 27, 27, 8, 8, 8, 8, 8, 8, 8, 18
db 18, 27, 27, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 27, 27, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 27, 27, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 27, 27, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18
db 18, 27, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18, 27, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18, 27, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18, 27, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18
db 18, 27, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18, 27, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18, 27, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18, 27, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18
db 18, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18, 18, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18, 18, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18, 18, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18, 18
db 18, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18, 18, 18, 18, 18, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18, 18, 18, 18, 18, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18, 18, 18, 18, 18, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18, 18, 18, 18, 18
db 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18

brick_dark:
db 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18
db 18, 3, 3, 3, 3, 3, 3, 17, 17, 17, 17, 17, 17, 17, 17, 18, 3, 3, 3, 3, 3, 3, 17, 17, 17, 17, 17, 17, 17, 17, 18, 3, 3, 3, 3, 3, 3, 17, 17, 17, 17, 17, 17, 17, 17, 18, 3, 3, 3, 3, 3, 3, 17, 17, 17, 17, 17, 17, 17, 18
db 18, 3, 3, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 18, 3, 3, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 18, 3, 3, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 18, 3, 3, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 18
db 18, 3, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 146, 18, 3, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 146, 18, 3, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 146, 18, 3, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 146, 18
db 18, 3, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 146, 18, 3, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 146, 18, 3, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 146, 18, 3, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 146, 18
db 18, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 146, 146, 18, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 146, 146, 18, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 146, 146, 18, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 146, 146, 18
db 18, 17, 17, 17, 17, 17, 17, 17, 17, 17, 146, 146, 146, 146, 146, 18, 17, 17, 17, 17, 17, 17, 17, 17, 17, 146, 146, 146, 146, 146, 18, 17, 17, 17, 17, 17, 17, 17, 17, 17, 146, 146, 146, 146, 146, 18, 17, 17, 17, 17, 17, 17, 17, 17, 146, 146, 146, 146, 146, 18
db 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18


brokenBrick:
db 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18
db 18, 27, 27, 27, 27, 27, 224, 8, 8, 8, 8, 8, 8, 8, 224, 18, 27, 27, 27, 27, 27, 224, 8, 8, 8, 8, 8, 8, 8, 224, 18, 27, 27, 27, 27, 27, 224, 8, 8, 8, 8, 8, 8, 8, 224, 18, 27, 27, 27, 27, 27, 224, 8, 8, 8, 8, 8, 8, 224, 18
db 18, 27, 27, 8, 8, 8, 8, 8, 8, 8, 8, 8, 224, 224, 224, 18, 27, 27, 8, 8, 8, 8, 8, 8, 8, 8, 8, 224, 224, 224, 18, 27, 27, 8, 8, 8, 8, 8, 8, 8, 8, 8, 224, 224, 224, 18, 27, 27, 8, 8, 8, 8, 8, 8, 8, 8, 224, 224, 224, 18
db 18, 27, 8, 8, 224, 224, 8, 8, 8, 8, 8, 224, 224, 8, 18, 18, 27, 8, 8, 224, 224, 8, 8, 8, 8, 8, 224, 224, 8, 18, 18, 27, 8, 8, 224, 224, 8, 8, 8, 8, 8, 224, 224, 8, 18, 18, 27, 8, 8, 224, 224, 8, 8, 8, 8, 224, 224, 8, 18, 18
db 18, 20, 20, 224, 224, 20, 20, 224, 20, 20, 224, 224, 20, 20, 18, 18, 20, 20, 224, 224, 20, 20, 224, 20, 20, 224, 224, 20, 20, 18, 18, 20, 20, 224, 224, 20, 20, 224, 20, 20, 224, 224, 20, 20, 18, 18, 20, 20, 224, 224, 20, 20, 224, 20, 224, 224, 20, 20, 18, 18
db 18, 8, 224, 224, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18, 18, 8, 224, 224, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18, 18, 8, 224, 224, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18, 18, 8, 224, 224, 8, 8, 8, 8, 8, 8, 8, 8, 18, 18, 18
db 18, 224, 224, 8, 8, 8, 8, 8, 8, 8, 224, 18, 18, 18, 18, 18, 224, 224, 8, 8, 8, 8, 8, 8, 8, 224, 18, 18, 18, 18, 18, 224, 224, 8, 8, 8, 8, 8, 8, 8, 224, 18, 18, 18, 18, 18, 224, 224, 8, 8, 8, 8, 8, 8, 224, 18, 18, 18, 18, 18
db 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18


quitGame:
dw 0


mouseHeight:
    dw 7

mouseWidth:
    dw 14

mouseX:
    dw 0

mouseY:
    dw 0

mouseExists:
    dw 0 ; 1 means mouse on screen

currMouseBrick:
    dw 1

mouse:
db 255, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 255
db 0, 29, 29, 0, 255, 255, 255, 255, 255, 255, 0, 29, 29, 0
db 0, 29, 15, 29, 0, 0, 0, 0, 0, 0, 29, 29, 29, 0
db 255, 0, 29, 15, 29, 29, 29, 29, 29, 29, 29, 29, 0, 255
db 0, 29, 29, 29, 0, 0, 0, 0, 0, 0, 29, 29, 28, 0
db 0, 29, 28, 0, 255, 255, 255, 255, 255, 255, 0, 28, 28, 0
db 255, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 255

; db 255, 30, 30, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255
; db 30, 255, 255, 30, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255
; db 255, 255, 255, 255, 30, 255, 255, 255, 255, 30, 30, 30, 30, 30, 30, 255, 255, 255, 30, 255, 255
; db 255, 255, 255, 255, 30, 255, 255, 255, 30, 30, 30, 30, 30, 30, 30, 30, 255, 255, 30, 255, 255
; db 255, 255, 255, 255, 255, 30, 255, 255, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 255
; db 255, 255, 255, 255, 255, 131, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30
; db 255, 255, 255, 255, 255, 255, 255, 255, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 255, 255, 255
; db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 30, 30, 255, 255, 255, 255, 255, 30, 30, 255, 255

movePlatformsDown: ; 1 means moving them down
    dw 0

movedLastPlatformUp:
    dw 0

timeText:db '1'

isJumping:
    dw 0

fallingCatto:
    dw 0

cattoY:
    dw 178

cattoX:
    dw 104

cattoWidth:
    dw 16 ; width of the catto in pixels

cattoHeight:
    dw 15 ; height of the catto in pixels

catto:
db 255, 255, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 255, 255
db 255, 0, 23, 23, 0, 255, 255, 255, 255, 255, 0, 0, 23, 23, 0, 255
db 255, 0, 23, 23, 23, 0, 255, 255, 255, 255, 0, 23, 23, 23, 0, 255
db 255, 0, 23, 23, 23, 23, 0, 0, 0, 0, 23, 23, 23, 23, 0, 255
db 255, 0, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 0, 255
db 0, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 0
db 0, 23, 23, 23, 15, 0, 23, 23, 23, 23, 23, 15, 0, 23, 23, 0
db 0, 23, 23, 23, 0, 0, 23, 23, 23, 0, 23, 0, 0, 23, 23, 0
db 0, 23, 38, 38, 23, 23, 23, 23, 23, 23, 23, 23, 23, 38, 38, 0
db 0, 23, 38, 38, 23, 0, 23, 23, 0, 23, 23, 0, 23, 38, 38, 0
db 255, 0, 23, 23, 23, 0, 0, 0, 0, 0, 0, 0, 23, 23, 0, 255
db 255, 255, 0, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 0, 255, 255
db 255, 255, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255
db 255, 255, 25, 0, 23, 23, 0, 255, 0, 23, 23, 0, 255, 255, 255, 255
db 255, 255, 255, 255, 0, 0, 0, 255, 255, 0, 0, 255, 255, 255, 255, 255


spaceToStartHeight: dw 6
spaceToStartWidth: dw 107

spaceToStart:
db 172, 172, 172, 172, 255, 255, 172, 172, 172, 172, 172, 255, 255, 172, 172, 172, 172, 172, 255, 255, 172, 172, 172, 255, 255, 172, 172, 172, 172, 255, 255, 255, 255, 172, 172, 172, 172, 172, 172, 172, 172, 255, 255, 255, 172, 172, 172, 255, 255, 172, 172, 172, 172, 172, 255, 172, 172, 172, 172, 172, 255, 255, 255, 172, 172, 172, 172, 172, 172, 255, 172, 172, 172, 172, 255, 255, 255, 255, 255, 255, 172, 172, 172, 255, 172, 172, 172, 172, 172, 255, 255, 172, 172, 255, 255, 172, 172, 172, 172, 172, 255, 255, 172, 172, 172, 172, 172, 
db 172, 255, 255, 255, 172, 255, 172, 255, 255, 255, 255, 172, 255, 172, 255, 255, 255, 255, 172, 172, 255, 255, 255, 255, 172, 255, 255, 255, 255, 255, 255, 255, 172, 255, 255, 255, 255, 172, 255, 255, 255, 172, 255, 172, 255, 255, 255, 172, 172, 255, 255, 255, 255, 172, 255, 172, 255, 255, 255, 255, 255, 255, 255, 255, 255, 172, 255, 255, 255, 172, 255, 255, 255, 255, 172, 255, 255, 255, 255, 172, 255, 255, 255, 255, 255, 255, 172, 255, 255, 255, 172, 255, 255, 172, 172, 172, 255, 255, 255, 255, 172, 255, 255, 255, 172, 255, 255, 
db 172, 172, 172, 172, 255, 255, 172, 172, 172, 172, 172, 255, 255, 172, 172, 255, 255, 255, 255, 255, 172, 172, 255, 255, 255, 172, 172, 172, 255, 255, 255, 255, 255, 172, 172, 172, 255, 172, 172, 172, 172, 255, 255, 172, 172, 172, 172, 172, 172, 255, 255, 255, 255, 255, 255, 172, 172, 255, 255, 255, 255, 255, 255, 255, 255, 172, 255, 255, 255, 172, 255, 255, 255, 255, 172, 255, 255, 255, 255, 255, 172, 172, 255, 255, 255, 255, 172, 255, 255, 255, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 255, 255, 255, 255, 172, 255, 255, 
db 172, 255, 255, 255, 255, 255, 172, 255, 255, 172, 255, 255, 255, 172, 255, 255, 255, 255, 255, 255, 255, 255, 172, 255, 255, 255, 255, 255, 172, 255, 255, 255, 255, 255, 255, 172, 172, 172, 255, 255, 255, 255, 255, 172, 255, 255, 255, 172, 172, 255, 255, 255, 255, 255, 255, 172, 255, 255, 255, 255, 255, 255, 255, 255, 255, 172, 255, 255, 255, 172, 255, 255, 255, 255, 172, 255, 255, 255, 255, 255, 255, 255, 172, 255, 255, 255, 172, 255, 255, 255, 172, 255, 255, 172, 172, 172, 255, 255, 172, 255, 255, 255, 255, 255, 172, 255, 255, 
db 172, 255, 255, 255, 255, 255, 172, 255, 255, 255, 172, 255, 255, 172, 255, 255, 255, 255, 255, 255, 255, 255, 172, 255, 255, 255, 255, 255, 172, 255, 255, 255, 255, 255, 255, 172, 172, 172, 255, 255, 255, 255, 255, 172, 255, 255, 255, 172, 172, 255, 255, 255, 255, 172, 255, 172, 255, 255, 255, 255, 255, 255, 255, 255, 255, 172, 255, 255, 255, 172, 255, 255, 255, 255, 172, 255, 255, 255, 255, 255, 255, 255, 172, 255, 255, 255, 172, 255, 255, 255, 172, 255, 255, 172, 172, 172, 255, 255, 255, 172, 255, 255, 255, 255, 172, 255, 255, 
db 172, 255, 255, 255, 255, 255, 172, 255, 255, 255, 255, 172, 255, 172, 172, 172, 172, 172, 172, 172, 172, 172, 255, 255, 172, 172, 172, 172, 255, 255, 255, 255, 172, 172, 172, 172, 255, 172, 255, 255, 255, 255, 255, 172, 255, 255, 255, 172, 255, 172, 172, 172, 172, 172, 255, 172, 172, 172, 172, 172, 255, 255, 255, 255, 255, 172, 255, 255, 255, 255, 172, 172, 172, 172, 255, 255, 255, 255, 255, 172, 172, 172, 255, 255, 255, 255, 172, 255, 255, 255, 172, 255, 255, 172, 172, 172, 255, 255, 255, 255, 172, 255, 255, 255, 172, 255, 255, 



isGameOver:
    dw 0

gameOverWidth:
    dw 60

gameOverHeight:
    dw 33

gameOver:
db 255, 255, 224, 224, 224, 224, 224, 224, 224, 224, 255, 255, 255, 255, 255, 255, 255, 255, 224, 224, 224, 224, 224, 224, 224, 255, 255, 255, 255, 255, 255, 224, 224, 224, 224, 255, 255, 255, 255, 255, 255, 224, 224, 224, 224, 255, 255, 255, 255, 224, 224, 224, 224, 224, 224, 224, 224, 224, 25, 255
db 255, 255, 176, 176, 176, 176, 176, 176, 176, 176, 52, 255, 255, 255, 255, 255, 255, 255, 225, 176, 176, 176, 176, 176, 176, 3, 255, 255, 255, 255, 255, 247, 176, 176, 176, 52, 255, 255, 255, 255, 255, 225, 176, 176, 176, 52, 255, 255, 255, 225, 176, 176, 176, 176, 176, 176, 176, 176, 149, 100
db 224, 176, 176, 176, 52, 37, 37, 37, 176, 176, 177, 225, 255, 255, 255, 255, 225, 225, 225, 176, 52, 37, 37, 5, 176, 176, 224, 255, 255, 255, 255, 247, 176, 176, 176, 176, 225, 255, 255, 225, 225, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 37, 37, 37, 37, 37, 78
db 224, 176, 176, 176, 52, 52, 37, 37, 225, 176, 176, 176, 52, 255, 255, 255, 225, 176, 176, 176, 52, 52, 37, 63, 200, 176, 176, 52, 255, 255, 255, 247, 176, 176, 176, 176, 176, 52, 255, 225, 176, 176, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 37, 37, 37, 37, 62
db 224, 176, 176, 176, 52, 52, 255, 255, 255, 37, 37, 37, 37, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 30, 200, 176, 176, 52, 52, 255, 255, 247, 176, 176, 176, 176, 176, 176, 225, 225, 176, 176, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 255, 255, 255
db 224, 176, 176, 176, 52, 52, 255, 255, 255, 255, 37, 37, 37, 37, 255, 255, 225, 176, 176, 176, 52, 52, 255, 30, 200, 176, 176, 52, 52, 255, 255, 247, 176, 176, 176, 176, 176, 176, 176, 176, 176, 176, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 255, 255, 255
db 224, 176, 176, 176, 52, 52, 225, 225, 225, 225, 247, 225, 255, 255, 255, 255, 225, 176, 176, 176, 52, 52, 255, 30, 200, 176, 176, 52, 52, 255, 255, 247, 176, 176, 176, 52, 37, 176, 176, 52, 37, 176, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 176, 226, 225, 225, 255, 255, 255
db 224, 176, 176, 176, 52, 52, 225, 176, 176, 176, 176, 176, 52, 255, 255, 255, 225, 176, 176, 176, 52, 52, 255, 30, 200, 176, 176, 52, 52, 255, 255, 247, 176, 176, 176, 52, 52, 225, 176, 52, 52, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 176, 176, 176, 199, 11, 255, 255
db 224, 176, 176, 176, 52, 52, 255, 37, 176, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 176, 225, 225, 225, 200, 176, 176, 52, 52, 255, 255, 247, 176, 176, 176, 52, 52, 255, 37, 37, 52, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 37, 37, 37, 57, 11, 255
db 224, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 176, 176, 176, 176, 176, 176, 176, 52, 52, 255, 255, 247, 176, 176, 176, 52, 52, 255, 255, 37, 37, 247, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 37, 37, 37, 61, 255
db 224, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 37, 37, 5, 176, 176, 176, 52, 52, 255, 255, 247, 176, 176, 176, 52, 52, 255, 255, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 255, 255, 255
db 224, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 37, 25, 200, 176, 176, 52, 52, 255, 255, 247, 176, 176, 176, 52, 52, 255, 255, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 255, 255, 255
db 255, 37, 176, 176, 176, 225, 225, 225, 225, 176, 52, 37, 37, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 200, 176, 176, 52, 52, 255, 255, 247, 176, 176, 176, 52, 52, 255, 255, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 176, 225, 225, 225, 225, 25, 255
db 255, 255, 225, 176, 176, 176, 176, 176, 176, 176, 52, 52, 37, 37, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 200, 176, 176, 52, 52, 255, 255, 247, 176, 176, 176, 52, 52, 255, 255, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 176, 176, 176, 176, 176, 149, 100
db 255, 255, 255, 37, 37, 37, 37, 37, 37, 37, 37, 52, 255, 255, 255, 255, 255, 37, 37, 37, 37, 52, 255, 255, 86, 37, 37, 37, 52, 255, 255, 255, 13, 37, 37, 37, 52, 255, 255, 255, 255, 255, 62, 37, 37, 37, 52, 255, 255, 255, 37, 37, 37, 37, 37, 37, 37, 37, 37, 78
db 255, 255, 255, 255, 37, 37, 37, 37, 37, 37, 37, 37, 255, 255, 255, 255, 255, 255, 37, 37, 37, 37, 255, 255, 255, 62, 37, 37, 37, 255, 255, 255, 255, 37, 37, 37, 37, 255, 255, 255, 255, 255, 255, 37, 37, 37, 37, 255, 255, 255, 255, 37, 37, 37, 37, 37, 37, 37, 37, 37
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255
db 255, 255, 255, 225, 225, 225, 225, 225, 225, 225, 225, 255, 255, 255, 255, 255, 255, 225, 225, 225, 247, 255, 255, 255, 25, 128, 225, 225, 255, 255, 255, 255, 224, 225, 225, 225, 225, 225, 225, 225, 225, 225, 255, 255, 255, 255, 225, 225, 225, 225, 225, 225, 225, 225, 225, 245, 255, 255, 255, 255
db 255, 255, 255, 225, 176, 176, 176, 176, 176, 176, 176, 52, 255, 255, 255, 255, 255, 225, 176, 176, 176, 52, 255, 255, 25, 177, 176, 176, 52, 255, 255, 255, 224, 176, 176, 176, 176, 176, 176, 176, 176, 176, 52, 255, 255, 255, 225, 176, 176, 176, 176, 176, 176, 176, 176, 176, 52, 255, 255, 255
db 255, 225, 225, 224, 176, 52, 62, 37, 37, 176, 176, 176, 224, 255, 255, 255, 255, 225, 176, 176, 176, 52, 52, 255, 25, 177, 176, 176, 52, 52, 255, 255, 224, 176, 176, 176, 52, 37, 37, 37, 37, 37, 37, 52, 255, 255, 225, 176, 176, 176, 52, 37, 37, 37, 176, 176, 176, 228, 255, 255
db 255, 225, 176, 176, 176, 52, 52, 62, 62, 225, 176, 176, 176, 52, 255, 255, 255, 225, 176, 176, 176, 52, 52, 255, 25, 177, 176, 176, 52, 52, 255, 255, 224, 176, 176, 176, 52, 52, 37, 37, 37, 37, 37, 37, 255, 255, 225, 176, 176, 176, 52, 52, 62, 62, 225, 176, 176, 198, 11, 255
db 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 25, 177, 176, 176, 52, 52, 255, 255, 224, 176, 176, 176, 52, 52, 255, 255, 255, 255, 255, 255, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 198, 52, 100
db 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 25, 177, 176, 176, 52, 52, 255, 255, 224, 176, 176, 176, 52, 52, 255, 255, 255, 255, 255, 255, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 198, 52, 100
db 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 25, 177, 176, 176, 52, 52, 255, 255, 224, 176, 176, 176, 176, 224, 224, 224, 255, 255, 255, 255, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 198, 52, 100
db 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 25, 177, 176, 176, 52, 52, 255, 255, 224, 176, 176, 176, 126, 180, 180, 180, 78, 100, 255, 255, 255, 255, 225, 176, 176, 176, 125, 124, 23, 23, 225, 176, 3, 132, 79, 100
db 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 25, 177, 176, 176, 52, 52, 255, 255, 224, 176, 176, 176, 52, 53, 37, 37, 37, 62, 255, 255, 255, 255, 225, 176, 176, 176, 176, 176, 176, 176, 176, 176, 52, 78, 37, 255
db 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 255, 37, 176, 176, 176, 222, 225, 226, 177, 11, 62, 62, 52, 255, 255, 224, 176, 176, 176, 52, 52, 255, 255, 255, 255, 255, 255, 255, 255, 225, 176, 176, 176, 52, 37, 37, 62, 176, 177, 176, 151, 255, 255
db 255, 225, 176, 176, 176, 52, 52, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 255, 255, 128, 176, 176, 176, 176, 176, 176, 53, 52, 13, 62, 255, 255, 224, 176, 176, 176, 52, 52, 255, 255, 255, 255, 255, 255, 255, 255, 225, 176, 176, 176, 52, 52, 37, 37, 176, 176, 176, 198, 11, 255
db 255, 255, 37, 176, 176, 176, 224, 225, 128, 225, 176, 52, 13, 37, 52, 255, 255, 255, 255, 255, 37, 176, 176, 176, 124, 57, 13, 11, 255, 255, 255, 255, 224, 176, 176, 176, 176, 128, 225, 225, 225, 226, 255, 255, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 176, 176, 176, 124, 52, 255
db 255, 255, 255, 226, 176, 176, 176, 176, 176, 176, 176, 52, 52, 37, 13, 255, 255, 255, 255, 255, 255, 224, 176, 176, 124, 52, 58, 37, 255, 255, 255, 255, 224, 176, 176, 176, 176, 176, 176, 176, 176, 176, 52, 255, 255, 255, 225, 176, 176, 176, 52, 52, 255, 255, 176, 176, 176, 176, 52, 255
db 255, 255, 255, 255, 37, 37, 37, 37, 37, 37, 37, 62, 52, 255, 255, 255, 255, 255, 255, 255, 255, 255, 37, 37, 37, 78, 255, 255, 255, 255, 255, 255, 255, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 52, 255, 255, 255, 37, 37, 37, 37, 52, 255, 255, 255, 37, 37, 37, 24, 255
db 255, 255, 255, 255, 255, 37, 37, 37, 37, 37, 37, 37, 62, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 37, 37, 37, 255, 255, 255, 255, 255, 255, 255, 255, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 255, 255, 255, 255, 37, 37, 37, 37, 255, 255, 255, 255, 37, 37, 37, 255


start_game_width:
    dw 114
start_game_height:
    dw 13


start_game:
db 255, 255, 126, 126, 126, 126, 126, 126, 126, 126, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 255, 255, 255, 126, 126, 126, 126, 126, 126, 255, 255, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 255, 255, 255, 126, 126, 126, 126, 126, 126, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126
db 255, 255, 126, 126, 126, 126, 126, 126, 126, 126, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 255, 255, 255, 126, 126, 126, 126, 126, 126, 255, 255, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 255, 255, 255, 126, 126, 126, 126, 126, 126, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126
db 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 255, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 126, 126, 126, 126, 255, 255, 126, 126, 126, 126, 126, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255
db 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 255, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 126, 126, 126, 126, 255, 255, 126, 126, 126, 126, 126, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255
db 255, 255, 126, 126, 126, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 126, 126, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 255, 126, 126, 255, 255, 126, 126, 255, 255, 255, 126, 126, 255, 126, 126, 126, 126, 126, 126, 255, 255, 255, 255, 255
db 255, 255, 126, 126, 126, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 126, 126, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 255, 126, 126, 255, 255, 126, 126, 255, 255, 255, 126, 126, 255, 126, 126, 126, 126, 126, 126, 255, 255, 255, 255, 255
db 255, 255, 126, 126, 126, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 126, 126, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 255, 126, 126, 255, 255, 126, 126, 255, 255, 255, 126, 126, 255, 126, 126, 126, 126, 126, 126, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 255, 126, 126, 126, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 255, 126, 126, 126, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 255, 126, 126, 126, 255, 255, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 255, 126, 126, 126, 255, 255, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255
db 126, 126, 126, 126, 126, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 255, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 126, 126, 126, 126, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126
db 126, 126, 126, 126, 126, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 255, 126, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 255, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 126, 126, 126, 126, 126, 126, 126, 255, 255, 255, 255, 126, 126, 255, 255, 255, 255, 255, 126, 126, 126, 255, 126, 126, 255, 255, 255, 255, 255, 255, 255, 126, 126, 255, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126


qainatWidth: dw 140
qainatHeight: dw 9

qainat:
db 255, 5, 5, 5, 255, 255, 255, 255, 5, 5, 5, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 5, 5, 5, 5, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 5, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 5, 5, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 
db 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 5, 5, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 
db 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 5, 5, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 
db 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 5, 5, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 5, 5, 255, 255, 255, 5, 5, 5, 5, 255, 255, 255, 5, 5, 5, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 5, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 5, 5, 5, 5, 255, 255, 255, 255, 255, 255, 255, 5, 5, 5, 5, 5, 5, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 5, 5, 5, 5, 
db 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 5, 5, 5, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 5, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 5, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 
db 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 5, 5, 255, 255, 255, 255, 5, 5, 5, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 5, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 5, 5, 255, 255, 5, 255, 255, 255, 5, 5, 5, 5, 255, 255, 255, 5, 5, 5, 5, 255, 255, 255, 5, 255, 255, 255, 5, 
db 255, 5, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 5, 5, 5, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 5, 5, 255, 5, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 5, 
db 5, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 5, 5, 255, 5, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 
db 5, 5, 5, 5, 5, 255, 255, 5, 5, 5, 5, 5, 255, 255, 5, 5, 5, 5, 5, 255, 255, 255, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 5, 5, 5, 5, 5, 5, 255, 255, 5, 5, 5, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 5, 5, 255, 5, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 5, 5, 5, 5, 5, 5, 255, 255, 255, 5, 5, 5, 255, 5, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 5, 5, 5, 255, 


asjadWidth: dw 154
asjadHeight: dw 13

asjad:
db 255, 5, 5, 5, 255, 255, 255, 255, 5, 5, 5, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 5, 5, 5, 5, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 5, 5, 5, 5, 255, 5, 5, 255, 5, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 5, 255, 5, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 5, 
db 5, 255, 255, 255, 5, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 5, 5, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 
db 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 
db 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 5, 5, 5, 255, 255, 255, 5, 5, 5, 5, 255, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 5, 255, 255, 255, 5, 5, 5, 255, 255, 255, 5, 5, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 255, 5, 5, 5, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 5, 5, 5, 5, 255, 255, 255, 5, 255, 255, 255, 255, 5, 5, 5, 5, 255, 255, 255, 5, 5, 5, 5, 5, 255, 255, 5, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 5, 255, 
db 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 5, 5, 5, 5, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 5, 255, 
db 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 5, 5, 5, 5, 5, 255, 5, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 5, 255, 255, 255, 5, 5, 5, 255, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 5, 255, 
db 255, 5, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 5, 5, 255, 5, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 5, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 5, 255, 
db 5, 5, 255, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 5, 255, 255, 255, 5, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 5, 255, 5, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 5, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 255, 255, 255, 5, 5, 255, 255, 5, 255, 255, 255, 5, 255, 255, 255, 5, 255, 255, 5, 5, 255, 255, 5, 5, 255, 255, 5, 255, 
db 5, 5, 5, 5, 5, 255, 255, 5, 5, 5, 5, 5, 5, 255, 255, 5, 5, 5, 5, 5, 255, 255, 255, 255, 255, 255, 255, 5, 5, 5, 5, 5, 5, 255, 255, 5, 5, 5, 5, 5, 255, 255, 5, 5, 5, 5, 5, 5, 255, 255, 5, 5, 5, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 5, 255, 255, 5, 5, 5, 5, 5, 5, 255, 5, 5, 255, 255, 255, 5, 5, 5, 5, 5, 5, 255, 255, 5, 5, 5, 5, 5, 5, 255, 255, 255, 255, 255, 255, 255, 5, 5, 5, 5, 5, 5, 5, 5, 255, 255, 5, 255, 255, 255, 5, 5, 5, 5, 5, 255, 255, 5, 5, 5, 5, 5, 5, 255, 255, 5, 255, 255, 255, 5, 5, 5, 5, 5, 255, 255, 5, 5, 5, 5, 5, 5, 255, 255, 5, 255, 
db 5, 5, 5, 5, 5, 255, 255, 5, 5, 5, 5, 5, 5, 255, 255, 255, 5, 5, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 5, 5, 5, 5, 255, 255, 255, 5, 5, 5, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 5, 255, 255, 255, 5, 5, 5, 255, 255, 255, 5, 5, 255, 255, 255, 255, 5, 5, 5, 255, 5, 255, 255, 255, 5, 5, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 5, 5, 5, 5, 255, 255, 255, 5, 255, 255, 255, 255, 5, 5, 5, 255, 255, 255, 255, 5, 5, 5, 5, 255, 255, 255, 5, 255, 255, 255, 255, 5, 5, 5, 5, 255, 255, 255, 5, 5, 5, 255, 5, 255, 255, 5, 255, 
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 


carWidth:
    dw 70 ; width of the car in pixels
carHeight:
    dw 29 ; height of the car in pixels

car:
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 138, 255, 255, 255, 255, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 0, 18, 18, 18, 18, 30, 30, 30, 30, 30, 30, 30, 18, 18, 18, 18, 7, 7, 7, 18, 18, 18, 18, 18, 18, 18, 18, 18, 7, 0, 28, 22, 27, 30, 23, 0, 18, 255, 255, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 18, 18, 18, 18, 30, 30, 30, 30, 30, 30, 30, 18, 18, 18, 18, 7, 7, 7, 7, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 27, 7, 0, 28, 22, 27, 30, 30, 30, 27, 18, 255, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 7, 18, 18, 18, 24, 30, 30, 30, 30, 30, 30, 18, 18, 18, 7, 7, 7, 7, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 27, 0, 0, 29, 30, 22, 27, 30, 30, 30, 30, 28, 0, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 7, 7, 18, 18, 18, 24, 24, 24, 24, 18, 18, 18, 18, 18, 7, 7, 7, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 23, 23, 23, 23, 23, 23, 27, 0, 15, 30, 30, 30, 22, 27, 30, 30, 30, 27, 18, 74, 0, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 7, 7, 7, 18, 18, 18, 24, 24, 24, 24, 18, 18, 18, 18, 7, 7, 7, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 27, 0, 30, 30, 30, 30, 23, 27, 30, 30, 30, 7, 18, 75, 75, 0, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 7, 7, 18, 18, 18, 18, 24, 24, 24, 24, 18, 18, 18, 18, 7, 7, 18, 18, 18, 7, 7, 7, 7, 18, 18, 18, 23, 23, 23, 23, 23, 23, 23, 23, 18, 18, 18, 18, 18, 27, 0, 30, 30, 30, 30, 23, 27, 30, 30, 30, 30, 27, 0, 77, 11, 0, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 7, 7, 18, 18, 18, 24, 24, 24, 24, 24, 18, 18, 18, 18, 7, 7, 18, 18, 18, 7, 7, 0, 0, 0, 0, 7, 23, 23, 23, 23, 23, 18, 18, 18, 18, 18, 18, 18, 18, 18, 27, 0, 30, 30, 30, 30, 30, 23, 27, 30, 30, 30, 30, 30, 28, 0, 11, 0, 255, 255
db 255, 255, 255, 255, 255, 255, 0, 0, 7, 7, 18, 18, 18, 24, 24, 24, 24, 24, 18, 18, 18, 18, 7, 7, 18, 18, 18, 7, 7, 0, 0, 30, 30, 30, 30, 0, 7, 23, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 27, 0, 30, 30, 30, 30, 29, 22, 27, 30, 30, 30, 30, 30, 28, 0, 75, 75, 0, 255
db 255, 255, 255, 255, 255, 0, 7, 7, 18, 18, 18, 24, 24, 24, 24, 24, 18, 18, 18, 18, 18, 7, 18, 18, 18, 7, 7, 0, 0, 30, 30, 20, 30, 30, 30, 0, 7, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 27, 0, 15, 30, 30, 30, 30, 22, 28, 30, 30, 30, 30, 30, 30, 30, 29, 0, 0, 0, 255
db 255, 255, 255, 255, 0, 30, 0, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 0, 0, 30, 30, 30, 30, 30, 8, 30, 30, 30, 0, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 0, 15, 30, 30, 30, 30, 30, 22, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 15, 0, 255
db 255, 255, 255, 0, 30, 30, 29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 30, 30, 30, 30, 30, 30, 30, 8, 30, 30, 30, 30, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 30, 30, 30, 30, 30, 30, 8, 29, 30, 30, 30, 30, 30, 30, 30, 30, 29, 0, 0, 0, 0
db 255, 255, 255, 0, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 8, 29, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 8, 29, 30, 30, 30, 30, 30, 30, 27, 0, 0, 8, 8, 8, 8
db 255, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 8, 29, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 20, 29, 30, 30, 30, 30, 30, 30, 7, 17, 8, 8, 18, 18, 18, 18
db 255, 255, 0, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 0, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 8, 29, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 20, 30, 30, 30, 30, 30, 30, 29, 24, 17, 8, 18, 18, 18, 18, 18, 18
db 255, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 30, 30, 30, 30, 30, 30, 30, 30, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 30, 30, 30, 30, 30, 30, 30, 23, 18, 20, 18, 18, 18, 18, 18, 18, 18
db 255, 0, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 0, 8, 8, 8, 8, 8, 0, 0, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 23, 18, 20, 18, 18, 18, 18, 18, 18, 17
db 0, 8, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 8, 0, 8, 18, 18, 18, 18, 18, 18, 18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 18, 20, 18, 18, 18, 18, 17, 18, 18, 17
db 0, 8, 18, 75, 75, 75, 77, 18, 18, 18, 18, 18, 18, 18, 75, 75, 75, 75, 18, 8, 0, 8, 18, 18, 18, 18, 18, 18, 18, 18, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 19, 18, 18, 18, 17, 18, 17, 17, 18, 18
db 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
db 255, 255, 255, 255, 0, 17, 17, 17, 17, 17, 17, 17, 17, 17, 0, 255, 255, 255, 255, 255, 255, 255, 0, 18, 18, 18, 18, 17, 18, 18, 18, 18, 0, 255, 255, 255, 255, 255, 0, 17, 17, 17, 17, 17, 17, 17, 17, 17, 0, 255, 255, 255, 255, 255, 255, 255, 18, 17, 17, 18, 17, 18, 18, 17, 17, 18, 17, 0, 255, 255
db 255, 255, 255, 255, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 0, 18, 18, 18, 42, 42, 42, 42, 18, 18, 0, 255, 255, 255, 255, 255, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 18, 17, 17, 18, 137, 42, 42, 42, 42, 18, 17, 0, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 75, 99, 75, 99, 98, 75, 99, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 75, 75, 99, 99, 75, 99, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 75, 75, 99, 99, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 99, 75, 99, 99, 75, 99, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 75, 99, 99, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 75, 99, 77, 75, 101, 77, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 75, 75, 99, 75, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 99, 99, 75, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 75, 99, 75, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 99, 77, 75, 75, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 75, 255, 75, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 75, 99, 75, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 255, 75, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 77, 77, 75, 75, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 75, 255, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 75, 255, 75, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 74, 255, 255, 255, 255, 255, 255, 255


car2:
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 138, 255, 255, 255, 255, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 0, 18, 18, 18, 18, 30, 30, 30, 30, 30, 30, 30, 18, 18, 18, 18, 7, 7, 7, 18, 18, 18, 18, 18, 18, 18, 18, 18, 7, 0, 28, 22, 27, 30, 23, 0, 18, 255, 255, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 18, 18, 18, 18, 30, 30, 30, 30, 30, 30, 30, 18, 18, 18, 18, 7, 7, 7, 7, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 27, 7, 0, 28, 22, 27, 30, 30, 30, 27, 18, 255, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 7, 18, 18, 18, 24, 30, 30, 30, 30, 30, 30, 18, 18, 18, 7, 7, 7, 7, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 27, 0, 0, 29, 30, 22, 27, 30, 30, 30, 30, 28, 0, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 7, 7, 18, 18, 18, 24, 24, 24, 24, 18, 18, 18, 18, 18, 7, 7, 7, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 23, 23, 23, 23, 23, 23, 27, 0, 15, 30, 30, 30, 22, 27, 30, 30, 30, 27, 18, 74, 0, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 7, 7, 7, 18, 18, 18, 24, 24, 24, 24, 18, 18, 18, 18, 7, 7, 7, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 27, 0, 30, 30, 30, 30, 23, 27, 30, 30, 30, 7, 18, 75, 75, 0, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 7, 7, 18, 18, 18, 18, 24, 24, 24, 24, 18, 18, 18, 18, 7, 7, 18, 18, 18, 7, 7, 7, 7, 18, 18, 18, 23, 23, 23, 23, 23, 23, 23, 23, 18, 18, 18, 18, 18, 27, 0, 30, 30, 30, 30, 23, 27, 30, 30, 30, 30, 27, 0, 77, 11, 0, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 7, 7, 18, 18, 18, 24, 24, 24, 24, 24, 18, 18, 18, 18, 7, 7, 18, 18, 18, 7, 7, 0, 0, 0, 0, 7, 23, 23, 23, 23, 23, 18, 18, 18, 18, 18, 18, 18, 18, 18, 27, 0, 30, 30, 30, 30, 30, 23, 27, 30, 30, 30, 30, 30, 28, 0, 11, 0, 255, 255
db 255, 255, 255, 255, 255, 255, 0, 0, 7, 7, 18, 18, 18, 24, 24, 24, 24, 24, 18, 18, 18, 18, 7, 7, 18, 18, 18, 7, 7, 0, 0, 30, 30, 30, 30, 0, 7, 23, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 27, 0, 30, 30, 30, 30, 29, 22, 27, 30, 30, 30, 30, 30, 28, 0, 75, 75, 0, 255
db 255, 255, 255, 255, 255, 0, 7, 7, 18, 18, 18, 24, 24, 24, 24, 24, 18, 18, 18, 18, 18, 7, 18, 18, 18, 7, 7, 0, 0, 30, 30, 20, 30, 30, 30, 0, 7, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 27, 0, 15, 30, 30, 30, 30, 22, 28, 30, 30, 30, 30, 30, 30, 30, 29, 0, 0, 0, 255
db 255, 255, 255, 255, 0, 30, 0, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 0, 0, 30, 30, 30, 30, 30, 8, 30, 30, 30, 0, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 0, 15, 30, 30, 30, 30, 30, 22, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 15, 0, 255
db 255, 255, 255, 0, 30, 30, 29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 30, 30, 30, 30, 30, 30, 30, 8, 30, 30, 30, 30, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 30, 30, 30, 30, 30, 30, 8, 29, 30, 30, 30, 30, 30, 30, 30, 30, 29, 0, 0, 0, 0
db 255, 255, 255, 0, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 8, 29, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 8, 29, 30, 30, 30, 30, 30, 30, 27, 0, 0, 8, 8, 8, 8
db 255, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 8, 29, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 20, 29, 30, 30, 30, 30, 30, 30, 7, 17, 8, 8, 18, 18, 18, 18
db 255, 255, 0, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 0, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 8, 29, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 20, 30, 30, 30, 30, 30, 30, 29, 24, 17, 8, 18, 18, 18, 18, 18, 18
db 255, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 30, 30, 30, 30, 30, 30, 30, 30, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 30, 30, 30, 30, 30, 30, 30, 23, 18, 20, 18, 18, 18, 18, 18, 18, 18
db 255, 0, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 0, 8, 8, 8, 8, 8, 0, 0, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 23, 18, 20, 18, 18, 18, 18, 18, 18, 17
db 0, 8, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 8, 0, 8, 18, 18, 18, 18, 18, 18, 18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 18, 20, 18, 18, 18, 18, 17, 18, 18, 17
db 0, 8, 18, 75, 75, 75, 77, 18, 18, 18, 18, 18, 18, 18, 75, 75, 75, 75, 18, 8, 0, 8, 18, 18, 18, 18, 18, 18, 18, 18, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 19, 18, 18, 18, 17, 18, 17, 17, 18, 18
db 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
db 255, 255, 255, 255, 0, 17, 17, 17, 17, 17, 17, 17, 17, 17, 0, 255, 255, 255, 255, 255, 255, 255, 0, 18, 18, 18, 18, 17, 18, 18, 18, 18, 0, 255, 255, 255, 255, 255, 0, 17, 17, 17, 17, 17, 17, 17, 17, 17, 0, 255, 255, 255, 255, 255, 255, 255, 18, 17, 17, 18, 17, 18, 18, 17, 17, 18, 17, 0, 255, 255
db 255, 255, 255, 255, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 0, 18, 18, 18, 42, 42, 42, 42, 18, 18, 0, 255, 255, 255, 255, 255, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 18, 17, 17, 18, 137, 42, 42, 42, 42, 18, 17, 0, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 75, 99, 75, 99, 98, 75, 99, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 75, 75, 99, 99, 75, 99, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 75, 75, 99, 99, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 99, 75, 99, 99, 75, 99, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 75, 99, 99, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 75, 99, 77, 75, 101, 77, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 75, 75, 99, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 99, 99, 75, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 75, 99, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 99, 77, 75, 75, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 75, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 99, 75, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 77, 77, 75, 75, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 75, 255, 255, 255, 255, 255, 255
db 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 75, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 74, 255, 255, 255, 255, 255, 255, 255



bgWidth:
    dw 320

bgHeight:
    dw 148


bg:
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 131, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 3, 3, 129, 3, 3, 129, 53, 53, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 131, 9, 9, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 131, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 131, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 3, 3, 129, 3, 3, 129, 53, 53, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 3, 3, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 3, 3, 129, 3, 3, 129, 53, 53, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 3, 3, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 53, 53, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 131, 131, 131, 131, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 53, 53, 128, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 9, 9, 132, 132, 131, 132, 132, 132, 132, 131, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 128, 128, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 128, 128, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 151, 151, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 132, 132, 132, 132, 131, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 3, 3, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 128, 128, 128, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 128, 128, 151, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 131, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
db 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 131, 9, 9, 131, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132
db 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132
db 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 128, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132
db 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 9, 9, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 132, 131, 9, 9, 132, 132, 132, 132
db 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 3, 3, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 131, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 131, 132, 132, 132, 132, 132
db 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 128, 3, 3, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132
db 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 131, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 131, 9, 9, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 131, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 131, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 131, 132, 132, 132, 132, 132, 132, 132
db 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132
db 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 128, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132
db 132, 132, 9, 9, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 129, 129, 129, 132, 132, 132, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132
db 132, 132, 9, 9, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132
db 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 5, 5, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 9, 9, 132, 132
db 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 131, 9, 9, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 128, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 9, 9, 132, 132
db 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 5, 132, 132, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 5, 132, 5, 132, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132
db 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 132, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 131, 131, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 132, 131, 131, 132, 131, 132, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 131, 132, 132, 132, 132, 132, 132, 132, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 128, 128, 151, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 5, 132, 5, 132, 5, 132, 5, 132, 132, 132, 132, 132, 132, 132, 132
db 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 131, 132, 132, 132, 132, 132, 132, 132, 131, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 131, 132, 132, 132, 132, 132, 132, 132, 131, 132, 132, 132, 132, 132, 132, 132, 131, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 53, 53, 128, 3, 3, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132
db 132, 132, 132, 132, 132, 5, 5, 5, 5, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 53, 53, 128, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 128, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 128, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 53, 53, 129, 151, 151, 129, 129, 129, 129, 129, 129, 129, 129, 128, 128, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 128, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 53, 53, 129, 151, 151, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 132, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 131, 131, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 3, 3, 128, 53, 53, 128, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 151, 151, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 131, 5, 9, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 3, 3, 128, 53, 53, 128, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 151, 151, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 3, 3, 129, 125, 125, 128, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 128, 128, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 3, 3, 129, 125, 125, 128, 3, 3, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 151, 151, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 132, 132, 132, 131, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 128, 128, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 3, 3, 129, 53, 53, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 3, 3, 129, 53, 53, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 128, 128, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 128, 128, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 9, 9, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 128, 3, 3, 129, 3, 3, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 9, 9, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 9, 9, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 9, 9, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 53, 53, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 228, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 53, 53, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 128, 128, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 151, 151, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 228, 53, 52, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 5, 5, 5, 5, 5, 5, 5, 5, 5, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 3, 3, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 151, 151, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 125, 125, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 9, 131, 131, 131, 131, 131, 131, 130, 54, 9, 54, 54, 9, 131, 131, 131, 9, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 131, 131, 131, 131, 131, 130, 54, 54, 54, 54, 9, 131, 131, 131, 131, 131, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 79, 79, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 54
db 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 130, 131, 131, 131, 131, 131, 131, 131, 131, 131, 130, 130, 130, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 130, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 130, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 130, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 130, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 130, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 130, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 130, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 130, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 130, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 130, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 130, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 130, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 130, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 130, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 130, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 131, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 131, 131, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 129, 129, 129, 129, 129, 130, 129, 129, 129, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 129, 129, 129, 129, 129, 130, 129, 129, 129, 129, 129, 130, 130, 129, 129, 129, 129, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 129, 129, 129, 129, 129, 130, 130, 129, 129, 129, 130, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 130, 130, 129, 129, 129, 130, 130, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 9, 9, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 9, 9, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9
db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9
db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 9, 54, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 9, 54, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 9, 9, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 54, 54, 54, 54, 54, 54, 54, 9, 9, 54, 54, 54, 54, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129
db 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130
db 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130
db 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 5, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130
db 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 5, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130
db 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 201, 202, 202, 224, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 5, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 224, 224, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130
db 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 201, 201, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 224, 202, 202, 224, 202, 132, 131, 130, 202, 202, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 223, 224, 201, 5, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 201, 5, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130
db 130, 130, 130, 130, 130, 130, 130, 130, 130, 202, 130, 130, 202, 130, 130, 130, 130, 130, 130, 130, 130, 131, 131, 131, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 201, 224, 224, 224, 202, 224, 224, 224, 202, 5, 130, 130, 224, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 131, 202, 5, 131, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 224, 224, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 5, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130
db 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 202, 130, 130, 130, 130, 224, 224, 5, 5, 5, 5, 130, 130, 130, 224, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 201, 131, 224, 201, 131, 224, 224, 131, 224, 224, 202, 5, 131, 224, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 202, 5, 131, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 202, 202, 224, 224, 224, 224, 131, 201, 131, 201, 131, 201, 224, 224, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 130, 130, 130, 130, 130, 130, 224, 224, 202, 224, 224, 5, 131, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130
db 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 202, 130, 130, 130, 224, 202, 224, 224, 224, 201, 223, 5, 131, 130, 201, 131, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 131, 224, 202, 131, 224, 202, 131, 224, 224, 224, 224, 201, 201, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 201, 224, 5, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 224, 224, 224, 131, 201, 130, 201, 131, 224, 224, 224, 224, 224, 224, 224, 224, 224, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 5, 131, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130
db 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 202, 130, 130, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 202, 131, 224, 202, 131, 224, 202, 131, 224, 224, 224, 201, 5, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 5, 130, 130, 130, 224, 224, 224, 224, 130, 130, 130, 130, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 131, 130, 130, 5, 5, 5, 5, 5, 5, 5, 5, 130, 130, 130, 130, 130, 130, 130, 130, 130, 201, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 202, 5, 131, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 202, 202, 130, 130, 130, 130, 130
db 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 202, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 202, 202, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 202, 131, 224, 224, 131, 224, 202, 131, 224, 201, 131, 224, 202, 5, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 5, 130, 130, 224, 224, 224, 224, 224, 130, 130, 130, 130, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 5, 5, 5, 5, 202, 202, 202, 202, 202, 202, 225, 5, 130, 130, 130, 130, 130, 224, 202, 130, 130, 130, 130, 130, 202, 130, 130, 130, 224, 224, 224, 224, 132, 202, 5, 5, 5, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 130, 130, 130, 130, 130, 224, 224, 202, 224, 224, 224, 224, 202, 5, 5, 5, 5, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130
db 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 131, 130, 130, 130, 130, 130, 130, 130, 130, 130, 201, 224, 131, 224, 224, 224, 130, 130, 224, 224, 131, 224, 201, 131, 224, 201, 131, 224, 224, 130, 202, 202, 224, 5, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 202, 224, 224, 224, 5, 130, 130, 224, 202, 224, 224, 223, 130, 130, 130, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 201, 224, 202, 202, 224, 224, 224, 224, 224, 224, 202, 5, 130, 130, 130, 130, 130, 130, 130, 130, 130, 201, 201, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 224, 202, 202, 5, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 202, 224, 224, 224, 202, 224, 201, 130, 130, 131, 224, 224, 224, 224, 224, 224, 224, 202, 224, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 202, 5, 130, 130, 130, 130, 130, 130, 130, 130, 201, 201, 130, 130, 130, 130, 130, 130, 130
db 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 130, 130, 130, 130, 130, 224, 224, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 130, 130, 130, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 131, 130, 224, 224, 224, 131, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 5, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 202, 224, 224, 5, 131, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 202, 224, 202, 201, 5, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 202, 224, 224, 130, 130, 130, 130, 130, 130, 224, 224, 224, 224, 224, 224, 224, 224, 224, 130, 130, 130, 130, 130, 224, 202, 224, 131, 130, 224, 224, 202, 224, 224, 224, 202, 5, 131, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130
db 225, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 201, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 201, 201, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 225, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 201, 224, 224, 224, 224, 224, 224, 202, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 202, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224
db 224, 224, 202, 224, 202, 202, 224, 224, 224, 202, 202, 202, 224, 224, 224, 224, 202, 202, 202, 202, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 202, 202, 224, 202, 202, 202, 224, 202, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 202, 202, 202, 202, 202, 202, 224, 202, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 202, 202, 202, 202, 202, 202, 202, 202, 202, 202, 202, 224, 224, 202, 202, 202, 202, 202, 202, 202, 202, 202, 224, 202, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 202, 202, 202, 202, 202, 202, 202, 202, 224, 202, 202, 202, 202, 202, 202, 202, 202, 202, 224, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 224, 224, 202, 224, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 202, 202, 202, 202, 202, 202, 224, 224, 202, 224, 224, 202, 224, 224, 202, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 224, 224, 202, 202, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 224, 224, 202, 202, 202, 202, 202, 202, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 202, 202, 202, 202
db 225, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 201, 131, 224, 224, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 224, 202, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224
db 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 201, 224, 224, 224, 201, 224, 224, 224, 224, 224, 202, 201, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 224, 224, 224, 224, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 131, 131, 131, 131, 131, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 224, 224, 224, 201, 201, 202, 202, 202, 201, 202, 201, 224, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224
db 224, 224, 224, 224, 224, 224, 202, 224, 202, 202, 224, 224, 202, 224, 224, 224, 224, 224, 224, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 131, 224, 224, 224, 131, 201, 202, 202, 224, 224, 201, 130, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 201, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 201, 131, 131, 131, 131, 131, 131, 131, 131, 131, 224, 202, 202, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224
db 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 201, 201, 202, 224, 224, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 131, 131, 131, 131, 224, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 202, 202, 224, 225, 202, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224
db 224, 224, 224, 224, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 131, 224, 224, 224, 224, 131, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 131, 131, 131, 131, 131, 131, 131, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224
db 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 202, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224
db 225, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224
db 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224
db 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224


