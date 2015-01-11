BITS 16

;;;;;;;;;;;;;;
; CONSTANTS
;;;;;;;;;;;;;;
stacksize       EQU 0200h ;define constant DEC 512
; starting address of video memory
 
videobase       EQU 0a000h
 
; some colors
 
black           EQU 0
green           EQU 00110000b
blue            EQU 00001001b
red             EQU 00000100b
white           EQU 00001111b
grey        	EQU 00000111b
 
; screen width in pixels, in the graphics mode 320x200
 
scrwidth    EQU 320

;;;;;;;;;;;;;;
; DATA AND STACK SEGMENTS
;;;;;;;;;;;;;;

segment videoseg data
	resb 320*200

segment memscreen data ;segment places the data here also in the compiled file
	resb 320*200 ;reserve a byte(or 64000 of them)

segment background data
	resb 320*200
		
segment mystack stack
	resb stacksize
stacktop:	
	
segment mydata data
	; DEFINE YOUR VARIABLES HERE
	foo	resw	1 ;reserve a word aka 2 bytes, 1 word
	pressesc dw 0 ;reserves a double word
	oldintseg resw 1 ;reserve word
	oldintoff resw 1 ;reserve word
	delay dw 01h ;double word for default delay 
	gmode db 0
	plocation dw 05A0h ;players starting location
	slocation dw 037A0h ;styx starting location
	move dw 0141h ;direction
	moveselector dw 0 ;help determining direction
	smove dw 01h ;movement for styx

	playerColor db white ;define players starting color(changes constantly due to rotation for blinking)
	lineColor EQU grey ;define line color for borders and trail

;;;;;;;;;;;;;;
; The code segment - YOUR CODE HERE
;;;;;;;;;;;;;;

segment mycode code

; Subroutines here
copyBG: ;copy background routine
    push ds
    pusha
    ;Pointers
    mov word si, 0
    mov word di, 0
    mov cx, 64000
 
    ;Segment registers to correct locations
    mov ax, background;memscreen
    mov es, ax
    mov ax, videobase;background
    mov ds, ax
 
    ;REPEAT COPY!
    rep movsb
    popa
    pop ds
    ret
	
copyMS: ;copy memscreen routine
    push ds
    pusha
    ;Pointers
    mov word si, 0
    mov word di, 0
    mov cx, 64000
 
    ;Segment registers to correct locations
    mov ax, videobase;memscreen
    mov es, ax
    mov ax, background;background
    mov ds, ax
 
    ;REPEAT COPY!
    rep movsb
    popa
    pop ds
    ret

drawPlayer:
	pusha
	mov    ax, videobase 
	mov    es, ax ; move videobase to es
	mov di, [plocation]  ;player starting coord

	mov dl, [playerColor] ;get players color
	rol dl, 3 ;rotate left by 3 to cause blinking(CAUTION: might cause eye cancer)
	mov [playerColor], dl ;store new color

	;player is built of 5 pixels

	mov byte [es:di], dl
	sub di, 0140h
	mov byte [es:di], dl
	add di, 013fh
	
	mov byte [es:di], dl
	add di, 02h
	mov byte [es:di], dl
	add di, 013fh
	mov byte [es:di], dl

	;draw trail
	mov    ax, background 
	mov    es, ax ;move (copy of)videobase to es
	mov    di, [plocation]  ;player coord
	mov byte [es:di], lineColor ;color of trail 
	popa	
	ret

drawStyx:
	pusha
	mov ax, videobase
	mov es, ax
	mov di, [slocation]
	mov byte [es:di], green
	add di, 01h
	mov byte [es:di], green
	add di, 01h
	mov byte [es:di], green
	sub di, 03h
	mov byte [es:di], green
	sub di, 01h
	mov byte [es:di], green
	sub di, 013Dh
	mov byte [es:di], green
	sub di, 01h
	mov byte [es:di], green
	sub di, 01h
	mov byte [es:di], green
	sub di, 013Fh
	mov byte [es:di], green
	add di, 03C0h
	mov byte [es:di], green
	sub di, 01h
	mov byte [es:di], green
	add di, 02h
	mov byte [es:di], green
	add di, 013Fh
	mov byte [es:di], green

	popa
	ret



moveStyx:
	pusha
	mov    cx, background 
	mov    es, cx ; move bg to es
	mov di, 0h  
	mov ax, [smove]
	cmp ax, 01h
	jz .sRight
	jnz .sLeft

.sRight
	mov bx, [slocation]
	add bx, 03h
	mov di, bx
	mov dl, [es:di]
	cmp dl, blue
	jz .changeLeft
	jnz .sEnd

.changeLeft
	mov bx, 0FFFFh
	mov [smove], bx 
	jmp .sEnd

.sLeft
	mov ax, [smove]
	cmp ax, 0FFFFh
	jz .sEnd
	mov bx, [slocation]
	sub bx, 03h
	mov di, bx
	mov dl, [es:di]
	cmp dl, blue
	jz .changeRight
	jnz .sEnd

.changeRight
	mov bx, 01h
	mov [smove], bx 
	jmp .sEnd



.sEnd

	mov bx, [slocation]
	add bx, 0FFFFh
	mov [slocation], bx
	
	popa
	ret

;this routine checks(and moves) which location is valid for next location of player	
movePlayer:
	pusha

	mov ax, [move]
	mov [moveselector], ax
	
	mov ax, [moveselector]
	sub ax, 0140h
	cmp ax, 01h ;cmp ax, 063Ch ;if we are at right top corner
	jz .rightDir ; if we are at right corner jump to right top 
	jnz .continue1

		.rightDir ;if going right
			call .routineRight

.continue1
	cmp ax, 0FFFFh 
	jz .leftDir
	jnz .continue2

		.leftDir ;if going left
			call .routineLeft

.continue2
	cmp ax, 0FEC0h
	jz .upDir
	jnz .continue3

		.upDir ;if going up
			call .routineUp

.continue3
	cmp ax, 0140h
	jz .downDir
	jnz .leave

		.downDir ;if going down
			call .routineDown

.leave

	popa
	ret





.routineRight:
	pusha
	mov    cx, background 
	mov    es, cx ; move bg to es
	mov di, 0h 

	mov bx, [plocation] ;store players location in bx
	add bx, 0140h ;increment by 320
	mov di, bx ;store result in di
	mov dl, [es:di] ;store stuff starting from es pointer by di
	cmp dl, lineColor ;compare it to line color
	jz .rgoDown
	jnz .isrgoUp
.rgoDown
	mov word [move], 0280h
	jmp .leaveRight
.isrgoUp
	sub bx, 0280h
	mov di, bx
	mov dx, [es:di]
	cmp dx, lineColor ;compare to line color
	jz .rgoUp
	jnz .leaveRight
.rgoUp
	mov word [move], 0h
	
.leaveRight
	mov ax, [plocation]
	add ax, [move]
	sub ax, 0140h
	mov [plocation], ax
	popa
	ret
	
	
	
.routineLeft:
	pusha
	; below we fetch the background data
	mov    cx, background 
	mov    es, cx ; move bg to es
	mov di, 0h  ;draw some shit here

	mov bx, [plocation]
	sub bx, 0140h
	mov di, bx
	mov dl, [es:di]
	cmp dl, lineColor ;compare to line color
	jz .lgoUp
	jnz .islgoDown
.lgoUp
	mov ax, 0h
	mov word [move], ax
	jmp .leaveLeft
.islgoDown
	add bx, 0280h
	mov di, bx
	mov dx, [es:di]
	cmp dx, lineColor ;compare to line color
	jz .lgoDown
	jnz .leaveLeft
.lgoDown
	mov word [move], 0280h
	
.leaveLeft
	mov ax, [plocation]
	add ax, [move]
	sub ax, 0140h
	mov [plocation], ax
	popa
	ret
	
.routineUp:
	pusha
	
	mov    cx, background 
	mov    es, cx ; move bg to es

	mov bx, [plocation]
	add bx, 01h
	mov di, bx
	mov dl, [es:di] ; es:di is only one byte so instead of dx we put it to dl
	cmp dl, lineColor ;compare to line color
	jz .ugoRight
	jnz .isugoLeft
.ugoRight
	mov word [move], 0141h
	jmp .leaveDown
.isugoLeft
	sub bx, 02h
	mov di, bx
	mov dl, [es:di]
	cmp dl, lineColor ;compare to line color
	jz .ugoLeft
	jnz .leaveUp
.ugoLeft
	mov word [move], 013Fh
	
.leaveUp
	mov ax, [plocation]
	add ax, [move]
	sub ax, 140h
	mov [plocation], ax
	popa
	ret
	
	
	
	
.routineDown:
	pusha
	
	; below we fetch the background data
	mov    cx, background 
	mov    es, cx ; move bg to es

	mov bx, [plocation]
	sub bx, 01h
	mov di, bx

	mov dl, [es:di] ; es:di is only one byte so instead of dx we put it to dl
	cmp dl, lineColor ;compare to line color
	jz .dgoLeft
	jnz .isdgoRight
.dgoLeft
	mov word [move], 013Fh
	jmp .leaveDown
.isdgoRight
	add bx, 02h
	mov di, bx
	mov dl, [es:di]
	cmp dl, lineColor ;compare to line color
	jz .dgoRight
	;mov word [move], 013Fh
	jnz .leaveDown
.dgoRight
	mov word [move], 0141h
	
.leaveDown
	mov ax, [plocation]
	add ax, [move]
	sub ax, 0140h
	mov [plocation], ax
	popa
	ret






KeybInt:
        push    ds              ; put the value of ds,ax to safety
        push    ax              
 
        mov     ax,mydata       ; Re-initialisation of 
        mov     ds,ax           ; the data segment
 
        cli                     ; Disable other interrupts
                            ; during this one
 
 
 
 
.getstatus:
        in      al, 64h
        test    al, 02h
        loopnz  .getstatus      ; wait until the port is ready
 
        in      al,60h          ; Get the scan code of 
                            ; the pressed/released key
 
 
                ; scan codes can be found in helppc
                ; interrupt services -> 
                ; keyboard interrupt -> make codes
 
                ; here begins the actual key scanning
 
        cmp     al, 01h     ; 1 is the 'make code' for ESC
        jne     .leftArrow      ; if ESC was not pressed, continue
        mov     word [pressesc], 1
    jmp .kbread
	
.leftArrow:
	cmp al, 0004Bh
	jne .rightArrow
	mov word ax, [move]
	
	cmp word ax, 0140h ; check if movement is zero
	jz .moveLeft ; if zero, move to left
	mov ax, 013Fh
	mov [move], ax
	jmp .kbread
	
.moveLeft
	mov word ax, [plocation]
	sub word ax, 01h
	mov word [plocation], ax
	
	mov ax, 013Fh
	mov [move], ax
	
	jmp .kbread

.rightArrow:
	cmp al, 0004Dh
	jne .upArrow
	mov ax, [move]
	
	cmp ax, 0140h ; check if movement is zero
	jz .moveRight ; if zero, move to left
	mov ax, 0141h
	mov [move], ax
	jmp .kbread
	
.moveRight
	mov ax, [plocation]
	add ax, 01h
	mov [plocation], ax
	
	mov ax, 0141h
	mov [move], ax
	
	jmp .kbread
	
.upArrow:
	cmp al, 048h
	jne .downArrow
	mov ax, [move]
	
	cmp ax, 0140h ; check if movement is zero
	jz .moveUp ; if zero, move to left
	mov ax, 0h
	mov [move], ax
	jmp .kbread	
	
.moveUp
	mov ax, [plocation]
	sub ax, 0140h
	mov [plocation], ax
	
	mov ax, 0h
	mov [move], ax
	
	jmp .kbread
	
.downArrow:
	cmp al, 050h
	jne .stopMovement
	mov ax, [move]
	
	cmp ax, 0140h ; check if movement is zero
	jz .moveDown ; if zero, move to left
	mov ax, 0280h
	mov [move], ax
	jmp .kbread	
	
.moveDown
	mov ax, [plocation]
	add ax, 0140h
	mov [plocation], ax
	
	mov ax, 0280h
	mov [move], ax
	
	jmp .kbread	
	
	
.stopMovement:
	cmp al, 02Ah
	jne .cplus
	mov ax, [move]
	
	cmp ax, 0140h ; check if movement is zero
	jz .startMovement ; if zero, move to left
	mov ax, 0140h
	mov [move], ax
	jmp .kbread	
	
.startMovement
	mov ax, 0141h
	mov [move], ax
	jmp .kbread
	
	
	
.cplus:
    cmp al, 04Eh    ; 4E is the 'make code' for keypad plus
    jne .cminus
    mov ax, [delay]
	inc ax
	mov [delay], ax
    jmp .kbread
.cminus:
    cmp     al, 04Ah    ; 4A is the 'make code' for keypad minus
    jne .kbread
    mov ax, [delay]
	dec ax
	mov [delay], ax
 
 
 
 
.kbread:
        in      al,61h          ; Send acknowledgment without
        or      al,10000000b    ; modifying the other bits.
        out     61h,al          ;                            
        and     al,01111111b    ;                            
        out     61h,al          ;                            
        mov     al,20h          ; Send End-of-Interrupt signal 
        out     20h,al          ;                              
 
 
        sti                     ; Enable interrupts again
 
        pop     ax     
		pop ds      ; Regain the ds,ax from stack
 
 
        iret                    ; Return from interrupt





	
..start:
		
	mov     ax, mydata
	mov     ds, ax
        mov     ax, mystack
        mov     ss, ax
        mov     sp, stacktop
	
	mov ah, 35h     ;Note use of HEXADECIMAL!
	mov al, 9       ;No need for h, because 9h = 9
					;Alternatively, both at once: MOV AX, 3509h
	int 21h         ;Call interrupt 21,35 - note again the use of HEXADECIMAL values
	
	
	mov [oldintseg], es
	mov [oldintoff], bx
	
	;after this line we will create our own interrupt handler :D:DDD:D
	mov dx, KeybInt
	mov ax, mycode
	push ds ;to later get the data segment address back to DS we temp store it to stack
	mov ds, ax
	mov ah, 25h
	mov al, 9 
	int 21h
	pop ds ; here we get it back
	
	call .graphmode
	
	call .initbackground

	call .mainloop
	call .dosexit

;a routine for initializing the background	
.initbackground:
	mov    ax, videobase 
	mov    es, ax ; move videobase to es
	mov di, 0504h 

.loop:	;draw upper line
	mov byte [es:di], lineColor
	inc di
	cmp di, 063Ch
	jnz .loop
	mov di, 0504h
	mov cx, 0beh
.loop2: ;draw columns
	mov byte [es:di], lineColor
	add di, 0138h
	mov byte [es:di], lineColor
	add di, 08h
	dec cx
	jnz .loop2
.loop3: ;draw bottom line
	mov byte [es:di], lineColor
	inc di
	cmp di, 0f3bdh
	jnz .loop3
	call copyBG ;copy the resulted background to memory
	ret
	


.graphmode:
	mov ah, 0Fh
	int 10h
	mov [gmode], al ;save the old mode
	mov ah, 0 
	mov al, 13h ;set new mode
	int 10h
	ret



.mainloop:

	mov     word dx, [delay]
.pause1:
	mov     cx, 65535
.pause2:
	dec     cx
	jne     .pause2
	dec     dx
	jne     .pause1
	call .draw
	call movePlayer
	call moveStyx
	;below we check for esc	
	mov ax, [pressesc]
	cmp ax, 0 ; 0-0 -> ZF = 0 | 1-0 -> ZF = 1
	jz .mainloop
	ret

.draw
	call copyMS ;copy background to screen buffer
	call drawPlayer ;draw player in next location
	call drawStyx ;draw styx
	ret 	

	
.dosexit:
	
	mov ah, 0
	mov al, [gmode]
	int 10h
	
	mov word dx, [oldintoff]        ; Move VALUE at address oldintoff to DX (not the address!)
	mov ax, [oldintseg]				; Move VALUE at address oldintseg to DS, step 1 (remember you cannot move directly to segment registers)
	mov ds, ax						; Move VALUE at address oldintseg to DS, step 2
	mov ah, 25h						; Move parameters to AH and AL (or both together by moving to AX). Interrupt 
	mov al, 9						; number is still 9 and AH should be 25h (note hexadecimal)
	INT 21H                         ; Finally, set the interrupt vector by calling int 21H
	
	mov	al, 0 ; 0 to AL
	mov     ah, 4ch ; 76 to AH
	int     21h ;interrupt 21h = close program exit(0)

.end


