;TODO: Font?
;*********************************************************
; ZX Spectrum Minesweeper game
; GDS October 2023
; This is an implementaton of the classic minefield game
; mainly written to test out my interrupt mouse pointer
; software.
; Background screen should be loaded to $MINESCREEN ($a000)
;
; Minefield sizes. These can be tweaked in DifficultyArray
; +-----+-------+----------+
; |easy | 9x9   | 10 mines |
; |med  | 16x16 | 40 mines |
; |diff | 19x28 | 60 mines |
; +-----+-------+----------+
;*********************************************************
; Mine storage begins as $C020
; It is a grid, but the grid starts at 0,0 and goes 
;    to MAXX+1,MAXY+1 for easier processing. 
;
; the function deToMine gets the address of the mine
;    for a given cooordinate.
;Usage of Mine bits:
; 76543210
; MSL0rrrr     
; where 
;    M = Actual mine present
;    S = Mine selected
;    L = position looked at
;    rrrr = mines bordering-1 where 
;         0=unselected, 1=0, 2=1 ect
;*********************************************************
; Ram usage:
; a000 -> baff - background graphic
; bb00 -> bfff - reserved for mouse
;
;
;$c000
;  variables
;$c005
;    unused
;$C020
;   Mine storage
;$c2AB  (Max)
;   Unused
;$d000
;   scratchpad for attribute area when doing Boom animation
;$d300
;*********************************************************


;*********************************************************
;Constants
;*********************************************************
CHARSET:        equ $3c00 ;Location of the character set bitmap - $100
MINESCREEN:     equ $A000 ;Location of the background bitmap 
BORDCR:         equ $5c48 ;Border colour system variable.

;Variables
MINESTORE:      equ $c020 ;Location of the mine status store
MAX_X:          equ $c000 ;X size of board 
MAX_Y:          equ $c001 ;Y size of board 
NUMMINES:       equ $c002 ;Number of mines remaining (Displayed)
ACTUALNUMMINES: equ $c003 ;Number of mines remaining (Actual)
RANDSEED:       equ $c004 ;Random" number seed.
ISFIRST:        equ $C005 ;flag for the first mine.

;Exports from mouse driver
mouse_on:       equ $bc10 ;Turn mouse Cursor on
mouse_off:      equ $bc13 ;Turn mouse Cursor off
mouse_buttons:  equ $bc16 ;Return Mouse button state in A
mouse_x:        equ $bc24 ;X position of mouse (pixels)
mouse_y:        equ $bc23 ;Y position of the mouse (Pixels)
mouse_input:    equ $bc25 ;Mouse input method

;*********************************************************
;Program entry point.
;*********************************************************
org $9600
    call fixupMinescreen    ;hack the background screen so the cursor shows up

    call CLEARSCREEN
    call AskInputMethod     ;Ask and set input method

Go_again:
    call CLEARSCREEN
    CALL AskDifficultyLevel ;Ask for difficulty.

    LD a,(NUMMINES)         ;Copy the number of mines to actualnummines
    LD (ACTUALNUMMINES),A   

    call CLEARSCREEN
;Blank minefield
    ld hl,MINESTORE
    ld de,MINESTORE+1
    ld (hl),0
    ld bc,560           ;max size = 26,18 = 28x20 incl borders = 560
    ldir

;Create minefield
    ld hl,2341
    ld (RANDSEED),hl
    call create_minefield
    call DisplayBlankBoard

    ld a,1
    ld (ISFIRST),a

    ld hl,2341
    ld (RANDSEED),hl
    call display_message
    defb 01111000b,$17,$00,"Mines Remaining:  ",$FF

;Preset the mouse pointer to somewere visible.
    ld a,$18
    ld (mouse_x),a
    ld (mouse_y),a

main_loop:
;wait for mouse to no longer be pressed.
    call mouse_buttons 
    cp 0
    jr nz, main_loop

    call UpdateMinesRemaining
    call mouse_on
;wait for input
input_loop:
    call mouse_buttons 
    cp 0
    jr z, input_loop
;mouse off
    push af
    call makeClick
    call mouse_off

;divide X by 8 to get character and subtract 1 so we have position in grid
    ld a,(mouse_x)
    sub a,8         ;Subtract one column
    sra a           ;divide by 8
    sra a
    sra a
    and $1f         ;Mask out any rubbish
    cp 0            ;Are we at 0? (border)
    jr z,main_loop  ;if do, invalid location ignore click.
    ld e,a              
    ld a,(MAX_X)    ;check against Right border
    cp e
    jr c,main_loop  ;if we are there or greater, ignore click.

;divide Y by 8 to get character and subtract 1 so we have position in grid
    ld a,(mouse_y)
    sub a,8
    sra a
    sra a
    sra a
    and $1f
    cp 0
    jr z,main_loop
    ld d,a
    ld a,(MAX_Y)
    cp d
    jr c,main_loop
    pop af

;Right mouse button?
    cp 1
    jp z,RIGHTMB

;Left mouse button
    call deToMine
    ld a,(hl)
    bit 7,a             ; Mine?
    jr nz, DOBOOM       ; BOOM!
notbooming:
    ld hl,ISFIRST       ; No longer the first mine...
    LD (hl),0
    call ProcessSquareAtDe
    cp 0
    call z,CheckSurroundingSquares
    jp main_loop

PROGRAM_FINISH:
    CALL display_message
    defb 01111000b,$17,$00,"           Press Fire           ",$ff

    call mouse_buttons 
    cp 0
    jr nz, PROGRAM_FINISH
unpress:
    call mouse_buttons 
    cp 0
    jr z, unpress

    jp Go_again


;**********************************************************
; User has stepped on a mine. Oops!
;**********************************************************
DOBOOM:
    ld hl,ISFIRST               ;Check if this is the first click
    bit 0,(hl)                  
    jr nz,SPECIALCASEDONTBOOM   ;If so, special case it.
    call makeboom               ;if not, BOOM!!!!!
    call displaymines           ;show all mine locations
    jr PROGRAM_FINISH

;**********************************************************
;This is a special case for if the first click is a mine.
;to make it a bit fairer, if this happens, the mine will
;just get deleted.
;**********************************************************
SPECIALCASEDONTBOOM:
    call deToMine               ;Get the mine location
    res 7,(hl)                  ;Erase the mine
    ld hl,NUMMINES              ;Decrement the number of mines.
    dec (hl)
    ld hl,ACTUALNUMMINES        ;And the actual number of mines
    dec (hl)
    push de                     ;Preserve X/Y
    call UpdateMinesRemaining   ;Update text on screen for no. of mines
    pop de                      ;restore X/Y
    jr notbooming               ;rejoin the flow for a non-mine

;**********************************************************
;check the surrounding blocks for mines.
; on entry, DE=square to check.
; if C, coordinate invalid.
;**********************************************************
CheckSurroundingSquares:
    xor a           ; Check for X coordiate of 0
    cp d            ; Equal?
    ccf
    ret z           ; If so, return with carry set

    cp e            ; Check for Y coordinate of 0
    ccf 
    ret z           ; If so, return with carry set
    
    ld a,(MAX_X)    ; Check for X > MAX
    cp e
    ret c           ; If so, return with carry set

    ld a, (MAX_Y)   ; Check for Y > Max
    cp d
    ret c           ; If so, return with carry set

;Will check a 3x3 block. To start with, call X-1,Y-1
;           X X X
;           X N X
;           X X X
    dec d           ; point to top left off box
    dec e
    ld bc,$0303     ; Counters, 3x3

css_loop2:
    push bc
    push de
css_loop:
    push bc

    push de
    call ProcessSquareAtDe   ;Get totals at DE
    pop de
    jr c,css_endOfprocessing ; If invalid coordinates / Already processed, ignore the rest.
    cp 0                     ; no mines surrounding?
    push de    
    call z,CheckSurroundingSquares  ;If not, process a 3x3 around that.

    pop de                  ; Get back XY
css_endOfprocessing:
    pop bc                  ; Get back counters
    inc e                   ; Next Col
    dec c                   ; X counter
    jr nz,css_loop          ; go again if we have not done 3
    pop de                  ; get back XY
    pop bc                  ; and ge back main counter
    inc d                   ; next row
    dec b                   ; Y counter
    jr nz,css_loop2         ; next line
    ret

;**********************************************************
;Display the background
;**********************************************************
CLEARSCREEN:
    ld hl,MINESCREEN
    ld de,$4000
    ld bc,$1b00
    ldir
    ret

;**********************************************************
;Process one square
; Returns value in A
; HL,F corrupt
;**********************************************************
ProcessSquareAtDe:              
    call deToMine             ; convert DE to an address in the grid storage
    ld a,(hl)                 ; fetch the data
    bit 5,a                   ; Have we already looked at this square?
    jr z,psadskip2            ; No, so go ahead and process
    xor $0f                   ; Return the actual value found
    scf                       ; Return Carry = true
    ret 
psadskip2:
    push de
    call CalcSurroundingMines ; get the number
    pop de
    jr nc, psadSkip           ; If Carry is set, invalid location. If not, skip
    ld a,0                    ; Return with Carry = true
    ret
psadSkip:
    push af                   ; Store calculated value.
    inc a                     ; If its 0, it needs to be marked as 1.
    push af                   ; Store A for later
    call deToMine             ; Get DE
    pop af                    ; Get back A
    or 00100000b              ; Set bit 5 "We have processed this"
    ld (hl),a                 ; and set it back.
    pop af                    ; restore our calculation
    push af                   
    push de
    call displayNumberOrSpace   ;update grid on screen.
    pop de
    pop af
    scf
    ccf
    ret

;**********************************************************
; Calculate the number of surrounding mines given DE.
; DE = YX, returns a = number of mines, Carry=true if invalid location
; AF corrupt.
;**********************************************************
; Check we are not in the border. If so, return C=true
CalcSurroundingMines:
    xor a       ;Check for D=0
    cp d
    scf         ;return Carry=true if so
    ret z
    cp e        ;Check for E=0
    scf         ;return carry=true if so
    ret z

    ld a,(MAX_X)    ;Check E > MAX
    inc a
    cp e
    scf             ;return Carry=true if so
    ret z           
    ld a,(MAX_Y)    ;Check D > Max
    inc a
    cp d
    scf             ;return Carry=true if so
    ret z   
    
    push hl     ;Preserve all registers for easier coding later
    push de
    push bc

    ld a,0      ; Number of mines counter
    dec d       ; point to X-1,Y-1
    dec e
    ld b,3      ; 3 lines

calcSMLoop:
    push af
    push bc
    call deToMine   ;Get the location address
    pop bc
    pop af
    ld c,(hl)       ;Get the status of this location
    bit 7,c         ;Mine here?
    jr z,calcSMLoopskip1    ;if not, skip
    inc a           ;mine +1
calcSMLoopskip1:
    inc hl          ;Next mine along
    ld c,(hl)       ;check again
    bit 7,c         
    jr z,calcSMLoopskip2
    inc a
calcSMLoopskip2:
    inc hl          ;Next mine along
    ld c,(hl)
    bit 7,c
    jr z,calcSMLoopskip3
    inc a
calcSMLoopskip3:
    inc d           ;Next line.
    djnz calcSMLoop 
    pop bc
    pop de
    pop hl
    scf             ;Make sure carry=0
    ccf             
    ret

;**********************************************************
;Handle the right mouse button press. 
;**********************************************************
RIGHTMB:
;mark location as a mine
    call deToMine       ;get the address
    ld a,(hl)           ;Get the contents.
    and $3f
    jp nz,main_loop     ;if we have already revealed the contents of the square, skip.

    ld a,(hl)           ;Get the contents.
    xor $40             ;flip
    ld (hl),a

    ld b,a
    bit 6,b
    jr z,nomine
    ld a,(NUMMINES)
    dec a
    ld (NUMMINES),A
    bit 7,b
    jr z,notrealmine
    ld a,(ACTUALNUMMINES)
    dec a
    ld (ACTUALNUMMINES),A
notrealmine:
    ld a,12
    jr rignt_minesel
nomine:
    ld a,(NUMMINES)
    inc a
    ld (NUMMINES),A
    bit 7,b
    jr z,notrealmine2
    ld a,(ACTUALNUMMINES)
    inc a
    ld (ACTUALNUMMINES),A
notrealmine2:
    ld a,11         
rignt_minesel:
    call DisplayBlock
    ld a,(ACTUALNUMMINES)
    cp 0
    jr z,DOUSERWON
    jp main_loop
    
;**********************************************************
;**********************************************************
DOUSERWON:   
    call display_message
    defb 11001110b,$0A,$00,"YOU HAVE FOUND ALL THE MINES!",$FF
    call displaymines
    jp PROGRAM_FINISH


;**********************************************************
; Update the "Mines remaining" number.
;**********************************************************
UpdateMinesRemaining:
    ld a,(NUMMINES)
    ld de,$1710
    CALL DisplayNumberAt
    ret

;**********************************************************
; Display the 2 digit number in A at DE
;**********************************************************
DisplayNumberAt:
    dec d
    dec e
; divide number into its upper and decimal lower digits
    ld c,$ff            ; "-1"
div10loop:
    inc c               ; Add one to MSB count.
    sub 10              ; subtract 10
    jr nc,div10loop     ; If not more 10's left, go back for more
    add a,10            ; Add the last 10 subtracted so we have the remainder
    ld b,a              ; b contains the remainder (LSB), C contains the upper digit (MSB)

;Now to display the two digits in C,B order.
    push bc             ; store BC
    ld a,c              ; Deal with MSB first (C)
    push DE
    call displayNumberOrSpace
    pop de
    pop bc              ; get back B
    ld a,b              ; 
    inc e
    call DisplayBlock
    ret    
;**********************************************************
;Display a message on the screen. (
; Message after call, terminated with $FF
;**********************************************************
display_message:
    pop hl              ; Get address of next byte after the CALL
    ld b,(hl)           ; attribute byte
    inc hl
    ld d,(hl)
    inc hl
    ld e,(hl)
    inc hl
domsgnext1:
    ld a,(hl)           ; get byte
    inc hl              ; (Point to next byte)
    cp $FF              ; No more string?
    jr z,msg2_finish     ; If not, we are done.
    push hl             ; Store character pointer.
    push de
    push bc
    call DisplayASCII
    pop bc
    pop de
    inc e
    pop hl              ; Get back character pointer
    jr domsgnext1        ; Next character
msg2_finish:
    jp (hl)             ; Jump back after the $FF.
;**********************************************************
;Create a random minefield given the X and Y parameters.
;**********************************************************
create_minefield:
    ld a,(NUMMINES)     ;Number of mines.

createminefieldloop:
    push af
createminefieldloopA:
    ld a,(MAX_X)        ;Get value for X
    call RandomMaxa
    push af
    ld a,(MAX_Y)        ;Get value for Y
    call RandomMaxa
    ld d,a
    pop af
    ld e,a              ;DE is our random location.

;See if we have a mine there
    call deToMine       ;get the address
    ld a,(hl)           ;check for a mine already there
    bit 7,a             
    jr nz,createminefieldloopA  ;if we do, try again.
    ld (hl),$80         ;set a mine there
    pop af              ;next mine.
    dec a
    jr nz,createminefieldloop
    ret


;**********************************************************
; Convert the Mine address of X,Y address in DE to an address in HL.
; d=y e=x  returns hl=address
; af, bc corrupt
;**********************************************************
deToMine:
    push de
    ld hl,MINESTORE
    ld b,0
    xor a
    cp d
    jr z,detomine_skipadd      ;if we are on line 0, skip adding lines

;get the line length+2 -> bc
    ld a,(MAX_X)    ;Last mine
    inc a           ;add in our "Border" (0,10)
    inc a
    ld c,a

detomine_nextline:
    add hl,bc
    dec d
    jr nz,detomine_nextline

detomine_skipadd:
    ld c,e      ;Addin the X 
    add hl,bc
    pop de
    ret
    
;**********************************************************
;Get random number from 1-a (max 31)
;**********************************************************
RandomMaxa:             ;
    inc a               ; a=a+1. This is for carry later
    ld c,a              ; Store for later.
Randomtryagain:
    call random         ; Get random number between 0 and 255
    and $1f             ; mask out the first 5 bits (0-31)
    inc a               ; Add 1 so number is from 1-32
    cp c                ; Over max?
    ret c               ; If not, return value.
    jr Randomtryagain   ; try again.

;**********************************************************
;-----> Generate a random number - Ion Random
; from https://wikiti.brandonw.net/index.php?title=Z80_Routines:Math:Random
; output a=answer 0<=a<=255
; Note, this is not really random, chances are its not even balanced.
;**********************************************************
random:
        ld      hl,(RANDSEED)   ; Get the seed
        ld      a,r             ; Get semi-random number to D
        ld      d,a             
        ld      e,(hl)          ; Get E from the seed
        add     hl,de           ; Add number together
        add     a,l             ; Add in bottom of the seed
        xor     h               ; and mess with the top of the seed
        ld      (RANDSEED),hl   ; store the new seed
        ret
    
;**********************************************************
; Display all the mines.
; This actually displays them backwards from bottom to top 
; and right to left for efficency sake.
;**********************************************************
displaymines:
    ld a,(MAX_Y)         ;For MAXY lines...
    ld d,a
dispmmfNextY:
    ld a,(MAX_X)         ;For MAXX character cells
    ld e,a
dispmmfLoop:
    push de              ;Store DE for looping
    call deToMine        ;Get the grid at D,E
    ld a,(hl)            ;fetch status    
    bit 7,a              ;Mine present?
    jr z,dontdisplayMine ;Nope, skip out
    ld a,12              ;display a mine
    call DisplayBlock
    jr dispmmDone
dontdisplayMine:
    bit 6,a              ;Has the player marked it despite no mine present?
    jr z,dispmmDone      ;Nope, ignore it.
    ld a,13              ;display a cross
    call DisplayBlock
dispmmDone:
    pop de               ;Get back DE for looping
    dec e                ;next character..
    jr nz,dispmmfLoop    ;If not zero, go back again
    dec d                ;go to the previous line..
    jr nz,dispmmfNextY    ;and if not zero, next..
    ret


;**********************************************************
; Display a blank board. 
; This displays a blank board from X,Y -> 1,1
;**********************************************************
DisplayBlankBoard:          
    ld a,(MAX_X)        ; Max X->E
    ld e,a              ;
    ld a,(MAX_Y)        ; Max Y->d
    ld d,a              ;
dispmfLoop:
    push de             ; Store our DE
    ld a,11             ; Box character
    call DisplayBlock   ; Display at D,E
    pop de              ; Get back DE
    dec e               ; DEC column
    jr nz,dispmfLoop    ; next column 
    ld a,(MAX_X)        ; Reset Column to Max
    ld e,a              
    dec d               ; Decrement line
    jr nz,dispmfLoop    ; If more, go back for more

;Display border
    ld a,(MAX_X)        ; Max X->E
    ld e,a              ;
    ld a,(MAX_Y)        ; Max Y->d
    ld d,a              ;
    push de             ; Store copies for later
    push de
    inc d               ; Point 1 point past the grid size
topbtnloop:
    push de             ;Store DE for later
    push de
    ld a,14             ; "="
    call DisplayBlock   ; Display at line E
    pop de              ; Get back
    ld d,0              ; Point to line 0
    ld a,14             ; "="
    call DisplayBlock   ; Display at line 0
    pop de              ; Get back the XY
    dec e               ; Point to previous character in the line
    xor a               ; Have we reached 0?
    cp e
    jr nz,topbtnloop    ; If not, go back for another

    pop de              ;Get back one of our MaxMaxY pairs
    inc e               ; Point to one line past the gridd
leftrightloop:
    push de             ;Store DE for later
    push de
    ld a,15             ; "||"
    call DisplayBlock   ; Display at Column D
    pop de              ; get back the XY
    ld e,0              ; Point to colum 0
    ld a,15             ; "||"
    call DisplayBlock   ; Display at column 0
    pop de              ; Get back the XY
    dec d               ; Point to previous line
    xor a               ; Have we reached 0?
    cp d
    jr nz,leftrightloop ; If not, go back for another

;Four corner blocks.
    ld de,$0000         ;0,0
    ld a,16             ;top left
    call DisplayBlock   

    pop de              ;get the MaxX, MaxY
    push de             ;store it for later
    ld d,0              ;Top right
    inc e
    ld a,17         
    call DisplayBlock
    pop de
    push de
    ld e,0              ;Bottom left
    inc d
    ld a,18
    call DisplayBlock
    pop de              ;Bottom right
    inc d
    inc e
    ld a,19
    call DisplayBlock
    ret

;**********************************************************
; Display an ascii character A at DE B = attribute
;**********************************************************
DisplayASCII:
    push bc
    push de   
    ld l,a
    ld h,0
    add hl,hl
    add hl,hl
    add hl,hl
    ld bc, CHARSET
    add hl,bc
    JR DoneDisplayLocationCalc
;**********************************************************
; Display blank square for 0 or a number for 1-9
;**********************************************************
displayNumberOrSpace:
    cp 0                ; 0?
    jr nz,DisplayBlock  ; if not just display character
    ld a,10             ; Set to character 10 (blank space)
;**********************************************************
; Display Icon A at e(x),d(y)  - 0-9 = numbers 0-9, 10=space, 11=box, 12=mine
;**********************************************************
DisplayBlock:
    ld b, 01111000b ; = 01 111 000 = flash 0 bright 1, paper 7 ink 0
    push bc
    inc d
    inc e
    push de
;BC = A * 8
    rla                      ; A = A * 8
    rla 
    rla
    and $F8                  ; Mask out any rubbish shifted in.
    ld c,a                   ; Move to bc.
    ld b,0

    cp 80                    ; > 80 (chr 9)
    jr nc,dontusespeccyCS    ; if not, use our own character set    
    ld hl,CHARSET+$180       ; Address of "0" bitmap
    add hl,bc                ; Add in the value for A*8
    jr DoneDisplayLocationCalc
dontusespeccyCS:
    ld hl,MINE_CHARACTERS-80 ; Address of our special characters
    add hl,bc           
DoneDisplayLocationCalc:     
    push hl
    call LineToAddress       ; Convert DE to a line address
    pop de
;Write the character
    ld b,8                   ; 8 lines                 
displayBlockLoop:   
    ld a,(de)                ; source
    ld (hl),a                ; target
    inc de                   ; next source address
    inc h                    ; next target line
    djnz displayBlockLoop    ; next character

;Set the attribute to Black on white.
    pop de                 ;Get back X,Y
;Calculate address
    ld h,d                 ;HL = d*32 (Set H=D, L=0 (d*256) / 8)
    ld l,0
    RR h
    RR l
    RR h
    RR l
    RR h
    RR l

    ld a,l                  ;Merge in the X coordinate
    or e
    ld l,a

    ld a,h                  ;Mask out any rubbish shifted in
    and 00011111b
    ld h,a                  
    ld bc,$5800             ;Add in Attribute base
    add hl,bc

;Set attribute
    pop bc
    LD (hl),b
    
    ret

;**********************************************************
;* Calculates the address of a character line
;* 
;* D=Character line no e=character
;* return HL=address
;* AF, HL corrupt
;*
;* Address: 010A A000 BBBY YYYY
;* Where X=000AABBB 
;**********************************************************
LineToAddress:
    ld a,d      ;Mask out "AA"
    and $18
    or $40      ;Add in $040
    ld h,a      ;This put 010AA000 into H

    ld a,d      ; Move "BBB" to the last 3 bits (shift by 5)
    add a,a
    add a,a
    add a,a
    add a,a
    add a,a
    and $e0     ; Mask out any rubbish
    or e        ; Mask in the Y value.
    ld l,a      ; and L is the LSB.
    ret

;**********************************************************
;Graphics characters
;**********************************************************
CHAR_UNSELECTED: equ 0

MINE_CHARACTERS:
    defb 00000000b  ;10
    defb 00000000b
    defb 00000000b
    defb 00000000b
    defb 00000000b
    defb 00000000b
    defb 00000000b
    defb 00000000b

    defb 11111111b  ;11
    defb 10000001b
    defb 10000001b
    defb 10000001b
    defb 10000001b
    defb 10000001b
    defb 10000001b
    defb 11111111b

    defb 00001000b  ;12
    defb 01011010b
    defb 00111100b
    defb 11111110b
    defb 01111111b
    defb 00111100b
    defb 01011010b
    defb 00010000b

    defb 00000000b  ;13
    defb 01000010b
    defb 00100100b
    defb 00011000b
    defb 00011000b
    defb 00100100b
    defb 01000010b
    defb 00000000b

    defb 00000000b  ;14
    defb 11111111b
    defb 11111111b
    defb 00000000b
    defb 00000000b
    defb 11111111b
    defb 11111111b
    defb 00000000b

    defb 01100110b  ;15
    defb 01100110b
    defb 01100110b
    defb 01100110b
    defb 01100110b
    defb 01100110b
    defb 01100110b
    defb 01100110b

    defb 00000000b  ;16
    defb 01111111b
    defb 01111111b
    defb 01100000b
    defb 01100000b
    defb 01100111b
    defb 01100111b
    defb 01100110b

    defb 00000000b  ;17
    defb 11111110b
    defb 11111110b
    defb 00000110b
    defb 00000110b
    defb 11100110b
    defb 11100110b
    defb 01100110b

    defb 01100110b  ;18
    defb 01100111b
    defb 01100111b
    defb 01100000b
    defb 01100000b
    defb 01111111b
    defb 01111111b
    defb 00000000b

    defb 01100110b  ;19
    defb 11100110b
    defb 11100110b
    defb 00000110b
    defb 00000110b
    defb 11111110b
    defb 11111110b
    defb 00000000b

;*************************************************************
;*************************************************************
VARS:
    defb 0,0,0,0
DRAWBOX:
    ld IX,VARS
    ld (IX+0),d
    ld (IX+1),e
    ld (IX+2),b
    ld (IX+3),c

    ld a,16             ;top left
    call DisplayBlock   

    ld d,(ix+0)         ;top right
    ld e,(ix+3)
    ld a,17             
    call DisplayBlock

    ld d,(ix+2)         ;Bottom left
    ld e,(ix+1)
    ld a,18
    call DisplayBlock

    ld d,(ix+2)         ;Bottom right
    ld e,(ix+3)
    ld a,19
    call DisplayBlock

    ld d,(Ix+0)
    ld e,(ix+1)
    inc e
drawbox_loop:
;top and bottom lines
    push de             ;Top bar
    ld a,14             ;Vertical char
    call DisplayBlock
    pop de
    push de
    ld d,(ix+2)         ;Bottom bar
    ld a,14
    call DisplayBlock
    pop de
    inc e
    ld a,e
    cp (ix+3)           ;At the rightmost item?
    jr nz,drawbox_loop

;Left and right lines
    ld d,(Ix+0)
    ld e,(ix+1)
    inc d               ;One line down
drawbox_vloop:
    push de
    push de             
    ld a,15             ;Left line
    call DisplayBlock
    pop de
    inc e

drawbox_hloop:      ;Next the spaces
    ld a,10         ;space
    push de
    call DisplayBlock   ;display space
    pop de
    inc e
    ld a,e
    cp (ix+3)
    jr nz,  drawbox_hloop  ;until we reach the end

    ld a,15                ;last character is a vertical line
    call DisplayBlock

    pop de
    inc d
    ld a,d
    cp (ix+2)
    jr nz,drawbox_vloop    ;Next line

    ret
;*************************************************************
;Get the mouse X/Y in character squares.
;Returns DE = YX
;*************************************************************
getMouseXY:
    ld a,(mouse_x)
    sra a           ;a=a/8
    sra a
    sra a
    and $1f         ;Mask out the top bits as they may contain junk
    ld e,a

    ld a,(mouse_y)
    sra a
    sra a
    sra a
    and $1f         ;Mask out the top bits as they may contain junk
    ld d,a
    ret

;*************************************************************
; Convert DE (Y/X) character square to an attribute address
; DE = Y/X
; returns HL 
; This is just (d*32) + e + $5800
; No registers corrupted
;*************************************************************
DEtoAttributeAddress:
    ld l,d              ;d -> hl
    ld h,0
    add hl,hl           ;hl = hl * 32
    add hl,hl    
    add hl,hl    
    add hl,hl    
    add hl,hl    
    push bc
    ld c,e              ;e -> bc 
    ld b,0
    add hl,bc           ;hl = hl + bc
    ld bc,$5800         ;hl = hl + $5800
    add hl,bc
    pop bc
    ret
;*************************************************************
;Check the cooordinates DE are in the given chararacter box.
;DE = Y,X   IX = Box
; Returns Carry = true if in box.
; IX+0: Y left
; IX+1: Y right
; IX+2: X left 
; IX+3: X Right
;AF corrupt
;*************************************************************
CheckDEInBox:
    ld a, (ix+0)
    cp d
    jr z,cdeib_skipx
    ret nc
cdeib_skipx:
    ld a,(ix+2)
    cp d
    ccf 
    ret nc
    ld a, (ix+1)
    cp e
    jr z,cdeib_skipy
    ret nc
cdeib_skipy:
    ld a,(ix+3)
    cp e
    ccf 
    ret
;*************************************************************
; Highlight the box details referenced to by IX with the 
; given attribute.
; IX = box; A = attribute
; IX+0: Y left
; IX+1: Y right
; IX+2: X left 
; IX+3: X Right
; AF,BC,DE,HL corrupt
;*************************************************************
highlightBox:
    ld d,(ix+0)
    ld e,(ix+1)
    push af          ; store attribute for later
;Want to calculate the number of squares to modify.
    ld a,(ix+3)      
    sub e
    inc a
    ld b,a          ; B = counter

    pop af          ; get back attributes
hb_rowloop:
    push de         ; preserve XY
    call DEtoAttributeAddress       ;Convert to Address
    ld c,b          ; Store counter so we dont have to re-calculate it later.

hb_colloop:
    ld (hl),a       ; Set attribute
    inc hl          ; next one along
    djnz hb_colloop ; back

    pop de          ; Get X/Y back.
    ld b,a          ; preserve Attribute
    ld a,d          ; Next line down
    inc d           
    cp (ix+2)       ; Have we reached the end?
    ret nc          ; If so, return
    ld a,b          ; get attribute back
    ld b,c          ; get counter from our store
    jr hb_rowloop   ; back for the next line.
    
;*************************************************************
;Temp variables for menu. 
; +0,+1 =address of highlight boxes
; +2 Highlight value
; +3 Unselected value
;*************************************************************
wfm_working:  
    defb $0,$0,$0,$0
;*************************************************************
;Menu loop. Note, assumed text already printed.
; in input, HL=menu details.
;*************************************************************
do_mouse_WaitForSelection:
;Copy the first two values of the menu description to the temp store.
    ld de,wfm_working+2         
    ld bc,2
    ldir
;copy the start of the menu description to IX and to the temp store
    push hl    
    pop ix
    ld (wfm_working),HL

waitformenu_loop:
    call getMouseXY             ;Get current X/Y
    call CheckDEInBox           ;Check if its within the current box area
    jr c,waitformenu_selected   ;If do, go and process it
    ld a,(wfm_working+3)        ;if not, un-highlight it. 
    call highlightBox
waitformenu_Nextitem:
    inc ix                      ;point to next menu item
    inc ix 
    inc ix
    inc ix
    inc ix
    ld a,(ix+0)                 ;get first byte of the menu item
    cp $FF                      ;Terminator?
    jr nz,waitformenu_loop      ;If not, go back to next menu item
    ld ix,(wfm_working)         ;get the first menu item
    jr waitformenu_loop         ;and go again
waitformenu_selected:
    ld a,(wfm_working+2)        ;Highlight the current menu item
    call highlightBox
    call mouse_buttons          ;Check if buttons are being pressed
    cp 0
    jr nz,Mouse_item_selected   ;If we are, end processing
    jr waitformenu_Nextitem     ;Go back for the next item

Mouse_item_selected:
    ld a,(IX+4)                 ;Return the RETVAL from the array
    ret

;*************************************************************
;List of highlight boxes.
;*************************************************************
ADL_boxes:
    defb 01101000b,01111000b    ;SELECTED, UNSELECTED attributes
    defb $05,$03,$05,25,0       ;top,left,bottom,right, retval
    defb $06,$03,$06,25,1
    defb $07,$03,$07,25,2
    defb $FF                    ;terminator
;*************************************************************
; Ask difficulty level dialog.
;*************************************************************
AskDifficultyLevel:
    LD DE,$0101
    LD BC,$0719
    CALL DRAWBOX
    CALL display_message
    defb 01001111b,$03,$03,"Select Difficulty level",$ff
    CALL display_message
    defb 01111000b,$05,$04,"Easy (9x9)   11 mines",$ff
    CALL display_message
    defb 01111000b,$06,$04,"Med  (16x16) 30 mines",$ff
    CALL display_message
    defb 01111000b,$07,$04,"Hard (19x28) 60 mines",$ff

;Preset the mouse pointer to somewere visible.
    ld a,$18
    ld (mouse_x),a
    ld (mouse_y),a

;and wait for input.
    call mouse_on
adl_loop:
    ld hl,ADL_boxes
    call do_mouse_WaitForSelection

;Convert the number (1-3) to an address
    ld hl, DifficultyArray
    ld b,a      ;Multiply Ax3
    add a,a     
    add a,b
    ld b,0
    ld c,a
    add hl,bc

;Copy the parameters for the selected board.
    ld de,MAX_X
    ld bc,3
    ldir
    call mouse_off
    ret    

DifficultyArray:
    defb 9,9,11
    defb 16,16,40
    defb 26,18,60

;*************************************************************
;This is a hack to deal with the background image i have.
;It seems when encoding it, there are bits where the ink and paper
;are the same. the cursor cannot be seen in those locations
;This function just trawls the attribute area for squares where
;this is the case, and inverts the paper for that area.
;*************************************************************
fixupMinescreen:
    ld hl,MINESCREEN+$1800          ;Start of attribute area.
    ld bc,$300                      ;attribute area size
fms_loop:
    ld a,(hl)                       ;Extract INK -> D
    and 00000111b
    ld d,a

    ld a,(hl)                       ;Extract Paper
    rra
    rra
    rra
    and 00000111b                   

    cp d                            ;Same as ink?
    jr nz,fms_dontmod               ;No, go next

    ld a,(hl)                       ;Invert paper
    xor 00111000b                   
    ld (hl),a

fms_dontmod:
    inc hl                          ;Next attribute byte
    dec bc                          ;dec counter
    ld a,b                          ;if there is anything left, go again
    or c
    jr nz,fms_loop
    ret

;*************************************************************
;Read the keys 1-0  (with 0 returning 10)
;Return A = value (1-10)
;       Carry = True - Number pressed, 
;*************************************************************
NUMBERKEY_TO_A:         
;For first row, Keys are 12345 -> bits 01234, so just keep shifting
;in until we run out of bits or we find one pressed.
    ld d,0                  
    ld bc,$f7fe
    in a,(c)
    and 00011111b   ;Mask 5 keys

numkey_15loop:
    inc d
    rra
    jr c,numkey_15loop  ;key isnt pressed
    ld a,d              ;Check we havent got to 6 (Ie, no key pressed)
    cp 6
    jr z,numkey_not05   ;If we havent, go on to 6-0
    scf                 ;Yes, key pressed, set carry flag and return A
    ret

;For the second row, keys are 09876 -> bit 01234, so this time 
; D starts at 11 and counts down.
numkey_not05:
    ld d,11
    ld bc,$effe
    in a,(c)
    and 00011111b

numkey_06loop:
    dec d
    rra
    jr c,numkey_06loop      ;key isnt pressed
    ld a,d                  ;Check we havent got to 5 (Ie, no key pressed)
    cp 5
    jr z,numkey_notpressed  ;if we have, key is not pressed
    scf                     ;set the carry flag and return value in A
    ret
numkey_notpressed:
    scf                     ;reset carry flag and return.
    ccf
    ret

;*************************************************************
; Ask input method dialog.
;*************************************************************
AskInputMethod:
    LD DE,$0101
    LD BC,$0a1c
    CALL DRAWBOX
    CALL display_message
    defb 01001111b,$03,$03,"Control Method            ",$ff
    CALL display_message
    defb 01111000b,$05,$04,"1. Kempston Mouse",$ff
    CALL display_message
    defb 01111000b,$06,$04,"2. Sinclair I/f 2 port 1",$ff
    CALL display_message
    defb 01111000b,$07,$04,"3. Sinclair I/f 2 port 2",$ff
    CALL display_message
    defb 01111000b,$08,$04,"4. Kempston Joystick",$ff
    CALL display_message
    defb 01111000b,$09,$04,"5. Cursor keys",$ff

ami_loop:
    call NUMBERKEY_TO_A     ;Get number
    jr nc,ami_loop          ;If no button pressed, do again
    cp 6                    ;Number > 5?
    jr nc,ami_loop          ;Again

    dec a                   ;1-5 -> 0-4
    ld (mouse_input),a      ;Set mouse input flag
    ret


;*************************************************************
; This does the BOOM text and box animation
;*************************************************************
makeboom:
    ;show the BOOM message
    call display_message
    defb 11001110b      ;bright, flash, paper 1,ink 6
    defb $0A,$0D        ;at 10,13
    defb "BOOM!",$FF    ;text and terminator

    ld hl,$5800         ;Store the original attributes so 
    ld de,$d000         ;we can restore them later
    ld bc,$0300
    ldir

    ld a,01100011b      ;First attribute is Bright, paper 4, ink 3
    ld b,$80           ;Loop 128 times
makeboom_lm:
    push bc             ;store loop
    
    ld hl,$0307         ;Start dimensions = 3,7
    ld de,$090C         ;Start position = 9,12
mainboom_goagain:
    push af
    push hl
    push de
    call AttributeBox   ;output box
    pop de
    pop hl
    pop af

    inc h               ;new height +2
    inc h
    inc l               ;new width +2
    inc l

    ld b,a              ;save attribute

    inc a               ;Add 1 to INK
    and 00000111b       ;mask out ink bits
    ld c,a              ;store it in C temporerilly

    ld a,b              ;Get back attribute byte
    add a,8             ;Add 1 to PAPER
    and 00111000b       ;mask out paper bits
    or c                ;merge back in the ink
    or 01000000b        ;set BRIGHT

    dec e               ;start x - 1
    dec d               ;start y - 1
    jr nz,mainboom_goagain  ;If our height hasnt reached zero, go again.
    
    call makeWhiteNoise ;DO a little bit of noise
    pop bc              ;Get counter back
    dec b               ;-1
    ld a,b              ; Set new attribute byte
    jr nz,makeboom_lm   ; go again

    ld hl,$d000         ;restore original attributes
    ld de,$5800
    ld bc,$0300
    ldir

    ret

;*************************************************************
;Output an box with the colour attributes
; DE = top left (Y/X), H=height, L=Width
;*************************************************************
AttributeBox:
    push de
    push hl

;Top line
    ld b,l                      ;Width = counter
    call DEtoAttributeAddress   ;convert to attribute address
ab_hloop1:
    ld (hl),a                   ;Set attribute
    inc hl                      ;next character along
    djnz ab_hloop1              ;Loop back for the rest of the lines

    ld b,a                      ;Preserve attribute for later

    pop hl                      ;Get back the width/height
    push hl
    ld a,d                      ;Get Y position
    add a,h                     ;add in the height
    dec a                       ;decrement to correct height
    ld d,a                      ;and set the new Y position

    ld a,b                      ;restore the attribute

    ld b,l                      ;Width = counter again
    call DEtoAttributeAddress   ;DE-> Attribute Address
ab_hloop2:
    ld (hl),a                   ;Set attribute
    inc hl                      ;Next character
    djnz ab_hloop2              ;loop

    pop hl                      ;Get XY and Width/height back
    pop de
    push de
    push hl

    ld b,h                      ;This time, the loop is the height
    call DEtoAttributeAddress   ;DE-> Attribute Address  
    ld de,$0020                 ;Line width
ab_vloop1:
    ld (hl),a                   ;Set attribute
    add hl,de                   ;Add 1 line
    djnz ab_vloop1              ;Loop back

    pop hl                      ;Get XY and Width/height back
    pop de                      ;for the final time

    ld b,a                      ;preserve attribute
    ld a,e                      ;get X position
    add a,l                     ;Add in width
    dec a                       ;Correct width
    ld e,a                      ;and set the new X position

    ld a,b                      ;Get attribute back
    ld b,h                      ;Counter is width
    call DEtoAttributeAddress   ;DE-> Attribute Address  
    ld de,$0020                 ;Line width
ab_vloop2:
    ld (hl),a                   ;Set attribute
    add hl,de                   ;Add 1 line
    djnz ab_vloop2              ;Loop back
    ret


;*************************************************************
;Make some white noise. As usual, this is not really random, just
; It has a duration of about 2/50th sec
;*************************************************************
makeWhiteNoise:
    ld a,(BORDCR)    ;Get the border colour into C
    rra
    rra
    rra
    and 0000111b
    ld c,a

    ld h,$5         ;Repeat 5 times....
mwn_loop1:
    push hl
    call random     ;random number between 0 and 255
    ld b,a          ;Use as a counter
mwn_loop2:
    nop             ;delay
    nop
    nop
    nop
    djnz mwn_loop2

    ld a,c          ;Get the border colour with mic bit=0
    out ($fe),a

    call random     ;Now another delay
    ld b,a
mwn_loop3:
    nop
    nop
    nop
    nop
    djnz mwn_loop3

    ld a,c          ;Get the border colour with mic bit=1
    or 00011000b
    out ($fe),a

    pop hl          ;get counter back and loop.
    dec h
    jr nz,mwn_loop1
    ret
;*************************************************************
;Make a click. Used when a mouse button is clicked.
;*************************************************************
makeClick:
    ld a,(BORDCR)    ;Get the border colour into C
    rra
    rra
    rra
    and 0000111b
    ld c,a            ;Store for later

    or 00011000b      ;Set EAR/MIC bit
    out ($fe),a             

    ld b,100          ;Delay
mck_loop2:
    nop             
    nop
    nop
    nop
    djnz mck_loop2

    ld a,c            ;Get the border colour with mic bit=0
    out ($fe),a
    
    ret



