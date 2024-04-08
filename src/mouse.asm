;TODO: Want to support AMX mouse
;*******************************************************************
;* ZX Spectrum interrupt Mouse pointer implementation              *
;*  Graham Salkin 1 Sept 2023.                                     *
;*                                                                 * 
;* Uses the code between 48128 and 49151                           *
;* Usage:                                                          *
;*  Clear 48127                                                    *
;*  load "thiscode" code                                           *
;*  Turn on:  Randomize usr 48144      ($bc10)                     *
;*  Turn off: Randomize usr 48147      ($bc13)                     *
;*  Read Y,X from 48163(Y),48164(X)    ($bc23,$bc24)               *
;*  Read Buttons: let a=usr 48150      ($bc16)                     *
;*  Input method: (48165) ($bc25)                                  *
;*              0: Kempston mouse                                  *
;*              1: Sinclair IF/2 port 1                            *
;*              2: Sinclair IF/2 port 2                            *
;*              3: Kempston                                        *
;*              4: Cursor keys  (requires shift)                   *
;*              5: Keyboard                                        *
;*  Setting Bit 3 (Ie, adding 8) will set +mouse mode,             *
;*  Eg,         9: Mouse + IF/2 p1                                 * 
;*             10: Mouse + IF/2 p2                                 * 
;*             11: Mouse + Kempston                                * 
;*             12: Mouse + Cursors (requires shift)                *
;*             13: Mouse + keyboard                                *
;* NOTE, Space is used for the RMB in all methods except for 0 + 5 * 
;*                                                                 *
;* NOTES:                                                          *
;*   If you are outputting to the screen, its best to turn cursor  *
;*      off to avoid corrupting the screen when the cursor redraws *
;*      the background.                                            *
;*   If you want to use your own interrupt handler and just add    *
;*      this, just put in a call to MOUSE_INTERRUPT2 ($bc73)       *
;*      to preserve interrupts or  MOUSE_HANDLER ($bc76) if you    *
;*      are already saving interrupts.                             *
;*                                                                 *
;*                                                                 *
;* IM2 notes for the speccy:                                       *
;*  (Note this is more for my reference, 2 years down the line)    *
;*  As you cant guarantee the bus content during the interrupt     *
;*  response (As there is nothing to actually reply to an IM2),    *
;*  your interrupt table has to be populated from 00 to 100        *
;*  and must therefore be a doubled byte, Eg, F0F0 A0A0 ect.       *
;*                                                                 *
;*  Here, we have chosen bc00-bd00 for the interrupt table         *
;*  and bfbf for the interrupt location to waste least memory while*
;*  remaining below $c000                                          *
;*  (The space from bd01-bdbc and bdc1-bdff remains free).         *
;*                                                                 *
;*  On the +2A/+3 with the +3E roms, which is is initially aimed   *
;*  at, We could probably get away with just populating bdff and   *
;*  be00, as the bus is always $FF, but may cause issues on        *
;*  things like the Harlequin 128 with the +3E roms which can      *
;*  implement floating bus for better 48k compatibility.           *
;*                                                                 *
;*  The reason its so low in memory, is so that it cant be paged   *
;*  out as this will cause a crash, so need to keep it below $c000 *
;*  (EG, disk operation, PLAY or return to 128/+3 basic)           *
;*  This means that BASIC must be kept below bb00 (47872) which    *
;*  is the start of the interrupt table.                           *
;*                                                                 *
;*******************************************************************
ORG $bc10

mouseX: equ $fbdf
mouseY: equ $ffdf
mouseBtn: equ $fadf

    JP INTERRUPT_SETUP
    JP MOUSE_DISABLE
    JP GET_BUTTONS
    defb 0,0,0          ;reserved for later use
    defb 0,0,0

MOUSEDIFF:
    defb 0,0            ;Calculated differences after reading hardware
lastread:
    defb 0,0            ;Last value read from mouse port. (Unused elsewere)
MOUSELOC:
    defb 96,128         ;Actual Y/X location
INPUTMODE: 
    defb 2             ;Input mode. (See notes)
;Defined keys. These are in the format: PortLSB, bit
REDEFKEYS:      
    defb $fd,00000001b ;Down  V   default: A
    defb $fb,00000001b ;UP    ^   default: Q
    defb $df,00000010b ;Right <-  default: O
    defb $df,00000001b ;Left  ->  default: P
BUTTONS:
    defb $bf,00000001b ;RMB       default: Enter
    defb $7f,00000001b ;LMB       default: Space
    

;*****************************************************************
; Disable the mouse cursor. 
;*****************************************************************
MOUSE_DISABLE:
    IM 1
    CALL undoCursor
    RET
;*****************************************************************
; Setup the interrupt handler. This needs 256 bytes at $bb00
; and an interrupt handler at $bfbf
;*****************************************************************
INTERRUPT_SETUP:
    DI
;Set Bb00->bc00 to $bf
    LD HL,$bb00         
    LD A,H
    LD I,A
    LD (HL),$bf
    LD d,h
    ld e,1
    LD BC,$0103
    LDIR
;Copy the mouse interrupt jump to $bfbf
    LD HL,MOUSE_INTERRUPT
    LD DE,$bfbf
    LD BC,4
    LDIR
;Display the cursor
    ld hl,MOUSELOC
    ld d,(hl)
    inc hl
    ld e,(hl)
    call DISPLAYCURSOR
;set IM2 and start.
    IM 2
    EI
    RET

;*****************************************************************
;Actual handler for the mouse interrupt.
;*****************************************************************
MOUSE_INTERRUPT2:
    PUSH IX
    PUSH BC
    PUSH DE
    PUSH HL
    PUSH AF
    CALL MOUSE_HANDLER
    POP AF
    POP HL 
    POP DE
    POP BC
    POP IX    
    JP $38      ;continue with BASIC interrupt.

;*****************************************************************
;Call to mouse interrupt moved to BFBF
;*****************************************************************
MOUSE_INTERRUPT:
    JP MOUSE_INTERRUPT2

;*****************************************************************
; Actually do the work here. 
;*****************************************************************
MOUSE_HANDLER:
;Work out the difference since the last 
;read and write it to (IX+[0,1])
    ld ix,MOUSEDIFF

    xor a            ;Reset difference log
    ld (ix+0),a
    ld (ix+1),a

;Get the input
    call DoControllerInput
;Check for any changes
    ld a,(ix+0)         ;If no change, dont redraw cursor
    or (ix+1)
    ret z    
;Append the difference values
    ld a,(ix+0)
    neg
    add (ix+4)
    ld (ix+4),a
;check for rollover
    cp 192      ;check if we are in the dead zone from 192-255
    jr c, skip_to_x
    bit 7,(ix+0)        ;check if we were incrementing...
    jr z,mh_setmaxy     ;Note, flag is NEGd zo -ve values = incrementing
    ld (ix+4),191       ;if so, max out at 192
    jr skip_to_x
mh_setmaxy:
    ld (ix+4),0         ;else. min out at 0


skip_to_x:
;Deal with X including dealing with rollover
    ld a,(ix+1)
    ld e,(ix+5)
    add e
    ;check for rollover
    ld (ix+5),a
    xor e
    bit 7,a
    jr z,check_X_done   ;no rollover

    bit 7,(ix+1)  ; 1 = check for 0->255, 0 = 255->0
    jr z,check_X_HIGH_rollover ;if we are Incrementing, do that check
    bit 7,e        ;check to see if the MSB is 1
    jr nz,check_X_done     ;No so we didnt roll lover
    ld (ix+5),0    ;If Yes, set to 0
    jr check_X_done
check_X_HIGH_rollover:
    bit 7,e        ;Check to see if the MSB is positive
    jr z,check_X_done      ;No, so we didnt roll over
    ld (ix+5),255  ;if yes, set to 255
check_X_done:

;display the cursor
    ld d,(ix+4)
    ld e,(ix+5)
    call DISPLAYCURSOR
    ret

;****************************************************
;Display the cursor at the X/Y address
;DE = xy
;****************************************************
DISPLAYCURSOR:
    push de                 ;store the XY location
    ld a,(pxflags)          ;Check to see if we need to remove
    bit 0,a                 ;a previous cursor.
    call nz,undoCursor      ;if so, undelete it
    ld a,(pxflags)          ;and whatever happens, set the flag
    set 0,a
    ld (pxflags),a    
    pop de                  ;Get back the XY location
;reset the display overwrite store
    ld hl,ptrstore
    ld (ptrstoreptr),hl

    ld ix,ptrBitmap         ;location of the bitmap
; extract the shift to C
    ld a,e                  
    and $7
    ld c,a

; convert the pixel Y to character Y
    ld a,e
    rra 
    rra
    rra 
    and $1f
    ld e,a

;store the character coordinates for undeleting later.
    ld (lastcoords),de

;number of lines, c= pixel shift, de = xy, ix=ptr
    ld b,12
disploop:
    push bc
    push de
    call LineToAddress
    ld a,c
;get the data to de and the mask to bc
    ld d,(ix+0)
    ld e,0
    ld b,(ix+13)
    ld c,$ff

;shift both the data(de) and mask (bc)
;However if the shift is zero, skip.
    cp 0
shiftloop:
    jr z,shiftdone
    scf             ;want so shift 0 into data byte 
    ccf
    rr d            ;shift right d->e 
    rr e

    scf             ;want so shift 1 into mask byte
    rr b            ;shift right b->c
    rr c

    dec a           ;next shift
    jr shiftloop
shiftdone:
;ix = restore ptr
;bc = mask
;de = data
    push ix          ;preserve the current data location
;check if we have gone off the end of the display area
    ld a,h           ;get the MSB
    cp $58           ;see if we are >5700
    jr nc,endofdisplay ;if so, dont modify memory.

;Get the next address for the display undelete store
    ld ix,(ptrstoreptr)

;Do first byte:
    ld a,(hl)       ;Get data
    ld (ix+0),a     ;Store it for undeleting it later
    and b           ;Mask out the cursor
    or d            ;Add in the cursor pixels
    ld (hl),a       ;store data

    inc hl          ;next byte in display file
    inc ix          ;next byte for undelete buffer
    ld a,l          ;Check to see if second byte is off screen.
    and $1f
    jr z,endofdisplay  ;Yes, dont display 
;now second byte
    ld a,(hl)       ;Get data
    ld (ix+0),a     ;Store it for undeleting it later
    and c           ;Mask out the cursor
    or e            ;Add in the cursor pixels
    ld (hl),a       ;store data

endofdisplay:
;record the current location in the undelete buffer
    inc ix
    ld (ptrstoreptr),ix

;point to the next line
    pop ix          ;pop data
    inc ix          ;Next character byte
    pop de          ;get back the XY location
    inc d           ;next line down
    pop bc          ;get back the counter
    djnz disploop   ;loop

    ret

;****************************************************
; This function un-does cursor changes from
; the data stored when it was written.
;****************************************************
undoCursor:
    ld de,(lastcoords)   ;Last display location
    ld ix,ptrstore       ;stored overwritten data
    ld b,12              ;Lines to restore
    
undo_loop:
;check to see if we have gone off the end of the display
    ld a,d              ;get the Y coordinate
    cp 192              ; > 192?
    jr nc,skipundo      ; If so, no more lines

;Copy the two bytes of the saved memory back to the screen
    call LineToAddress  ;convert DE to a screen address
    ld a,(ix+0)         ;Get the first byte
    ld (hl),a           ;write it to the screen
    inc ix              ;next data location
    inc hl              ;next screen location
    ld a,l
    and $1f
    jr z,nextline
    ld a,(ix+0)         ;get the second byte
    ld (hl),a           ;write it to the screen
nextline:
    inc ix              ;next data location
;next line
    inc d               ;Next line down
    djnz undo_loop      ;and next line
;reset the "memory valid" flag
skipundo:
    ld hl,pxflags
    res 0,(hl)
    ret


;****************************************************
;Variables.
;****************************************************
pxflags:
    defb $0          ;Used to store if we have displayed the cursor
lastcoords:
    defb $0,$0       ;Last coordinates of cursor displayed 
ptrstoreptr:
    defb $0,$0       ;Raw address of the cursor. Saves recalculating
ptrBitmap:           ;Pixel data
    defb 10000000b
    defb 11000000b
    defb 10100000b
    defb 10010000b

    defb 10001000b
    defb 10000100b
    defb 11001000b
    defb 00101000b

    defb 00100100b
    defb 00010100b
    defb 00011000b
    defb 00000000b
ptrmask:            ;cursor mask
    defb 00111111b
    defb 00011111b
    defb 00001111b
    defb 00000111b

    defb 00000011b
    defb 00000001b
    defb 00000011b
    defb 10000011b

    defb 10000001b
    defb 11000001b
    defb 11000011b
    defb 11110011b
    defb 11111111b

ptrstore:           ;Storage for data underneath the cursor.
    defb 0,0,0,0,0,0,0,0
    defb 0,0,0,0,0,0,0,0
    defb 0,0,0,0,0,0,0,0
    defb 0,0,0,0,0,0,0,0

;**********************************************************
;* Calculates the address of a pixel line
;* 
;* D=pixel line  e=character cell
;* return HL=address
;* AF corrupt
;d= AABB BCCC  e=000yyyyy
;
; 010a accc  bbbY YYYY
;**********************************************************
LineToAddress:
    ld a,d          ;Extract AA and move it from 11000000 to 00011000
    rra 
    rra
    rra
    and $18
    ld h,a          ;Store it to h 

    ld a,d          ;Now extract CCC
    and $7
    or h            ;merge it into the LSB of H
        
    or $40          ;merge in 0100000
    ld h,a          ;and we are done with H

    ld a,d          ;extract BB and move it from 00BBB000 -> BBB00000
    rla
    rla
    and $e0
    or e            ;merge in our X coordinate 
    ld l,a          ;and now HL = address
    ret
;**********************************************************
; Return the mouse buttons in BC
; bit = 0 lmb, 1 = rmb (or space)
;**********************************************************
GET_BUTTONS:
GET_MOUSEBUTTONS:
;Dispach to appropriate button handler.
    ld a,(INPUTMODE)        ;Get input mode
    and 00000111b           ;Mask out the plus mouse mode
    cp 1
    jr Z,GET_SINCLAIR1CURSOR
    cp 2
    jr Z,GET_SINCLAIR2
    cp 3
    jr Z,GET_KEMPSTON
    cp 4
    jr Z,GET_SINCLAIR1CURSOR
    cp 5
    jr Z,GET_KEYBOARD
;Read directly from the mouse button port
    call GET_MOUSEBTN_DIRECTLY

;Check for plusmouse mode, 
GETMOUSEBTN_WRITEBTN:   
    push af
    ld a,(INPUTMODE)    ;plus mouse mode?
    bit 3,a
    jr z, GETMOUSEBTN_dontmerge    ;Nope, skip
    call GET_MOUSEBTN_DIRECTLY     ;Get the mouse buttons
    pop bc                         ;Merge in to the previous input
    or b
    push af
GETMOUSEBTN_dontmerge: ;Return the value in A in BC
    pop af
    and $3
    ld b,0
    ld c,a
    ret

GET_MOUSEBTN_DIRECTLY:
    ld bc,mouseBtn      ;Output is straight from the  port.
    in a,(c)
    cpl
    ret                     

;**********************************************************
GET_KEMPSTON:       ;CHeck bit 5 in $1f
    in a,($1f)      ;Extract fire button to bit 1
    rra
    rra 
    rra
    and $2
    ld d,a
    ld bc,$7FFE
    in a,(c)
    cpl
    and $01
    or d
    jr GETMOUSEBTN_WRITEBTN

;**********************************************************
GET_SINCLAIR1CURSOR:          ;check fire key on IF/2 (button "0")
    ld hl,SINCLAIR1CURSOR_buttons
    jr get_keys

SINCLAIR1CURSOR_buttons:
    defb $EF,0000001b   ;0
    defb $7f,0000001b   ;space

;**********************************************************
GET_SINCLAIR2:          ;check fire key on IF/2 (button "5")
    ld hl,SINCLAIR2_buttons
    jr get_keys

SINCLAIR2_buttons:
    defb $f7,0010000b   ;5
    defb $7f,0000001b   ;space
;**********************************************************
;Get the fire and menu buttons for all keyboard based inputs
;**********************************************************
GET_KEYBOARD:
    ld hl,BUTTONS
get_keys:
    ld d,0
    ld c,$fe

    ld b,(hl)
    inc hl
    in a,(c)
    and (hl)    
    jr nz,gkb_next1
    set 0,d
gkb_next1:
    inc hl
    ld b,(hl)
    inc hl
    in a,(c)
    and (hl)    
    jr nz,gkb_next2
    set 1,d
gkb_next2:
    ld a,d
    jr GETMOUSEBTN_WRITEBTN
    
;**********************************************************
; Decode the mouse differential input. 
;**********************************************************
MOUSEINPUT:
    ld bc,mouseY      ;Get data from mouse Y port
    in a,(c)
    ld e,a            ;Store it for later
    sub (ix+2)        ;get a difference with the last Y value
    ld (ix+0),a       ;store the new difference
    ld (ix+2),e       ;and store the new read value

    ld bc,mouseX      ;get the data from the mouse X port
    in a,(c)
    ld e,a            ;Store it for later
    sub (ix+3)        ;get a difference with the last Y value
    ld (ix+1),a       ;store the new difference
    ld (ix+3),e       ;and store the new read value
    ret

;****************************************************
;Decode input from the Mouse, keys or kempston joysticks
;This will also deal with plus mouse mode.
;****************************************************
DoControllerInput:
    ld hl,MOUSEDIFF         ;Address of the differential variables
    ld a,(INPUTMODE)        ;get input mode
    and 00000111b           ;mask out base type
    cp 1                    ;Sinclair 1
    jr z,SINCLAIR1INPUT
    cp 2                    ;Sinclair 2
    jr z,SINCLAIR2INPUT
    cp 3                    ;Kempston
    jr z,KEMPSTONINPUT
    cp 4                    ;Cursor
    jr z,CURSORINPUT
    cp 5                    ;redefined keys
    jr z,KEYBOARDINPUT
    CP 0                    ;Mouse.    
    jr z,dnmi_mouseonly

dnmi_FinshedInput:          
    ld a,(INPUTMODE)        ;Do we have the "x+mouse mode?"
    bit 3,a
    ret z                   ;If not we are done.

dnmi_mouseonly:             ;merge the mouse input with the keyboard input
    ld h,(ix+0)             ;Store the diffs created by non-mouse input->hl
    ld l,(ix+1)
    call MOUSEINPUT         ;get mouse modificaitons
    ld a,h                  ;add in the keyboard input to the diffs part 1
    add (ix+0)
    ld (ix+0),a

    ld a,l                  ;add in the keyboard input to the diffs part 2
    add (ix+1)
    ld (ix+1),a

    ret

dnmi_Double_nonmouse:
;Double the X and Y differences.
    ld a,(ix+0)
    sla a
    ld (ix+0),a

    inc hl
    ld a,(ix+1)
    sla a
    ld (ix+1),a
    jr dnmi_FinshedInput

;**********************************************************
;Decode the kempston port.
; This is port $1F. 
;*Bit 43210
;*    00000
;*    ||||+----Right  =>
;*    |||+-----Left   <=
;*    ||+------Down   V
;*    |+-------Up     ^
;*    +--------Fire   X
;**********************************************************
KEMPSTONINPUT:
    in a,($1f)
SameAsKempston:
    rra
    inc hl
    jr nc,kempskip1
    inc (hl)    
kempskip1:
    rra
    jr nc,kempskip2
    dec (hl)    
kempskip2:
    dec hl
    rra
    jr nc,kempskip3
    dec (hl)    
kempskip3:
    rra
    jr nc,kempskip4
    inc (hl)    
kempskip4:
    jr dnmi_Double_nonmouse

;**********************************************************
;Decode the Sinclair port 1 (6-0)
; This is port $EFFE. 
;*Bit 43210
;*    00000
;*    ||||+----Fire   X
;*    |||+-----Up     ^
;*    ||+------Down   V
;*    |+-------Right  =>
;*    +--------Left   <=
;**********************************************************
SINCLAIR1INPUT:
;Now check each key
    ld de,SINCLAIR1
    jr FIXEDKEYS

SINCLAIR1: 
    defb $ef,00000100b  ; down
    defb $ef,00000010b  ; up
    defb $ef,00010000b  ; <=
    defb $ef,00001000b  ; =>

;**********************************************************
;Decode the Sinclair port 2 (1-5)
; This is port $F7FE. 
;*Bit 43210
;*    00000
;*    ||||+----Left   <=
;*    |||+-----Right  =>
;*    ||+------Down   V
;*    |+-------Up     ^
;*    +--------Fire   X
;**********************************************************
SINCLAIR2INPUT:
;Now check each key
    ld de,SINCLAIR2
    jr FIXEDKEYS

SINCLAIR2: 
    defb $f7,00000100b  ; down
    defb $f7,00001000b  ; up
    defb $f7,00000001b  ; <=
    defb $f7,00000010b  ; =>

;**********************************************************
;Decode cursor keys. These are:
; 7ffe bit 5 = "5" <= 
; effe bit 5 = "6" down
; effe bit 4 = "7" up
; effe bit 3 = "8" =>
;**********************************************************
CURSORINPUT:
;Check to see if caps-shift is pressed
    ld bc,$FEFE
    in a,(C)
    rra
    jr c,kempskip4

;Now check each key
    ld de,CURSORKEYS
    jr FIXEDKEYS

CURSORKEYS: 
    defb $ef,00010000b  ; down
    defb $ef,00001000b  ; up
    defb $f7,00010000b  ; <=
    defb $ef,00000100b  ; =>

;**********************************************************
;decode keyboard. 
;These defualt to QAOP
;**********************************************************
KEYBOARDINPUT:
    ld DE,REDEFKEYS    ;point to defined keys
;**********************************************************
;This is a generic input for for the keyboard input. 
; it expects a table of keys in the format portMSB, Bit mask
; with 5 entries in the order:
;       down, up, left,right
; HL will contain address of mouse differential variables.
;**********************************************************
FIXEDKEYS:
    ex de,hl           
    ld c,$fe            ;default ports are $xxFE

    ld b,(hl)           ;get the port
    inc hl              ;next address
    in a,(c)            ;read the data on this port
    and (hl)            ;mask out the bit required
    jr nz,keyin_next1   ;if set (keys are active low), skip.
    ld a,(de)           ;(de) = (de)-1
    dec a
    ld (de),a
keyin_next1:    
    inc hl              ;Next port to read
    ld b,(hl)           ;get port
    inc hl              ;point at next
    in a,(c)            ;read in data       
    and (hl)            ;mask out bit required
    jr nz,keyin_next2   ;if not pressed, skip
    ld a,(de)           ;(de) = (de) + 1
    inc a
    ld (de),a
keyin_next2:
    inc hl              ;Next port to read
    inc de              ;Point at the second differential variable. (Horezontal)
    ld b,(hl)           ;get port
    inc hl              ;point at next
    in a,(c)            ;read in data       
    and (hl)            ;mask out bit required
    jr nz,keyin_next3   ;if not pressed, skip
    ld a,(de)           ;(de) = (de)-1
    dec a
    ld (de),a
keyin_next3:
    inc hl              ;Next port to read
    ld b,(hl)           ;get port
    inc hl              ;point at next
    in a,(c)            ;read in data       
    and (hl)            ;mask out bit required  
    jr nz,keyin_next4   ;if not pressed, skip
    ld a,(de)           ;(de) = (de) + 1
    inc a
    ld (de),a
keyin_next4: 
    jr kempskip4



    
    
