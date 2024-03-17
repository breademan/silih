INCLUDE "src/hardware.inc"

INCLUDE "src/general.inc"

SECTION "Payload UI Data SECTION",ROM0[$1000 + ($1000*UI_RAMBANK)]
UIStorage::
    LOAD "Payload UI Data LOAD", WRAMX [$D000]
UIJumpTable: ;12 bytes
dw MenuHandler_CameraOpts, MenuHandler_DitherOptions, MenuHandler_Selected, MenuHandler_TakeConfirm, MenuHandler_Gallery, MenuHandler_DeleteConfirm

  ;determines what happens when you press up/down to modify a value during MENU_SELECTED menustate
ChangeOptionHandler_table:
  MACRO X
  dw \6
  ENDM
  INCLUDE "src/ui_elements.inc"
  
  ASSERT .end < $D100, "ChangeOptionHandler_table is not aligned on 256 bytes"
    .end

HandleInput::
    ldh a, [MENU_STATE]
    add a,a     ;each jump table entry is 2 bytes long, so multiply MENU_STATE by 2
    ld l, a    ;Assumes the jump table starts at $D000
    ld h, $D0
    ld e, [hl]
    inc hl
    ld h, [hl]
    ld l, e    
    jp hl

;--------------MenuHandler Subroutines------------------------------------
;--HandleInput jumps to these. When MenuHandlers are done, they should jp to HandleInputDone, which will (if necessary) switch RAM banks back

MenuHandler_CameraOpts:
    ;Which keys take priority? Let's first check direction keys, then process the other keys once we've changed the cursor
    ;We can have both U/D and L/R at the same time: check U/D first, then L/R.

    ldh a, [joypad_active]
    ld b, a ; b holds joypad_active
    and a, %11110000
    jp z, .state0Buttons
    ldh a, [MENU_POSITION]
    and a, $0F ;get only the NEW menu position
    DEF MENU0_ROWLEN EQU 5
    DEF MENU0_LEN EQU 10
    ;dpad has been pressed -- which one?
    bit JOYPAD_DOWN, b 
    jr z, :+
      add a, MENU0_ROWLEN
    :bit JOYPAD_UP, b
    jr z, :+
      sub a, MENU0_ROWLEN
    :bit JOYPAD_LEFT, b
    jr z, :+
      dec a    
    :bit JOYPAD_RIGHT, b
    jr z, :+
     inc a
    :
    .boundsCheck:
    bit 7, a ;check for underflow
    jr z,:+
    add a, MENU0_LEN ;fix underflow
    :cp a, MENU0_LEN
    jr c, :+
    sub a, MENU0_LEN ;fix overflow

    ;load current menu position (a) into lower bit,current 
    :ld b, a
    ldh a,[MENU_POSITION]
    swap a ;move old position to old position slot
    and a, $F0 ;clear new position slot
    or a, b
    ldh [MENU_POSITION], a ;write back to menu position

    call MoveCursorSpriteToMenuPosition

    ldh a, [joypad_active]
    ld b,a
    .state0Buttons:
    bit JOYPAD_START, b ;check START

    :bit JOYPAD_SELECT, b ;check SELECT
    jr z,:+
    ;Start handover, possibly after waiting for capture to complete
      bit JOYPAD_DOWN, b
      jr z,:+
      call StartHandover

    :bit JOYPAD_B, b; check B - take picure
    jr z,:+
      call InitMenuState_TakeConfirm
      jp .end ; don't check for b button if we pressed a -- only one state transition at once.
    :bit JOYPAD_A, b ;check A - select a value to change
    jr z, :+
        ;transition to state "selected"
        call InitMenuState_Selected
    :
  .end:
jp HandleInputDone

MenuHandler_DitherOptions:

jp HandleInputDone


MenuHandler_Selected:
  ldh a, [joypad_active]
  ld b, a ; b holds joypad_active
  and a, (JOYPAD_UP_MASK | JOYPAD_DOWN_MASK)
  ;Do separate checks for Up/Down (11xxxxxx) and L/R (xx11xxxx)
  jp z, .dpad_updown_end 

    ;Either down or up is pressed
    ;HL = SelectedAddrTable[MENU_POSITION]
    ;In real pointers, that's SelectedAddrTable + (MENU_POSITION<<1)
    ld hl, SelectedAddrTable ;3c 3b
    ldh a, [MENU_POSITION] ;3c 2b
    and a, $0F ;2c 2b
    add a,a ;1c 1b
    ld e,a ;1c 1b
    ld d,$00 ;2c 2b
    add hl, de ;2c 1b
    ;dereference hl and hl+1 into hl, without messing with b. a-cde are safe to change
    ld a,[hli] ;2c 1b
    ld h,[hl] ;2c 1b
    ld l,a ;1c 1b
    push hl ;4c 1b

    bit JOYPAD_DOWN, b
    jp z, .up_pressed
  .down_pressed:
    IF SCREEN_FLIP_V == 1
        ld c, $01; Increment the selected nybble
    ELSE
        ld c, $FF; Decrement the selected nybble
    ENDC
    jp .mod_nybble

    .up_pressed:
    IF SCREEN_FLIP_V == 1
        ld c, $FF; Increment the selected nybble
    ELSE
        ld c, $01; Decrement the selected nybble
    ENDC

    .mod_nybble:
    ;make sure not to modify B, or C or HL if calling mod_nybble. however, we need to use hl for jp. so push hl instead and have ModifyNybble pop it off stack
    ld hl, ChangeOptionHandler_table
    ldh a, [MENU_POSITION]
    and a, $0F
    add a,a
    add a,l
    ld l, a ;add a into hl -- since hl is byte-aligned, we only need to add into l 
    ;load the address from the table entry in [hl] into hl
    ld a, [hli];low addr byte
    ld h, [hl] ;high addr byte
    ld l, a
    jp hl
  .modifyValueTail:

;Change the displayed nybble value here, or signal that it should be changed in VBlank
;Or just have Vblank draw every nybble every frame!

  .dpad_updown_end:
  ldh a, [joypad_active] ; b is holding the joypad_active
  ld b,a
  and a, %00110000
  jp z, .leftright_end
  ;Either left or right is pressed: which one? If bit 5, left. Else, right.
  ldh a,[MENU_NYBBLE]
  and a, $0F ; modify LOW(current) nybble of MENU_NYBBLE
  bit JOYPAD_LEFT, b
  jp z,.right_pressed ;Check LEFT
  ;Change nybble number, then move it within range if it goes outside of it
  ;By the end of this, the correct value for our new nybble position (%000000ab) will be in c
  .left_pressed:
  IF SCREEN_FLIP_H == 1
    call Dec_nybble_position_a_into_c
  ELSE
    call Inc_nybble_position_a_into_c
  ENDC
  jr .writeback_nybble_position_c

  .right_pressed:
  IF SCREEN_FLIP_H == 1
    call Inc_nybble_position_a_into_c
  ELSE
    call Dec_nybble_position_a_into_c
  ENDC

  .writeback_nybble_position_c:
    ;writeback nybble position, held in c
    ldh a,[MENU_NYBBLE]
    swap a   ;Swap old position into high nybble
    and a, $F0 ; clear low nybble
    or a,c ; OR in our new nybble position, assumes higher bits of c are all 0
    ldh [MENU_NYBBLE], a
  call MoveCursorSpriteToSelectedNybble

.leftright_end:
  ldh a, [joypad_active]
  and a, (JOYPAD_START_MASK | JOYPAD_SELECT_MASK |JOYPAD_B_MASK | JOYPAD_A_MASK)

  jp z, .no_buttons_pressed
  ld b,a
  bit JOYPAD_START, b ;check start
  jp z, .check_select

  .check_select:
  bit JOYPAD_SELECT, b
  jp z, .check_a

  .check_a:
  bit JOYPAD_B, b
  jp z, .check_b
  call InitMenuState_CameraOpts

  .check_b:
  bit JOYPAD_A,b
  jp z,.buttons_end

  .buttons_end:

  .no_buttons_pressed:

jp HandleInputDone

MenuHandler_TakeConfirm:

  ;ignore upper 4 bits:don't care about dpad (later, we can swap between two camera shots, since we're holding two in VRAM anyway)
  ldh a, [viewfinder_state]
  cp a, VF_STATE_PAUSED
  jp nz, .no_buttons_pressed ;if state isn't VF_STATE_PAUSED, don't take input (otherwise we might be able to try to save a capture while the camera is still capturing)

  ldh a, [joypad_active]
  .check_a:
  bit JOYPAD_B, a
  jp z, .check_b ;check A BUTTON
  ;A button pressed: save picture to free slot, then go back to the previous menu state.
  ;Save picture to free slot (saving to free slot assumed there exists a free slot and will fail silently -- check for free space before entering this menu)

    ;Switch to SRAMbank 0 and enable writing to it.
    ld h,$0A 
    ld [hl],h ;enable SRAM writes
    ld h, HIGH(rRAMB) ;2c2b
    ld [hl], $00 ;3c2b ; switch to SRAM bank 0: state vector
    ;Find a free slot in the state vector and update the state vector while you're there, but keep ahold of the correct bank index
    call StateVector_FindAndFillFreeSlot ;the slot index + 1 is held in c
    ld a, c
    and a
    jp z, .save_cleanup ;skip saving if FindAndFillFreeSlot returns 0

    dec c
    ;the bank we want to switch to, held in c, should be 1 + (slot index>>1).
    ;Copy image data to the correct slot
    bit 0, c
    ld e, $FF ;if slot index is even
    jr z,:+
    ld e, $0F     ;if slot index is odd, set the value to add/sub from HIGH(src/dest pointer) to $0F. Otherwise, use -1 (FF)
  :
  ;shift right and add 1 to get the bank number
  srl c
  inc c

  call SaveCaptureDataFromSRAM0


  ;SaveCaptureDataFromSRAM0 ends with the bank we want to write to already selected
  ;if e is unchanged by this function, we can use it to decide whether we should go with AE00 or BE00
  ld hl, $AE00
  inc e ;this is 0 if e is FF (index is even -- we want to pass AE00)
  jp z, :+
  ;if it's not FF, index is odd -- we want to pass BE00
  ld h, $BE
  ;load HL as either AE00 or BE00
  :call GenerateThumbnail

  ;Now that we've saved, update the vertical UI buffer with the new free value
  ld hl, SAVE_SLOTS_FREE
  IF SCREEN_FLIP_H
  ld de, UIBuffer_Vertical+3
  ELSE
  ld de, UIBuffer_Vertical+2
  ENDC
  call UpdateByteInTilemap

  ;Update the backup state vector upon successful save completion
  call StateVector_Backup

  .save_cleanup
  ld h,$00
  ld [hl],h ; disable SRAM writes

  jp .exit_take_confirm_menustate ;don't fall through to check for B button when the A button was already pressed

  .check_b: ; check B BUTTON
  ldh a, [joypad_active]
  bit JOYPAD_A,a
  jp z,.no_buttons_pressed   ;B button pressed: go back to previous menu state without saving capture.

  ;This should be jumped to if either A or B is pressed
  .exit_take_confirm_menustate:
  ;undraw the UI for this and return it to whatever it was

  ;change the menustate
  call InitMenuState_CameraOpts
  .no_buttons_pressed:

jp HandleInputDone

MenuHandler_Gallery:

jp HandleInputDone

MenuHandler_DeleteConfirm:

jp HandleInputDone



;----------------------Functions---------------------------
; These are called when entering and returning to menu states
InitMenuState_CameraOpts::
  xor a
  ldh [MENU_STATE], a
  ldh [MENU_POSITION], a
  call MoveCursorSpriteToMenuPosition
  ret

InitMenuState_Selected:
  ld a, MENU_STATE_SELECTED
  ldh [MENU_STATE], a
  xor a
  ldh [MENU_NYBBLE], a
  call MoveCursorSpriteToSelectedNybble
  ret

InitMenuState_TakeConfirm:
  ld a,[SAVE_SLOTS_FREE] ;Only enter this state if the number of free entries is non-zero
  and a
  ret z ;return silently if there are no free slots
  ld a, MENU_STATE_TAKE_CONFIRM
  ldh [MENU_STATE], a
  call MoveCursorSpriteToNowhere ;We will need to undo this upon EXITING the Take_Confirm menustate
  ;Display the take confirm menu (which at the moment will just be changing the bottom-left vertical tile to display a confirmation tile) 
  ret


MoveCursorSpriteToNowhere:
  xor a
  ld [Sprite0_CursorY],a
  ret



;Clobbers hl, e
;Moves cursor to nybble (relative to its icon)
MoveCursorSpriteToSelectedNybble:
call MoveCursorSpriteToMenuPosition
;Move the cursor down 8px
ld hl,Sprite0_CursorY
ld a, [hl] ;2c 1b
add a, $08 ;2c 2b
ld [hli], a ;write to CursorY; hl now points to CursorX
;Move cursor right 8*(3-MENU_NYBBLE) if not flipped, -8*(MENU_NYBBLE if Hflipped)
ldh a,[MENU_NYBBLE]
and a, $0F ; only use current (low nybble) of MENU_NYBBLE

add a,a ; Multiply by 8
add a,a
add a,a
IF SCREEN_FLIP_H==0
  cpl   ;make the offset a negative number, such that the cursor moves left as you point to more significant nybbles
  inc a
ENDC
add a,[hl] ;If H flipped, a = Cursor position + offset calculated above. If not Hflipped, subtract the offset instead.

ld [hl], a

ret


;Moves the cursor sprite in the working OAM buffer based on MENU_POSITION 
;Y position is 16 (if menu position is 0,1,2,3,4) or 32 (if 5,6,7,8,9)
;X position is 8 + (4*8*MENU_POSITION=menu position << 5), but we must mod the menu position by 5 (since it's only 2 rows, subtract 5 if menu position >= 5)
;Clobbers e, hl
MoveCursorSpriteToMenuPosition:
  ld e, $20 ;holds Y position -- $20 if MENU_POSITION greater than 4
  ld hl, Sprite0_CursorX ; holds the address of the cursor location
  ldh a, [MENU_POSITION]
  and a, %00001111 ; only use CURRENT menu position
  sub a, MENU0_ROWLEN ;if carry, LOW(MENU_POSITION) < 5
  jp nc, :+
    ;if carry, add 5 to a to return to MENU_POSITION, put $10 in e (y-pos)
    ld e, $10 ;2b2c
    add a, MENU0_ROWLEN

    :rrca ;multiply a by 4*8
    rrca 
    rrca 
    add a,$08 + (24*(SCREEN_FLIP_H ^ $01)) ;add 8 to get x-position on-screen, and right 3 tiles if not flipped
    ld [hld], a
    ld [hl], e
  ret



;incs/decs a nybble in [HL], within min/max values defined in global tables
;arg HL address of byte to change (1-byte or 2-byte little-endian values)
;arg c = $01 for add, $FF for subtract
ModifyNybble::
  ldh a, [MENU_NYBBLE]
  bit 1, a    ;select high byte if MENU_NYBBLE is %xxxxxx1x -- this works only for LITTLE-ENDIAN values.
  jr z,:+
      inc hl
  :bit 0, a ;change addend to modify the high nybble if MENU_NYBBLE is odd.
  ld a,c
  jr z,:+
      and a,$F0   ;turn an $FF into an F0 (-$10)
      or a, $10   ;or an $01 into an $10
  :add a, [hl] ;add or subtract 1 from the selected nybble
  ld [hl], a ;write it back
  
  call GetMin_ConfigVal ;bounds check
  cp a, [hl]
  jr c, :+
  jr z, :+
  ld [hl], a ;write min value to byte
  jr :++ ; skip max check if you've already hit min
  
  :call GetMax_ConfigVal
  cp a, [hl]
  jr nc, :+
  jr z, :+
  ld [hl], a ;write max value to byte
  :
  ret

;Gets the minimum allowable value for a byte based on MENU_POSITION and returns it in A
;Doesn't check two-byte values, but the only two-byte value (C, exposure) has no limits for each byte.
;Keep in mind, this checks the entire byte, not a single nybble, so whatever uses this function should change the BYTE back to the min
;Preserves HL
;Clobbers a, de
GetMin_ConfigVal::
  push hl
  ldh a,[MENU_POSITION]
  and a,$0F
  ld  hl, SelectedMinTable
  ld e, a
  ld d, $00
  add hl, de
  ld a, [hl]

  pop hl
  ret
GetMax_ConfigVal::
  push hl
  ldh a,[MENU_POSITION]
  and a,$0F
  ld hl, SelectedMaxTable
  ld e,a
  ld d,$00
  add hl,de
  ld a,[hl]

  pop hl
  ret

Dec_nybble_position_a_into_c:
  ;Dec nybble
  ;Dec r8 doesn't have an overflow flag, only Z, so we'll need to sub $01
  ;Carry check only works if we cleared out higher bits -- decrementing from %1000 0000 will cause the higher nybble to change without an overflow set
  sub a, $01
  jr nc, :+
  xor a ; if a went below 0, set it back to 0
  :ld c,a
  ret

;Changes MENU_NYBBLE and does bound-checking to ensure MENU_NYBBLE is only on valid nybbles
Inc_nybble_position_a_into_c:
  inc a ;Inc MENU_NYBBLE
  ;Check whether it goes over the maximum allowed nybble position for this menu option
  ;This check will go into its own GetMaxNybblePosition function if it gets long enough
  ld c, a ;c = tentative nybble position that we want to check
  ld hl, SelectedMaxNybblesTable
  ldh a, [MENU_POSITION]
  and a, $0F
  ld d,$00
  ld e, a
  add hl, de ; HL is now &SelectedMaxNybblesTable[MENU_POSITION]
  ld a,[hl] ; a = max nybble position
  cp a, c ;carries if c>a (tentative is greater than max) -- out-of-bounds
  jr nc, :+     ; At the end of this, reg c will contain the nybble position to write back
  ld c,a ; If there was a carry, we need to set c to a -- our max value
  :ret ;Otherwise, we can leave c, as it is within bounds 

ModifyCamOptN_UI:
  pop hl ;we pushed hl 
  call ModifyNybble
  jp MenuHandler_Selected.modifyValueTail
ModifyCamOptVH_UI:
  pop hl
  call ModifyNybble
  jp MenuHandler_Selected.modifyValueTail
ModifyCamOptC_UI: 
  pop hl
  call ModifyNybble
  jp MenuHandler_Selected.modifyValueTail
ModifyCamOptO_UI:
  pop hl
  call ModifyNybble
  jp MenuHandler_Selected.modifyValueTail
ModifyCamOptG_UI:
  pop hl
  call ModifyNybble
  jp MenuHandler_Selected.modifyValueTail
ModifyCamOptE_UI:
  pop hl
  call ModifyNybble
  jp MenuHandler_Selected.modifyValueTail
ModifyCamOptV_UI:
  pop hl
  call ModifyNybble
  jp MenuHandler_Selected.modifyValueTail
ModifyContrast_UI:
  ;Set dither table modified flag
  pop hl
  call ModifyNybble
  ;TODO switch to the appropriate ROM bank holding the dither bases
  call PrepareDitherPattern
  jp MenuHandler_Selected.modifyValueTail
ModifyDitherTable_UI:
  pop hl
  call ModifyNybble
  ;TODO switch to the appropriate ROM bank holding the dither bases
  call PrepareDitherPattern
  jp MenuHandler_Selected.modifyValueTail
ModifyDitherPattern_UI:
  pop hl
  call ModifyNybble
  jp MenuHandler_Selected.modifyValueTail
ModifyEdgeMode_UI:
  pop hl
  call ModifyNybble
  ;Modify working N/VH registers based off the value in EdgeControlModes[CamOptEdgeMode]
  call SetNVHtoEdgeMode
  jp MenuHandler_Selected.modifyValueTail
  




;Clobbers hl, b, a
;arg c: dest bank number to copy to
;arg e: value to add to HIGH(src pointer) to get dest pointer: $0F for B0 (odd-numbered slot) or $FF for A0 (even-numbered slot)
SaveCaptureDataFromSRAM0:
ld d,HIGH(rRAMB) ;d: location of the SRAM switching write
ld h, $A1 ; hl: source pointer and starting point for dest pointer. If the bank to write to is even, we subtract 1 from A1 to get A0. If it's odd, we add 0F to get B0
ld l, $00
.loop ;>20 cycles per byte, $E00 bytes = over 71680 cyles -- should be fine if it takes a few frames
  xor a
  ld [de], a ; switch to src bank
  ld b, [hl] ; b is src byte
  ld a, c
  ld [de], a ; switch to dest bank
  ;add e into h to get the dest address
  ld a,h
  add a,e ; a = h+e
  ld h,a
  ld [hl], b ; write byte
  ;sub e from h to get the src address again
  ld a,h
  sub a,e ; a = h-e
  ld h,a
  inc hl
  ld a,h ; if h is $AF, we've reached the end
  cp a,$AF
  jr nz,.loop
  ret

/*  read from SRAM bank X:A000 or B000 (we don't need to know the bank, since we won't bankswitch if we're in the same bank as our thumbnail)
  generates a 32x32 (effectively,32x28) thumbnail. This is 2^(8) bytes, or from xE00-xEDF
  hl will be the dest pointer -- (passed in as AE00 or BE00 depending on the index)
  prior to calling, bankswitch to the correct bank and load the source/dest pointers
  Since I don't understand how thumbnails are generated, I will just copy a pixel from each of the 4x4-pixel areas into the thumbnail
*/
GenerateThumbnail:
  ;hl is passed in as AE00 or BE00
  ;c is our y-value "loop variable." Every 16 y-values, instead of resetting the pointer to the beginning of the line, reset it to the beginning of the NEXT row of tiles
  ld c, $00
  
.outerloop  
  .ConvertThumbnailAddressToSourceRange:
  ;convert from dest address in HL to source address in de. This is the address of the start of a line containing 4 source tiles
  ld a, h
  and a, $F0 ; get high nybble of h, either a or b
  ld d, a
  ld e, $00

  bit 7,l ; 2c 2b
  jr z,:+ ;3/2c 2b
  set 3,d ;2c 2b
  :bit 6,l
  jr z,:+
  set 2,d
  :bit 5,l
  jr z,:+
  set 7,e
  :bit 4,l
  jr z,:+
  set 6,e
  :bit 3,l
  jr z,:+
  set 1, d
  :bit 2,l
  jr z,:+
  set 0, d
  :bit 1,l
  jr z,:+
  set 3,e
  :bit 0,l
  jr z,:+
  set 0,e

  macro SHIFT_6_2_A_TO_B ;shifts bits 6 and 2 from a into reg -- from Photo!
        rla
        rla
        rl b
        rla
        rla
        rla
        rla
        rl b
  endm
  macro  ADD_A_DE
        add e
        ld e, a
        adc d
        sub e
        ld d, a
  endm
  
    ;shifts values from de into b
  macro SHIFT_LINE_INTO_THUMBNAIL_BYTE
        rept 3
        ld a,[de]
        SHIFT_6_2_A_TO_B
            ld a, $10
            ADD_A_DE
        endr
        ld a,[de]
      SHIFT_6_2_A_TO_B
  endm
  
  :SHIFT_LINE_INTO_THUMBNAIL_BYTE   ;read from source address range (4 tiles)
  ld [hl], b ;writeback b into [hli]
  inc l

  bit 7,l
  jp z, .outerloop
  bit 6,l
  jp z, .outerloop
  bit 3,l
  jp z, .outerloop ;these 3 test for C8,D8,E8,or F8. Going into the 8 bytes past these ranges are beyond the image data, and should not be populated.
  ;If it's the first three, add 8 and loop
  ld a,l
  add a,8   ;if l is F8, this will overflow and we should end
  ld l,a
  jp nc, .outerloop

  ret

;Modify working N/VH registers based off the value in EdgeControlModes[CamOptEdgeMode]
;@param: hl: address of CamOptEdgeMode
;@clobber: b, hl, a
SetNVHtoEdgeMode:
  ld a, [hl] ;a = CamOptEdgeMode
  ld hl, EdgeControlModes
  add a, l
  ld l,a
  ld a, [hl] ;a = EdgeControlModes[CamOptEdgeMode]
  ld b, a
  srl b ; b = VH
  and a, $01 ; a = N

  ;Load N VH (regs a b) into CamOpt variables so they can be used in the next capture
  /*ld [CamOptN_RAM], a 4c3b     
  ld a, b 1c1b                 
  ld [CamOptVH_RAM], a 4c3b   */     
  ld hl, CamOptN_RAM;3c3b
  ld [hli], a ;2c1b ;load a into N working reg
  ld [hl],b ;2c1b  ;load b into VH working reg <- HL method uses hl to save 2c2b, requires N/VH work registers be contiguous
  ret

;Restores Rambank 0 from backup stored in BACKUP_BANK
RestoreBank0:
  ld a, BACKUP_BANK
  ldh [rSVBK], a   ;bank switch to backup bank
  ld hl, $D000
  ld de, $C000
  :ld a, [hli]
  ld [de],a
  inc de
  bit 4,h
  jr nz, :-
  ret

ENDL
