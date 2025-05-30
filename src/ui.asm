INCLUDE "src/hardware.inc"

INCLUDE "src/general.inc"

SECTION "Payload UI Data SECTION",ROM0[$1000 + ($1000*UI_RAMBANK)]
UIStorage::
    LOAD "Payload UI Data LOAD", WRAMX [$D000]
UIJumpTable: ;14 bytes
dw MenuHandler_CameraOpts, MenuHandler_DitherOptions, MenuHandler_Selected, MenuHandler_TakeConfirm, MenuHandler_Gallery, MenuHandler_DeleteConfirm, MenuHandler_Settings, MenuHandler_BurstShot

  ;determines what happens when you press up/down to modify a value during MENU_SELECTED menustate
ChangeOptionHandler_table:
  MACRO X
  dw \6
  ENDM
  INCLUDE "src/ui_elements.inc"

Settings_ApressHandler_table:
  MACRO X
  dw \8
  ENDM
  INCLUDE "src/settings.inc"
  ASSERT .end < $D100, "Tables in ui.asm are not aligned on 256 bytes"
    .end

Settings_LogicalLeftHandler_table:
  MACRO X
  dw \6
  ENDM
  INCLUDE "src/settings.inc"

Settings_LogicalRightHandler_table:
  MACRO X
  dw \7
  ENDM
  INCLUDE "src/settings.inc"

;\4 is min, 5 is max.
Settings_MinMaxTable:
  MACRO X
  IF (\3==1)
    DEF MIN_LABELNAME EQUS "\\2_Min"
    DEF MAX_LABELNAME EQUS "\\2_Max"
    {MIN_LABELNAME}: db \4
    {MAX_LABELNAME}: db \5
    purge MIN_LABELNAME
    purge MAX_LABELNAME
  ENDC
  ENDM
  INCLUDE "src/settings.inc"


OnTakeActionStrings:
    db "SAVE"
    db "PRNT"
    db "S PR"
    db "XFER"

ModeStrings:
    db "ONE "
    db "BRST"
    db "AEB "

SidebarArrangementViewfinder:
  db (SidebarArrangementViewfinder.end - SidebarArrangementViewfinder) - 1 ;size of this data structure -- this should really be a struct
  db BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID
  db ACTION_MODIFY, BLANK_TILE_ID, BLANK_TILE_ID, PROMPT_B;Modify: B button
  db BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID
  db ACTION_TAKEPHOTO, BLANK_TILE_ID, BLANK_TILE_ID, PROMPT_A;Take photo: A button
  db BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID
  db ACTION_HANDOVER, BLANK_TILE_ID, PROMPT_UP, PROMPT_SELECT;Cart Handover: 
  .end
  ASSERT SidebarArrangementViewfinder.end - SidebarArrangementViewfinder <= 52, "SidebarArrangementViewfinder is too large"

SidebarArrangementSelected:
  db (SidebarArrangementSelected.end - SidebarArrangementSelected) - 1 ;size of this data structure -- this should really be a struct
  db BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID
  db ACTION_TAKEPHOTO, BLANK_TILE_ID, BLANK_TILE_ID, PROMPT_A ;Take photo: A button
  db BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID
  db ACTION_HANDOVER, BLANK_TILE_ID, PROMPT_UP, PROMPT_SELECT;Cart Handover: select+up
  db BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID
  db ACTION_OPTIONS, BLANK_TILE_ID, BLANK_TILE_ID, PROMPT_START;Settings: start
  .end
ASSERT SidebarArrangementSelected.end - SidebarArrangementSelected <= 52, "SidebarArrangementSelected is too large"

SidebarArrangementTakeConfirm:
  db (SidebarArrangementTakeConfirm.end - SidebarArrangementTakeConfirm) - 1 ;size of this data structure -- this should really be a struct
  db BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID
  db ACTION_OK, BLANK_TILE_ID, BLANK_TILE_ID, PROMPT_A;Confirm: A button

  db BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID
  db ACTION_RETURN, BLANK_TILE_ID, BLANK_TILE_ID, PROMPT_B;Cancel: B button
  .end

  ASSERT SidebarArrangementTakeConfirm.end - SidebarArrangementTakeConfirm <= 52, "SidebarArrangementTakeConfirm is too large"

SidebarArrangementBurstShot:
db (SidebarArrangementBurstShot.end - SidebarArrangementBurstShot) - 1 ;size of this data structure -- this should really be a struct
db BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID
db BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID, BLANK_TILE_ID
.end

ASSERT SidebarArrangementBurstShot.end - SidebarArrangementBurstShot <= 52, "SidebarArrangementBurstShot is too large"

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
      bit JOYPAD_UP, b
      jr z,:+
      call StartHandover

    :bit JOYPAD_A, b; check A - take picure
    jr z,:+
      call InitMenuState_TakeConfirm
      jp .end ; don't check for b button if we pressed a -- only one state transition at once.
    :bit JOYPAD_B, b ;check B - select a value to change
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
    ;We don't need to preserve B before calling one of the value modifiers, only C. HL is used to jump

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
  jr z,.right_pressed ;Check LEFT
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
  ldh a, [joypad_active]
  ld b,a
  bit JOYPAD_START, b ;check start
  jp z, .check_select

  .check_select:
  bit JOYPAD_SELECT, b
  jr z, .check_a
  ;Start handover, possibly after waiting for capture to complete
    bit JOYPAD_UP, b
    jr z,.check_a
    call StartHandover
    jp .buttons_end

  .check_a:
  bit JOYPAD_A,b
  jp z,.check_b

  ;Check whether burst shot setting is set. If it is, initiate its appropriate menustate and signal to the viewfinder to start a burst shot.
  ;Remember that for burst shots, you can't use an already-initiated capture -- you need to set the capture parameters first, then take them.

  ld a,[Setting_Burst_AEB]
  and a
  jr z,:+
    call InitMenuState_BurstShot
    jp .buttons_end
  :
    call InitMenuState_TakeConfirm
    jp .buttons_end ;we don't want to do any other state transitions if we transition to CameraOpts, so skip the other button checks

  .check_b:
  bit JOYPAD_B, b
  jp z, .check_start
  ;call InitMenuState_CameraOpts
  jp .buttons_end ;we don't want to do any other state transitions if we transition to CameraOpts, so skip the other button checks

  .check_start:
  bit JOYPAD_START, b
  jp z, .buttons_end
  call InitMenuState_Settings
  jp .buttons_end

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
  bit JOYPAD_A, a
  jp z, .check_b ;jump to check B BUTTON
  ;A button pressed: save picture to free slot, then go back to the previous menu state.
  ;Save picture to free slot (saving to free slot assumed there exists a free slot and will fail silently -- check for free space before entering this menu)

  call SaveToFreeSlot
  jp .exit_take_confirm_menustate ;don't fall through to check for B button when the A button was already pressed

  .check_b: ; check B BUTTON
  ldh a, [joypad_active]
  bit JOYPAD_B,a
  jp z,.no_buttons_pressed   ;B button pressed: go back to previous menu state without saving capture.

  ;This should be jumped to if either A or B is pressed
  .exit_take_confirm_menustate:
  ;undraw the UI for this and return it to whatever it was

  ;change the menustate
  call InitMenuState_Selected
  .no_buttons_pressed:

jp HandleInputDone

MenuHandler_Gallery:

jp HandleInputDone

MenuHandler_DeleteConfirm:

jp HandleInputDone

MenuHandler_Settings:

  ;Decrease SettingsPosition by 1 and bounds-check
  macro DEC_SETTINGS_POSITION_INTO_A
    ldh a,[SettingsPosition]
    and a
    jr z,:+	
    dec a
    :
  endm

;Increase SettingsPosition by 1 and bounds-check
  macro INC_SETTINGS_POSITION_INTO_A
    ldh a,[SettingsPosition]
    cp a,NUM_SETTINGS-1
    jr z,:+
    inc a
    :
  endm	

;Don't take input if the viewfinder state is not paused
  ldh a, [viewfinder_state]
  cp a, VF_STATE_PAUSED
  jp nz, .no_buttons_pressed ;if state isn't VF_STATE_PAUSED, don't take input

  .check_up
  ldh a,[joypad_active]
  bit JOYPAD_UP, a
  jp z,.check_down
  IF !SCREEN_FLIP_V
    DEC_SETTINGS_POSITION_INTO_A
  ELSE
    INC_SETTINGS_POSITION_INTO_A
  ENDC
  ldh [SettingsPosition],a
  xor a
  ldh [SettingsNybble],a
  call MoveCursorSpriteToSettingsPosition

  .check_down
  ldh a,[joypad_active]
  bit JOYPAD_DOWN, a
  jp z,.check_left
  IF SCREEN_FLIP_V
    DEC_SETTINGS_POSITION_INTO_A
  ELSE
    INC_SETTINGS_POSITION_INTO_A
ENDC
ldh [SettingsPosition],a
xor a
ldh [SettingsNybble],a
call MoveCursorSpriteToSettingsPosition

  .check_left
  ldh a,[joypad_active]
  bit JOYPAD_LEFT, a
  jp z,.check_right
    ;we have a jump table containing all the left-press handlers. Dereference that table based on Settings_Position
    ld hl,.LeftPressHandler_return
    push hl
    IF !SCREEN_FLIP_H
      ld hl, Settings_LogicalLeftHandler_table
    ELSE
      ld hl, Settings_LogicalRightHandler_table
    ENDC   
    ldh a,[SettingsPosition]
    add a,a
    add a,l
    ld l,a
    ld a,[hli]
    ld e,a
    ld h, [hl]
    ld l,e
    jp hl
  .LeftPressHandler_return


  .check_right
  ldh a,[joypad_active]
  bit JOYPAD_RIGHT, a
  jp z,.check_b
    ;we have a jump table containing all the right-press handlers. Dereference that table based on Settings_Position
    ld hl,.RightPressHandler_return
    push hl
    IF !SCREEN_FLIP_H
      ld hl, Settings_LogicalRightHandler_table
    ELSE
      ld hl, Settings_LogicalLeftHandler_table
    ENDC
    ldh a,[SettingsPosition]
    add a,a
    add a,l
    ld l,a
    ld a,[hli]
    ld e,a
    ld h, [hl]
    ld l,e
    jp hl
  .RightPressHandler_return

  ldh a,[joypad_active]
  .check_b
  bit JOYPAD_B,a
  jp z, .check_a
    call InitMenuState_Selected
    jp .end

  ldh a, [joypad_active]
  .check_a:
  bit JOYPAD_A, a
  jp z, .no_buttons_pressed
    ;we have a jump table containing all the A-press handlers. Dereference that table based on Settings_Position
    ld hl,.Apresshandler_return
    push hl
    ld hl, Settings_ApressHandler_table
    ldh a,[SettingsPosition]
    add a,a
    add a,l
    ld l,a
    ld a,[hli]
    ld e,a
    ld h, [hl]
    ld l,e
    jp hl
  .Apresshandler_return
  

  .no_buttons_pressed
.end

call DrawSettings

jp HandleInputDone


MenuHandler_BurstShot:
;Don't do anything until the burst shot is finished
; If current action is burst, wait for it to finish.
ld a,[vfCurrentAction]
cp a,VF_ACTION_BURST
jp z, HandleInputDone
;If current action isn't burst, then next action should be.
ld a,[vfNextAction]
cp a,VF_ACTION_BURST
jp z, HandleInputDone

;If neither current nor next action is burst, either it's finished or something has gone wrong.
;Either way, transition to the Selected state
call InitMenuState_Selected
jp HandleInputDone
;----------------------Functions---------------------------
; These are called when entering and returning to menu states

;@param hl: address of actions/prompts table used to fill sidebar
PrepareSidebar:
	ld a,[hli] ; size to memcpy, hl now points to start of data 2c1b
	ld c,a ; size for memcpy_8bit, hl set to source
	;size for second memcpy is 52-size
	ld a,52 	;2c2b
	sub a,c	;1c1b
	ld b,a ; b = 52-c ;1c1b
	ld de, SidebarBuffer+4 ;ignore the first line, which holds free image slots
	call memcpy8_hl_to_de ;after this, de should be the start of our next region (dest)
	ld a,BLANK_TILE_ID ;2c2b
	call memfill8_a_into_de_sizeb ;10c4b call/ret, but memfill may be faster than memcpy, compensating for the decreased cycles
  ret

InitMenuState_CameraOpts::
  xor a
  ldh [MENU_STATE], a
  ldh [MENU_POSITION], a
  call MoveCursorSpriteToMenuPosition
  
  ld hl, SidebarArrangementViewfinder   ;Fill sidebar buffer with new prompts
  call PrepareSidebar

  call SetBGPalette0to0

  ret

InitMenuState_Selected::
  ld a, MENU_STATE_SELECTED
  ldh [MENU_STATE], a
  ;make sure MENU_POSITION is within range
  ldh a,[MENU_POSITION]
  and a,$0F
  cp a,$09
  jr c,:+ ; if carry, it's within range. Else 
  ld a, $09
  ldh [MENU_POSITION],a
  :

  ;make sure MENU_NYBBLE is within range by checking SelectedMaxNybblesTable[MENU_POSITION] and aliasing to it
  ldh a,[MENU_NYBBLE]
  and a,$0F
  ld c,a ;C = current menu nybble

  ld hl, SelectedMaxNybblesTable
  ldh a,[MENU_POSITION]
  and a,$0F
  add a,l
  ld l,a
  ld a, [hl]
  cp a,c ;a -c :  max nybble position - menu_nybble -- carry if out-of-bounds, no carry if in-bounds
  jr nc,:+   ;load max nybble position into LSN of MENU_NYBBLE
  swap c ; c = old4:0000, a = 0000:max nybble position
  or a,c
  ldh [MENU_NYBBLE], a
  :
  call MoveCursorSpriteToSelectedNybble

  ld hl, SidebarArrangementSelected   ;Fill sidebar buffer with new prompts
  call PrepareSidebar  
  call SetBGPalette0to0

  ld a,$70
  ldh [rSCY], a
  xor a
  ldh [rSCX], a
  ld a,[wLCDC] ;Switch to tilemap 0
  res 3,a
  ld [wLCDC],a

  ret

InitMenuState_TakeConfirm:
  ld a,[SAVE_SLOTS_FREE] ;Only enter this state if the number of free entries is non-zero
  and a
  ret z ;return silently if there are no free slots

  ld a,VF_ACTION_TAKE_SINGLE
  ldh [vfNextAction],a
  ld a, MENU_STATE_TAKE_CONFIRM
  ldh [MENU_STATE], a
  call MoveCursorSpriteToNowhere ;We will need to undo this upon EXITING the Take_Confirm menustate
  
  ld hl, SidebarArrangementTakeConfirm   ;Fill sidebar buffer with new prompts
  call PrepareSidebar

  call SetBGPalette0to1
  ret

InitMenuState_BurstShot:

  ld a,[Setting_AEB_Count]
  ld c,a
  ld a,[SAVE_SLOTS_FREE] ;Only enter this state if the number of free entries is GTE the number of shots you're supposed to take
  cp a,c ;count - free: if z, OK. If carry, count > free, and we should return
  ret c ;return silently if there are not enough free slots

  ld a,VF_ACTION_BURST
  ldh [vfNextAction],a
  ld a, MENU_STATE_BURST_SHOT
  ldh [MENU_STATE], a
  call MoveCursorSpriteToNowhere ;We will undo this upon exiting the BurstShot menustate / entering the Selected menustate
  
  ld hl, SidebarArrangementBurstShot   ;Fill sidebar buffer with new prompts
  call PrepareSidebar

  call SetBGPalette0to1
  ret

InitMenuState_Settings:
  ld a, MENU_STATE_SETTINGS
  ldh [MENU_STATE],a

  ld a, VF_ACTION_TAKE_SINGLE
  ldh [vfNextAction],a

  xor a
  ldh [SettingsPosition],a
  ldh [SettingsNybble],a
  ldh [rSCX], a
  IF (SCREEN_FLIP_V)
  ld a,8*(32-18)
  ENDC
  ldh [rSCY], a
  ;move cursor to appropriate position.
  call MoveCursorSpriteToSettingsPosition
  ;set LCDC.3 to 1 (tilemap 1)
  ld a,[wLCDC]
  set 3,a
  ld [wLCDC],a
ret

MoveCursorSpriteToSettingsPosition:
  ;First, move cursor to the appropriate line. Then, move it to the nybble position (rightmost location minus the nybble position)
  ld a,[SettingsPosition]
  add a ;multiply by 8
  add a
  add a
  IF SCREEN_FLIP_V
    sub a,152 ;CursorY: 152-(8*SettingsPosition) if flipped.
    cpl
    inc a
  ELSE
    add a,$10 ; CursorY = 16 + 8*SettingsPosition.
  ENDC
  ld hl,Sprite0_CursorY
  ld [hli],a 
  ld a,[SettingsNybble]
  IF SCREEN_FLIP_H
    add a,8 ;CursorX = 8+SettingsNybble if flipped
  ELSE
    sub a,160 ;CursorX = 160-SettingsNybble
    cpl
    inc a
  ENDC
  ld [hli],a

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
  
  push af ;We don't really need the new value pushed here: it's in [hl]. We need the flags to check for overflow/underflow.
  ld a,$01
  cp a,c
  jr z, .increment_check; if this was called to increment
  .decrement_check;decrement
  pop af ;check for underflow
  jr c, .noUnderflow
  .underflow   ;if underflow occurred, get max, add 1, add it into [hl]
    call GetMax_ConfigVal
    inc a
    add a,[hl]
    ld [hl],a
    ret
  .noUnderflow ;if no underflow, check if new value < min. If it is, add max-min.
    call GetMin_ConfigVal
    sub a,[hl] ;if c or z, [hl]>=min (do nothing)
    ret c
    ret z
    ;if no carry, a = min - newval, and we need to sub max
    ld c,a 
    call GetMax_ConfigVal
    sub a, c ;a = newval -min +max
    inc a ;newval - min + max + 1
    ld [hl], a  
    ret
  .increment_check
  pop af ;check for overflow
  jr nc, .noOverflow
  .overflow ;if overflow occurred, val_new+=min
    call GetMin_ConfigVal
    add a,[hl]
    ld [hl],a
    ret
  .noOverflow ;if no overflow, check whether new value > max. If so, add min-max
  call GetMax_ConfigVal
  sub a, [hl] ;nc or z, [hl]<=max
  ret nc
  ret z
  ;if c and not z, a = newval + min-max. We already subtracted max, so we need to add min to a and write it back
  ;a = -(newval - max). We can subtract it from min to make a = min + newval - max
  ld c,a
  call GetMin_ConfigVal
  sub a,c ; newval + min - max - 1
  dec a
  ld [hl], a
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
  jr nc, .inBounds
  .outOfBounds
  ldh a,[MENU_POSITION]
  and a,$0F
  IF SCREEN_FLIP_H
    sub a,$01
    jr nc,:+
    ld a,$09
    :
  ELSE
    inc a ;increment MENU_POSITION
    cp a,$0A ;if out of right side, set to $90
    jr nz,:+
    ld a,$90
    :
  ENDC
  ;load new MENU_POSITION into memory(old:new)
  ld c,a
  ldh a,[MENU_POSITION]
  swap a
  and a,$F0
  or a,c
  ldh [MENU_POSITION],a
  ; set return value (new MENU_NYBBLE value) to the most significant nybble acceptable for new MENU_POSITION
  ;add LOW(a) to SelectedMaxNybblesTable to get SelectedMaxNybblesTable[MENU_POSITION]
  ld hl, SelectedMaxNybblesTable
  and a, $0F
  ld d,$00
  ld e, a
  add hl, de ; HL is now &SelectedMaxNybblesTable[MENU_POSITION]
  ld a,[hl] ; a = max nybble position
 
  .inBounds
  ld c,a
  ret

;Changes MENU_NYBBLE/MENU_POSITION and does bound-checking to ensure MENU_NYBBLE is only on valid nybbles
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
  jr nc, .inbounds     ; At the end of this, reg c will contain the nybble position to write back
  .outOfBounds 
    ldh a,[MENU_POSITION]
    and a,$0F
    IF SCREEN_FLIP_H
    inc a ;increment MENU_POSITION
    ;if it's greater than the max MENU_POSITION
    cp a,$0A
    jr nz,:+
    ld a,$90 ;last menu position 9, current menu position 0
    :
    ELSE
    sub a,$01 ;decrement MENU_POSITION
    jr nc, :+;if you just went to FF (carry), set it to the last menu item
    ld a,$09 ;last menu position 0, current 9
    :
    ENDC
    ;Load old:new menu position into a, then write it back
    ld c,a
    ldh a,[MENU_POSITION]
    and a,$0F
    swap a
    or a,c
    ldh [MENU_POSITION],a
    xor a
    ld c,a ; If there was a carry, we need to set c to 0
  .inbounds
  ret ;Otherwise, we can leave c, as it is within bounds 

ModifyCamOptN_UI:
  ld hl,CamOptN_RAM
  call ModifyNybble
ModifyNothing_UI:
  jp MenuHandler_Selected.modifyValueTail
ModifyCamOptC_UI: 
  ld hl,CamOptC_RAM
  call ModifyNybble
  jp MenuHandler_Selected.modifyValueTail
ModifyCamOptO_UI:
  ld hl,CamOptO_RAM
  call ModifyNybble
  jp MenuHandler_Selected.modifyValueTail
ModifyCamOptG_UI:
  ld hl,CamOptG_RAM
  call ModifyNybble
  jp MenuHandler_Selected.modifyValueTail
ModifyCamOptE_UI:
  ld hl,CamOptE_RAM
  call ModifyNybble
  jp MenuHandler_Selected.modifyValueTail
ModifyCamOptV_UI:
  ld hl,CamOptV_RAM
  call ModifyNybble
  jp MenuHandler_Selected.modifyValueTail
ModifyContrast_UI:
  ;TODO: Set dither table modified flag
  ld hl,CamOptContrast
  call ModifyNybble
  ;TODO switch to the appropriate ROM bank holding the dither bases
  call PrepareDitherPattern
  jp MenuHandler_Selected.modifyValueTail
ModifyDitherTable_UI:
  ld hl, CamOptDitherTable
  call ModifyNybble
  ;TODO switch to the appropriate ROM bank holding the dither bases
  call PrepareDitherPattern
  jp MenuHandler_Selected.modifyValueTail
ModifyDitherPattern_UI:
  ld hl,CamOptDitherPattern
  call ModifyNybble
  call PrepareDitherPattern
  jp MenuHandler_Selected.modifyValueTail
ModifyEdgeMode_UI:
  ld hl,CamOptEdgeMode
  call ModifyNybble
  ;Modify working N/VH registers based off the value in EdgeControlModes[CamOptEdgeMode]
  call SetNVHtoEdgeMode
  jp MenuHandler_Selected.modifyValueTail
  




;Clobbers hl, b, a
;arg c: dest bank number to copy to
;arg e: value to add to HIGH(src pointer) to get dest pointer: $0F for B0 (odd-numbered slot) or $FF for A0 (even-numbered slot)
SaveCaptureDataFromSRAM0::
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
  call WriteThumbnailMetadata
  ret


;Call this after saving capture data
;Writes metadata to the lower 4 lines of the thumbnail
;Thumbnail location is A/BE00 -- the metadata location is spread out as the last 4 lines (8 Bytes each) of the last 4 tiles
;Prior to calling, bankswitch to the correct bank (since this is usually called right after a copy, the correct bank is already loaded)
;@param e: $0F for odd-numbered slot (BExx), $FF for even-numbered (AExx)
;@clobber: a, hl, bc
WriteThumbnailMetadata:
  ;Set hl to start of metadata location plus 12.5 tiles = A/BE00 + 16*12.5 = $C8 bytes
  ld l,$C8

  ld a, $AE
  push de ; the thumbnail generate code uses e, so we don't want to change it
  inc e ;e is either $10 (for BE) or $00 for AE. Add to h.
  add a,e ; a = $AE + 00 or $10
  ld h,a

  ;write first 8 bytes -- shadow A000-A005
  ;We don't have a shadow A000 since we just write %00000011 into A000 directly to start capture
  ;Inaccurately write static %00000010 instead
  ld a, %00000010
  ld [hli],a
  ld bc, CamOptA001_RAM
  ld a, [bc]
  inc bc
  ld [hli],a ; Write shadow A001
  ld a, [bc]
  inc bc
  ld [hli],a ; Write shadow A002
  ld a, [bc]
  inc bc
  ld [hli],a ; Write shadow A003
  ld a, [bc]
  inc bc
  ld [hli],a ; Write shadow A004
  ld a, [bc]
  ld [hli],a ; Write shadow A005
  ;Write current_exposure
  ; ld bc,CamOptC_RAM
  ; ld a, [bc]
  ; inc bc
  ; ld [hli],a ;low byte
  ; ld a,[bc]
  ; ld [hli],a ; high byte

  ;Jump to next tile
  ;ld l, $D8
  ;current exposure index
  ;This would likely require adding an index->value table.
  ;The web gallery doesn't use it, so I'll leave the other fields unimplemented
    ;xor a,0
    ;ld [hli],a
    ;current gain:
    ;ld a,[CamOptG_RAM]
    ;ld [hli],a
    ;current zero point
    ;current edge ratio
    ;current voltage ref
    ;voltage_out (2bytes)
    ;current contrast

    ;Jump to next tile
    ;ld l,$E8
    ;edge operation
    ;current brightness (2 bytes)
    ;dithering4 : ditheringHighLight1 : invertOutput1 : edge_exclusive1 : cpu_fast
  ;checksum 2 bytes: 

  ;Calculate checksum in pieces: start with the 2 bytes at xEC8+6
  ld l, $C8+6
  ld c,2
  ld d,$55 ;sum seed: I may have gotten endianness wrong and we may need to flip this
  ld e,$AA ;xor seed
  call CalculateChecksum
  ;Jump to next set of 8 bytes at $D8
  ld l, $D8
  ld c,8
  call CalculateChecksum
  ;Jump to next set of 4 bytes at $E8
  ld l,$E8
  ld c,4
  call CalculateChecksum
  ;write to E8+4 and E8+5
  ld l,$E8+4
  ld a,d
  ld [hli],a
  ld [hl], e

  pop de ; restore e so GenerateThumbnail can use it
ret

;@param hl: source address
;@param c: size
;@param d: initial value for sum (lower address)
;@param e: initial value for xor (higher address)
;@return d: sum result
;@return e: xor result
;@clobber a, c, de, hl
CalculateChecksum:

  .loop
  ld a, [hl] ;add to the running sum
  add a,d
  ld d,a
  ld a,[hli] ;add to the running xor
  xor a,e
  ld e,a
  dec c
  jr nz,.loop

ret


/*  read from SRAM bank X:A000 or B000 (we don't need to know the bank, since we won't bankswitch if we're in the same bank as our thumbnail)
  generates a 32x32 (effectively,32x28) thumbnail. This is 2^(8) bytes, or from xE00-xEDF
  hl will be the dest pointer -- (passed in as AE00 or BE00 depending on the index)
  prior to calling, bankswitch to the correct bank and load the source/dest pointers
  Since I don't understand how thumbnails are generated, I will just copy a pixel from each of the 4x4-pixel areas into the thumbnail
*/
GenerateThumbnail::
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

;-----------------Setting Modify Actions-------------------------------------

Setting_SerialRemote_Toggle:
  ld a,[Setting_SerialRemote]
  xor a,$01
  ld [Setting_SerialRemote],a
ret

Setting_OnTakeAction_SetDefault:
  ld a,TAKE_ACTION_SAVE
  ld [Setting_OnTakeAction],a
ret

; Setting_Timer_Toggle:
;   ld a,[Setting_TimerEnable]
;   xor a,$01
;   ld [Setting_TimerEnable],a
; ret

Setting_DelayTime_SetDefault:
  xor a
  ld [Setting_DelayTime],a
ret

Setting_Burst_AEB_SetDefault:
  xor a
  ld [Setting_Burst_AEB],a
  call Setting_Burst_AEB_Sanitize
ret

Setting_AEB_Count_SetDefault:
  ld hl,Setting_AEB_Count

  ld a,[Setting_Burst_AEB]
  cp a,CAMERA_MODE_AEB  
  ld a,$01
  jr nz,:+ ;if we're not in AEB mode, set count to 1
  ld a,$03 ;If we're in AEB mode, set count to 3

  :ld [hl],a


ret

Setting_AEB_Interval_SetDefault:
  ld a,[Setting_AEB_Interval]
  ld a,$03
  ld [Setting_AEB_Interval],a
  call Setting_AEB_Interval_Sanitize
ret

; @arg a: max
; @arg b: min
; @arg hl: address of variable
; @clobber none
IncByteWithWraparound:
  cp a, [hl] ;if a = max, set to min
  jr z,:+
    inc [hl] ;value isn't at max? increment and return
    ret
  :
    ld [hl], b
ret

; @arg a: min
; @arg b: max
; @arg hl: address of variable
; @clobber none
DecByteWithWraparound:
  cp a, [hl] ;if a = min, set to max
  jr z,:+
    dec [hl] ;value isn't at min? dec and return
    ret
  :
    ld [hl], b
ret

Setting_OnTakeAction_Inc:
  ld hl, Setting_OnTakeAction_Min+1
  ld a,[hld] ;max in a
  ld b,[hl]  ;min in b
  ld hl, Setting_OnTakeAction
  call IncByteWithWraparound
ret

Setting_OnTakeAction_Dec:
  ld hl, Setting_OnTakeAction_Min
  ld a,[hli] ;min in a
  ld b,[hl]  ;max in b
  ld hl, Setting_OnTakeAction
  call DecByteWithWraparound
ret

Setting_AEB_Interval_Inc:
  ld hl, Setting_AEB_Interval_Min+1
  ld a,[hld] ;max in a
  ld b,[hl]  ;min in b
  ld hl, Setting_AEB_Interval
  call IncByteWithWraparound
  call Setting_AEB_Interval_Sanitize
ret

Setting_AEB_Interval_Dec:
  ld hl, Setting_AEB_Interval_Min
  ld a,[hli] ;min in a
  ld b,[hl]  ;max in b
  ld hl, Setting_AEB_Interval
  call DecByteWithWraparound
  call Setting_AEB_Interval_Sanitize
ret

Setting_AEB_Count_Inc:
  ld hl, Setting_AEB_Count_Min+1
  ld a,[hld] ;max in a
  ld b,[hl]  ;min in b
  ld hl, Setting_AEB_Count
  call IncByteWithWraparound
  
  ;If mode is AEB(2), bounds-check it's gte $03
  ld a,[Setting_Burst_AEB]
  cp a,$02
  ret nz
  .aebSet
  ;If AEB is set, keep incrementing-with-wraparound until it's an odd number above 3
  ;if it's under 3, set it to 3 and return.
  ld a,[hl]
  cp a,$03 ;count - 3: no carry: in-bounds. Zero: in-bounds. carry: out-of-bounds, set to 3 and return
  jr c,.outOfBounds
  .inBounds
    ;if count >= 3, make it odd by setting bit 0
    set 0,[hl]
    ret
  .outOfBounds
    ld a,$03
    ld [hl],a
ret


Setting_AEB_Count_Dec:
  ld hl, Setting_AEB_Count_Min
  ld a,[hli] ;min in a
  ld b,[hl]  ;max in b
  ld hl, Setting_AEB_Count
  call DecByteWithWraparound
  
  ;If mode is AEB(2), bounds-check it's gte $03
  ld a,[Setting_Burst_AEB]
  cp a,$02
  ret nz
  .aebSet
  ;If AEB is set, keep incrementing-with-wraparound until it's an odd number above 3
  ;if it's under 3, set it to 0F (DecByteWithWraparound won't roll over to 0F since it's not reached the min yet) and return.
  ld a,[hl]
  cp a,$03 ;count - 3: no carry: in-bounds. Zero: in-bounds. carry: out-of-bounds, set to 3 and return
  jr c,.outOfBounds
  .inBounds
    ;if count >= 3, make it odd by setting bit 0
    dec [hl]
    set 0,[hl]
    ret
  .outOfBounds
    ld a,$0F
    ld [hl],a
ret

Setting_DelayTime_Inc:
  ld hl, Setting_DelayTime_Min+1
  ld a,[hld] ;max in a
  ld b,[hl]  ;min in b
  ld hl, Setting_DelayTime
  call IncByteWithWraparound
ret

Setting_DelayTime_Dec:
  ld hl, Setting_DelayTime_Min
  ld a,[hli] ;min in a
  ld b,[hl]  ;max in b
  ld hl, Setting_DelayTime
  call DecByteWithWraparound
ret

Setting_Burst_AEB_Inc:
  ld hl, Setting_Burst_AEB_Min+1
  ld a,[hld] ;max in a
  ld b,[hl]  ;min in b
  ld hl, Setting_Burst_AEB
  call IncByteWithWraparound
  call Setting_Burst_AEB_Sanitize
ret

Setting_Burst_AEB_Dec:
  ld hl, Setting_Burst_AEB_Min
  ld a,[hli] ;min in a
  ld b,[hl]  ;max in b
  ld hl, Setting_Burst_AEB
  call DecByteWithWraparound
  call Setting_Burst_AEB_Sanitize
ret

;This is called when the
Setting_Burst_AEB_Sanitize:
  ld a,[Setting_Burst_AEB]
  ;When the mode is changed,
  and a
  jr nz,.burstCheck
  ;If it's Single (0),
    ret
  .burstCheck
  dec a
  jr nz,.aebCheck
  ;If it's Burst(1), set INTERVAL to 0 (it should stay this way while Burst is set)
    xor a
    ld [Setting_AEB_Interval],a
    ret
  .aebCheck
  dec a
  jr nz,.otherCheck
  ;If it's AEB(2), reset COUNT to 3 (an odd number that's not 1), and SHIFT to 3
    ld a,$03
    ld [Setting_AEB_Count],a
    ld [Setting_AEB_Interval],a
    ret
  .otherCheck
  ;If we reach here, Setting_Burst_AEB has an out-of-bounds value

ret


;If modifying AEB interval, it should be stuck at zero if the mode is burst
;@arg hl: Address of Setting_AEB_Interval
Setting_AEB_Interval_Sanitize:
  ld a,[Setting_Burst_AEB]
  dec a ; z if shot mode is 1 (burst). If mode isn't burst, return: we 
  ret nz
  xor a
  ld [hl],a ; Set Interval to 00
ret

Init_PrintAll:
  ;TODO
ret

Init_SaveSettings:
  ;TODO
ret

Init_SaveCamOpts:
  ;TODO
ret

;Shortcut to delete all photos -- quicker than handover to ROM
Init_DeleteAll:
  ;Capture is not guaranteed to be complete when we enter Settings menustate, so wait for it here.
  ld h,$50 ;Switch to I/O register bank (write to $4000-5FFF with 4 set)
  ld [hl],h
  :ld a, [$A000]  ;Read capture register
  and a,$01
  jr nz, :- ;Wait until bit 0 of A000 is 0 (capture unit not running).


  ;TODO: right now, the save rambank is just located in WRAM0. We should move it to banked RAM. It'll be called from the UI bank, so some functions may need to be turned to trampoline calls.
  ;TODO: create some kind of visual cue to let the user know it worked. 
      ;This should be fine, as no interactions with the save function are especially latency-sensitive
  ;Switch to save WRAMbank, unlock SRAM writes and switch to SRAM bank 0
  call StateVector_EnableWrite
  call StateVector_DeleteAll
  ld h,$00
  ld [hl],h ; disable SRAM writes

  ;Update free count
  ld a,30
  ld hl,SAVE_SLOTS_FREE
  ld [hl], a
  ;Update free count on UI
  ld de, SidebarBuffer+2
  call UpdateByteInTilemap

ret
;------------Setting Draw Actions-----------------------------------------------

;Draws the various settings using dedicated functions
DrawSettings:
  ;TODO replace with X macro -- currently, they are limited to \1-\9
  call DrawSetting_SerialRemote
  call DrawSetting_OnTakeAction
  call DrawSetting_DelayTime
  call DrawSetting_Burst_AEB
  call DrawSetting_AEB_Count
  call DrawSetting_AEB_Interval

ret

;Takes a LOGICAL X,Y (\1,\2) value (rotation-independent) of a tile on the settings screen and returns its address in the tilemap into R16 (\3).
macro SETTINGS_PUT_TILEMAP_ADDR_IN_R16
  DEF TEMPADDR = _SCRN1
  ;Get address of logical line
  IF (SCREEN_FLIP_V)
    DEF TEMPADDR += (31-\2)*32
  ELSE
    DEF TEMPADDR += (\2)*32
  ENDC

  IF (SCREEN_FLIP_H)
    DEF TEMPADDR += 19-\1
  ELSE
    DEF TEMPADDR += \1
  ENDC
  ld \3,TEMPADDR
endm

DrawSetting_SerialRemote:
  SETTINGS_PUT_TILEMAP_ADDR_IN_R16 19,0,HL
  ld a,[Setting_SerialRemote]
  add a,CHECKBOX_TILE_ID_DIS
  ld b,a
  call DrawTileInHBlank
ret

DrawSetting_OnTakeAction:
  ld hl, OnTakeActionStrings ;each entry is 4 bytes long (does not need to be byte-addr-aligned)
  ld a,[Setting_OnTakeAction]
  add a,a
  add a,a ;multiply by 4 for 4-byte entries

	ld d, 0 ; add a into hl: 4 bytes, 5 cycles
	ld e, a
	add hl, de

  SETTINGS_PUT_TILEMAP_ADDR_IN_R16 16,1,de
  call DrawFourTilesInHBlank

ret

DrawSetting_DelayTime:
  ld a,[Setting_DelayTime]
  ld c,a
  swap a
  and a,$0F
  add a,UI_ICONS_BASE_ID
  SETTINGS_PUT_TILEMAP_ADDR_IN_R16 18,2,hl
  ld b,a
  call DrawTileInHBlank

  ld a,c
  and a,$0F
  add a,UI_ICONS_BASE_ID
  ld b,a
  call DrawTileInHBlank

ret

; DrawSetting_Burst_AEB:
;   SETTINGS_PUT_TILEMAP_ADDR_IN_R16 19,3,HL
;   ld a,[Setting_Burst_AEB]
;   add a,CHECKBOX_TILE_ID_DIS
;   ld b,a
;   call DrawTileInHBlank
; ret

DrawSetting_Burst_AEB:
  ld hl, ModeStrings ;each entry is 4 bytes long (does not need to be byte-addr-aligned)
  ld a,[Setting_Burst_AEB]
  add a,a
  add a,a ;multiply by 4 for 4-byte entries

	ld d, 0 ; add a into hl: 4 bytes, 5 cycles
	ld e, a
	add hl, de

  SETTINGS_PUT_TILEMAP_ADDR_IN_R16 16,3,de
  call DrawFourTilesInHBlank

ret

DrawSetting_AEB_Count:
  ld a,[Setting_AEB_Count]
  add a,UI_ICONS_BASE_ID
  ld b,a
  SETTINGS_PUT_TILEMAP_ADDR_IN_R16 19,4,hl
  call DrawTileInHBlank
ret

DrawSetting_AEB_Interval:

  ld a,[Setting_AEB_Interval]
  ld c,a
  swap a
  and a,$0F
  add a,UI_ICONS_BASE_ID
  SETTINGS_PUT_TILEMAP_ADDR_IN_R16 18,5,hl
  ld b,a
  call DrawTileInHBlank

  ld a,c
  and a,$0F
  add a,UI_ICONS_BASE_ID
  ld b,a
  call DrawTileInHBlank

ret


;Waits for Hblank and draws a single tileID b to tilemap location hl
;To interface with its callee, side effect: Returns with hl incremented if not horizontally flipped
; If horizontally flipped, returns with hl decremented
;This should be called in user-facing left-to-right order (for numbers, most-significant nybble first)
;@clobber: a, hl
;@param b: tile ID to draw
;@param hl: address in tilemap to draw it
DrawTileInHBlank:
  ;Busy wait for sending pixels - mode 3
  :ldh a,[rSTAT]
  and a,%00000011
  cp a,$03
  jr nz,:-
  ;Busy wait for Hblank - mode 0
  :ldh a,[rSTAT]
  and a,%00000011
  jr nz,:-
  ;Draw tile in tilemap - we have <dec94 cycles
  ld a,b
  IF SCREEN_FLIP_H
  ld [hld],a
  ELSE
  ld [hli],a
  ENDC
ret

;@param hl: location of table with 4-char entries containing the strings for setting values
;@param de: location in tilemap to start drawing at (should be the "start" side of the string, or most significant nybble)
DrawFourTilesInHBlank:

  push de

  ld a,[hli] ;1st char in b
  ld b,a
  ld a,[hli] ;2nd char in c
  ld c,a
  ld a,[hli] ;3rd char in d
  ld d,a
  ld e,[hl] ;4th char in e
   
  pop hl
  call DrawTileInHBlank

  ld b,c
  call DrawTileInHBlank

  ld b,d
  call DrawTileInHBlank

  ld b,e
  call DrawTileInHBlank

ret




ENDL
