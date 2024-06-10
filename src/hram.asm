    SECTION "OAM Transfer Function/HRAM funcion", HRAM[$FF80]
    DS 10 ;the size of jump to Vblank handler is actually only 3, but we want to leave space for an OAM function if we need it
       
    SECTION "Input Variables", HRAM[$FF9F]
    ;Input reader HRAM variables used by the ROM's code -- these are set by the original ROM and therefore can't be moved.
    joypad_hold_ready_interval:: db ;$FF9F
    joypad_hold_interval:: db  ;$FFA0
    joypad_state:: db ;$FFA1
    joypad_new_pressed:: db ;$FFA2
    joypad_active:: db ;$FFA3 ; DOWN UP LEFT RIGHT / START SELECT B A
    DEF joypad_new_released EQU $FFA4
    DEF joypad_prev_state EQU $FFA5
    DEF joypad_hold_counter EQU $FFA6
    DEF joypad_selfOR_state EQU $FFA7
    DEF joypad_selfOR_new_pressed EQU $FFA8
    DEF joypad_selfOR_active EQU $FFA9
    
    SECTION "HRAM Variables", HRAM[$FFAA]
;HRAM Variables used by our code
    ;Next VRAM bank to load into. 
next_vram_bank: db
    ;Window status. 0 = displays VRAM
window_status: db
viewfinder_state:: db  ;Set to 1 if waiting on capture complete, 0 otherwise.

  ;Menu variables
MENU_STATE:: db ;holds which menu we're in

MENU_POSITION:: db ;holds which menu item we're on (old4:new4)
MENU_NYBBLE:: db ; holds which nybble we've selected in the menu; this can be reused as a variable for other menus. (old4:new4)
MENU_STATE_PREV:: db ;holds the menu to return to from TAKE_CONFIRM and GALLERY -- SELECTED shouldn't need it
    ;DEF MENU_POSITION_PREV EQU $FFB2 ;these are both <16 values, we may be able to squeeze PREV and current values into the same memory location 4:4-style
MENU_NYBBLE_PREV:: db  ;^^^^^    
VBlank_finished_flag:: db
Vblank_Sidebar_DrawLine:: db ;contains the line to draw in the vertical UI
VBlank_AnimationCounter:: db
