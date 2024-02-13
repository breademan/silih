    SECTION "OAM Transfer Function/HRAM funcion", HRAM[$FF80]
    DS 10 ;the size of jump to Vblank handler is actually only 3, but we want to leave space for an OAM function if we need it
       
    SECTION "Input Variables", HRAM[$FF9F]
    DS 11
    ;Input reader HRAM variables used by the ROM's code -- these are set by the original ROM and therefore can't be moved.
    DEF joypad_hold_ready_interval EQU $FF9F
    DEF joypad_hold_interval EQU $FFA0
    DEF joypad_state EQU $FFA1
    DEF joypad_new_pressed EQU $FFA2
    DEF joypad_active EQU $FFA3 ; DOWN UP LEFT RIGHT / START SELECT B A
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
viewfinder_state: db  ;Set to 1 if waiting on capture complete, 0 otherwise.
        DEF VF_STATE_IDLE EQU 0 ;DMA finished or init state: start a capture from here
        DEF VF_STATE_CAPTURING EQU 1 ;Waiting on capture
        DEF VF_STATE_DMA EQU 2 ;During a DMA
        DEF VF_STATE_PAUSED EQU 3 ;Another screen is going, so don't do any of the viewfinder action/checks

  ;Menu variables
MENU_STATE: db ;holds which menu we're in
        DEF MENU_STATE_CAMERA_OPTS EQU 0
        DEF MENU_STATE_DITHER_OPTS EQU 1
        DEF MENU_STATE_SELECTED EQU 2
        DEF MENU_STATE_TAKE_CONFIRM EQU 3
        DEF MENU_STATE_GALLERY EQU 4
        DEF MENU_STATE_DELETE_CONFIRM EQU 5
MENU_POSITION: db ;holds which menu item we're on (old4:new4)
MENU_NYBBLE: db ; holds which nybble we've selected in the menu; this can be reused as a variable for other menus. (old4:new4)
MENU_STATE_PREV: db ;holds the menu to return to from TAKE_CONFIRM and GALLERY -- SELECTED shouldn't need it
    ;DEF MENU_POSITION_PREV EQU $FFB2 ;these are both <16 values, we may be able to squeeze PREV and current values into the same memory location 4:4-style
MENU_NYBBLE_PREV: db  ;^^^^^


VBlank_finished_flag: db
