
Blank_Display:
  ;If LCD is already off (as is the case in a ROM->RAM handover), skip the check for VBlank
  ldh a,[rLCDC]
  bit 7,a
  jr z, :+
.waitVBlank: ; Do not turn the LCD off outside of VBlank
  ldh a, [rLY]
  cp 144
  jr c, .waitVBlank

  ; Turn the LCD off
  :xor a
  ld [wLCDC], a
  ldh [rLCDC], a

Reset_vram:
  ld  a,$1
  ldh  [rVBK],a
  xor a
  ld  hl,$8000
.reset_vram1_loop ;resets VRAM bank 1
  ld  [hl+],a
  bit 5,h
  jr  z,.reset_vram1_loop
  ldh  [rVBK],a
  ld  hl,$8000
  cpl    ;Modify to fill with $FF instead of $00 -- this should make all the unused UI tiles $FF
.reset_vram0_loop ;Resets VRAM bank 0
  ld  [hl+],a
  bit 5,h
  jr  z,.reset_vram0_loop
  .blank_oam
  xor a
  ld  hl,$FE00
  ld  c,$A0
  .blank_oam_loop
  ld  [hl+],a
  dec c
  jr nz,.blank_oam_loop
;Clears HRAM between FF80 and FFFF, inclusive, since HRAM likely contains some code from the loader.
ClearHRAM:
    xor a
    ld hl, $FF80
    :ld [hli], a
    cp a, l
    jr nz, :-

; Clears the WRAM area that holds the tilemap data for the options UI.
ClearOptionLinesBuffer:
  ld a,BLANK_TILE_ID
  ld hl, OptionLinesBuffer
  ld b,$41
  :ld [hli], a
  dec b
  jr nz,:-

;When returning from ROM handover, we want to restore WRAM0. This will reset working variables and fix any polymorphic code changes that may have been made
BackupBank0:
ld a, BACKUP_BANK
ldh [rSVBK], a   ;bank switch to backup bank
ld hl, $C000
ld de, $D000
:ld a, [hli]
ld [de],a
inc de
bit 4,d
jr nz, :-

Load_VBlank_ISR_stub:
;The first thing the ROM's VBlank ISR does is PUSHes all registers, then jumps to HRAM (presumably for OAM DMA).
;We control HRAM, so we replace the OAM DMA handler with a jp to our VBlank handler
;If we need to do OAM DMA (which is faster than manual copying if we have more than 6 total sprites), we can also put that function here
  ld hl, .hram_function_storage
  ld c, $80
.loop:
  ld a, [hli]
  ldh [c], a
  inc c
  ld a, c
  cp a, ($80 + $03) ;terminate when we reach HRAM+size of our function
  jr nz,.loop
  
jr .end

;This holds the OAM DMA function
.hram_function_storage:
  jp VBlank_ISR ;3 bytes
;Below is the OAM DMA transfer function. 
; .run_dma:
;   ld a, HIGH(OAM_Work_Area) ;2b 2c : FF80-FF81
;   ldh [$FF46], a  ; start DMA transfer (starts right after instruction) ;2b 3c : FF82-FF83
;   ld a, 40        ; delay for a total of 4×40 = 160 cycles ; 2b 2c : FF84-FF85
; .wait
;   dec a           ; 1 byte 1 cycle : FF86
;   jr nz, .wait    ; 2 bytes 3 cycles :FF87-FF88 ;jr can be relocated without changing anything because they are relative jumps
;   ret     ;1 byte 4 cycles
.end
    
;Initializes the cursor in the OAM work area. If more objects are initialized, they should be initialized here.
OAM_Cursor_TileLoad:
  ld a, $10 ;Y-Position 16
  ld hl, OAM_Work_Area
  ld [hli], a
  ld a, $8 ;X-Position 8
  ld [hli], a
  xor a ; Tile ID 0
  ld [hli], a
  ld a, %01100001;attributes:  
  ld [hl], a


InitInput:
  ld a, $09 ; Amount of frames to wait before setting held buttons as active buttons
  ldh [joypad_hold_ready_interval], a
  ldh [joypad_hold_interval], a
  .findGetInput: ;Finds the GetInput function from the original ROM by finding the fifth call in the Vblank ISR
  ;Follow the VBlank vector
  ld a, [$0041]	;3b4c
	ld l, a		;1b1c
	ld a, [$0042]	;3b4c
	ld h, a		;1b1c 8b
	
    ;Go through until you find the fifth $cd (call). This assumes all $cd values in the ISR are call instructions, and not instruction args
    ; and also that it will always be the fifth call in all the ROMs.
	ld b, $5 	;2b2c
	
	:ld a, [hli]	;1b2c
	cp a, $cd	;2b2c
	jr nz, :-	;2b3.2c ; if not cd, keep iterating
	dec b		;1b1c
	jr nz, :-	;2b3.2c	;if cd, but not the FIFTH cd, loop
	
	;Put the address into the 2nd/3rd bytes of a CALL n16 instruction
	ld a, [hli] 	;1b2c
	ld [GetInputPtr+1], a ;3b4c low bit
	ld a, [hli]	;1b2c
	ld [GetInputPtr+2], a ; 3b4c high bit

;Changes tile attributes in the VRAM bank 1 attribute map.
;All tiles flip horizontally and vertically (for an upside-down GBA SP)
;Also fills the tilemaps to point to the correct VRAM bank, making the area that we use as the window (the top) use tiles from VRAM bank 1 
InitTilemapAttributes:
  ld a, $01
  ldh [rVBK], a ;Switch to VRAM Bank 1

  ;Set bit 6+5 of each byte in the 32x32 attribute tilemap 1:0x9800-9BFF
  ;Bit 3 controls the VRAM bank the tile is pulled from
  DEF INIT_TILEMAP_ATTRS EQU SCREEN_FLIP_V<<6 | SCREEN_FLIP_H<<5
  ld a, INIT_TILEMAP_ATTRS | %00001000; bank 1, flip tiles over X and Y if that's set
  ld hl, $9800

  ;Top points to VBank1, fills until $99FF ($99C0-$99FF will be overwritten later), used for the Window area's buffered captures
  .FillWithBank1:
    ld [hli], a
    bit 1, h
    jr z, .FillWithBank1
  ;Now, fill starting at 99C0
  ld a, INIT_TILEMAP_ATTRS
  ld hl, $99C0
  .FillWithBank0: ;Everything under the 12th tileline is filled with bank 0 tiles
    ld [hli], a
    bit 2, h
    jr z, .FillWithBank0

  LoadUITiles:
  ;Switch back to VRAM Bank 0 to mess with tilemap
  xor a 
  ldh [rVBK], a
  ;Unpacks the 1bpp UI tiles into the appropriate area of VRAM (last 32 tiles of bank 0)
  DEF UI_TILES_ORIGIN EQU $9600
  ld hl, Viewfinder_UI_Tiles
  ld de, UI_TILES_ORIGIN
  ld b, $FF
  call memcpy_1bpp

;Loads the section of tilemap that we can see with blank tiles
ClearTileMap0:
ld hl, TILEMAP_UI_ORIGIN_H ;fill from 99C0 to 9BFF -- a size of $240 (576) -- possible with an 8-bit counter if we fill 4 tiles at a time
ld a, BLANK_TILE_ID
ld b,$90
:ld [hli],a
ld [hli],a
ld [hli],a
ld [hli],a
dec b
jr nz,:-
  
BuildWindowTilemap_VH_Flipped: ;Fills the top-left part of tilemap (used as window) to map to capture tiles
  ;load tiles $80-FF and $00-5F by counting down to $80 from $5F 
  ld a, $60
  ld hl, $9800
  ld bc, $10
  .innerloop
    dec a
    ld [hli], a
    bit 4, l ;every 16 tiles, skip the next 16 tiles (since they're out-of-view)
    jr z, :+
    add hl, bc
    :cp a,$80
    jr nz, .innerloop

BuildBGTilemap_TileIDCountup: ;Maps the bottom-left tilemap area (+4 tiles on the left) to captured tiles in the same fashion as the window tilemap
  DEF TILEMAP0_CAPTURE1_ORIGIN EQU $9A44; $9800 + (18rows=$240) + 4
  ld a, $7F ;this can be used as the counter within the row, too. If low nybble is (row-backwards:0, row-forwards:F),  we just wrote to the end of line
  ld hl, TILEMAP0_CAPTURE1_ORIGIN + ($0F*SCREEN_FLIP_H) + ($20*13*SCREEN_FLIP_V);Depending on the VHflip, the original tilemap index will change (noflip =+0. Hflip = +$0F. Vflip = +13 lines (+32*13))
  ld bc, $0010+($20*SCREEN_FLIP_H) - ($40*SCREEN_FLIP_V) ;The value to add to the tilemap index at the start/end of each line -- may be 2's complement negative
  ld e, $10 ; e = inner counter (x-offset)
  .loop
    inc a
    IF SCREEN_FLIP_H
      ld [hld], a ;depending on whether Hflip is set, this will decrement instead
    ELSE
      ld [hli],a
    ENDC
    dec e ; every 16 tiles
    jr nz, :+ 
    set 4,e
    add hl, bc ;increment the tilemap index by half a row ($10) if increasing and no flip. If VHflip, decrement by $10. If Hflip, increment by $30. If Vflip, decrement by $30.
    :cp a,$5F
    jr nz, .loop

BuildHorizontalHeader: ;Fills the UI with the icons for each setting
  DEF TILEMAP_UI_ORIGIN_H EQU $99C0 ;$9800 + 14 rows -- this includes the 'edge' at the top left
  IF SCREEN_FLIP_H
    ld hl, TILEMAP_UI_ORIGIN_H
  ELSE
    ld hl, TILEMAP_UI_ORIGIN_H+3
  ENDC
  ld de, UI_ICONS_ARRANGEMENT

  ld b, $2 ; number of lines (skips every other)
  : ld c, $5 ; number of tiles per line
  : ld a,[de]
  inc de
  ld [hli], a
  ld a,BLANK_TILE_ID
  ld [hli], a
  ld [hli], a
  ld [hli], a ; 3 blank tiles between each icon
  dec c
  jr nz, :- ;Line finished check
  dec b
  jr z, BuildUITilemapV ;if b=0, you've finished
  ld bc, $2C
  add hl, bc  ;Go to next line
  ld b, 1
  jr :--

;This holds the horizontal parts of the UI. Each line is separated by an extra 3 zeros, and every other tileline is skipped (so it can hold data).
;Tile IDs are UI_ICONS_BASE_ID + index in the source image
UI_ICONS_ARRANGEMENT:
  DEF UI_ICONS_BASE_ID EQU $60
  MACRO X
    db UI_ICONS_BASE_ID+\2
  ENDM
  INCLUDE "src/ui_elements.inc"

BuildUITilemapV:
  DEF TILEMAP_UI_ORIGIN_V EQU $9A40 ;9800 + 18 rows ($240) this DOESN'T include the 'corner' at the top left.
  ;After 4 tiles, skip 1C tiles to reach start of next tile line
  ;Right now, this is a stub since we're not putting any icons into the vertical area yet


;Init LCD by setting the scroll registers, enabling the screen, and enabling VBlank interrupt
Init_LCD:
  ; Set scroll register to scroll down 112px, right 0px
  xor a
  ldh [rSCX], a
  ld a, $70
  ldh [rSCY], a
  ; Set window location
  ld a, $20
  ldh [rWY], a
  ld a, $27 ; 20px + 7 base
  ldh [rWX], a

  ; Turn the LCD on, Window off to be different from the current VRAM bank
  IF CAPTURE_BUFFERING
  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_WINON | LCDCF_OBJON
  ELSE
  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_WINOFF | LCDCF_OBJON
  ENDC
  ld [wLCDC],a
  ldh [rLCDC], a

  ; During the first (blank) frame, initialize display registers
  ld a, %11100100
  ldh [rBGP], a

  ;Clear interrupt flags
  xor a
  ldh [rIF], a

  ;Enable VBlank interrupt only
  inc a ; Sets a to %00000001, Vblank only enabled
  ldh [rIE], a
  
  ei ;Enable ISRs
;---------------------------------------------------------------------

;---------------------------------------------------------------------
  ;Init save/SRAM stuff
  ;Before calling these functions, you should switch to the correct SRAM bank and enable SRAM writing
  ;Afterwards, disable SRAM writing
  ld h,$0A 
  ld [hl],h ;enable SRAM writes
  ld h, HIGH(rRAMB)
  ld [hl], $00 ; switch to SRAM bank 0: state vector
  
  call StateVector_Init

  ld h,$00
  ld [hl],h ; disable SRAM writes after initializing the state vector
;---------------------------------------------------------------------------------

  ;Camera setup  
  ld a, $10
  ld [rRAMB], a ; switch to CAM registers

  ;Load invariant values into camera registers, with don't-cares (values that change) set to 0 so we can OR them in
    ;DEF CAM_OPT_INVARIANT_A000 EQU
    DEF CAM_OPT_INVARIANT_A001 EQU $00 ;Register 1: N(changes) VH1:0 (changes), G4:0 (changes) -- middle value is N=1, VH=11, G=4
    DEF CAM_OPT_INVARIANT_A002 EQU $00  ;Exposure time registers all can change
    DEF CAM_OPT_INVARIANT_A003 EQU $00  ;^
    DEF CAM_OPT_INVARIANT_A004 EQU $00; Register 7 - E7:4 (Edge enhancement), I, V2:0
    DEF CAM_OPT_INVARIANT_A005 EQU %10000000; Register 0: Z1:0, O5:0. Z is %10 unless we're using edge extraction. Let's set O to 0x27 by default.
    

  ;Load initial camera options into variables
    DEF CAMOPT_N_INIT EQU %1
    DEF CAMOPT_VH_INIT EQU %11
    DEF CAMOPT_C_INIT_L EQU $00
    DEF CAMOPT_C_INIT_H EQU $2C
    DEF CAMOPT_O_INIT EQU $27
    DEF CAMOPT_G_INIT EQU $04
    DEF CAMOPT_V_INIT EQU $03
    DEF CAMOPT_E_INIT EQU $00

  ld a, CAMOPT_N_INIT
  ld [CamOptN_RAM], a
  ld a, CAMOPT_VH_INIT
  ld [CamOptVH_RAM], a
  ld a, CAMOPT_C_INIT_L
  ld [CamOptC_RAM_L], a
  ld a, CAMOPT_C_INIT_H
  ld [CamOptC_RAM_H], a
  ld a, CAMOPT_O_INIT
  ld [CamOptO_RAM], a
  ld a, CAMOPT_G_INIT
  ld [CamOptG_RAM], a
  
  ld a, CAMOPT_V_INIT
  ld [CamOptV_RAM], a
  ld a, CAMOPT_E_INIT
  ld [CamOptE_RAM], a
  


InitDitherTable:
  ld a,DITHER_BASE_ROMBANK  ;Switch to ROM bank $0A
  ld [rROMB0], a
  
  ld a, $07
  ld [CamOptContrast], a
  ld a, $01
  ld [CamOptDitherTable], a

  call PrepareDitherPattern

  ;load working dither table into the real dither table
  ld hl, $A006 ;dest
  ld de, CamOptDither_RAM ;src
  ld b, $30

  .loop   ;copy $30 bytes
  ld a,[de]
  ld [hli],a
  inc e
  dec b
  jr nz, .loop


;Load CamOpt variables into RAM buffer, then move that buffer into camera registers
call PrepareCameraOpts
call UpdateCameraOpts
  
ld a, UI_RAMBANK
ldh [rSVBK],a
call InitMenuState_CameraOpts

;TEST trampoline caller
;Switch bank to caller's bank
ld a,TEST_CALLER_RAMBANK
ldh [rSVBK],a
call Trampoline_test_caller ;Set a watchpoint here and see if $DEADBEEF ends up in hlbc

;Switch VRAM bank to 1 so that it alternates in-sync with the window
IF CAPTURE_BUFFERING
ld a, $1 
ELSE
xor a ; if we're doing no-tear, then VBK should be 0 at all times
ENDC
ldh [rVBK], a