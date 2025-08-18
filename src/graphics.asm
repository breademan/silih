INCLUDE "src/hardware.inc"

INCLUDE "src/general.inc"

SECTION "Payload Graphics Data SECTION",ROM0[$1000 + ($1000*GRAPHICS_BANK)]
GraphicsStorage::
LOAD "Payload Graphics Data LOAD", WRAMX [$D000]

Viewfinder_UI_Tiles::
  incbin "assets/viewfinderUI.1bpp", 0,256
gfxButtons_storage::
  incbin "assets/UserButtons.1bpp",0,128
gfxActions_storage::
  incbin "assets/actions.1bpp",0,128
gfxAlphabet_storage::
  incbin "assets/alphabet.1bpp",0,256
gfxObjects0_storage::
  incbin "assets/objects0.1bpp",0,128

Palette::
PaletteBG::
PaletteOBJ::
    ;Palettes in the bin file are specified in big-endian: RGBDS will automatically convert them to little-endian
    ;In the bin file, specify in RGB555: (dont-care/0):Blue5:Green5:Red5
    INCBIN "assets/palette.bin"


;---Graphics functions---------

;TODO: Redo this: instead of dangerous interrupt modification stuff, put this in the VBlank handler (only for Settings menustate?)
; Have two variables: Setting_Palette_Scheme and Active_Palette_Scheme (Set on init and in this function). If they aren't equal, call something like this in VBlank.
;Interrupt modification should be fine since this will only be done on init and in the Settings menu where there are no important interrupts to begin with.

InitPaletteScheme::
  ldh a,[rIE] ;Disable interrupts and save the Interrupt enable state for restore later
  push af
  xor a
  ldh [rIE],a ;disable interrupts

  ;If LCD is off, go ahead and transfer.
  ldh a,[rLCDC]
  bit 7,a
  jr z,:+
  ; Else if LCD is on, wait for VBlank
  call WaitForVBlankStart

  :

  ld a, $80
  ldh [rBCPS],a ;Set BG palette index to 0, auto-increment
  ldh [rOCPS],a ;Set OBJ palette index to 0, auto-increment

  .init_palette
  ld  b,NUM_OBJ_PALETTES*$08

  ;hl = PaletteBase + (PALETTE_SCHEME_SIZE*Setting_Palette_Scheme) -- ie add PALETTE_SCHEME_SIZE to hl (Setting_Palette_Scheme-1) times])
  ld a,[Setting_Palette_Scheme]
  ld b,a ; b is how many times we add PALETTE_SCHEME_SIZE
  ld a,PALETTE_SCHEME_SIZE ; a is what we add to HL on each iteration
  ld hl,$00
  .multiplyLoopOBJ
  dec b
  jr z,.finishMultiplyOBJ
  ld d, 0
	ld e, a
	add hl, de
  .finishMultiplyOBJ
  ld d,h
  ld e,l ;store the offset in de for reuse when populating the BG palette
  ;add PaletteOBJ into hl
  ld hl,PaletteOBJ
  add hl,de ;hl = PaletteBase+(PALETTE_SCHEME_SIZE*Setting_Palette_Scheme)
  ld  b,NUM_OBJ_PALETTES*$08
  .palette_loop_obj
  ld  a,[hl+]
  ldh  [rOCPD],a
  dec b
  jr  nz,.palette_loop_obj
  
  ld hl,PaletteBG ;hl = PaletteBG + (PALETTE_SCHEME_SIZE*Setting_Palette_Scheme)
  add hl,de
  ld  b,NUM_BG_PALETTES*$08
  .palette_loop_bg
  ld  a,[hl+]
  ldh  [rBCPD],a
  dec b
  jr  nz,.palette_loop_bg

  xor a
  ldh [rIF],a ;clear interrupts
  pop af
  ldh [rIE],a ;enable whatever interrupts were enabled before
ret

ENDL