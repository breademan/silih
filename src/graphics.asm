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



ENDL