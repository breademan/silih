; SPDX-License-Identifier: MIT

INCLUDE "src/hardware.inc"             ; system defines

INCLUDE "src/general.inc"

;This section exists to ensure the addresses of WRAM tables are all aligned on 256bytes (h never changes)
;If we run out of space, make another $100-size section before it
SECTION "WRAM Variables Area 0 SECTION", WRAM0[$CD00]
WRAM_Var_Area0:
UNION
;64 bytes
OptionLinesBuffer: DS 64 ;UpdateByteInTileMap assume this line's addresses are aligned on 256 bytes. Since we can only see the first 20 bytes of each 32-byte line, we can use the last 12 bytes for storage
;A line buffer size of 64 means two lines of data. We may want to split this into two labels so we can work with the lines separately, but we may also just be able to work with it contiguously.
;OptionLinesBuffer is HDMA'd, meaning it must be aligned on 16bytes
NEXTU
DS 20
;Insert variables here, 12 bytes
DS 20
;Insert more variables here,12 bytes
ENDU
;Previously had to be placed in UI order for UpdateValsMap_WRAM, but it no longer cares.
;PrepareCameraOpts doesn't care about their in-memory order, so it should be safe to move them around otherwise.
;8 bytes
CamOptN_RAM:: db ;N must be stored immediately before VH for SetNVHtoEdgeMode
  CamOptVH_RAM: db
  CamOptC_RAM: ;CamOptC_RAM is stored in little-endian order, inconsistent with the CAM registers, but easily updated by modify_nybble
    CamOptC_RAM_L: db 
    CamOptC_RAM_H: db
  CamOptO_RAM: db
  CamOptG_RAM: db
  CamOptE_RAM: db
  CamOptV_RAM: db
  ;Non-CAM-register manual config options: 4 bytes
  CamOptContrast: db ; 0-F
  CamOptDitherTable: db ;selects whether to use light or dark dither table. 0=light (sramA:7c20), 1=dark (sramA:7c60)
  CamOptDitherPattern: db
  CamOptEdgeMode: db ;Meta-option that modifies NVH
  ;These variables are downstream of the above individual CamOpt variables. UpdateCameraOpts uses these contiguously and with CamOptDither_RAM, so they must be contiguous
  ; 5 + 48 bytes
  CamOptA001_RAM: db
  CamOptA002_RAM: db
  CamOptA003_RAM: db ; Watch out here: C may be stored in RAM little-endian, but the camera registers use big-endian.
  CamOptA004_RAM: db
  CamOptA005_RAM: db
  
  CamOptDither_RAM: ds 48 ; Working copy of the dither table
.end  
  ;Video mirror register: 1 byte
  wLCDC: db ;LCDC can be modified at any time, but there may be bugs with changing window visibility while drawing ;Must not be placed in the OptionLinesBuffer region as it is cleared after wLCDC was written.
  
  ;HDMA variables: 7 bytes
  hdma_current_transfer_length: db
  hdma_total_transferred: db
  hdma_total_remaining: db
  wHDMA1: db
  wHDMA2: db
  wHDMA3: db
  wHDMA4: db
  
  ;Save Slot variables: 2 bytes
  SAVE_SLOTS_FREE:: db
  ShowPromptsFlag:: db  
  ;1 byte
  ContrastChangedFlag: db ;When contrast is changed, set this to 1
                        ;May be wise to change this to a CamOptChangedFlags bitfield to check if CamRegs changed, Contrast, Dither Pattern, or Dither Lighting
  ;5 bytes
  RemoteJoypadHoldThreshold: db
  RemoteJoypadHoldCounter: db
  RemoteJoypadState: db
  RemoteJoypadPrevState: db
  RemoteJoypadNewPressed: db
  RemoteJoypadActive: db
  ;16 bytes
  GENERATED_DITHER_THRESHOLDS: ds 16 ;temporary storage space for dither threshold values from GenerateThresholdsFromRange. Used 3 times per dither pattern construction. Must not cross address byte boundary
  .end
  UIBuffer_Vertical:: ds $38 ;4x14 bytes: holds the tilemap information for the vertical UI. Must be aligned on 256 within a line due to 8-bit math

  ;Cumulative $DD bytes
  ;$22 bytes remaining


SECTION "Payload SECTION",ROM0[$1000]
PayloadStorage::
    LOAD UNION "Payload LOAD", WRAM0 [$C000]
PayloadEntrypoint:
    xor a
    ldh [rNR52], a ;disable sound
  
    ld sp, _RAMBANK-1 ;set the stack pointer to top of lowRAM

    INCLUDE "src/init.asm"

ViewfinderMain:
  ; Things done every frame
  xor a
  ldh [VBlank_finished_flag],a ;clear the VBlank finished flag

  call GetSerialInput

  ld a, UI_RAMBANK
  ldh [rSVBK], a

  jp HandleInput   ;jump to the input handler subroutine, and at the end it will jump back here.

HandleInputDone::

call UpdateValsMap_WRAM

;Checks (ideally, these work somewhat like interrupts)
ViewfinderChecks:

    ;If VBlank ISR has run, jump back to the stuff done every frame. If it's not run yet, keep checking for events to handle
    ldh a, [VBlank_finished_flag]
    and a
    jp nz, ViewfinderMain
    ;Jump to relevant state handler
    ldh a, [viewfinder_state]
    add a,a
    ld c, a
    ld b, 0
    ld hl, Viewfinder_jp_table
    add hl, bc
    ld a, [hli]
    ld h, [hl]
    ld l, a
    jp hl
    ; If you were waiting for a capture AND capture is complete, initialize a DMA transfer (+remaining data counter). 
    ; With HBlank-only, this is trivial, but may be nontrivial if we use VBlank transfers too.
Viewfinder_jp_table:
    DW IdleHandler
    DW CapturingHandler
    DW DMAHandler
    DW PauseHandler

VBlank_ISR: ;We have 1140 M-cycles to work our magic here
  ;Because we are jumping from the ROM's vblank ISR, we lose 22 cycles to PUSHesx4 and call to+jp from HRAM

  pop bc ;the top of the stack is the address of the ROM's VBlank ISR. Since we don't want to return to it, we pop it off the stack and don't use it.
  
  ;cursor transparency effect: alternate cursor sprite blank(Vbank1)->white(Vbank0) every other frame. The sprite has the same ID, just different banks. 
  ld a, [OAM_Work_Area+2] ;+4c3b
  xor a, %00000001 ;2c2b
  ld [OAM_Work_Area+2], a ;+4c3b

  ;If we have few objects, instead of transferring the entire OAM, we can just modify the position (2bytes) or entirety (4bytes) of a small number of hardcoded ones.
  .OAM_Transfer:
    ld de, _OAMRAM+OAM_USED_BYTES ;+3c3b ;We set this to one-higher since we decrement _before_ each loop to test if we just wrote to dest-address 0 +3b3c
    ld hl, OAM_Work_Area+(OAM_USED_BYTES)-1 ;source to copy from. +3b3c
    ;Since hld(src) decrements _after_ writing, we want to set this to one lower than de(dest) at that time, so the addresses match
    :dec e ;+1c1b
    ld a, [hld] ;+2c1b
    ld [de], a ;2c1b

    dec e
    ld a, [hld]
    ld [de], a

    dec e
    ld a, [hld]
    ld [de], a

    dec e
    ld a, [hld]
    ld [de], a
  jr nz, :- ;2b3taken2untaken cycles

  ;Write working LCDC variable to real LCDC -- technically can be done anytime, but may fix glitches with window switching
  ld a,[wLCDC] ;+4c3b
  ldh [rLCDC],a ;+3c2b

  call DrawValueLines_DMAMethod 
  call DrawSidebar ;TODO inline this

  ;Anything under here can run even if we're not in VBlank, but keep in mind interrupts won't be enabled
  ;set VBlank_finished_flag
  ld a, $01 ;+2c2b
  ldh [VBlank_finished_flag], a ;+3c2b

  pop hl ;+3c1b
  pop de ;+3c1b
  pop bc ;+3c1b
  pop af ;+3c1b
  reti





;----------------------------- Event handlers------------------------------------------------------
IdleHandler: ;Start a capture, change state to capturing
  call StartCapture
  ld a, VF_STATE_CAPTURING
  ldh [viewfinder_state], a
  jp ViewfinderChecks

CapturingHandler:
  ld a, [$A000]
  and a, $01 ;If nonzero, capture isn't complete
  jp nz, ViewfinderChecks ; leave if still capturing
  ;Capturing complete:---------------------
  ;Start DMA transfer 
  ;Switch SRAM bank to first bank, which contains image data (we need to remember not to switch away from this while transferring)
  ;viewfinder state = capturing means you should wait for it to finish before switching SRAM or VRAM banks.
  ;At the moment, the only non-init SRAM switches are saving a capture, and that waits until the capture is complete. 

.start_hdma_transfer
  xor a
  ld [rRAMB], a
  ;Set up the hdma variables
  ld [hdma_total_transferred],a ; total transferred bytes so far is 0
  ld a,$E0
  ld [hdma_total_remaining],a ;remaining bytes so far: all of them!
  ld a,$80
  ld [hdma_current_transfer_length], a ;Transfer length is the maximum allowable by DMA

  ;Set HDMA source to start of SRAM
  ld hl, wHDMA1
  DEF CAPTURE_DATA_START_SRAM EQU $A100
  ld a, HIGH(CAPTURE_DATA_START_SRAM)

  ld [hli],a
  ldh [rHDMA1], a
  xor a
  ld [hli],a
  ldh [rHDMA2], a

    ;Set HDMI dest to start of tile data
  ld a, HIGH(CAPTURE_TILEDATA_START)
  ld [hli],a
  ldh [rHDMA3], a
  ld a, LOW(CAPTURE_TILEDATA_START)
  ld [hli],a
  ldh [rHDMA4], a
  
  ;wait for after Hblank (OAM search) or 1 (VBlank) to initiate the DMA transfer
  ;if only one of the two LSBs is set, initiate the transfer. If it's %00 or %11, waitloop.
  :ldh a, [rSTAT]
  and a, $3
  cp a, %11
  jr z, :-
  cp a, %00
  jr z, :-
  ; If ISR happens after this check, we will have a TOCTOU bug. So long as VBlank handler actually finishes while there are >5  cycles left in VBlank, we're OK. 

  ;Set Length - 16x14x16 bytes , divided by $10, minus 1. Upper bit needs to be set for HBlank DMA = DF. This is an overflow, the HDMA can only transfer up to $800 bytes
  ; But we want to do $E00 bytes - we will need two passes, one of $800 (7F), one of $600 (5F).
  ld a,$FF
  ldh [rHDMA5], a ;transfer $800 bytes, Hblank DMA

  ld a, VF_STATE_DMA ;Change state to wait on DMA to finish
  ldh [viewfinder_state], a
  jp ViewfinderChecks

DMAHandler:

  ;Return if transfer unfinished
  ldh a, [rHDMA5]
  bit 7, a ;Bit 7 set? Not active (complete)
  jp z, ViewfinderChecks ; Return if the transfer is active
  
  ;if we got past the active check, 1 of 2 things has happened:
  ;1: The DMA transfer completed without VBlank resetting it, but there is still data to transfer. In this case, hdma_total_remaining > $80. 
  ;2: The DMA transfer completed and there is no more data to transfer. In this case, hdma_total_remaining <= $80.
  ;Since there are no active transfers now, the VBlank ISR shouldn't change HDMA registers, so we don't need to disable interrupts.

  ;Check if all data is transferred. If not, initiate a new DMA transfer. If so, go to next state (initiating new capture or leaving the viewfinder) and switch window visibility.
  ld a,[hdma_total_remaining]
  cp a,$81 ; carry if a < $81, no carry if a >= $81. We're using $81 so that if a==$80, it will still end up in the carry case. So carry if a<= 80, nc if a>80 
  jp c, .transfer_complete

  .transfer_incomplete:
  ;If DMA transfer completed with data still to transfer
  ;Initiate another DMA transfer where the size is the amount of data remaining.
  xor a
  ld [rRAMB], a  ;switch SRAM bank to camera's capture bank

  ;update the hdma variables, but it's a little simpler than in VBlank: We know that the amount transferred was hdma_current_transfer_length instead of needing to subtract hdma5
  ld a, [hdma_current_transfer_length]
  ld b, a ; b = marginal amount transferred in the just-finished DMA transfer
  
  ld a,[hdma_total_transferred]
  add a, b
  ld [hdma_total_transferred], a

  ld a,[hdma_total_remaining]
  sub a, b
  ld [hdma_total_remaining],a

  ;new hdma_current_transfer_length = MIN($80,hdma_total_remaining ).Note that HDMA_current_transfer_length is the ACTUAL length,not length-1, and must be converted.
  ;It might be more convenient to store it and do math with it in -1form,but not now
  cp a, $80 ;carry if $80 is bigger -- next transfer size will be total_remaining.
  jr c, :+
    ld a,$80    ;no carry case, $80 < hdma_total_remaining, so hdma_current_transfer_length = max allowed transfer length
  :ld [hdma_current_transfer_length], a

  ;Set DMA transfer parameters
  call RestartHDMATransfer

  jp ViewfinderChecks

    .transfer_complete

    ;If we're not going to restart a capture, change the viewfinder state to VF_STATE_PAUSED
    ldh a,[MENU_STATE]
    cp MENU_STATE_TAKE_CONFIRM
    jp z, .pausecapture

    call StartCapture
    
    ld a, VF_STATE_CAPTURING
    ldh [viewfinder_state], a
    jp ViewfinderChecks

  .pausecapture
  ;Draw the capture confirm UI here too. If we draw it immediately upon pressing the button, the user won't have any indication of which picture they're saving 
  ;Also, we should not accept input in the menu handler if Viewfinder state is not VF_PAUSED - this means it hasn't finished capturing the current image yet
  ;I think users might expect the button to capture the photo they SEE when they pressed it -- currently it's showing the next capture after the user pressed a button.
  ld a,VF_STATE_PAUSED
  ldh [viewfinder_state],a
  jp ViewfinderChecks

PauseHandler:
  ; While the menustate is not CAMERA_OPTS, DITHER_OPTS, or SELECTED (MENU_STATE<=2), don't start a capture
  ldh a, [MENU_STATE]
  cp a,MENU_STATE_SELECTED+1
  jp nc, ViewfinderChecks;if carry, we're in one of the camera states where viewfinder is active and need to restart the capture + change viewfinder state
  
  call StartCapture  
  ld a, VF_STATE_CAPTURING
  ldh [viewfinder_state], a
  jp ViewfinderChecks


;------------------------------------------------------------------------------
;Functions
StartCapture:
    ld a, $10
    ld [rRAMB], a ; switch to GB Camera register
    call PrepareCameraOpts ; calling PrepareCameraOpts here instead of after a change to a camera option increases the latency between captures every time. Calling it only on changes increases latency only when you change it.
    call UpdateCameraOpts
    ld a, %00000011 ;Start capture
    ld [$A000], a
    ret 


;Function to load 1bpp tiles into VRAM (skipping every other dest byte)
;Assumes VRAM accessible and size of < FF bytes.
;src hl
;dest de
;size b (in source bytes), minus one (this is so that we can transfer $100 bytes and a size of 0 is unused)
;If src is <$FF bytes and L starts at 0, we can speed this up using inc r8 and possibly changing the counter check condition
memcpy_1bpp:
  inc b
  .loop
    ld a, [hli]
    ld [de],a
    inc de ; 1b 2c
    ld [de], a
    inc de ; 1b 2c
    dec b ;since dec r16 doesn't set flags, we'd need extra instructions to check for sizes > $FF
    jp nz, memcpy_1bpp.loop
  ret

;Converts the various camera opt WRAM variables into 5 register variables that can be applied to A001-A005 upon recapture.
;THESE should probably be WRAM vars, but ld a,[hli] (2c*5 + 3c ld r16,n16) is probably faster than ldh a, [n16] (3c2b). 
;Even with HRAM ld c,n8, ldh a, [c] (2c1b), inc c (1c1b), total HRam load time would be (1c + 2c*5 + 1*5), slower than the wram version, and perhaps taking more space.

;If we store the CAMOPT_A00x_RAM inside the function itself, we can save space and a small amount of ld time, but we can't make the values contiguous
;But we will have to worry about making sure this function is accessible when the variables are used in the capture handler
;Relocating the CAMOPT WRAM variables to UpdateCameraOpts has the same problem: we need it accessible to both the capture handler and PrepareCameraOpts

;Should be run right after updating WRAM varibles -- we get increased latency if we run this before each capture.

PrepareCameraOpts::
  ; We will use register d as our work register since the input function may use b to store the input
  ; And if we put things in HRAM, we might use C to address them instead of n8.
  DEF reg_x EQUS "d"
  ;Options for A001: N, VH6:5, G4:0
  .prepareA001:
  ;ld {reg_x}, CAM_OPT_INVARIANT_A001 ; This reg has no invariants
  ld a, [CamOptVH_RAM]
  rrca
  rrca
  ld {reg_x}, a ; {reg_x} = %VH00 0000
  ld a, [CamOptN_RAM]
  or {reg_x}
  rrca
  ld {reg_x},a ; {reg_x} = %NVH0 0000
  ld a, [CamOptG_RAM]
  or {reg_x}
  ld [CamOptA001_RAM], a ;CamOptA001_RAM = NVHG GGGG
  .prepareA002:
  ld a, [CamOptC_RAM_H]
  ld [CamOptA002_RAM], a
  .prepareA003:
  ld a, [CamOptC_RAM_L]
  ld [CamOptA003_RAM], a
  .prepareA004: ;EEEE I VVV 
  ;ld a, [CAM_OPT_INVARIANT_A004] ;invariant is 0, so ignore
  ld a, [CamOptE_RAM]
  swap a
  ld {reg_x},a
  ld a, [CamOptV_RAM]
  or a, {reg_x}
  ld [CamOptA004_RAM], a

  .prepareA005: ;ZZOO OOOO
  ;ZZ is invariant
  ld {reg_x}, CAM_OPT_INVARIANT_A005
  ld a, [CamOptO_RAM]
  or a, {reg_x}
  ld [CamOptA005_RAM], a
ret

;Assumes we've already switched to the Camera Opts register and loaded A00x variables using PrepareCameraOpts
;Since this is only used for right before we restart a capture and in init, it should probably be a macro or subroutine to avoid a call; it's also pretty short
UpdateCameraOpts::

  ; ld hl, CamOptA005_RAM
  ; ld de, $A005
  ld hl, (CamOptDither_RAM.end-1) ;+3
  ld de, $A035 ;+3
  :ld a, [hld] ;+2 : 8 cycles/byte
  ld [de], a ;+2
  dec e ;+1
  jr nz, :- ;+3
ret

;Updates the working tilemap buffer of camera+other options based off the underlying values
UpdateValsMap_WRAM:
  ;Write the values to the WRAM buffer

  call UpdateOptionBuffer_EdgeMode

  call UpdateOptionBuffer_CamOptC
  
  call UpdateOptionBuffer_CamOptO

  call UpdateOptionBuffer_CamOptG

  call UpdateOptionBuffer_CamOptE

  call UpdateOptionBuffer_CamOptV

  call UpdateOptionBuffer_CamOptContrast

  call UpdateOptionBuffer_DitherTable

  call UpdateOptionBuffer_Undecided1

  call UpdateOptionBuffer_Undecided2
  ret


;hl: location of byte to put into tilemap
;de location in tilemap to load the data
;changes [de] and [de-1] if flipped. [de] and [de+1] if not flipped, de will end up in the location of the least-significant nybble (dec if flipped, else inc), hl incremented
;assumes de doesn't cross a byte address boundary
UpdateByteInTilemap::
  ;17 cycles + call/ret
  ld a, [hl] ;display high nybble +2c1b
  swap a ;+2c2b
  and a,$0F ;+2c2b
  or a,UI_ICONS_BASE_ID ;+1c1b
  ld [de], a ;+2c1b
  inc e
  ld a,[hli] ;+2c1b
  and a,$0F ;+2c2b
  or a,UI_ICONS_BASE_ID ;+1c1b
  ld [de],a ;+2c1b
  ret ;+4c1b -- function call/ret is 10c/4b total

;hl: location of byte to put into tilemap
;de location in tilemap to load the data
;changes [de] and [de-1] if flipped. [de] and [de+1] if not flipped, de will end up in the location of the least-significant nybble (dec if flipped, else inc), hl incremented
;assumes de doesn't cross a byte address boundary
UpdateByteInTilemap_flipped::
  ;17 cycles + call/ret
  ld a, [hl] ;display high nybble +2c1b
  swap a ;+2c2b
  and a,$0F ;+2c2b
  or a,UI_ICONS_BASE_ID ;+1c1b
  ld [de], a ;+2c1b
  dec e ;low nybble ;+1c1b
  ld a,[hli] ;+2c1b
  and a,$0F ;+2c2b
  or a,UI_ICONS_BASE_ID ;+1c1b
  ld [de],a ;+2c1b
  ret ;+4c1b -- function call/ret is 10c/4b total


; Stop the current HDMA transfer, if there is one, and draw a UI line using GDMA
;DrawValueLines will not write to VRAM; instead it will write to the WRAM space and no longer switch/switchback VRAM banks

DrawValueLines_DMAMethod:
  ;Save current VRAM bank: 17 cycles across start and end
  ldh a,[rVBK] ;+3c2b
  push af ;+4c1b
  ;Switch to VRAM bank 0 to change tile IDs
  xor a ;+1c1b
  ldh [rVBK],a ;+3c2b

  ldh a,[rHDMA5] ;+3c2b
  bit 7, a ;+2c2b
  jr nz,.none_active ;+3/2c2b
  .pauseandsaveHDMAtransfer:
  ;"terminate an active HBlank transfer by writing zero to Bit 7 of FF55" from Pandocs. Does this mean I should write a full 0 byte, or should I save the value of HDMA5 and reset bit 7 only?
  res 7,a ;+2c2b ;reset bit 7 only
  ldh [rHDMA5], a ;+3c2b ;write 0 to HDMA5:7 to stop the transfer
  ; update variables for the transfer  
    ;due to checking and stopping the transfer, reg a already has the amount remaining for this transfer, minus one
    inc a ;+1c1b
    ld b,a ;+1c1b
    ld a,[hdma_current_transfer_length] ;+4c3b
    sub a,b ;+1c1b
    ld b, a ;+1c1b ;b =marginal amount transferred = hdma_current_transfer_length - (local_remaining+1)

    ;hdma_total_remaining -= marginal_amount_transferred
    ld a,[hdma_total_remaining] ;+4c3b
    sub a,b ;+1c1b
    ld [hdma_total_remaining],a ;+4c3b

    ;new hdma_current_transfer_length = MIN($80,hdma_total_remaining ). Note that HDMA_current_transfer_length is the ACTUAL length,not length-1, and must be converted.
    cp a, $80 ;+2c2b ;carry if $80 is bigger, so we'll use total_remaining.
    jr c, :+ ;+3/2c 2b
      ld a,$80 ;+2c2b   ;no carry case, $80 < hdma_total_remaining, so hdma_current_transfer_length=$80
   :ld [hdma_current_transfer_length], a ;+4c3b  ;carry case, hdma_current_transfer_length=total_remaining=a

    ;hdma_total_transferred+=marginal_amount_transferred
    ld a,[hdma_total_transferred] ;+4c3b
    add a,b ;+1c1b
    ld [hdma_total_transferred],a ;+4c3b

  call StartGDMATransfer_Line1
  call StartGDMATransfer_Line2

  call RestartHDMATransfer

  jr .cleanup ;3c2b

  .none_active:
  call StartGDMATransfer_Line1
  call StartGDMATransfer_Line2
  ;restore HDMA registers (if we were in the middle of populating them to start a new HDMA transfer outside of the ISR). wHDMA regs must be contiguous.
  ld hl,wHDMA1 ;+3c3b
  ld a,[hli] ;+2c1b
  ldh [rHDMA1],a ;+3c2b    :cp a,$5F ;this is an infinite loop since the high nybble from a is removed.

  ld a,[hli] ;+2c1b
  ldh [rHDMA2],a ;+3c2b
  ld a,[hli] ;+2c1b
  ldh [rHDMA3],a ;+3c2b
  ld a,[hli]  ;+2c1b
  ldh [rHDMA4],a ;+3c2b

  .cleanup:  ;Restore current VRAM bank after setting it to 0 to change the tilemap.
  pop af ;+3c1b
  ldh [rVBK],a ;+3c2b
  ret 


StartGDMATransfer_Line1:
  ld a, HIGH(OptionLinesBuffer) ;+2c2b
  ldh [rHDMA1], a ;+3c2b
  ld a, LOW(OptionLinesBuffer) ;+2c2b
  ldh [rHDMA2], a ;+3c2b
  ld a, HIGH(TILEMAP_UI_ORIGIN_H+$20) ;+2c2b
  ldh [rHDMA3],a ;+3c2b
  ld a, LOW(TILEMAP_UI_ORIGIN_H+$20) ;+2c2b
  ldh [rHDMA4],a ;+3c2b
  ld a, %00000001 ;+2c2b ;lower 7 bits = number of $10-sized transfers minus 1. a value of 1 transfers $20 bytes. high bit 0=general-purpose DMA
  ldh [rHDMA5],a ;+3c2b
  ;+8*2 = 16 cycles for the DMA
ret

StartGDMATransfer_Line2:
  ld a, HIGH(OptionLinesBuffer+$20) ;+2c2b
  ldh [rHDMA1], a ;+3c2b
  ld a, LOW(OptionLinesBuffer+$20) ;+2c2b
  ldh [rHDMA2], a ;+3c2b
  ld a, HIGH(TILEMAP_UI_ORIGIN_H+$60) ;+2c2b
  ldh [rHDMA3],a ;+3c2b
  ld a, LOW(TILEMAP_UI_ORIGIN_H+$60) ;+2c2b
  ldh [rHDMA4],a ;+3c2b
  ld a, %00000001 ;+2c2b ;lower 7 bits = number of $10-sized transfers minus 1. a value of 1 transfers $20 bytes. high bit 0=general-purpose DMA
  ldh [rHDMA5],a ;+3c2b
  ;+8*2 = 16 cycles for the DMA
ret


RestartHDMATransfer:
  ; Calculate addend as to_dma16addr(hdma_total_transferred) and put it in de
	ld a,[hdma_total_transferred] ;+4c3b
	ld e, a ;+1c1b
	swap e ;+2c2b
	and a,$F0 ;+2c2b
	swap a ;+2c2b
	ld d,a ;+1c1b ;de is now our addend
  ; Load new source and dest addresses
	ld HL, CAPTURE_TILEDATA_START ;+3c3b ;new dest address 
	add HL, DE ;+2c1b
	ld a,H ;+1c1b
  ld [wHDMA3],a
	ldh [rHDMA3],a ;+3c2b
	ld a,L ;+1c1b
  ld [wHDMA4],a
	ldh[rHDMA4],a ;+3c2b
	
	ld HL, CAPTURE_DATA_START_SRAM ;+3c3b ;new src address
	add HL, DE ;+2c1b
	ld a,H ;+1c1b
  ld [wHDMA1],a
	ldh [rHDMA1],a ;+3c2b
	ld a,L ;+1c1b
  ld [wHDMA2],a
	ldh[rHDMA2],a ;+3c2b

  ;wait for afterHblank (OAM search) or 1 -- if only one of the two LSBs is set, break. If it's %00 or %11, loop.
  ;This wastes a few cycles in VBlank but means the code can be reused both in the DMA handler and in VBlank
  :ldh a, [rSTAT] ;+3c2b
  and a, $3 ;2c2b
  cp a, %11 ;+2c2b
  jr z, :- ;+3/2c1b
  cp a, %00 ;+2c2b
  jr z, :- ;+3/2c1b

  ; Load new length into HDMA5 and start
	ld a, [hdma_current_transfer_length] ;+4c3b
	dec a ;+1c1b ;Next transfer length is stored as the actual length in blocks of 16, but HDMA5 takes length-1
	set 7,a ;+2c2b ;Bit 7 set – HBlank DMA
	ldh [rHDMA5],a ;+3c2b
  ret

;Largely based on the GBDK's banked calling convention (https://gbdk-2020.github.io/gbdk-2020/docs/api/docs_coding_guidelines.html)
;arg e: WRAM bank to switch to
;arg hl: address of callee
;clobbers a,bc, and whatever the function it calls clobbers.
;pushes the current WRAM bank to stack (as AF), switches to the callee's WRAM bank, and jumps to the function in hl.
 
  ;The callee must add sp,-(6+(2*num_POPs)) in order to return to the top of the stack.
  ;after it RETs and the trampoline switches back to the caller WRAM bank, the caller must also add 2*num_POPs to the stack to clean up.
Trampoline_hl_e::
  ldh a,[rSVBK]
  push af ;push current bank
  ld a,e
  ldh [rSVBK], a ;switch to callee bank
  ld bc, .cleanup
  push bc ;push trampoline return address onto stack
  jp hl   ;jump to hl

  .cleanup
  ;switch back to caller WRAM bank
  pop af    ;callee function can return in any reg except a
  ldh [rSVBK], a

  ret


    ;arg bc: b is min/start, c is max
    ; stores the thresholds in a $10-byte static region, GENERATED_DITHER_THRESHOLDS -- this uses all our regs and we'd have no counter, so we'd have to use 16 duplicate add16s+ldn16s to fill. 
    ;instead, we could push the 16 values to the stack by first PUSHing de, then incrementing it to erase the value of e. At the end, increment sp by 16 and return.
    ;All these sp instructions seem quite complex -- we might
    ;sp is now at the (start of backward threshold table + 3), so we ld hl, sp-3 to get the table's minimum value. When stored this way, the matrix is addressed negatively,
    ;e.g val2 is at [baseaddr-2].
    ;clobbers all regs
GenerateThresholdsFromRange:
  ;bc: step size (calculated from arg bc) in 8.8 fixed-point
  ;de: running 8.8 fixed-point that holds start + i*step


  macro ADD16_BC_INTO_DE ;6c6b
    ld a, e ;1c1b
    add c ;1c1b
    ld e, a ;1c1b
    ld a, d ;1c1b
    adc b ;1c1b
    ld d, a ;1c1b
  endm

  ;16-bit saturating add -- if the result is >$FF, it sets D to $FF
  macro ADD16_BC_INTO_DE_SATURATING ;9/10 cycles, 10b
    ADD16_BC_INTO_DE
    jr nc, :+     ;if there's no carry flag, finish ; +3 taken, 2 untaken 2b
    ld d, $FF ;2c 2b
    :
  endm

  ;3c3b
  ld d, b ;load start of range into our 8.8 running sum
  ld e, $00 ; fractional part initialized to 0

  ;12c
  ld a, c ;+1
  sub a, b ;a = max - min ;+1
  swap a ;+2
  ld b, a ;temporarily hold swap(max-min) in b ;+1
  and a, $F0 ;+2
  ld c, a ;stepsize_L = swap(max-min)[7:4] ;+1
  ld a, b ;+1
  and a, $0F ;+2
  ld b, a ;stepsize_H = swap(max-min)[3:0] ;+1

  ld hl, GENERATED_DITHER_THRESHOLDS ; 3c

  .loop
  ld a, d ;+1
  ld [hli],a ;load whole-part of running sum into GENERATED_DITHER_THRESHOLDS[i] ;+2 
  ADD16_BC_INTO_DE_SATURATING ;generate next value ;+10
  ld a, LOW(GENERATED_DITHER_THRESHOLDS.end) ;+2
  cp a, l ;if the next address to write to is out-of-bounds, finish. We use a check for equality because even if GENERATED_DITHER_THRESHOLDS crosses a byte boundary,
  ;the l value will never loop ;+1
  jr nz, .loop ;+3

  dec hl   ;leaves hl at GENERATED_DITHER_THRESHOLDS.end-1 (last element) +2

  ret ;+2

  ;Called after calling GenerateThresholdsFromRange and setting a=0,1,2. puts the values in GENERATED_DITHER_THRESHOLDS every 3 spaces in the working dithering table, according to the ordering matrix
  ;arg a = offset within the working dithering table (0, 1, or 2), which represents which group of threshold (dark, middle, or light) values are being arranged.
  ;arg hl = address of last element of dither thresholds generated by GenerateThresholdsFromRange
    ;Though it's always the same address, this is always called after a call to the former, so it's more efficient to not reload it
ArrangeThresholdsInPattern:
  ;bc = source - last element of dither thresholds generated by GenerateThresholdsFromRange
  ;de = dest - working dither pattern
  ;hl = order table
  ld b, h ;using hl for the order tables loses us a byte for this reg switch, but saves 32 cycles due to hli
  ld c, l

  ld de, CamOptDither_RAM
    ;moving hl to bc loses us 2 cycles and 2 bytes, but instead of using ld a, [bc] 2c1b + inc bc 2c1b, we can just use ld a, [hli] 2c1b, 
    ;saving 2c per loop (32c) and but taking 1 more byte
  add a, e
  ld e, a ;destination start position is the WRAM dither table, plus an offset to select which threshold (z) you're modifying

  ld hl, OrderTableRelative

  .loop
  ld a, [bc] ;load dither value ;+2
  ld [de], a;into CamOptDither_RAM[(3*i)+arg_a] +2
  ;add OrderTable[i] into bc (source address)
  ld a,[hli] ;+2
  ;since the temporary table of thresholds' addresses are guaranteed to have the same l, we can just add it to the low byte of source
  add a,c ;+1
  ld c,a ;+1
  ;add 3 to e to get to the next destination pattern
  ld a,e ;+1
  add a,3 ;+2
  ld e,a ;+1
  cp a, LOW(CamOptDither_RAM.end);if next destination address is out-of-bounds, return ;+1
  ; if carry, a<(lastelement+1) and we're still in bounds. If nc, next address is out-of-bounds
  jr c, .loop  ;+3

  ret

;fill dither table
;reads the contrast + light/dark variable to select the dither pattern, then fills the table
;args: null
PrepareDitherPattern::
  ;add the dither table number to DITHER_BASE_TABLE (the table start addresses are all in the same byte, though the last table actually goes outside)
  ld hl, DITHER_BASE_TABLE ;+3
  ld a, [CamOptDitherTable] ;+4
  rrca ;+1
  rrca ;Multiply it by $40 to get offset of table start. ;+1
  ;and a, %11000000 ; ensure that the value is in range. We'll save cycles by assuming whatever modifies CamOptDitherTable takes care of that.
  add a, l ;+1
  ld l, a ;hl = start of one of the 4 dither base tables ;+1


  ld a, [CamOptContrast] ;+4
  add a,a ;+1
  add a,a ;multiply contrast value by 4 ;+1
  add a, l ;+1
  ld l, a ; hl = dither base table + (4*contrast). ;+1
 

  ld a, [hli]
  ld b, a
  ld c, [hl]   ;put min in b and max in c
  push hl ;push address of start of first range
  call GenerateThresholdsFromRange
  xor a
  call ArrangeThresholdsInPattern ; arrange our thresholds into the DARK part of the pattern
  pop hl

  ld a, [hli] ;+2
  ld b, a ;+1
  ld c, [hl]   ;put min in b and max in c ;+2
  push hl ;push address of start of second range ;+4
  call GenerateThresholdsFromRange
  ld a, $01 ;+2
  call ArrangeThresholdsInPattern ; arrange our thresholds into the DARK part of the pattern
  pop hl ;+3

  ld a, [hli]
  ld b, a
  ld c, [hl]   ;put min in b and max in c
  push hl ;push address of start of third range
  call GenerateThresholdsFromRange
  ld a, $02
  call ArrangeThresholdsInPattern ; arrange our thresholds into the DARK part of the pattern
  pop hl
  

  ret


;-------------------------------------------------------------------------------------------------------------------------
;These functions set OptionLinesBuffer to tiles corresponding to the underlying values

DEF DEFINE_NULL_X EQUS "MACRO X\n \n ENDM\n"

;Sets VALSMAP_OFFSET to (the offset of the left tile of) the area it should be drawn in OptionLinesBuffer
MACRO GET_UI_OFFSET
  DEF VALSMAP_OFFSET = 0
  DEF XMACRO_SEARCH_TERM EQUS \1
  ;This macro only works if the numerical values underlying the labels are unique. -- otherwise we'd need to compare the names of the labels instead of their values.
  DEF GETINDEX_X_DEF EQUS "MACRO X\nIF \\1=={XMACRO_SEARCH_TERM}\n PURGE X\n \{DEFINE_NULL_X\}\n         ELSE\n DEF VALSMAP_OFFSET+=1  \nENDC\n  ENDM\n"
  ;X counts every element that is not the search term, then redefines itself to be a null macro after
  GETINDEX_X_DEF
  INCLUDE "src/ui_elements.inc"
  PURGE XMACRO_SEARCH_TERM
  PURGE GETINDEX_X_DEF
  ;We will need to use X macros to set the offset to left side of the variable drawing space: 4*index.
  DEF VALSMAP_OFFSET *=4
  IF VALSMAP_OFFSET >= 20 ;If value is out of viewable area, wrap around to second line
  DEF VALSMAP_OFFSET+=12
  ENDC
ENDM

;@param: none
;@clobber: a, de
UpdateOptionBuffer_EdgeMode:
  GET_UI_OFFSET "CamOptEdgeMode"

  IF SCREEN_FLIP_H==1 ;With horizontal rotation, this is the leftmost (just under the text)
    ld de, OptionLinesBuffer+VALSMAP_OFFSET ;+3c3b ;Note: when adding to e, first line will not overflow, but starting at the second line of values ($9A20), it will. So between them, inc d
  ELSE ;With no rotation, this is the rightmost nybble
    ld de, OptionLinesBuffer+VALSMAP_OFFSET+3 ;+3c3b
  ENDC
  ld a,[CamOptEdgeMode] ;+2c1b ;put the value of the selected option in a 
  or a,UI_ICONS_BASE_ID
  ld [de], a ;+2c1b ;load nybble value into tilemap
  ret

UpdateOptionBuffer_CamOptE:
  GET_UI_OFFSET "CamOptE_RAM"
  IF SCREEN_FLIP_H==1
  ld de, OptionLinesBuffer+VALSMAP_OFFSET
  ELSE
  ld de, OptionLinesBuffer+VALSMAP_OFFSET+3
  ENDC
  ld a, [CamOptE_RAM] ;+4c3b
  or a,UI_ICONS_BASE_ID
  ld [de], a ;+2c1b
  ret

UpdateOptionBuffer_CamOptContrast:
  GET_UI_OFFSET "CamOptContrast"
  IF SCREEN_FLIP_H==1
  ld de, OptionLinesBuffer+VALSMAP_OFFSET
  ELSE
  ld de, OptionLinesBuffer+VALSMAP_OFFSET+3
  ENDC
  ld a, [CamOptContrast] ;+4c3b
  or a,UI_ICONS_BASE_ID
  ld [de], a ;+2c1b
  ret

UpdateOptionBuffer_DitherTable:
  GET_UI_OFFSET "CamOptDitherTable"
  IF SCREEN_FLIP_H==1
  ld de, OptionLinesBuffer+VALSMAP_OFFSET
  ELSE
  ld de, OptionLinesBuffer+VALSMAP_OFFSET+3
  ENDC
  ld a, [CamOptDitherTable] ;+4c3b
  or a,UI_ICONS_BASE_ID
  ld [de], a ;+2c1b
  ret

UpdateOptionBuffer_CamOptV:
  GET_UI_OFFSET "CamOptV_RAM"
  IF SCREEN_FLIP_H==1
  ld de, OptionLinesBuffer+VALSMAP_OFFSET
  ELSE
  ld de, OptionLinesBuffer+VALSMAP_OFFSET+3
  ENDC
  ld a, [CamOptV_RAM] ;+4c3b
  or a,UI_ICONS_BASE_ID
  ld [de], a ;+2c1b
  ret

UpdateOptionBuffer_CamOptC: 
  GET_UI_OFFSET "CamOptC_RAM"
   ;C; 4 nybbles from 2 bytes
  IF SCREEN_FLIP_H==1
    ld hl, CamOptC_RAM ;loads LOW byte's addr into hl
    ;When Hflipped, we display the least significant bytes and nybbles first -- set de to rightmost tilemap position, then dec it while incrementing hl, our source
    ld de, OptionLinesBuffer+VALSMAP_OFFSET+1 ;+2c2b ; go to leftmost tile of left byte (LSB's LSN)+1
    call UpdateByteInTilemap_flipped ;3b
    inc e ;display LSB:H
    inc e 
    inc e ;^^+3c3b
    call UpdateByteInTilemap_flipped ;3b
  ELSE
  ld hl, CamOptC_RAM ;loads LOW byte's addr into hl
  ;When Hflipped, we display the least significant bytes and nybbles first -- set de to rightmost tilemap position, then dec it while incrementing hl, our source
  ld de, OptionLinesBuffer+VALSMAP_OFFSET+2 ;+2c2b ; go to leftmost tile of left byte (LSB's LSN)+1
  call UpdateByteInTilemap ;3b
  dec e ;display LSB:H
  dec e
  dec e
  call UpdateByteInTilemap ;3b  
  ENDC
  ret


UpdateOptionBuffer_CamOptO: 
  GET_UI_OFFSET "CamOptO_RAM"
  DEF VALSMAP_OFFSET +=1 ;O; 2 nybbles
  ld hl, CamOptO_RAM
  IF SCREEN_FLIP_H==1 
  ;if screen flipped, de is second-to-right and we want to move it to the one-after-leftmost
  ld de, OptionLinesBuffer+VALSMAP_OFFSET
  ELSE
  ld de, OptionLinesBuffer+VALSMAP_OFFSET+1
  ENDC
  call {UpdateByteInTilemap_rotation}
  ret


UpdateOptionBuffer_CamOptG:
  GET_UI_OFFSET "CamOptG_RAM"
  ;G; 2 nybbles
  ld hl,CamOptG_RAM
  IF SCREEN_FLIP_H==1 
  ld de, OptionLinesBuffer+VALSMAP_OFFSET+1
  ELSE
  ld de, OptionLinesBuffer+VALSMAP_OFFSET+2
  ENDC  
  call {UpdateByteInTilemap_rotation}
  ret

UpdateOptionBuffer_Undecided1:
  GET_UI_OFFSET "RemoteJoypadActive"
  ; 2 nybbles
  ld hl,RemoteJoypadActive
  IF SCREEN_FLIP_H==1 
  ld de, OptionLinesBuffer+VALSMAP_OFFSET+1
  ELSE
  ld de, OptionLinesBuffer+VALSMAP_OFFSET+2
  ENDC  
  call {UpdateByteInTilemap_rotation}
  ret


UpdateOptionBuffer_Undecided2:
  ret

;--------------------------------Handover Payloads---------------------------------------------------------
HRAM_stock_stub: ;10 bytes -- this is the function that will go in stock ROM's HRAM instead of the normal OAM DMA function.
  .start
  ;switch bank to VRAM1
  ld a,$01 ;2b
  ldh [rVBK],a ;2b
  ;jump to location of our checker
  jp HandoverChecker ;3b
  .return_point
  DEF HRAM_RETURN_POINT EQU (.return_point - .start) + $FF80
  ;switch bank from VRAM1
  ;xor a ;1b -- must be done in checker function, as we don't have enough space here
  ldh [rVBK],a ;2b
  ret ;1b
  .end
  DEF HRAM_stub_size EQU HRAM_stock_stub.end - HRAM_stock_stub.start
  assert HRAM_stub_size <= 10, "HRAM_stock_stub must fit within 10 bytes"

  ;decimal 13 bytes -- goes at the end of the copied init sequence in VRAM1 or SRAM
  ;calls memclr on WRAM (except stack) and completes handover to later part of ROM init sequence
CompleteHandover_storage:
  .start
  ld hl, $C000 ;3b
  ld bc,$1ff0 ;3b ;Erase all except (a few bytes of) stack, so we can properly return.
  ;'Call' memclr with a return address of HANDOVER_ADDR. This code will be erased during the memclr call, so we can't return here.
  DEF HANDOVER_ADDR EQU $01ac
  ld de,HANDOVER_ADDR ;3b ;push return address: point in the ROM where we return control
  push de ;1b
  jp ROM_MEMCLR_ADDR ;3b
  .end
  DEF CompleteHandover_SIZE EQU CompleteHandover_storage.end - CompleteHandover_storage.start

checker_payload:
  DEF HandoverChecker EQU _VRAM
  .start
  ;OAM copy 
  ld a,$d4
  ldh [rDMA],a
  ld a,$24 ;OAM copy waitloop
  :dec a
  jr nz, :-
  ;check if reset button combination is pressed. Since this doesn't use WRAM, we can check during OAM DMA
  ldh a, [joypad_active] ; 3c
  DEF ROM_RAM_HANDOVER_MASK EQU (JOYPAD_SELECT_MASK | JOYPAD_UP_MASK)
  and a,ROM_RAM_HANDOVER_MASK ; 2c
  cp a, ROM_RAM_HANDOVER_MASK ; 2c
  jr z, .handoverToRAM ;2 or 3c -- for minimum, it's 2
  xor a   ;a must be zero on return to allow the HRAM code to switch back to VRAM bank 0
  jp HRAM_RETURN_POINT
  
  .handoverToRAM
  ;turn off the screen to ensure VRAM1 is accessible
  xor a ;1c
  ldh [rLCDC],a ;3c
  ;restore unbanked WRAM0 code
  ld a,BACKUP_BANK ;2c
  ldh [rSVBK],a ;3c
  ld de, _RAM ;3c
  ld hl, _RAMBANK ;3c
  :ld a,[hli]
  ld [de], a
  inc de
  bit 4,d ;copy until d reaches $D0
  jr z,:-
  jp _RAM

  .end
checker_payload_end:
  DEF checker_payload_size EQU checker_payload_end-checker_payload
;----------------------------------------------------------------------------------------------------------
;TODO: A hardcoded address may not be valid for all versions.
;memclr address should be stored after the 4th $cd (CALL) instruction. In my version, there are no $cd bytes aside from calls between $150 and there   
DEF ROM_MEMCLR_ADDR EQU $043f
;Due to RAMbank switching immediately before handover, this must either be located in unbanked RAM, or use a trampoline at the end
StartHandover::
  di;Vblank ISR might interfere with our code -- disable interrupts, though we will disable the screen, so this should not be a problem.
  ;Load HRAM stub into HRAM
  
  ;Load check + OAM transfer code into Vbank1, then switch to Vbank 0
  ;turn LCD off -- otherwise, blanking VRAM will have 'streaks' where it was not cleared.
  .waitVBlank: ; Do not turn the LCD off outside of VBlank
  ldh a, [rLY]
  cp 144
  jr c, .waitVBlank

  xor a
  ldh [rLCDC], a ;turn off LCD -- this enables VRAM will always be accessible for the long copy coming up -- otherwise will rst and we'll lose control
  ;Clear VRAM1 attribute tables 9800-9bff
  ld a,$01
  ldh [rVBK],a
  
  ld hl,$9800
  ld b, $9c
  :xor a
  ld [hli],a
  ld a,h
  cp b
  jr nz,:-

  ;Load checker payload into VRAM1 tiledata $8000-97FF
  ld de, _VRAM ;destination: VRAM1:8000
  ld hl, checker_payload
  ld b, checker_payload_size
  :ld a,[hli]
  ld [de],a
  inc de
  dec b
  jr nz,:-
  ;Switch back to VRAM0
  xor a
  ldh [rVBK],a

  ;Switch mapped RAMbank to one dedicated for use by the stock ROM
  ld a,STOCK_RAMBANK
  ldh [rSVBK],a

  ;Load HRAM stub
  ld hl, HRAM_stock_stub
  ld de, _HRAM
  ld b, HRAM_stub_size
  : ld a,[hli]
    ld [de], a
    inc de
    dec b
    jr nz,:-
  ;load the rest of HRAM with 00s
  xor a
  ld b,$7F-HRAM_stub_size
  :ld [de],a
  inc de
  dec b
  jr nz,:-

  ;Copy Init code from ROM and NOP out the HRAM clear/OAM copy functions $150-19f -- $50 bytes
  ld hl, $0150
  ld de, $D000
  ld b, $50
  : ld a,[hli]
    ld [de],a
    inc de
    dec b
    jr nz,:-
  ;add wram memclr and complete handover code to the end
  ld hl, CompleteHandover_storage
  ld b, CompleteHandover_SIZE
  :ld a,[hli]
  ld [de],a
  inc de
  dec b
  jr nz,:-

  ;patch out the call to memclr WRAM at +$87,88,89
  ld hl,$D037
  xor a
  ld [hli],a
  ld [hli],a
  ld [hli],a

  jp $D000 ;run copy of init code with patches

;Called by the Vblank ISR. Draws one of the 14 lines in the vertical UI

DrawSidebar:
  ld hl, UIBuffer_Vertical ;+3
  ;calculate src address
  ldh a,[Vblank_Sidebar_DrawLine] ;+3
  inc a ;+1
  ;if the line to draw is greater than 13, reset it to 0.
  cp a, 14 ;+2
  jr nz, :+ ;+3/2
  xor a ;+1
  :ldh [Vblank_Sidebar_DrawLine],a ;+3
  ;Calculate src address
  add a,a ;+1
  add a,a ;+1
  ld b, $00 ;+2
  ld c, a ;offset bc = DrawLine * 4 ;+1
  add hl,bc ;+2
  ld d,h ;de = start of 4 source addresses: UIBuffer_Vertical + (rownum*4) ;+1
  ld e,l ;+1

  ;Calculate destination address
  IF SCREEN_FLIP_V
  cpl
  inc a
  add a,52 ;If Vflipped, mirror line to draw to from top by changing a from rownum*4 to (13-rownum)*4 = 54-(rownum*4)
  ENDC
  IF SCREEN_FLIP_H ;If Hflipped, we'll start at the end of the line
  ld hl, TILEMAP_UI_ORIGIN_V+3
  ELSE
  ld hl, TILEMAP_UI_ORIGIN_V ;+3
  ENDC
  add a,a ;+1
  add a,a ;+1
  add a,a ;this last leftshift will overflow a; if there was a carry, increment b ;+1
  jr nc,:+ ;+3
  inc b ;+1
  :ld c,a ;+1
  add hl,bc ; hl = start of 4 dest addresses: TILEMAP_UI_ORIGIN_V + (rownum*32) ;+2

  IF SCREEN_FLIP_H
  ;Write buffer to the line, last to first ;19c
  ld a,[de] ;+2
  ld [hld],a ;+2
  inc e ;de(source buffer) does not cross byte boundaries ;+1
  ld a,[de]
  ld [hld],a
  inc e
  ld a,[de]
  ld [hld],a
  inc e
  ld a,[de]
  ld [hl],a
  ELSE
  ;Write buffer to the line in-order;19c
  ld a,[de] ;+2
  ld [hli],a ;+2
  inc e ;de(source buffer) does not cross byte boundaries ;+1
  ld a,[de]
  ld [hli],a
  inc e
  ld a,[de]
  ld [hli],a
  inc e
  ld a,[de]
  ld [hl],a
  ENDC
ret

;@param c = size in bytes
;@param de = dest
;@param hl = source
;Copies data from src to dest.
memcpy8_hl_to_de::
  :ld a,[hli]
  ld [de],a
  inc de
  dec c
  jp nz,:-
ret
;@param b = size in bytes
;@param de = dest
;@param a = value with which to fill
;Fills b bytes of de with a
memfill8_a_into_de_sizeb::
  :ld [de],a
  inc de
  dec b
  jp nz,:-
ret


;@param  c = size in bytes
;@param hl = pointer to str1
;@param de = pointer to str2
;Compares the (possibly non-null-terminated) strings in hl and de. Returns 0 in a if the strings are equal, -1 otherwise.
;@clobber a,hl,de,c
memcmp::
  .loop: 
  ld a,[de]
  inc de
  cp a,[hl] ;check if vlaues are equal
  inc hl ; doesn't affect flags
  jr z,:+
    ld a,-1 ;If the values are different, return -1
    ret
  :dec c
  jr nz,.loop ;if we've not reached end of buffer, loop
  
  ld a,0 ;if we've reached end of buffer with all bytes equal, return 0
ret

;Subroutine jumped to early in the init process -- since it's launching an alternate payload, it does not return.
LaunchAlternativePayload::
;Switch to the bank containing the alternate payload.
;Since we aim for DMG support here, we need to be able to do this without bankswitching.
;Presumably this means that we should store the alternative payload in the last bank that the launcher copies.
;Alternatively, we could check at the start of launcher for DMG mode, but that means we would have two separate launch systems for the alternate payload: here (for GBC) and in the launcher (for DMG) 
;in the launcher for DMG ROM

;A simpler solution is to set the flags as if it were a DMG, jump back to the launcher ROM, and let the launcher rom do the alternate payload.
;+We won't need to make the entire launcher DMG-compatable
;- The alternative payload will run from either CGB-mode or DMG mode, depending on what it's launched from.

xor a ;set flags as if DMG/MGB/SGB -- DMG/SGB is 01, MGB/SGB2 is $FF, but as long as it's not $11 we're not CGB.
jp $100 ; jp back to launcher ROM


GetSerialInput:
CheckSerial:
  .startTransfer
  ld a, SCF_START | SCF_SPEED | SCF_SOURCE ;Transfer enable, high clock speed, internal clock
  ldh [rSC], a
  ;Input format is D U L R: START SEL A B
  GetInputPtr: call $0000
  ;Here, the transfer should be finished and SC should be reset to 0.
  ;TODO: wait for SC to be low, with timeout. Currently we just assume the transfer finished.
  ldh a,[rSB]
  ;Verify the sent packet <= maximum valid value ($5E)
  cp a,DECODING_MAX_VALUE+1 ;if carry, a<MAX+1 (packet data) (invalid packet)
  jp nc, .badPacket

  ;Decode: add a to hl
  ld hl, DecodingTable
  add a, l
  ld l,a
  adc h
  sub l
  ld h,a
  ld a, [hl]
  ;if decoding returned FF, the encoding is invalid.
  cp a,$FF
  jp z,.badPacket
  ld [RemoteJoypadState],a



  ;Count down, filter out non-new inputs if counter is not hit
  ;RemoteJoypadActive = (RemoteJoypadState xor RemoteJoypadPrevState) and RemoteJoypadState "changed keys, but only the ones that are currently pressed"
  ld b,a ; b = RemoteJoypadState
  ld a,[RemoteJoypadPrevState]
  ld c,a ;c = RemoteJoypadPrevState, for later
  xor a,b
  and b
  ld [RemoteJoypadActive],a ;active is only newly-pressed buttons
  ;If no new joypad data (or 0), reset the counter to threshold
  ld a, b
  and a ; check if RemoteJoypadState is 0
  jp z, .notHeld
  xor a,c ; a = State xor PrevState
  jp nz, .notHeld
  .held
  ;decrement the hold counter
  ld hl,RemoteJoypadHoldCounter
  dec [hl]
  jp nz,.updatePrev
  ;if hold counter is zero,
  ld a,b ; a = RemoteJoypadState
  ld [RemoteJoypadActive],a ; set RemoteJoypadActive to RemoteJoypadState
  ld a,[RemoteJoypadHoldThreshold]
  ld [RemoteJoypadHoldCounter],a ;Reset remote joypad's hold counter

  .notHeld

  .updatePrev
  ld a,b ;RemoteJoypadPrevState = RemoteJoypadState
  ld [RemoteJoypadPrevState],a
  ret

.badPacket
  xor a
  ld [RemoteJoypadPrevState],a
  ld [RemoteJoypadActive],a
  ret

  ;---------------------------------Save Data Functions-----------------------------------------------------
  INCLUDE "src/save.asm"


    ;-------------------------------------------DATA-------------------------------
Viewfinder_UI_Tiles:
    incbin "assets/viewfinderUI.1bpp", 0,256
gfxButtons_storage:
    incbin "assets/UserButtons.1bpp",0,128
gfxActions_storage:
    incbin "assets/actions.1bpp",0,128
gfxObjects0_storage:
    incbin "assets/objects0.1bpp",0,128
;Constant values space
; ;define the SelectedOptionsEntry struct
; RSRESET
; DEF SelectedOptionsEntry_num_bits RB 1
; DEF SelectedOptionsEntry_min RB 1
; DEF SelectedOptionsEntry_max RB 1
; DEF SelectedOptionsEntry_addr RW 1
; DEF SelectedOptionsEntry_SIZEOF RB 0
; ;This table contains information about what to do with each menu entry in the camera options menu -- option number selected is implicit in the index
; ;num_bits   min     max     addrL   addrH
; SelectedOptionsTable:
; db $01, $00, $01
;     dw CamOptN_RAM ;N (0-1)
; db $02, $00, $03
;     dw CamOptVH_RAM ; V(H) (Vertical/Horizontal Edge Operation Mode) 00-11
;     ; 10 unused but we can keep it for simplicity
; db $10, $00, $FF 
;     dw CamOptC_RAM; C (Exposure Time) 0000-FFFF
; db $06, $20, $3F 
;     dw CamOptO_RAM; O  (Output Reference Voltage) 6 bits 20-3F
;     ; bit O5 is the sign and it's always positive. Questionably, bit O4 might not move in real hardware so we can possibly just make it one nybble 20-2F
; db $05, $00, $0A 
;     dw CamOptG_RAM; G (Analog Output Gain) 5 bits, but highest bit is unused in a real camera, usually 0/4/8/A
; ;this should go through all 10 options, but I currently only hav 5

;These tables follow the N(VH)COG / E V Contrast Dither <Null> pattern
; SelectedNumBitsTable: ;Number of bits we care about in each variable.
; db $01, $02, $10, $06, $05
; db $04, $03, $04, $02, $00
SelectedMaxNybblesTable:: ;Max nybble position each entry in the menu has -- manually set based off of Ceiling(SelectedNumBitsTable>>2)-1
  MACRO X
    db \3
  ENDM
INCLUDE "src/ui_elements.inc"
SelectedMinTable::
  MACRO X
    db \4
  ENDM
INCLUDE "src/ui_elements.inc"
SelectedMaxTable::
  MACRO X
    db \5
  ENDM
INCLUDE "src/ui_elements.inc"
;E (first entry of 2nd row) is technically 4 bits, but setting E3 does edge extraction, we'd have to set Z to 0 for that, and I can't imagine anyone would use it.
SelectedAddrTable:: ;holds the addresses of the WRAM variables for each camera register
  MACRO X
    dw \1
  ENDM
INCLUDE "src/ui_elements.inc"
;dw CamOptEdgeMode, CamOptVH_RAM, CamOptC_RAM, CamOptO_RAM, CamOptG_RAM
;dw CamOptE_RAM, CamOptV_RAM, CamOptContrast, CamOptDitherTable, CamOptDitherPattern

;Must not crrss byte-address boundary
EdgeControlModes::
  ;An entry is stored as 00000:VH1:VH0:N.
  db %00000000, \; Positive image N VH = 0 00
  %00000010, \; Horizontal Enhancement N VH = 0 01
  %00000101, \; Vertical Enhancement N VH = 1 10
  %00000111  ; 2D Enhancement N VH = 1 11

;Bayer matrix that maps threshold values to a dither pattern: 16 bytes
; OrderTable: db  15, 7, 13, 5, \
;   3, 11, 1, 9, \
;   12, 4, 14, 6, \
;   0, 8, 2, 10
;Precalculated table that jumps from one address to another
;We start at i=15, which is luckily our first value!
;OrderTableRelative[i] = currentposition - originalTable[i]
;currentposition = originaltable[i-1]; originaltable[-1]=15
OrderTableRelative: db (7-15),(13-7), (5-13), \
      (3-5),(11-3), (1-11), (9-1), \
      (12-9), (4-12), (14-4), (6-14), \
      (0-6), (8-0), (2-8), (10-2)
.end

LoaderTitle:: db "SILIH"
.end

;Any entry >$7F has the Z bit set and is invalid as a matter of course
;Due to degreees of freedom in encoding, the encoder should only produce outputs <$5F
;There are some valid encodings between $60 and $7F that are in-spec, but our encoder shouldn't produce them, and we can ignore them to save space.
DecodingTable:
  db $FF,$12,$14,$FF,$21,$FF,$FF,$28,$41,$FF,$FF,$48,$FF,$82,$84,$FF
  db $51,$FF,$FF,$58,$FF,$62,$64,$FF,$FF,$92,$94,$FF,$A1,$FF,$FF,$A8
  db $10,$FF,$FF,$20,$FF,$40,$80,$FF,$FF,$50,$60,$FF,$90,$FF,$FF,$A0
  db $FF,$01,$02,$FF,$04,$FF,$FF,$08,$00,$FF,$FF,$00,$FF,$00,$00,$FF
  db $11,$FF,$FF,$18,$FF,$22,$24,$FF,$FF,$42,$44,$FF,$81,$FF,$FF,$88
  db $FF,$52,$54,$FF,$61,$FF,$FF,$68,$91,$FF,$FF,$98,$FF,$A2,$A4;,$FF
;  db $FF,$10,$20,$FF,$40,$FF,$FF,$80,$50,$FF,$FF,$60,$FF,$90,$A0,$FF
;  db $01,$FF,$FF,$02,$FF,$04,$08,$FF,$FF,$00,$00,$FF,$00,$FF,$FF,$00

EndRAM0:
    assert EndRAM0 < WRAM_Var_Area0, "Code is outside of $D000-$100stack-$78 OAMtemp - variables area."
ENDL
 
  
SECTION "OAM Work Area SECTION", WRAM0[$CE00]
  ;OAM_Work_Area_Storage:
  ;LOAD "OAM Work Area LOAD", WRAM0[$CE00]
  OAM_Work_Area:
  ;I should probably use a struct for this instead of manually defining every field of every sprite
  ;Since I don't use many sprites it doesn't matter _too much_
  Sprite0_CursorY:: db
  Sprite0_CursorX:: db
  Sprite0_CursorTileIndex:: db
  Sprite0_CursorAttr:: db

  DS $A0;, $00 ;$A0 bytes allocated to store OAM -- we may not need all 40 objects, so we can shrink this to 4*(number of objects) if we really need space.
;ENDL

SECTION "Stack Area",WRAM0[$CEE0]
  Stack_Area:
  DS $100

  DEF DITHER_BASE_ROMBANK EQU $0A ;base of light
  DEF DITHER_BASE_TABLE EQU $7C20
  DEF DITHER_BASE_TABLE_LIGHT EQU $7C20
  DEF DITHER_BASE_TABLE_DARK EQU $7C60
