; SPDX-License-Identifier: MIT

INCLUDE "src/hardware.inc"             ; system defines

INCLUDE "src/general.inc"

;This section exists to ensure the addresses of WRAM tables are all aligned on 256bytes (h never changes)
;If we run out of space, make another $100-size section before it
SECTION "WRAM Variables Area 0 SECTION", WRAM0[$CD00]
WRAM_Var_Area0:
UNION
;64 bytes
TopbarBuffer: DS 64 ;UpdateByteInTileMap assume this line's addresses are aligned on 256 bytes. Since we can only see the first 20 bytes of each 32-byte line, we can use the last 12 bytes for storage
;A line buffer size of 64 means two lines of data. We may want to split this into two labels so we can work with the lines separately, but we may also just be able to work with it contiguously.
;TopbarBuffer is HDMA'd, meaning it must be aligned on 16bytes
NEXTU
DS 20
;Previously had to be placed in UI order for UpdateTopbarBuffer, but it no longer cares.
;PrepareCameraOpts doesn't care about their in-memory order, so it should be safe to move them around otherwise.
;8 bytes
  CamOptN_RAM:: db ;N must be stored immediately before VH for SetNVHtoEdgeMode
  CamOptVH_RAM: db
  CamOptC_RAM:: ;CamOptC_RAM is stored in little-endian order, inconsistent with the CAM registers, but easily updated by modify_nybble
    CamOptC_RAM_L: db 
    CamOptC_RAM_H: db
  CamOptO_RAM:: db
  CamOptG_RAM:: db
  ;E (first entry of 2nd row) is technically 4 bits, but setting E3 does edge extraction, we'd have to set Z to 0 for that, and I can't imagine anyone would use it.
  CamOptE_RAM:: db
  CamOptV_RAM:: db
  ;Non-CAM-register manual config options: 4 bytes
  CamOptContrast:: db ; 0-F
  CamOptDitherTable:: db ;selects whether to use light or dark dither table. 0=light (sramA:7c20), 1=dark (sramA:7c60)
  CamOptDitherPattern:: db
  CamOptEdgeMode:: db ;Meta-option that modifies NVH
DS 20
;Insert more variables here,12 bytes
ENDU

  ;These variables are downstream of the above individual CamOpt variables. UpdateCameraOpts uses these contiguously and with CamOptDither_RAM, so they must be contiguous
  ; 5 + 48 bytes
  CamOptA001_RAM:: db
  CamOptA002_RAM:: db
  CamOptA003_RAM:: db ; Watch out here: C may be stored in RAM little-endian, but the camera registers use big-endian.
  CamOptA004_RAM:: db
  CamOptA005_RAM:: db
  
  CamOptDither_RAM: ds 48 ; Working copy of the dither table
.end  
  ;Video mirror register: 1 byte
  wLCDC:: db ;LCDC can be modified at any time, but there may be bugs with changing window visibility while drawing, so we use this to only update LCDC between frames.
  ;Must not be placed in the TopbarBuffer region as it is cleared after wLCDC was written.
  
  ;HDMA variables: 7 bytes
  hdma_current_transfer_length: db ; Number of 16-byte DMA blocks that the current DMA transfer is set to transfer.
  hdma_total_transferred: db ; Total number of 16-byte DMA blocks that have been transferred in the 
  hdma_total_remaining: db  ; Remaining number of 16-byte DMA blocks that have yet to be transferred.  
  wHDMA1: db  ; Since VBlank interrupts stop and start DMA transfers by writing to HDMAx, we may overwrite them.
  wHDMA2: db  ; The outside-of-interrupt code to start an HDMA transfer writes to these in addition to rHDMA.
  wHDMA3: db  ; The code in the Vblank ISR that draws the Topbar restores rHDMA values from these variables after it finishes its GDMA transfers if no transfer was in progress.
  wHDMA4: db  ; We can't push-pop rDMA1-4 in the ISR because they're write-only. 
  
  ;Save Slot variables: 2 bytes
  SAVE_SLOTS_FREE:: db
  ShowPromptsFlag:: db  
  ;1 byte
  ContrastChangedFlag: db ;When contrast is changed, set this to 1
                        ;May be wise to change this to a CamOptChangedFlags bitfield to check if CamRegs changed, Contrast, Dither Pattern, or Dither Lighting
  ;5 bytes
  RemoteJoypadHoldThreshold: db ;TODO: This is unneccessary. We can just use the regular hold threshold.
  RemoteJoypadHoldCounter: db
  RemoteJoypadState: db
  RemoteJoypadPrevState: db
  RemoteJoypadNewPressed: db
  RemoteJoypadActive: db
  ;3 bytes
  BGPaletteChangeSrc: db ; Index of source within the BG palette table (each entry is 8 bytes)
  BGPaletteChangeDest: db ; Index of destination BG palette to be changed.
  BGPaletteChangeFlag: db ; When this is set, the Vblank handler should set the background palette according to BGPaletteChange{Src,Dest). This flag should be set after the other 2.
  
  ;7 bytes
  Setting_SerialRemote:: db ;When set, enables serial remote control.
  Setting_AEB_Interval:: db ;AEB shift: each AEB step either adds or subtracts previousExposure/(2 to the power of this value)
  Setting_AEB_Count:: db  ;Burst shot / AEB count: how many captures are taken if in Burst/AEB mode.
  Setting_Burst_AEB:: db  ;Camera mode: determines whether we're taking a single, burst, or AEB shot. Values are set by CAMERA_MODE_xxx 
  Setting_DelayTime:: db
  Setting_TimerEnable:: db
  Setting_OnTakeAction:: db ;OnTakeAction: determines what happens when a capture is completed and user confirms (for single shot), and when a capture is completed (in burst/AEB mode)

  ;2 bytes
  BurstShotRemainingCaptures: db
  BurstShotCurrentCapture: db
  
  ;16 bytes
  GeneratedDitherThresholds: ds 16 ;temporary storage space for dither threshold values from GenerateThresholdsFromRange. Used 3 times per dither pattern construction. Must not cross address byte boundary
  .end
  ;56 bytes
  SidebarBuffer:: ds $38 ;4x14 bytes: holds the tilemap information for the vertical UI. Must be aligned on 256 within a line due to 8-bit math
  BurstExposureList: ds $1E ; Stored little-endian, same as CamOptC_RAM, opposite of CamOptA00x
                            ; to hold $0F C-values, we need $0F * 2 bytes
  SerialEnable:: db  ; This is used by printing logic to tell the serial input logic to stop. Initialized to 1. 
                    ; When 0, we should stop initiating transfers with the remote control, since these will interfere with the printer's transfers.
  InTransaction_StepNumber:: db ;When in a printing transaction, this is set to the step number within the transaction before the packet is sent.If something goes wrong, we can display this step number for debugging.
  PrintTransaction_Keepalive_RetryCount:: db ;When a transaction fails due to a bad keepalive, 
  PrintPacket_Checksum_RetryCount:: db ;When a packet gets a "bad checksum" error (status bit 0), the packet should retry sending a certain number of times before giving up.


  NullVar: db     ;Placeholder variable that can be changed with no consequences. Currently used for ui_elements unused elements in the middle.

  ;Cumulative $FD bytes
  ;$02 bytes remaining
  .endVariables
assert .endVariables < OAM_Work_Area, "Variables are outside of $CD00-$CDFF - variables area, and stomping on OAM area"


SECTION "Payload SECTION",ROM0[$1000]
PayloadStorage::
    LOAD UNION "Payload LOAD", WRAM0 [$C000]
PayloadEntrypoint:
    xor a
    ldh [rNR52], a ;disable sound
  
    ld sp, _RAMBANK-1 ;set the stack pointer to top of lowRAM

    INCLUDE "src/init.asm"

MainLoop:
  ; Things done every frame
  xor a
  ldh [VBlank_finished_flag],a ;clear the VBlank finished flag

  call GetInput

  ld a, UI_RAMBANK
  ldh [rSVBK], a

  jp HandleInput   ;jump to the input handler subroutine, and at the end it will jump back here.

HandleInputDone::

call UpdateTopbarBuffer

;Checks (ideally, these work somewhat like interrupts)
ViewfinderChecks:

    ;If VBlank ISR has run, jump back to the stuff done every frame. If it's not run yet, keep checking for events to handle
    ldh a, [VBlank_finished_flag]
    and a
    jp nz, MainLoop
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
    DW vfActionTransition

VBlank_ISR: ;We have 1140 M-cycles to work our magic here
  ;Because we are jumping from the ROM's vblank ISR, we lose 22 cycles to PUSHesx4 and call to+jp from HRAM
  pop bc ;the top of the stack is the address of the ROM's VBlank ISR. Since we don't want to return to it, we pop it off the stack and don't use it.
  
  ldh a,[VBlank_AnimationCounter]
  inc a
  ldh [VBlank_AnimationCounter], a

  ;cursor transparency effect: alternate cursor sprite blank(Vbank1)->white(Vbank0) every other frame. The sprite has the same ID, just different banks. 
  ;to deal with extra colors, instead of moving it every frame, change its sprite every frame from (dark+transparent) to (light+transparent)
  ;We could create some frame counters to do looping animations and such with VBlank_AnimationCounter.
  ;Vblank_Sidebar_DrawLine could also be used
  ;animation order:
  ;sprite0+palette0
  ;sprite1+palette0
  ;sprite1+palette1 --simpler if we set to sprite 0
  ;sprite0+palette1 --simpler if we set to sprite 1
  DEF CURSOR_SPRITE_0 EQU 2
  DEF CURSOR_SPRITE_1 EQU 3
  DEF CURSOR_PALETTE_0 EQU 0
  DEF CURSOR_PALETTE_1 EQU 1

  .periodFourCheck
  bit 1,a
  jr nz,.periodFourNotSet
  .periodFourSet
    ld a, [OAM_Work_Area+3] ; 4c3b
    and a,%11111000;2c2b ;set palette to cursor palette 1
    or a,CURSOR_PALETTE_1
    ld [OAM_Work_Area+3], a
  jr .periodFourEnd 
  .periodFourNotSet
  ld a, [OAM_Work_Area+3] ; 4c3b
  and a,%11111000;2c2b ;set palette to cursor palette 0
  or a,CURSOR_PALETTE_0
  ld [OAM_Work_Area+3], a

  .periodFourEnd
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

  call DrawTopbar_DMAMethod  ;TODO inline this
  call DrawSidebar ;TODO inline this
  call ModifyPalette

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
  call PrepareCameraOpts
  call StartCapture
  ld a, VF_STATE_TRANSITION
  ldh [viewfinder_state], a

  ld a,VF_ACTION_VIEWFINDER
  ldh [vfNextAction],a

  jp ViewfinderChecks


vfActionTransition:
  ldh a,[vfNextAction]
  ldh [vfCurrentAction],a

  ;Run entry code and set viewfinder state
  and a ;VF_ACTION_VIEWFINDER is 0
  ;If currentAction is VF_ACTION_VIEWFINDER, call StartCapture and set VF_STATE to _CAPTURING
  jr nz,:+
  call ActionInit_Viewfinder
  jr .endActionInit
  :dec a ;VF_ACTION_TAKE_SINGLE is 1
  jr nz,:+;If currentAction is VF_ACTION_TAKE_SINGLE, set VF_STATE to _PAUSED
  call ActionInit_TakeSingle
  jr .endActionInit
  :dec a ;VF_ACTION_BURST is 2
  jr nz,:+  ;If currentAction is VF_ACTION_BURST, (precalculate capture parameters), set capture parameters, (set counter), start capture, and set VF_STATE to _CAPTURING
  call ActionInit_Burst
  jr .endActionInit
  : ;check for next Action

  .endActionInit
  ; To avoid needless latency, jump back to viewfinder handler
jp ViewfinderChecks

ActionInit_Viewfinder:
  call PrepareCameraOpts
  call StartCapture

  ld a,VF_ACTION_VIEWFINDER
  ld [vfNextAction],a

  ld a, VF_STATE_CAPTURING
  ldh [viewfinder_state],a
ret

ActionInit_TakeSingle:
  ld a, VF_STATE_PAUSED
  ldh [viewfinder_state],a
ret

ActionInit_Burst:
  
  ld a,[Setting_AEB_Count]
  ld [BurstShotRemainingCaptures],a ;Set BurstShotRemainingCaptures to Setting_AEB_Count

  xor a
  ld [BurstShotCurrentCapture],a ;Set BurstShotCurrentCapture to 0

  call PrepareCameraOpts

  ld a,[Setting_Burst_AEB]
  cp a,CAMERA_MODE_AEB
  jr nz,:+
  call FillAEBList   ;Populate the AEB list with C-values if we're doing an AEB shot
  call GetAEBExposure   ;Modify capture parameters
  ld a,h
  ld [CamOptA002_RAM],a
  ld a,l
  ld [CamOptA003_RAM],a
  :

  call StartCapture
  ld a,VF_STATE_CAPTURING
  ldh [viewfinder_state],a


ret


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
  ld [hdma_total_transferred],a ; total transferred 16-byte DMA transfer blocks is 0
  ld a,$E0
  ld [hdma_total_remaining],a ;remaining DMA blocks so far: all of them!
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
    ld a,[vfCurrentAction]
    cp a,VF_ACTION_BURST  ;If currentAction is viewfinder, just set viewfinder_state to VF_STATE_TRANSITION and move on
    jr nz, .notInBurstAEB  ;If currentAction VF_ACTION_TAKE_SINGLE (impossible, as vfActionTransition will never cause this), do the same
    .inBurstAEB
      ;if doing a burst shot, save/print/transfer, decrement remaining shot counter, increment current shot number. 
      call vfCompleteAction
      ld hl,BurstShotCurrentCapture
      inc [hl]
      ld hl,BurstShotRemainingCaptures
      dec [hl]
      jp z,.burstComplete
      .burstIncomplete ;If nonzero, change camera parameters and restart capture, next state is Capturing.
        ;Change capture parameters if in AEB mode
        call PrepareCameraOpts

        ld a,[Setting_Burst_AEB]
        cp a,CAMERA_MODE_AEB
        jr nz,:+
          call FillAEBList   ;Populate the AEB list with C-values if we're doing an AEB shot
          call GetAEBExposure   ;Modify capture parameters
          ld a,h
          ld [CamOptA002_RAM],a
          ld a,l
          ld [CamOptA003_RAM],a
        :
    
        call StartCapture
        ld a, VF_STATE_CAPTURING
        ldh [viewfinder_state],a
      jp ViewfinderChecks

      .burstComplete       ;If zero, set next action to viewfinder, and next state is Transition.
        ld a,VF_ACTION_VIEWFINDER
        ld [vfNextAction],a

        ld a, VF_STATE_TRANSITION
        ldh [viewfinder_state],a

      jp ViewfinderChecks
    jp ViewfinderChecks
  .notInBurstAEB
      ld a, VF_STATE_TRANSITION
      ldh [viewfinder_state], a
    jp ViewfinderChecks


PauseHandler:
  ;When menustate is SELECTED, restart capture
  ldh a, [MENU_STATE]
  cp a,MENU_STATE_SELECTED
  jp nz, ViewfinderChecks;if carry, we're in one of the camera states where viewfinder is active and need to restart the capture + change viewfinder state
  call PrepareCameraOpts
  call StartCapture
  ld a, VF_STATE_CAPTURING
  ldh [viewfinder_state], a
  jp ViewfinderChecks


;------------------------------------------------------------------------------
;Functions
;------------------------------------------------------------------------------

/**
* Updates the real CAM registers and signals the capture hardware to begin a capture. 
* Before calling, make sure to update shadow camopt registers with PrepareCameraOpts.
* @clobber a,hl,de
*/
StartCapture:
    ld a, $10
    ld [rRAMB], a ; switch to GB Camera register
    call UpdateCameraOpts
    ld a, %00000011 ;Start capture
    ld [$A000], a
    ret 


/**
* Loads 1bpp tiles into VRAM (skipping every other dest byte)
* Assumes VRAM accessible and size of <=$100 bytes.
* @param hl: source
* @param de: destination
* @param b: size (in source bytes), minus one (this is so that we can transfer $100 bytes and a size of 0 is unused)
* @clobber a, b, de, hl
*/
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

/**
* Converts the individual CamOpt_xx_RAM variables into 5 shadow CAM register variables that can be applied to A001-A005 using UpdateCameraOpts.
* Should be run right after updating CamOpt_xx_RAM varibles -- we get increased latency if we run this before each capture.
* @clobber a, d
*/
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

/**
* Writes from the shadow CAM registers to the real CAM registers in SRAM$10:A0xx
* Assumes we've already switched to the Camera Opts register and loaded A00x variables using PrepareCameraOpts
* @clobber a, hl, de
*/
UpdateCameraOpts::
  ld hl, (CamOptDither_RAM.end-1) ;+3
  ld de, $A035 ;+3
  :ld a, [hld] ;+2 : 8 cycles/byte
  ld [de], a ;+2
  dec e ;+1
  jr nz, :- ;+3
ret

/**
* Updates the working tilemap buffer Option based off the underlying values
* @clobber a, de, hl
*/
UpdateTopbarBuffer:
  ;Write the values to the WRAM buffer

  call UpdateTopbarBuffer_EdgeMode

  call UpdateTopbarBuffer_CamOptC
  
  call UpdateTopbarBuffer_CamOptO

  call UpdateTopbarBuffer_CamOptG

  call UpdateTopbarBuffer_CamOptE

  call UpdateTopbarBuffer_CamOptV

  call UpdateTopbarBuffer_CamOptContrast

  call UpdateTopbarBuffer_DitherTable

  call UpdateTopbarBuffer_DitherPattern

ret

/**
* Puts the value of a byte at hl into the tilemap at de
* Changes [de] and [de-1] if flipped. [de] and [de+1] if not flipped.
* (assumes de doesn't cross a byte address boundary)
* @param hl: location of byte to put into tilemap
* @param de location in tilemap to load the data
* @return de: least-significant nybble of byte in tilemap (de-1 if flipped, else de+1),
* @return hl: hl+1
*/
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

/**
* Puts the value of a byte at hl into the tilemap at de
* Changes [de] and [de-1] if flipped. [de] and [de+1] if not flipped.
* (assumes de doesn't cross a byte address boundary)
* @param hl: location of byte to put into tilemap
* @param de location in tilemap to load the data
* @return de: least-significant nybble of byte in tilemap (de-1 if flipped, else de+1),
* @return hl: hl+1
*/
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

/**
* Draws the topbar using 2 GDMA transfers.
* Stops and restarts the current HDMA transfer, if there is one.
* @clobber a, b, de, hl
*/
DrawTopbar_DMAMethod:
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
  ldh [rHDMA1],a ;+3c2b
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

/**
* Initiates a 32-byte GDMA transfer from the first line of TopbarBuffer to the topbar in the actual tilemap.
* @clobber a
*/
StartGDMATransfer_Line1:
  ld a, HIGH(TopbarBuffer) ;+2c2b
  ldh [rHDMA1], a ;+3c2b
  ld a, LOW(TopbarBuffer) ;+2c2b
  ldh [rHDMA2], a ;+3c2b
  ld a, HIGH(TILEMAP_UI_ORIGIN_H+$20) ;+2c2b
  ldh [rHDMA3],a ;+3c2b
  ld a, LOW(TILEMAP_UI_ORIGIN_H+$20) ;+2c2b
  ldh [rHDMA4],a ;+3c2b
  ld a, %00000001 ;+2c2b ;lower 7 bits = number of $10-sized transfers minus 1. a value of 1 transfers $20 bytes. high bit 0=general-purpose DMA
  ldh [rHDMA5],a ;+3c2b
  ;+8*2 = 16 cycles for the DMA
ret

/**
* Initiates a 32-byte GDMA transfer from the second line of TopbarBuffer to the topbar in the actual tilemap.
* @clobber a
*/
StartGDMATransfer_Line2:
  ld a, HIGH(TopbarBuffer+$20) ;+2c2b
  ldh [rHDMA1], a ;+3c2b
  ld a, LOW(TopbarBuffer+$20) ;+2c2b
  ldh [rHDMA2], a ;+3c2b
  ld a, HIGH(TILEMAP_UI_ORIGIN_H+$60) ;+2c2b
  ldh [rHDMA3],a ;+3c2b
  ld a, LOW(TILEMAP_UI_ORIGIN_H+$60) ;+2c2b
  ldh [rHDMA4],a ;+3c2b
  ld a, %00000001 ;+2c2b ;lower 7 bits = number of $10-sized transfers minus 1. a value of 1 transfers $20 bytes. high bit 0=general-purpose DMA
  ldh [rHDMA5],a ;+3c2b
  ;+8*2 = 16 cycles for the DMA
ret

/**
* Restarts the HDMA transfer that transfers captured images to VRAM.
* @clobber a, hl, de
*/
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

/**
* Calls trampoline functions, functions that can be called from other WRAM banks and are themselves located in banked WRAM.
* Largely based on the GBDK's banked calling convention (https://gbdk-2020.github.io/gbdk-2020/docs/api/docs_coding_guidelines.html)
* Caller passes arguments to callee by pushing to the stack.
* Pushes the current WRAM bank to stack (as AF), switches to the callee's WRAM bank, and jumps to the function in hl.
* To access arguments on the stack, the callee should ld hl, sp+6 (2 bytes for each of caller address, caller WRAM bank, and trampoline return address)
* Then move down the stack by loading and incrementing hl
* After return, the caller must pop the argument off the stack (by using add sp,2*number of pushes)
* Callee adds 6 to sp to reach pre-trampoline top of stack and access arguments.
* @param e: WRAM bank to switch to
* @param hl: address of callee
* @clobber a,bc, and whatever the function it calls clobbers.
*/
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
NullFunction::
  ret

/**
* Fills GeneratedDitherThresholds with 16 linearly-interpolated bytes between min and max.
* GeneratedDitherThresholds is read by ArrangeThresholdsInPattern 3 times to put into CamOptDither_RAM
* @param b: min
* @param c: max 
* @clobber a,bc,de,hl
*/
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
  ;load start of range (min) into our 8.8 running sum (de)
  ld d, b
  ld e, $00 ; fractional part initialized to 0

  ; 12c
  ; Calculate 8.8 fixed-point step size bc
  ; Step size is (max-min)>>4 such that we can generate 16 linearly-interpolated values from one min and one max value)
  ld a, c ;+1
  sub a, b ;a = max - min ;+1
  swap a ;+2
  ld b, a ;temporarily hold swap(max-min) in b ;+1
  and a, $F0 ;+2
  ld c, a ;stepsize_L = swap(max-min)[7:4] ;+1
  ld a, b ;+1
  and a, $0F ;+2
  ld b, a ;stepsize_H = swap(max-min)[3:0] ;+1

  ld hl, GeneratedDitherThresholds ; 3c

  .loop
  ld a, d ;+1
  ld [hli],a ;load whole-part of running sum into GeneratedDitherThresholds[i] ;+2 
  ADD16_BC_INTO_DE_SATURATING ;generate next value ;+10
  ld a, LOW(GeneratedDitherThresholds.end) ;+2
  cp a, l ;if the next address to write to is out-of-bounds, finish. We use a check for equality because even if GeneratedDitherThresholds crosses a byte boundary,
  ;the l value will never loop ;+1
  jr nz, .loop ;+3
ret ;+2

/** 
* Fills CamOptDither_RAM (either the dark, medium, or light third) with values from GeneratedDitherThresholds in the order dictated by OrderTableRelative.
* Called after calling GenerateThresholdsFromRange and setting a=0,1,2. puts the values in GeneratedDitherThresholds every 3 spaces in the working dithering table, according to the ordering matrix
* CamOptDither_RAM format is (comparison) [[Dark,Mid,Light], [Dark,Mid,Light] ...]
* @param a = offset within the working dithering table (0, 1, or 2), which represents which group of threshold (dark, middle, or light) values are being arranged.
*/
ArrangeThresholdsInPattern:
  ;bc = source: Dither thresholds generated by GenerateThresholdsFromRange: starting at last element of GeneratedDitherThresholds, 
    ;accessed randomly in an order dictated by OrderTableRelative
  ;de = dest: working dither pattern, written to sequentially in steps of 3
  ;hl = address in order table. The order table is how many to add/subtract from bc to get the address of the next dither threshold. Access sequentially, so in hl.


  ld de, CamOptDither_RAM
    ;moving hl to bc loses us 2 cycles and 2 bytes, but instead of using ld a, [bc] 2c1b + inc bc 2c1b, we can just use ld a, [hli] 2c1b, 
    ;saving 2c per loop (32c) and but taking 1 more byte
  add a, e
  ld e, a ;destination start position is the WRAM dither table, plus an offset to select which threshold (z) you're modifying

  ld hl, OrderTables
  ;add $10*CamOptDitherPattern to hl to determine which ordering table to use. There is no guarantee all tables share the same h, so it should be 16-bit.
  ld a,[CamOptDitherPattern]
  swap a
  ld b,$00   ;bc is unused at the moment, so we can use it for our 16-bit add.
  ld c,a
  add hl, bc

  ld bc, GeneratedDitherThresholds
  ld a, [hli] ;load first element of the Order Table and add it to bc to get our start position
  add a,c
  ld c,a ;bc is now equal to the address of the first element that the Order Table wants to put in


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

/**
* Fill effective dither table CamOptDither_RAM with values according to the selected dither table and contrast.
*/
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
  call ArrangeThresholdsInPattern ; arrange our thresholds into the MID part of the pattern
  pop hl ;+3

  ld a, [hli]
  ld b, a
  ld c, [hl]   ;put min in b and max in c
  push hl ;push address of start of third range
  call GenerateThresholdsFromRange
  ld a, $02
  call ArrangeThresholdsInPattern ; arrange our thresholds into the LIGHT part of the pattern
  pop hl
ret


/*-------------------------------------------------------------------------------------------------------------------------
* UpdateTopbarBuffer_xx Functions
* These functions set TopbarBuffer to tiles corresponding to the underlying values */
DEF DEFINE_NULL_X EQUS "MACRO X\n \n ENDM\n"

;Sets TOPBAR_OFFSET to (the offset of the left tile of) the variable's corresponding 4-tile area in TopbarBuffer
MACRO GET_UI_OFFSET
  DEF TOPBAR_OFFSET = 0
  DEF XMACRO_SEARCH_TERM EQUS \1
  ;This macro only works if the numerical values underlying the labels are unique. -- otherwise we'd need to compare the names of the labels instead of their values.
  DEF GETINDEX_X_DEF EQUS "MACRO X\nIF \\1=={XMACRO_SEARCH_TERM}\n PURGE X\n \{DEFINE_NULL_X\}\n         ELSE\n DEF TOPBAR_OFFSET+=1  \nENDC\n  ENDM\n"
  ;X counts every element that is not the search term, then redefines itself to be a null macro after
  GETINDEX_X_DEF
  INCLUDE "src/ui_elements.inc"
  PURGE XMACRO_SEARCH_TERM
  PURGE GETINDEX_X_DEF
  ;We will need to use X macros to set the offset to left side of the variable drawing space: 4*index.
  DEF TOPBAR_OFFSET *=4
  IF TOPBAR_OFFSET >= 20 ;If value is out of viewable area, wrap around to second line
  DEF TOPBAR_OFFSET+=12
  ENDC
ENDM

/**
* Populate EdgeMode's region in TopbarBuffer with tiles corresponding to its value
* @clobber a, de
*/
UpdateTopbarBuffer_EdgeMode:
  GET_UI_OFFSET "CamOptEdgeMode"

  IF SCREEN_FLIP_H==1 ;With horizontal rotation, this is the leftmost (just under the text)
    ld de, TopbarBuffer+TOPBAR_OFFSET ;+3c3b ;Note: when adding to e, first line will not overflow, but starting at the second line of values ($9A20), it will. So between them, inc d
  ELSE ;With no rotation, this is the rightmost nybble
    ld de, TopbarBuffer+TOPBAR_OFFSET+3 ;+3c3b
  ENDC
  ld a,[CamOptEdgeMode] ;+2c1b ;put the value of the selected option in a 
  or a,UI_ICONS_BASE_ID
  ld [de], a ;+2c1b ;load nybble value into tilemap
  ret

/**
* Populate CamOptE's region in TopbarBuffer with tiles corresponding to its value
* @clobber a, de
*/
UpdateTopbarBuffer_CamOptE:
  GET_UI_OFFSET "CamOptE_RAM"
  IF SCREEN_FLIP_H==1
  ld de, TopbarBuffer+TOPBAR_OFFSET
  ELSE
  ld de, TopbarBuffer+TOPBAR_OFFSET+3
  ENDC
  ld a, [CamOptE_RAM] ;+4c3b
  or a,UI_ICONS_BASE_ID
  ld [de], a ;+2c1b
  ret

/**
* Populate CamOptContrast's region in TopbarBuffer with tiles corresponding to its value
* @clobber a, de
*/
UpdateTopbarBuffer_CamOptContrast:
  GET_UI_OFFSET "CamOptContrast"
  IF SCREEN_FLIP_H==1
  ld de, TopbarBuffer+TOPBAR_OFFSET
  ELSE
  ld de, TopbarBuffer+TOPBAR_OFFSET+3
  ENDC
  ld a, [CamOptContrast] ;+4c3b
  or a,UI_ICONS_BASE_ID
  ld [de], a ;+2c1b
  ret

/**
* Populate DitherTable's region in TopbarBuffer with tiles corresponding to its value
* @clobber a, de
*/
UpdateTopbarBuffer_DitherTable:
  GET_UI_OFFSET "CamOptDitherTable"
  IF SCREEN_FLIP_H==1
  ld de, TopbarBuffer+TOPBAR_OFFSET
  ELSE
  ld de, TopbarBuffer+TOPBAR_OFFSET+3
  ENDC
  ld a, [CamOptDitherTable] ;+4c3b
  or a,UI_ICONS_BASE_ID
  ld [de], a ;+2c1b
  ret

/**
* Populate DitherPattern's region in TopbarBuffer with tiles corresponding to its value
* @clobber a, de
*/
UpdateTopbarBuffer_DitherPattern:
  GET_UI_OFFSET "CamOptDitherPattern"
  IF SCREEN_FLIP_H==1
  ld de, TopbarBuffer+TOPBAR_OFFSET
  ELSE
  ld de, TopbarBuffer+TOPBAR_OFFSET+3
  ENDC
  ld a, [CamOptDitherPattern] ;+4c3b
  or a,UI_ICONS_BASE_ID
  ld [de], a ;+2c1b
  ret


/**
* Populate CamOptV's region in TopbarBuffer with tiles corresponding to its value
* @clobber a, de
*/
UpdateTopbarBuffer_CamOptV:
  GET_UI_OFFSET "CamOptV_RAM"
  IF SCREEN_FLIP_H==1
  ld de, TopbarBuffer+TOPBAR_OFFSET
  ELSE
  ld de, TopbarBuffer+TOPBAR_OFFSET+3
  ENDC
  ld a, [CamOptV_RAM] ;+4c3b
  or a,UI_ICONS_BASE_ID
  ld [de], a ;+2c1b
  ret

/**
* Populate CamOptC's region in TopbarBuffer with tiles corresponding to its value
* @clobber a, de, hl
*/
UpdateTopbarBuffer_CamOptC: 
  GET_UI_OFFSET "CamOptC_RAM"
   ;C; 4 nybbles from 2 bytes
  IF SCREEN_FLIP_H==1
    ld hl, CamOptC_RAM ;loads LOW byte's addr into hl
    ;When Hflipped, we display the least significant bytes and nybbles first -- set de to rightmost tilemap position, then dec it while incrementing hl, our source
    ld de, TopbarBuffer+TOPBAR_OFFSET+1 ;+2c2b ; go to leftmost tile of left byte (LSB's LSN)+1
    call UpdateByteInTilemap_flipped ;3b
    inc e ;display LSB:H
    inc e 
    inc e ;^^+3c3b
    call UpdateByteInTilemap_flipped ;3b
  ELSE
  ld hl, CamOptC_RAM ;loads LOW byte's addr into hl
  ;When Hflipped, we display the least significant bytes and nybbles first -- set de to rightmost tilemap position, then dec it while incrementing hl, our source
  ld de, TopbarBuffer+TOPBAR_OFFSET+2 ;+2c2b ; go to leftmost tile of left byte (LSB's LSN)+1
  call UpdateByteInTilemap ;3b
  dec e ;display LSB:H
  dec e
  dec e
  call UpdateByteInTilemap ;3b  
  ENDC
  ret

/**
* Populate CamOptO's region in TopbarBuffer with tiles corresponding to its value
* @clobber a, de, hl
*/
UpdateTopbarBuffer_CamOptO: 
  GET_UI_OFFSET "CamOptO_RAM"
  DEF TOPBAR_OFFSET +=1 ;O; 2 nybbles
  ld hl, CamOptO_RAM
  IF SCREEN_FLIP_H==1 
  ;if screen flipped, de is second-to-right and we want to move it to the one-after-leftmost
  ld de, TopbarBuffer+TOPBAR_OFFSET
  ELSE
  ld de, TopbarBuffer+TOPBAR_OFFSET+1
  ENDC
  call {UpdateByteInTilemap_rotation}
  ret

/**
* Populate CamOptG's region in TopbarBuffer with tiles corresponding to its value
* @clobber a, de, hl
*/
UpdateTopbarBuffer_CamOptG:
  GET_UI_OFFSET "CamOptG_RAM"
  ;G; 2 nybbles
  ld hl,CamOptG_RAM
  IF SCREEN_FLIP_H==1 
  ld de, TopbarBuffer+TOPBAR_OFFSET+1
  ELSE
  ld de, TopbarBuffer+TOPBAR_OFFSET+2
  ENDC  
  call {UpdateByteInTilemap_rotation}
  ret

;--------------------------------Handover Payloads---------------------------------------------------------
/**
* In the stock ROM, switches to VRAM1, jumps to HandoverChecker, then switches back to VRAM0.
* Is placed in HRAM instead of the OAM DMA function.
*/
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

/** 
* Calls ROM's memclr on WRAM, except for some stack, with a return address to later part of the initialization sequence which doesn't overwrite HRAM, completing handover.
* After the ROM's initialization sequence is loaded into writable memory and patched, this routine is appended to the end.
*/
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
  DEF CompleteHandover_SIZE EQU CompleteHandover_storage.end - CompleteHandover_storage.start ;13 bytes

/**
* Initiates an OAM DMA, moves palette in BGP into CGB palette 0, checks whether the key combination is pressed to hand back over to RAM code,
* and the waits until OAM DMA is nearly complete so the HRAM_stock_stub can safely return.
* When the stock ROM is running, HRAM_stock_stub jumps here.
*/
checker_payload:
  DEF HandoverChecker EQU _VRAM
  .start
  ; OAM copy 
  ld a,$d4
  ldh [rDMA],a
  ld a,ROM_OAM_DMA_WAITLOOP_COUNT ;OAM copy waitloop: number of cycles burned = 4*counter- 1
  :dec a
  jr nz, :-
  ;Move BGP values into CGB BG palette 0 
  ;We will have 4 color values (Little-endian 555) stored. 0=White, 1:Light Gray, 2:Dark Gray, 3: Black
  ld a, BCPSF_AUTOINC | $00 ;2c2b
  ldh [rBCPS],a   ;Set BCPS address to CGB palette 0, autoincrement ;3c 2b
  ldh a,[rBGP] ;3c 2b
  ld d,a ;Load BGP (33221100) into d ;1c 1b
  ;9c

  and a,%00000011 ;2c 2b
  add a,a ; a = 2*BGP[1:0], index in our palette table ;1c 1b
    ld hl, .stockPaletteData+$8000-checker_payload ;3c3b
    add a,l ;1c 1b
    ld l,a ;hl = address in palette table ;1c 1b
    ld a, [hli] ;2c 1b
    ldh [rBCPD], a ;3c 1b
    ld a, [hl] ;2c 1b
    ldh [rBCPD], a ;load palette data from table into CGB palette ;3c 1b
    ;18c

  sra d ;2c2b
  ld a,d
  and a,%00000110 ;a = 2*BGP[3:2] ;2c2b
    ld hl, .stockPaletteData+$8000-checker_payload
    add a,l
    ld l,a ;hl = address in palette table
    ld a, [hli]
    ldh [rBCPD], a
    ld a, [hl]
    ldh [rBCPD], a ;load palette data from table into CGB palette
    ;5+15 = 20c

  sra d
  sra d
  ld a,d
  and a,%00000110 ;a = 2*BGP[5:4]
    ld hl, .stockPaletteData+$8000-checker_payload
    add a,l
    ld l,a ;hl = address in palette table
    ld a, [hli]
    ldh [rBCPD], a
    ld a, [hl]
    ldh [rBCPD], a ;load palette data from table into CGB palette
    ;22c

    sra d
    sra d
    ld a,d
    and a,%00000110 ;a = 2*BGP[7:6]
      ld hl, .stockPaletteData+$8000-checker_payload
      add a,l
      ld l,a ;hl = address in palette table
      ld a, [hli]
      ldh [rBCPD], a
      ld a, [hl]
      ldh [rBCPD], a ;load palette data from table into CGB palette
    ;22c
  ;total dec91 cycles=$5B; $24 (waitloop count including the button check)*4=90 cycles - $5B  = $35 cycles within our allowance. Lshift 2x -> new counter is $D
  ;Lets' recalculate this: Original counter is $28=40 loops. 
    ;Check = 9 cycles. 
    ;xor+jp+hramldh = 10 cycles (possibly not taken, the jr path is 7c until a WRAM bank-switch. It's safe to bankswitch during OAM-DMA, so that path is also 10c).
    ;rBGP copy code is 91 cycles 
    ;Total 107 or 110 cycles to subtract from 1 + 4*40. 4x must be 160 or higher. If we subtract 107, 4x must be 53 or higher. 4x must be dec14 with 3 wasted cycles, MAY be safe to make it 13 but cutting it close.
    ;0D works, even for Ram handover
    ;0C fails, as expected (white screen)

  ;check if reset button combination is pressed. Since this doesn't use WRAM, we can check during OAM DMA
  ldh a, [joypad_active] ; 3c
  DEF ROM_RAM_HANDOVER_MASK EQU (JOYPAD_SELECT_MASK | JOYPAD_DOWN_MASK)
  and a,ROM_RAM_HANDOVER_MASK ; 2c
  cp a, ROM_RAM_HANDOVER_MASK ; 2c
  jr z, .handoverToRAM ;2 or 3c -- for minimum, it's 2

  xor a   ;a must be zero on return to allow the HRAM code to switch back to VRAM bank 0
  jp HRAM_RETURN_POINT
  
  ;4 palettes: store little-endian
  ;dw's are little-endian, too
  .stockPaletteData
  dw $7FFF ;White
  dw $5294 ;Light Gray
  dw $294A ;Dark Gray
  dw $0000 ;Black

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
  ;We use memcpy 8, so make sure it's less than 256 bytes
  assert checker_payload_size < 256, "checker_payload_size iss more than 255 bytes."

  
;----------------------------------------------------------------------------------------------------------
/**
* Starts the handover process.
* Loads checker data into VRAM1, WRAM switches to STOCK_RAMBANK, loads the HRAM stub, 
* loads and patches ROM init code into banked WRAM, appends CompleteHandover, and jumps to the patched initialization code.
* Called from the UI handler.
*/
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
  ;Clear VRAM1 attribute tables 9800-9bff and 9c00-9fff
  ld a,$01
  ldh [rVBK],a
  
  ld hl,$9800
  ld b, $a0
  :xor a
  ld [hli],a
  ld a,h
  cp b
  jr nz,:-

  ;Load checker payload into VRAM1 tiledata $8000-97FF
  ld de, _VRAM ;destination: VRAM1:8000
  ld hl, checker_payload
  ld c, checker_payload_size
  call memcpy8_hl_to_de
  ;Switch back to VRAM0
  xor a
  ldh [rVBK],a

  ;Switch mapped RAMbank to one dedicated for use by the stock ROM
  ld a,STOCK_RAMBANK
  ldh [rSVBK],a

  ;Load HRAM stub
  ld hl, HRAM_stock_stub
  ld de, _HRAM
  ld c, HRAM_stub_size
  call memcpy8_hl_to_de
  ;load the rest of HRAM with 00s
  ;we patch out the stock ROM's HRAM clear, so we must do it here 
  xor a
  ld b,$7F-HRAM_stub_size
  call memfill8_a_into_de_sizeb

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

  ;patch out the call to memclr WRAM at +$187,88,89 (+$37,38,39)
  ld hl,$D037
  xor a
  ld [hli],a
  ld [hli],a
  ld [hli],a

  jp $D000 ;run copy of init code with patches

/**
* Draws one of the 14 lines in the sidebar buffer to the sidebar in VRAM.
* Called by the Vblank ISR. 
*/
DrawSidebar:
  ld hl, SidebarBuffer ;+3
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
  ld d,h ;de = start of 4 source addresses: SidebarBuffer + (rownum*4) ;+1
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

/**
* Copies up to 255 bytes from source to destination.
* 10 cycles / byte
* @param hl = source
* @param de = dest
* @param c = size in bytes
*/
memcpy8_hl_to_de::
  :ld a,[hli] ;2c 1b
  ld [de],a ;2c 1b
  inc de ;2c 1b
  dec c ;1c 1b
  jr nz,:- ;3c 2b
ret

/**
* Fills b bytes of [de] with a
* @param b = size in bytes
* @param de = dest
* @param a = value with which to fill
*/
memfill8_a_into_de_sizeb::
  :ld [de],a
  inc de
  dec b
  jp nz,:-
ret


/**
* Compares c bytes of hl and de.
* @param  c = size in bytes
* @param hl = pointer to str1
* @param de = pointer to str2
* @clobber a,hl,de,c
* @return　a: 0 if the strings are equal, -1 otherwise.
*/
memcmp::
  .loop: 
  ld a,[de]
  inc de
  cp a,[hl] ; check if values are equal
  inc hl ; doesn't affect flags
  jr z,:+
    ld a,-1 ;If the values are different, return -1
    ret
  :dec c
  jr nz,.loop ;if we've not reached end of buffer, loop
  
  ld a,0 ;if we've reached end of buffer with all bytes equal, return 0
ret

/**
* Function on ROM -- gets input and updates several variables.
* clobber a, ???? but not d
*/
GetInputROM:: jp $0000

/**
* Sets the registers as if not-CGB and jumps back to the launcher ROM, which launches the alternate payload.
*/
LaunchAlternativePayload::
;The launcher checks at the start for DMG mode.
  xor a ;set flags as if DMG/MGB/SGB -- DMG/SGB is 01, MGB/SGB2 is $FF, but as long as it's not $11 we're not CGB.
  jp $100 ; jp back to launcher ROM

/**
* Gets input, whether from keypad or remote control, and puts it into joypad_active.
*/
GetInput::
ld a,[Setting_SerialRemote]
ld d, a
ld a,[SerialEnable]
and a,d ; a = Setting_SerialRemote & SerialEnable
jr nz,:+ ; If it's OK to use the serial port, jump.
call GetInputROM
ret
:call GetSerialInput
ret

/**
* Gets input and puts it into joypad_active
* If Setting_SerialRemote is set, also initiates a serial transfer and updates joypad_active from serial controller.
*/
GetSerialInput:
  .startTransfer
  ld a, SCF_START | SCF_SPEED | SCF_SOURCE ;Transfer enable, high clock speed, internal clock
  ldh [rSC], a
  ;Input format is D U L R: START SEL A B
  : call GetInputROM
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
  jr z, .notHeld
  xor a,c ; a = State xor PrevState
  jr nz, .notHeld
  .held
  ;decrement the hold counter
  ld hl,RemoteJoypadHoldCounter
  dec [hl]
  jr nz,.updatePrev
  ;if hold counter is zero,
  ld a,b ; a = RemoteJoypadState
  ld [RemoteJoypadActive],a ; set RemoteJoypadActive to RemoteJoypadState
  ld a,[RemoteJoypadHoldThreshold]
  ld [RemoteJoypadHoldCounter],a ;Reset remote joypad's hold counter
  jr .updatePrev

  .notHeld
  ;If nothing is held, JoypadHoldCounter should be reset
  ld a,[RemoteJoypadHoldThreshold]
  ld [RemoteJoypadHoldCounter],a

  .updatePrev
  ld a,b ;RemoteJoypadPrevState = RemoteJoypadState
  ld [RemoteJoypadPrevState],a
  ;Combine joypad_active and RemoteJoypadActive
  ld a,[RemoteJoypadActive]
  ld b,a
  ldh a,[joypad_active]
  or a,b
  ldh [joypad_active],a
  ret

.badPacket
  xor a
  ld [RemoteJoypadPrevState],a
  ld [RemoteJoypadActive],a
  ret

/**
* Loads one 8-byte palette into the specified location in palette RAM.
* Watch out! BCPD can't be accessed during render mode 3
* @param hl: source address of stored palette in WRAM
* @param a: dest address in palette RAM to start loading into. Since palettes are 8 bytes, a = intended palette number * 8. Bit 7 (auto-increment) should be set.
* @clobber a, b, hl*/
LoadPaletteFromAddress:
  ldh [rBCPS],a ;3c
  ld b,$08 ;2c
  :ld a,[hli] ;2c
  ldh [rBCPD],a ;3c
  dec b ;1c
  jr nz,:- ;3c
ret

/**
* Signals to the VBlank handler that BG Palette 0 should be changed to stored BG palette number 1
* Use when "inverting" or "flashing" the BG palette.
* @clobber a
*/
SetBGPalette0to1::
  ld a,(1*8)
  ld [BGPaletteChangeSrc],a
  ld a, %10000000 | $00
  ld [BGPaletteChangeDest],a
  cpl
  ld [BGPaletteChangeFlag],a
ret

/**
* Signals to the VBlank handler that BG Palette 0 should be changed to stored BG palette number 1
* Used to return to the "original" BG palette.
* @clobber a
*/
SetBGPalette0to0::
  ld a,(0*8)
  ld [BGPaletteChangeSrc],a
  ld a, %10000000 | $00
  ld [BGPaletteChangeDest],a
  cpl
  ld [BGPaletteChangeFlag],a ;set flag so that palette is changed in VBlank
  ret

/**
* If BGPaletteChangeFlag is set, change the BG Palette in BGPaletteChangeDest to the stored palette in BGPaletteChangeSrc.
* Called in the VBlank ISR. 
* When changing these variables, set BGPaletteChangeFlag LAST, in case the VBlank ISR runs while writing to these 3 variables.
* @clobber a,de,hl
*/
ModifyPalette:
  ld a,[BGPaletteChangeFlag] ;If flag is unset, do nothing
  and a
  ret z
  xor a
  ld [BGPaletteChangeFlag],a ;If flag is set, reset it and load the palette
  ld a,[BGPaletteChangeSrc] ;add the src index (which should be passed as 8*palette index) to the address of the palette table.
  ld hl, PaletteBG
  ld d, 0
	ld e, a
	add hl, de
  ld a,[BGPaletteChangeDest] ;This should be the value passed to rBCPS, with bit 7 (auto-increment) set and the lower 6 bits set to palette number * 8
  ldh [rBCPS],a ;3c
  ld b,$08 ;2c
  :ld a,[hli] ;2c
  ldh [rBCPD],a ;3c
  dec b ;1c
  jr nz,:- ;3c
ret

/**
* Switch to SRAMbank 0 (State Vector, save data, photo slot 0) and enable writing to SRAM.
* Call this before running StateVector functions.
* @clobber h
*/
StateVector_EnableWrite::
  ld h,$0A 
  ld [hl],h ;enable SRAM writes
  ld h, HIGH(rRAMB) ;2c2b
  ld [hl], $00 ;3c2b ; switch to SRAM bank 0: state vector
ret


/**
* Enables accessing State Vector, writes a new entry to it, writes the capture data from SRAM0 to SRAMX, 
* generates a thumbnail+metadata, updates free counter, then disables SRAM writes.
* Does not require StateVector_EnableWrite to run before it, as it calls it itself.
* Callers should do their own check as to whether there is a free slot available. Silently aborts if no free slots are found, but this shouldn't be relied upon.
* @clobber a,hl,bc,de
*/
SaveToFreeSlot::
  call StateVector_EnableWrite
  ;Find a free slot in the state vector and update the state vector while you're there, but keep ahold of the correct bank index
  call StateVector_FindAndFillFreeSlot ;the slot index + 1 is held in a
  and a
  jp z, .save_cleanup ;skip saving if FindAndFillFreeSlot returns 0

  dec a
  ;the bank we want to switch to, held in c, should be 1 + (slot index>>1).
  ;Copy image data to the correct slot
  bit 0, a
  ld e, $FF ;if slot index is even
  jr z,:+
  ld e, $0F     ;if slot index is odd, set the value to add/sub from HIGH(src/dest pointer) to $0F. Otherwise, use -1 (FF)
  :
  ;shift right and add 1 to get the bank number
  srl a
  inc a
  ld c,a ;the transfer function takes the SRAM bank number in c
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
  ld de, SidebarBuffer+2
  call UpdateByteInTilemap

  .save_cleanup
  ld h,$00
  ld [hl],h ; disable SRAM writes
ret


/**
* Depending on Setting_OnTakeAction, saves, prints, or transfers the capture held in SRAM0.
* Called from the viewfinder's wait-on-DMA code when a transfer is complete.
* TODO: Also call this instead of SaveToFreeSlot in the TakeConfirm UI handler, so single-shots also match with the takecomplete setting. 
*/
vfCompleteAction:
  ;if setting = TAKE_ACTION_SAVE and the currentAction takes multiple pictures, call vfCompleteAction_Save_Multi
  ld a,[Setting_OnTakeAction]
  and a
  ;if setting = 0 = TAKE_ACTION_SAVE
  jr nz,.printCheck
  jp vfCompleteAction_Save_Multi ;Note: we use jp here because it's smaller than call->ret-from-callee->ret : callee just RETs to vfCompleteAction's caller

  .printCheck
  dec a
  jr nz,.savePrintCheck
  jp vfCompleteAction_Print

  .savePrintCheck
  dec a
  jr nz,.xferCheck
  call vfCompleteAction_Save_Multi
  jp vfCompleteAction_Print

  .xferCheck
  dec a
  jr nz,.invalidTakeAction
  jp vfCompleteAction_Xfer

  .invalidTakeAction
  ;TODO: throw an ERROR

ret


/**Saves a single image, when only one image is taken.
; Currently non-functional: Viewfinder initiates UI changes and other such things in the Transition state, and this would run in the CaptureComplete state.
TODO: Should probably be removed.*/
vfCompleteAction_Save_Single:

  ret
  
/**Saves a single image in a multi-image capture action
*/
vfCompleteAction_Save_Multi:
  call SaveToFreeSlot
  ret

/**
TODO: Print via serial port.
*/
vfCompleteAction_Print:

  ret
/**
TODO: Use a bespoke transfer protocol to transfer data faster.
  */
vfCompleteAction_Xfer:

  ret

/**
* Rightshifts de a times.
* Used by NextElementGeometric_Add/Sub to calculate next C-value in a geometric sequence.
* @clobber de, a
* @return a: 0
* @return de: de >> a
*/
rshift16: ;(a: number of right shifts, de: value to shift)
  .loop
  sra d
  rr	e
  dec a
  jr nz, .loop
ret
  
/**
* Calculates the next value in the sequence currentValue + currentValue>>a, clipped to $FFFF
* Mathematically, this is currentValue*(1 + 2**-a)
* Used in AEB mode to calculate the next C-value to use to capture.
* @param hl: currentValue
* @param a: number of shifts
* @clobber a,hl
* @return hl: value of next element in the sequence
*/
NextElementGeometric_Add:
  push de
  ld d,h
  ld e,l
  call rshift16
  add hl,de
  pop de
  ;Check for overflow
  ret nc ;if the last add caused a carry, we need to alias L back to max
  ld h,$FF
  ld l,h
ret
  
/**
* Calculates the next value in the sequence currentValue - currentValue>>a, clipped to $0100
* Mathematically, this is currentValue*(1 - 2**-a)
* Used in AEB mode to calculate the next C-value to use to capture.
* @param hl: currentValue
* @param a: number of shifts
* @clobber a,hl
* @return hl: value of next element in the sequence
*/
NextElementGeometric_Sub: ;(HL: currentValue a:number of shifts)
  push de
  ld d,h
  ld e,l
  call rshift16
  ld a,l ;hl = 16-bit subtract de from hl
  sub e
  ld l,a
  ld a,h
  sbc d
  ld h,a
  ;because a geometric sequence of subtraction never subtracts a number as large as itself, you will never get past or indeed TO 0.
  ;If the minimum value is 0001, you should be able to skip bounds-checking altogether
  ;if you want to bounds-check, say to ensure it's gte $0100, do this:
  xor a
  cp a,h
  pop de
  ret nz
  ld l,a
  ld h,$01
ret

/**
* Fill the BurstExposureList (used for AEB shots) with a center value, (count-1)/2 overexposed values, and (count-1)/2 underexposed values.
* First fills center of list, then overexposed counting up from center, then underexposed from the start of the list.
* Since it counts up based on an increment from center value, the underexposed values will get progressively darker, jump to the center value, and get progressively lighter.
*/
FillAEBList::
  .fillCenter
  ;Assuming odd count, place the center C-value in index (count-1)/2
  ;find (count-1)/2 and use it as the index for where to put our C-value.
  ld a,[Setting_AEB_Count]
  srl a ; a = (count-1)/2. assuming this is odd, the -1 is included in a right-shift
  add a ; turn this index into an offset by multiplying by 2, since each C-value is 2bytes   ;TODO: the prior parts are superfluous, we can just reset bit 0 to get the same result
  ld bc, BurstExposureList
  ;add a to bc to get the address of the entry.
  ; Since we know BurstExposureList's addresses all have the same high nybble, we can actually just do an 8-bit add to C
  add a,c
  ld c,a
  ;put current c-value (de) into BurstExposureList[(count-1)/2] ([hl]), little-endian
  ld a,[CamOptC_RAM_L] ;fetch current C-value
  ld [bc],a ;load lower byte into BurstExposureList + 2*(count-1)/2
  inc c
  ld l,a
  ld a, [CamOptC_RAM_H]
  ld [bc], a ;load higher byte into BurstExposureList + 2(count-1)/2 + 1
  inc c
  ld h,a
  push hl ;hl = current C-value for use by overexposure and underexposure list
          ;bc = address: BurstExposureList + 2(count-1)/2 + 2
  
  .fillOverexposed
  ;Then, place overexposed C-values in indices 0 through [(count-1)/2]-1
  ;initialize counter of overexposed images (count-1)/2
  ld a,[Setting_AEB_Count]
  srl a
  ld d,a ;d = counter (number of overexposed images to take), loop terminates when counter is 0
  ld a,[Setting_AEB_Interval]
  ld e,a ;e = number of shifts
  ;      put counter in d
  ;          number of shifts in e (passed in via a)
  ;          current address in bc
  ;          C-value in hl
  
  .fillOverExposedLoop
    ;before loop, previous C-val is already in HL
    ld a,e;put number of shifts into a
    call NextElementGeometric_Add
    ;put the result, hl, into [bc] (little-endian) and increment it
    ld a,l
    ld [bc], a
    inc c
    ld a,h
    ld [bc], a
    inc c
    ;decrement counter and check for complete (0)
    dec d
    jr nz,.fillOverExposedLoop
  
  
  ;Then, place underxposed C-values in indices ((count-1)/2 + 1) through (count-1)
  pop hl ;retrieve current C-value into hl
  ;initialize counter of underexposed images (count-1)/2
  ld a,[Setting_AEB_Count]
  srl a
  ld d,a ;d = counter (number of underexposed images to take), loop terminates when counter is 0
  ld a,[Setting_AEB_Interval]
  ld e,a ;e = number of shifts
  ;      put counter in d
  ;          number of shifts in e (passed in via a)
  ;          current address in bc
  ;          C-value in hl
  ld bc,BurstExposureList ;set address of exposure value to the start of the array

  .fillUnderExposedLoop
    ;before loop, previous C-val is already in HL
    ld a,e;put number of shifts into a
    call NextElementGeometric_Sub
    ;put the result, hl, into [bc] (little-endian) and increment it
    ld a,l
    ld [bc], a
    inc c
    ld a,h
    ld [bc], a
    inc c
    ;decrement counter and check for complete (0)
    dec d
    jr nz,.fillUnderExposedLoop

  ret

/**
* Returns the exposure/C-value to use for the next shot in a burst/AEB shot.
* Retrieves this value from BurstExposureList[BurstShotCurrentCapture].
* @clobber a,hl
* @return hl: C-value to use in the next capture, big-endian (A00x/register order)
*/
GetAEBExposure::
  ld a,[BurstShotCurrentCapture] ;a = index within BurstExposureList
  add a,a ;a = offset within BurstExposureList
  ld hl, BurstExposureList
  add a,l
  ld l,a ;hl = &BurstExposureList[BurstShotCurrentCapture]
  ld a,[hli] ;dereference [hl] into hl
  ld h, [hl]
  ld l,a
ret

  ;---------------------------------Save Data Functions-----------------------------------------------------
  INCLUDE "src/save.asm"


    ;-------------------------------------------DATA-------------------------------
Palette:
PaletteBG: ;TODO: This is a misnomer; however, at the moment, we use the same source palette for OBJ and BG palettes.
PaletteOBJ:
    ;Palette format is OBJ palette:BG palette. The number of each is defined in NUM_BG_PALETTES and NUM_OBJ_PALETTES
    ;Palettes in the bin file are specified in big-endian: RGBDS will automatically convert them to little-endian
    ;In the bin file, specify in RGB555: (dont-care/0):Blue5:Green5:Red5
    INCBIN "assets/palette.bin"

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
;First value is the first offset in the generated dither thresholds to put in the dither table
;Next values are offsets from that value within the generated dither thresholds table
;OrderTableRelative[i] = currentposition - originalTable[i]
;currentposition = originaltable[i-1]; originaltable[-1]=15
OrderTables:
OrderTableRelative_Standard: db 15, (7-15),(13-7), (5-13), \
      (3-5),(11-3), (1-11), (9-1), \
      (12-9), (4-12), (14-4), (6-14), \
      (0-6), (8-0), (2-8), (10-2)
.end


OrderTableRelative_NoDither: 
  db  7,0,0,0, \
      0,0,0,0, \
      0,0,0,0, \
      0,0,0,0
.end

;0,15,0,15,
;15,0,15,0
;0,15,0,15,
;15,0,15,0
OrderTableRelative_Naive:
  db  0,15,(0-15),15,\
      0,(0-15),15,(0-15),\
      0,15,(0-15),15,\
      0,(0-15),15,(0-15)
.end

;0,15,0,15,
;0,15,0,15,
;0,15,0,15,
;0,15,0,15,
OrderTableRelative_Vertical:
  db  0,15,(0-15),15,\
  (0-15),15,(0-15),15,\
  (0-15),15,(0-15),15,\
  (0-15),15,(0-15),15
.end

;0,0,0,0,
;15,15,15,15,
;0,0,0,0,
;15,15,15,15
OrderTableRelative_Horizontal:
  db  0,0,0,0,\
      15,0,0,0,\
      (0-15),0,0,0,\
      15,0,0,0
.end


;0,7,12,0,
;7,0,7,12,
;12,7,0,7,
;0,12,7,0
OrderTableRelative_Diagonal1:
  db  0,7,(12-7),(0-12),\
      7,(0-7),7,(12-7),\
      0,(7-12),(0-7),7,\
      (0-7),12,(7-12),(0-7)
.end

;0,7,12,15,
;7,0,7,12,
;12,7,0,7,
;15,12,7,0
OrderTableRelative_Diagonal2:
  db  0,7,(12-7),(15-12),\
      (7-15),(0-7),7,(12-7),\
      0,(7-12),(0-7),7,\
      (15-7),(12-15),(7-12),(0-7)
.end



SETCHARMAP ASCII
LoaderTitle:: db "SILIH"
.end

;Any entry >$7F has the Z bit set and is invalid as a matter of course
;Due to degreees of freedom in encoding, the encoder should only produce outputs <$5F
;There are some valid encodings between $60 and $7F that are in-spec, but our encoder shouldn't produce them, and we can ignore them to save space.
DecodingTable:
IF SCREEN_FLIP_H==0 && SCREEN_FLIP_V==0
  db $FF,$12,$14,$FF,$21,$FF,$FF,$28,$41,$FF,$FF,$48,$FF,$82,$84,$FF
  db $51,$FF,$FF,$58,$FF,$62,$64,$FF,$FF,$92,$94,$FF,$A1,$FF,$FF,$A8
  db $10,$FF,$FF,$20,$FF,$40,$80,$FF,$FF,$50,$60,$FF,$90,$FF,$FF,$A0
  db $FF,$01,$02,$FF,$04,$FF,$FF,$08,$00,$FF,$FF,$00,$FF,$00,$00,$FF
  db $11,$FF,$FF,$18,$FF,$22,$24,$FF,$FF,$42,$44,$FF,$81,$FF,$FF,$88
  db $FF,$52,$54,$FF,$61,$FF,$FF,$68,$91,$FF,$FF,$98,$FF,$A2,$A4;,$FF
;  db $FF,$10,$20,$FF,$40,$FF,$FF,$80,$50,$FF,$FF,$60,$FF,$90,$A0,$FF
;  db $01,$FF,$FF,$02,$FF,$04,$08,$FF,$FF,$00,$00,$FF,$00,$FF,$FF,$00
ELIF SCREEN_FLIP_H==1 && SCREEN_FLIP_V==1
  db $FF,$22,$24,$FF,$11,$FF,$FF,$18,$81,$FF,$FF,$88,$FF,$42,$44,$FF
  db $A1,$FF,$FF,$A8,$FF,$92,$94,$FF,$FF,$62,$64,$FF,$51,$FF,$FF,$58
  db $20,$FF,$FF,$10,$FF,$80,$40,$FF,$FF,$A0,$90,$FF,$60,$FF,$FF,$50
  db $FF,$01,$02,$FF,$04,$FF,$FF,$08,$00,$FF,$FF,$00,$FF,$00,$00,$FF
  db $21,$FF,$FF,$28,$FF,$12,$14,$FF,$FF,$82,$84,$FF,$41,$FF,$FF,$48
  db $FF,$A2,$A4,$FF,$91,$FF,$FF,$98,$61,$FF,$FF,$68,$FF,$52,$54,;$FF
;  db $FF,$20,$10,$FF,$80,$FF,$FF,$40,$A0,$FF,$FF,$90,$FF,$60,$50,$FF
;  db $01,$FF,$FF,$02,$FF,$04,$08,$FF,$FF,$00,$00,$FF,$00,$FF,$FF,$00
ENDC

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
