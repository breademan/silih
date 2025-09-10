INCLUDE "src/general.inc"
INCLUDE "src/hardware.inc"

SECTION "Printer SECTION",ROM0[$1000 + ($1000*PRINTER_RAMBANK)]
Printer_Storage::
LOAD "Printer LOAD", WRAMX [$D000]

;Less significant bits (starting from 0) are higher priority?


; Errors returned by a packet
DEF PACKET_ERR_BIT_KEEPALIVE EQU 7
DEF PACKET_ERR_BIT_STATUS EQU 6
DEF PACKET_ERR_BIT_CHECKSUM EQU 5

DEF PACKET_RET_OK EQU $00
DEF PACKET_ERR_MASK_CHECKSUM EQU %00000001 << PACKET_ERR_BIT_CHECKSUM
DEF PACKET_ERR_MASK_KEEPALIVE EQU %00000001 << PACKET_ERR_BIT_KEEPALIVE
DEF PACKET_ERR_MASK_STATUS EQU %00000001 << PACKET_ERR_BIT_STATUS

; Errors returned by a transaction
DEF TRANSACTION_ERR_BIT_KEEPALIVE EQU 7
DEF TRANSACTION_ERR_BIT_STATUS EQU 6
DEF TRANSACTION_ERR_BIT_CHECKSUM EQU 5
DEF TRANSACTION_ERR_BIT_STEP_TIMEOUT EQU 4

DEF TRANSACTION_RET_OK EQU $00
DEF TRANSACTION_ERR_MASK_CHECKSUM EQU %00000001 << TRANSACTION_ERR_BIT_CHECKSUM
DEF TRANSACTION_ERR_MASK_KEEPALIVE EQU %00000001 << TRANSACTION_ERR_BIT_KEEPALIVE
DEF TRANSACTION_ERR_MASK_STATUS EQU %00000001 << TRANSACTION_ERR_BIT_STATUS
DEF TRANSACTION_ERR_MASK_STEP_TIMEOUT EQU %00000001 << TRANSACTION_ERR_BIT_STATUS

; Errors returned by a transaction set.
DEF ACTION_ERR_BIT_KEEPALIVE EQU 7
DEF ACTION_ERR_BIT_STATUS EQU 6
DEF ACTION_ERR_BIT_CHECKSUM EQU 5
DEF ACTION_ERR_BIT_STEP_TIMEOUT EQU 4

DEF ACTION_RET_OK EQU $00
DEF ACTION_ERR_MASK_CHECKSUM EQU %00000001 << ACTION_ERR_BIT_CHECKSUM
DEF ACTION_ERR_MASK_KEEPALIVE EQU %00000001 << ACTION_ERR_BIT_KEEPALIVE
DEF ACTION_ERR_MASK_STATUS EQU %00000001 << ACTION_ERR_BIT_STATUS


DEF CHECKSUM_RETRIES EQU 3
DEF KEEPALIVE_MAX_RETRIES EQU 3

DEF INTRANSACTION_STEP_PRINTPHOTO_WAITPRINTFINISH EQU 5

DEF PRINTALL_DELAY_FRAMES EQU $20
DEF PRINT_STARTLOOP_DELAY_FRAMES EQU $20
DEF PRINT_ENDLOOP_DELAY_FRAMES EQU $20


; After Packet_ErrorCheck in a SendPacket_x function, call this to check the return values in b.
; @param b: Packet error type
; @return b: Packet error type
; @clobber a,b
;Cycle count is "worst-case" (no errors). If there's an error, this can take as few as 7/11/15 cycles. It can also jump back to .checkSumRetry.
MACRO PACKET_ERROR_POST
  ; If Keepalive error, return without doing any further checking.
  bit PACKET_ERR_BIT_KEEPALIVE,b
  ret nz
  ; If Status error, return without any further checking
  bit PACKET_ERR_BIT_STATUS,b
  ret nz
  ; If no checksum error (which means there are no packet errors), return
  bit PACKET_ERR_BIT_CHECKSUM,b
  ret z

  ; If no other errors, but checksum error, decrease checksum retry count by one.
  ld a,[Packet_Checksum_RetryCount] ;4c
  dec a ;1c
  ld [Packet_Checksum_RetryCount],a ;4c
  ; If non-zero (more retries left), retry sending packet
  jr nz,.checkSumRetry ;3/2 cycles
  ;Default case: if no errors, return.
ENDM


;Printer Variables
;SendPacket_* local vars
Packet_Checksum_RetryCount: db ;When a packet gets a "bad checksum" error (status bit 0), the packet should retry sending a certain number of times before giving up.  

;Transaction local vars
;In a transaction, this is set to the step number within the transaction before the packet is sent.If something goes wrong, we can display this step number for debugging.
InTransaction_StepNumber: db 
;When sending a conditional loop of packets, such as when waiting for printing to start/finish, waiting for buffer to clear, etc, this is used to prevent infinite loops using a timeout.
;Little endian. The lowest-address byte is the least-significant.
Transaction_StepTimeoutCounter_lilE: dw
;Action local vars
Transaction_Keepalive_RetryCount: db ;Counts how many times a transaction has been retried when a transaction fails due to a bad keepalive

;These local variables for ActionPrintAll exist because pushing/popping after each print makes register handling and conditionals more complicated.
PrintAll_PictureCounter: db ;local variable that holds the ID of the current photo to print. Always starts at 0. Iterates through photo IDs $00-$1D
PrintAll_ToPrint: db ;local variable that holds the number of photos to print



ActionPrintAll::
  di

  ld a,30 ;Initialize picture counter
  ld [PrintAll_ToPrint],a
  
  ld a,[PrintAll_ToPrint]
  and a
  jr z,.allPhotosFinished ;if number of photos to print, determined by the state vector, is 0, finish up early.

  xor a
  ld [PrintAll_PictureCounter],a ;Initialize photo counter to 0

.printPhotoLoop

  ;Reset keepalive retry count for the transaction
  ld a,KEEPALIVE_MAX_RETRIES
  ld [Transaction_Keepalive_RetryCount],a
  .retryPrintPhoto
  xor a
  ld [rRAMB],a ;Switch to SRAM bank that holds State Vector

  ;Set hl and rRAMB to point to our photo. hl = $A000 if ID is even, $B000 if odd
  ld a,[PrintAll_PictureCounter]
  ld d,a
  ld hl,StateVector_GetImageSlot
  ld e,SAVE_RAMBANK
  call Trampoline_hl_e

  ;Check whether d==FF If it is, skip to .printPhotoLoopCondition
  ld a,d
  cp a,$FF
  jr z,.printPhotoLoopCondition

  ld hl,$A000 ;Set hl to $A000 if even (default case)
  bit 0,a
  jr z,.evenSlotID ;if SLOT number is even, address starts at $A0, else $B0
    ld h,$B0 ;slot number is odd, location is B0
  .evenSlotID

  ;with a=slot ID of photo,
  srl a ;a = SlotID >> 1
  inc a ;a =(SlotID>>1)+1
  ld [rRAMB], a ;switch SRAM bank to the appropriate SRAM bank

  call SendTransaction_PrintPhoto_NoFrame

  ;Error checking
  bit TRANSACTION_ERR_BIT_STATUS,b
    jr nz,.generalError
  bit TRANSACTION_ERR_BIT_CHECKSUM,b
    jr nz,.generalError
  bit TRANSACTION_ERR_BIT_STEP_TIMEOUT,b
    jr nz,.generalError
  bit TRANSACTION_ERR_BIT_KEEPALIVE,b
    jr nz,.keepaliveFail
  ;End error checking

  .printPhotoLoopCondition ; Increments the Picture counter and determines whether to continue looping.
  ;Display to user: printing photo and step within the sendtransaction without asking for input
  call PrinterDebug_DisplayCurrentPrintingPhoto
  
  ld b, PRINTALL_DELAY_FRAMES
  call Printer_Delay_b_Frames

  ;Increment PictureCounter, then finish when it is equal to the number of photos to print
    ;Don't touch de (return value from the photo print) -- hl is OK, since our next photo has a very different address anyway.
    ld a,[PrintAll_PictureCounter]
    inc a 
    ld [PrintAll_PictureCounter],a ;increment counter and store it back
    ld b,a ;store counter in b
    ld a,[PrintAll_ToPrint]
    cp a,b ;cp ToPrint, (ID of next photo to print)
    jr z,.allPhotosFinished ;if all photos are finished printing, finish.

  ; Check for B button press
  push bc
  push de
  call GetInputROM 
  pop de
  pop bc
  ldh a,[joypad_state]
  and a,JOYPAD_B_MASK ; I forget if this is active-high like joypad_active or active-low like the actual registers.
  jr z, .printPhotoLoop


  .generalError
  .allPhotosFinished
  push af
  push bc
  push de
  push hl
  call PrinterDebug_DisplayCurrentPrintingPhoto
  pop hl
  pop de
  pop bc
  pop af

  ld h,d
  ld l,e
  call PrinterDebug_DisplayResult
  ;Redraw Settings screen
  ld e,UI_RAMBANK
  ld hl,InitSettingsGfx 
  call Trampoline_hl_e
  xor a
  ldh [rIF],a
reti

  .keepaliveFail
  call WaitPrinterTimeout ;Wait for printer to finish the timeout period to reset printer state.
  ld a,[Transaction_Keepalive_RetryCount]
  dec a
  ld [Transaction_Keepalive_RetryCount],a
  jr z,.generalError ;If we've retried the transaction due to keepalive too many times, end the entire transaction set.
  ld a,[InTransaction_StepNumber]
  cp a,INTRANSACTION_STEP_PRINTPHOTO_WAITPRINTFINISH ;nc means StepNumber>=threshold
  jr nc,.printPhotoLoopCondition
  ;If step number is less than waiting-for-print-finish, assume print hasn't started and retry the transaction.
  jp .retryPrintPhoto


/**
* Waits enough time for the printer to timeout and reset. This should be 100ms, about 7 frames.
* Make it 16 frames to be safe.
*/
WaitPrinterTimeout:
  push bc
  ld b,$10
  call Printer_Delay_b_Frames
  pop bc
ret

/**
* Sends a group of packets to the printer to print a single photo with no frame.
* Prior to calling this, ensure no captures are running and switch to the appropriate SRAM bank for the photo.
* @arg h: HIGH address of image data, either A0 or B0
* @return d: 0 in d if keepalive packet is $80/81
* @return e: status byte of the last packet
* @return b: TransactionError bitfield.
*/
SendTransaction_PrintPhoto_NoFrame:
  xor a
  ld [InTransaction_StepNumber],a ;Step 0, send init packet
  ;Clear buffer with an Init packet before sending packet.
  ;TODO: Call this AFTER after a photo instead of before, then only call this at the beginning if the "unprocessed data" status bit is set in the response to the initial status packet?
  push hl ; Push and pop the pointer to our image data.
  call SendPacket_Init
  pop hl
  call PacketErrorToTransactionError ; End transaction if there was a packet error.
  ret nz

  ;TODO: Send status until "unprocessed data" flag is unset, waiting a bit between packets. We do this because the buffer may not clear immediately.

  ;Send 7 data packets
  ld a,$01
  ld [InTransaction_StepNumber],a ;Step 1, send multiple lines
  ld c,$07 ;Counter for number of packets data packets to send
  : push bc
    call SendPacket_TwoLines_NoFrame
    ld a,b ; store error temporarily in a
    pop bc
    ld b,a ; restore error to b since we're only really popping c.
    call PacketErrorToTransactionError
    ret nz
    dec c
  jr nz,:-

  ;Send empty data packet before printing -- if we don't do this, the printer will send a packet error.
  ld a,$02 ; Step 2: send empty data packet before printing
  ld [InTransaction_StepNumber],a
  call SendPacket_EmptyData
  call PacketErrorToTransactionError ; Error checking: 
  ret nz ; If there is an error, return it in b

  ld a,$03 ; Step 3: send print packet.
  ld [InTransaction_StepNumber],a
  ;Send a print command
  call SendPacket_Print
  call PacketErrorToTransactionError
  ret nz

  .waitForPrintStart_Init
  ld a,$04 ; Step 4: Loop wait for printer start
  ld [InTransaction_StepNumber],a

  DEF PRINTSTART_MAX_POLLS EQU $00FF
  ld a,LOW(PRINTSTART_MAX_POLLS)
  ld [Transaction_StepTimeoutCounter_lilE],a ;Load LSB of step timeout counter.
  ld a,HIGH(PRINTSTART_MAX_POLLS)
  ld [Transaction_StepTimeoutCounter_lilE+1],a ;Load MSB of step timeout counter. -- $FF should be about 255/60 (4.25) seconds 
  .waitForPrintStart_Loop
    ;TODO easy out without timeout: if user presses B, return

    ;Wait for print command to start, with timeout of ?how many? tries
    call SendPacket_DetectPrinter
    call PacketErrorToTransactionError
    ret nz

    push de
    call PrinterDebug_DisplayCurrentPrintingPhoto
  
    ld b, PRINT_STARTLOOP_DELAY_FRAMES
    call Printer_Delay_b_Frames
    pop de
    ;For waiting for a printer, bit 2 (image data full) MAY be set, but for this borderless photo we don't fill up the buffer, so it shouldn't be set.
    ;TODO: Should we wait for the printer to START printing (bit 1 (printing) set, bit 3 'ready to print' unset from previous set value), then wait for it to STOP?
    ;or should we skip the check for printing START and check only for printing STOPPED (bit 1 unset, bit 3 unset)
    ;The very fast Pico Printer might shut off the "currently printing" flag before we get a chance to read it, leading to an infinite loop of waiting for print to start when it already has.
    bit 1,e
    ;if not printing, loop until it is
    jr nz,.waitForPrintStart_End ;if no error and Printing flag is set, finish the waitloop.
    ;decrement Transaction_StepTimeoutCounter and loop if it's non-zero.
    ld hl,Transaction_StepTimeoutCounter_lilE ;3c3b
    ld a,[hli] ;2c1b
    ld h,[hl] ;2c1b
    ld l,a ;Load timeoutcounter into hl ;1c1b

    dec hl
    
    ;Write back decremented timeout counter
    ld a,l ;load l into base address
    ld [Transaction_StepTimeoutCounter_lilE],a
    ld a,h ;ld h into +1
    ld [Transaction_StepTimeoutCounter_lilE+1],a

    ;test if hl==0
    xor a
    cp a,h
    jr nz,.waitForPrintStart_Loop     ;if high(counter) is nz, loop.
    xor a ;if high is zero, check whether low is zero.
    cp a,l
    jr nz,.waitForPrintStart_Loop
    ld b,TRANSACTION_ERR_MASK_STEP_TIMEOUT ; If TransactionStepTimeoutCounter is 0, return a Step Timeout error in b
    ret

  .waitForPrintStart_End

  .waitForPrintEnd_Init
  ld a,INTRANSACTION_STEP_PRINTPHOTO_WAITPRINTFINISH ; Step 5: Loop wait for printer end
  ld [InTransaction_StepNumber],a

  .waitForPrintEnd_Loop
    ;TODO: Easy out with no timeout: if user presses B, exit without timeout

    ;Wait for print command to finish, with timeout of ?how many? seconds
    call SendPacket_DetectPrinter ;this returns its results in hl instead of de. 
    call PacketErrorToTransactionError
    ret nz

    ;Wait here by displaying the photo number and step. (1 frames ~ 17ms)
    ;Real printer seems to be getting spammed with too many packets during this waitloop, adding the debug function ALMOST  fixes it
    ;we get to photo 8 before 0106 keepalive failed/ 0805 (on photo 8 step 5)
    ;also error at 0183 / 0705 -- this seems to imply we still have problems occurring in step 5....
    ; This implies to me it's a probabalistic error caused by packets being too close apart.
    push de
    call PrinterDebug_DisplayCurrentPrintingPhoto
    ld b, PRINT_ENDLOOP_DELAY_FRAMES
    call Printer_Delay_b_Frames
    pop de
    ;For waiting for a printer, bit 2 (image data full) MAY be set, but for this borderless photo we don't fill up the buffer, so it shouldn't be set.
    ;Wait for bit 1 unset
    ;Todo: add timeout, if the printer somehow ends up getting "stuck" printing
    bit 1,e
    ;if not printing, loop until it is
    ;TODO: Add a WAIT_FOR_PRINT_END timeout
  jr nz,.waitForPrintEnd_Loop

  ld a,$06 ; Step 6: Wait for printer has ended
  ld [InTransaction_StepNumber],a

ret

/**
* @clobber a,b,hl 
* @return d: keepalive packet
* @return e: status byte of the last packet
* @return b: Error, if any, or 0.
*/
SendPacket_EmptyData:
  ld a,CHECKSUM_RETRIES
  ld [Packet_Checksum_RetryCount],a ;Initialize checksum retry counter

  .checkSumRetry
  call SendMagicBytes
  ld hl,EmptyDataPacket
  ld c,(EmptyDataPacket.end-EmptyDataPacket)-1
  call SendMulti_hl_c
  ;Send keepalive byte
  ld b,$00
  call SendByte_b
  ldh a,[rSB]
  ld d,a ; d = Keepalive packet
  ;Request printer status
  :ld b,$00
  call SendByte_b
  ldh a,[rSB]
  ld e,a ; e = Status byte

  call Packet_ErrorCheck
  PACKET_ERROR_POST
ret
EmptyDataPacket: ;Does not include keepalive or status byte
  db $04,$00,$00,$00,$04,$00
.end

/**
* Checks for Keepalive, Status Byte, and Checksum errors.
* Call this near the end of every packet send.
* @param d: Keepalive byte response
* @param e: Status byte
* @return d: Keepalive byte response (unchanged from input)
* @return e: Status byte (unchanged from input)
* @return b: Packet error code, or 0 for OK
* Status system: bitfield b signals errors. Each error will set the bit in the bitfield. Multiple errors can be set at once: priority is decided by the callee.
* @clobber a, b
*/
Packet_ErrorCheck:
  ld b,PACKET_ERR_MASK_KEEPALIVE | PACKET_ERR_MASK_STATUS | PACKET_ERR_BIT_CHECKSUM ;2c
  ; Keepalive check: If no keepalive error, clear the corresponding bit in the error bitfield.
  ld a,d ;1c
  cp a,$81 ;2c
  jr nz,:+ ;3/2c
    res PACKET_ERR_BIT_KEEPALIVE,b ;2c
  :cp a,$80 ;2c
  jr nz,:+ ;3/2c
    res PACKET_ERR_BIT_KEEPALIVE,b ;2c
  :
  ;Status byte check -- If no errors in the upper bits, clear the corresponding bit in the error bitfield.
  ld a,e ;e = status byte of printer ;1c
  and a,%11110000 ;if any of the top 4 bits are set, status error ;2c
  jr nz,:+ ;3/2c
    res PACKET_ERR_BIT_STATUS,b ;2c
  : ; End Status byte check
  ;Checksum error check -- If no checksum error, clear the corresponding bit in the error bitfield.
  bit 0,e ; Checksum error check ;2c
  jr nz,:+ ;3/2c
    res PACKET_ERR_BIT_CHECKSUM,b ;2c
  : ;End checksum error check
ret

/**
* Sends an initialization packet, which clears the printer buffer.
*@return d: keepalive packet response
*@return e: status byte of printer
;@clobber a,bc,hl
*/
SendPacket_Init:
  xor a ;1c
  ld [Packet_Checksum_RetryCount],a ;4c

  .checkSumRetry
  call SendMagicBytes 
  ld hl,InitPacket ;3c
  ld c,(InitPacket.end-InitPacket)-1 ;2c
  call SendMulti_hl_c
  ;Send keepalive byte
  ld b,$00 ;2c
  call SendByte_b
  ldh a,[rSB] ;3c
  ld d,a ;1c
  ;Request printer status
  ld b,$00 ;2c
  call SendByte_b
  ldh a,[rSB] ;3c
  ld e,a ;e = status byte of printer ;1c
  call Packet_ErrorCheck
  PACKET_ERROR_POST
ret
InitPacket:
  db PRINTER_CMD_INIT,PRINTER_COMPRESSION_OFF,$00,$00,$01,$00
  .end

/**
* Sends a packet that contains two tilelines (160x16), 2bpp of an image.
* Since the DATA section is 640 bytes
* The address within the photo only increments by 128px horizontal x 16px vertical x 2bpp = 512 ($200) bytes.
* Therefore we can still pass in an h=address within the photo of the two tilelines to draw.
* @param h: location
* @clobber af,bc,hl,de
* @return d: result of keepalive byte from printer. Returns 0 if OK, else 1.
* @return e: status byte of printer
*/
;Print command is expected to be 88 33
; 02 00 04 00 
;01 number of sheets
;13 margin
;E4 palette
;7F exposure
;?? checksum
;?? checksum
;00 keepalive
;00 printer status
SendPacket_Print:
  xor a
  ld [Packet_Checksum_RetryCount], a

  .checkSumRetry
  call SendMagicBytes
  ld de,$00 ;initialize checksum
  ld b,PRINTER_CMD_PRINT
  call SendByte_b_checksum_de ;Send command
  ld b,PRINTER_COMPRESSION_OFF
  call SendByte_b_checksum_de
  ;Length of packet is 4
  ld b,$04 ;send low byte of length
  call SendByte_b_checksum_de
  ld b,$00 ;send high byte of length
  call SendByte_b_checksum_de
  ld b,$01 ;send number of sheets to print (1)
  call SendByte_b_checksum_de
  ld b,$13 ;send margin before/after print using default camera value
  call SendByte_b_checksum_de
  ld b,$E4 ;send palette
  call SendByte_b_checksum_de
  ld b,$7F ; send exposure value, +25%
  call SendByte_b_checksum_de

  ;Checksum (stored in de big-endian,sent little-endian)
  ld b,e
  call SendByte_b
  ld b,d
  call SendByte_b
  
  ;Send keepalive byte
  ld b,$00
  call SendByte_b
  ldh a,[rSB]
  ld d,a ; d = keepalive byte
  :;Request printer status
  ld b,$00
  call SendByte_b
  ldh a,[rSB]
  ld e,a ;e = status byte of printer
  call Packet_ErrorCheck
  PACKET_ERROR_POST
ret


/**
* Sends a packet that contains two tilelines (160x16), 2bpp of an image.
* Since the DATA section is 640 bytes
* The address within the photo only increments by 128px horizontal x 16px vertical x 2bpp = 512 ($200) bytes.
* Therefore we can still pass in an h=address within the photo of the two tilelines to draw.
* @param h: location
* @clobber af,bc,hl,de
* @return h: argument h plus 2.
* @return d: result of keepalive byte from printer. Returns 0 if OK, else 1.
* @return e: status byte of printer
*/
SendPacket_TwoLines_NoFrame:
  xor a
  ld [Packet_Checksum_RetryCount],a

  .checkSumRetry
  call SendMagicBytes
  ld de,$00 ;initialize checksum
  ld b,PRINTER_CMD_DATA
  call SendByte_b_checksum_de ;Send command
  ld b,PRINTER_COMPRESSION_OFF
  call SendByte_b_checksum_de
  ;Length of packet is 160x16x2bpp/8bitsperbyte = 640bytes= $0280 bytes
  ld b,$80 ;send low byte of length
  call SendByte_b_checksum_de
  ld b,$02 ;send high byte of length
  call SendByte_b_checksum_de

  call SendPacketFragment_OneLine_NoFrame ;First line
  call SendPacketFragment_OneLine_NoFrame ;Second line

  ;Checksum (stored in de big-endian,sent little-endian)
  ld b,e
  call SendByte_b
  ld b,d
  call SendByte_b
  
  ;Send keepalive byte
  ld b,$00
  call SendByte_b
  ldh a,[rSB]
  ld d,a
  ;Request printer status
  ld b,$00
  call SendByte_b
  ldh a,[rSB]
  ld e,a ;e = status byte of printer
  call Packet_ErrorCheck
  PACKET_ERROR_POST
ret

/**
*@param h: high byte of the line of 16 tiles to send
*@param de: checksum (big-endian)
*@return de: checksum
*@return hl: original address plus $100 (one line of Game Boy Camera tiles)
*/
SendPacketFragment_OneLine_NoFrame:
  ;Two blank tiles - $20 bytes of $FF
  ld b,$FF
  ld c,$20
  call SendFill_b_Checksum_de
  ;Send first line of 16 image tiles -- $100 bytes
  ld l,$00
  ld c,$FF
  call SendMulti_hl_c_Checksum_de
  ;Two blank tiles
  ld b,$FF
  ld c,$20
  call SendFill_b_Checksum_de
ret


; The problem with the generic packet pointer format is if only PART of the packet is a pointer, 
; such as with pointing to image data that will be printed surrounded with frame info. 
;This is still a usable system if we create a separate buffer with the non-packet data, but the send function will be more complex.
;We need to iterate through all data bytes, either while sending, or while building, for the checksum, 
;so it would be more time-efficient to copy the bytes to a buffer in WRAM while reading them, if calculating the checksum before sending.
;However, it means we will be using an extra $200 bytes to hold our capture data.

;One dirty trick we can use is to store or calculate the "partial checksum" of our packet
;Since 8-bit add is commutative this should work
;Additionally, 16-byte tiles of all-00s don't change the checksum, and each tile of 16 $FF's adds $F0
;If we have 8 of these, in each 2-tileline packet, it adds $80 to the checksum.
;the "partial checksum" of the two lines of our photo

;For calculate-during-send, all registers except de are used by SendMulti_hl_c
;We could create a new version of SendMulti_hl_c that calculated a running partial checksum of what it just sent in de.
;This means it would clobber every register, and we should probably make it push-pop all regs

PointerPacketScratchSpace:
;Magic bytes ignored
;1: Printer command
;1: Compression flag
;2: Packet Data length (send little endian, LSB first)
;2: Pointer to data
;2: Packet checksum
;Keepalive ignored
;Current Printer status ignored

.end




/**
* Sends a packet that requests printer status.
* Used to detect whether a functioning printer is connected.
* @clobber a,bc,de,hl
* @return b: Packet Error code
* @return d: value of keepalive response
* @return e: status byte of the last packet
*/
SendPacket_DetectPrinter:
  xor a
  ld [Packet_Checksum_RetryCount],a

  .checkSumRetry
  call SendMagicBytes
  ld hl,PrinterDetectionSequence
  ld c,(PrinterDetectionSequence.end-PrinterDetectionSequence)-1
  call SendMulti_hl_c
  ;Send keepalive byte
  ld b,$00
  call SendByte_b
  ldh a,[rSB]
  ld d,a ;d = keepalive byte
  ;Request printer status
  ld b,$00
  call SendByte_b
  ldh a,[rSB]
  ld e,a ;e = status byte of printer
  call Packet_ErrorCheck
  PACKET_ERROR_POST
ret
PrinterDetectionSequence: ;Does not include keepalive or status byte
  db $0F,$00,$00,$00,$0F,$00
.end

;@return d: PACKET_ERROR
ActionDetectPrinter::
  di
  call SendPacket_DetectPrinter
  ld d,b   ; Trampoline functions can only register-return in d, so move the packet error from b to d
  ;call PrinterDebug_DisplayResult
  xor a
  ldh [rIF],a
reti


/**
* Sends a byte over serial
* @param b: byte to send over serial
* @clobber a
* Waits after the byte is sent
*/
SendByte_b:
  : ldh a,[rSC]  ; Make sure nothing is being transmitted ;3c
    bit 7, a ;2c
  jr nz,:- ;2c untaken, 3c taken
  ld a,b ;load data into rSB ;1c
  ldh [rSB],a ;3c
  PatchCode_PrintSpeed_Location1: ;The second byte of the following instruction may be patchd to enable/disable fast printing
  ld a, SCF_START | SCF_SOURCE | SCF_SPEED ;2c
  ldh [rSC],a ;3c
  ;Wait for transmission to end
  :ldh a,[rSC] ;3c
  bit 7,a ;3c
  jr nz,:- ;2c untaken, 3c taken
ret

/**
* Sends a byte over serial, and adds it to the running checksum de
* @param b: byte to send over serial
* @param de: checksum (big endian) before current byte is included
* @clobber a
* @return de: new checksum (big endian) after current byte is included
* Waits after the byte is sent
*/
SendByte_b_checksum_de:
  : ldh a,[rSC]  ; Make sure nothing is being transmitted
    bit 7, a
  jr nz,:-
  ld a,b ;load data into rSB
  ldh [rSB],a
  ;start transmission at normal speed
  PatchCode_PrintSpeed_Location2: ;The second byte of the following instruction may be patchd to enable/disable fast printing
  ld a, SCF_START | SCF_SOURCE | SCF_SPEED
  ldh [rSC],a

  .addChecksum
  ld a,b ;load byte just sent into a
  ;16-bit add a into de -- running checksum
  ; 5 bytes, 5 cycles; no labels
  add e
  ld e, a
  adc d
  sub e
  ld d, a

  ;Wait for transmission to end
  :ldh a,[rSC]
  bit 7,a
  jr nz,:-
ret


/**
* @param hl: data to send over serial
* @param c: length of data-1 (can transmit up to $100 bytes)
* @clobber a,bc,hl
*/
SendMulti_hl_c:
  inc c ;1c
  : ld a,[hli] ;2c
    ld b,a ;1c
    call SendByte_b
    dec c ;1c
  jr nz,:- ;3/2 cycles
ret

/**
* Sends multiple bytes over serial, calculating a running 
* @param hl: data to send over serial
* @param c: length of data-1 (can transmit up to $100 bytes)
* @param de: checksum (stored big-endian, will be sent reversed). If 0000, will calculate a partial checksum that can be added to other partial checksums.
* @return de: new checksum (big-endian) with the bytes sent added. If de input is 0000, a partial checksum that can be added to other partial checksums is generated.
* @clobber a,bc,hl,de
*/
SendMulti_hl_c_Checksum_de:
  inc c
  : ld a,[hli]
    ld b,a
    call SendByte_b_checksum_de
  dec c
  jr nz,:-
ret

/**
* Sen
*@param de: checksum (stored big endian)
*@param b: byte to fill
*@param c: number of bytes to send
*@clobber af,bc,de
*@return de: new checksum
*/
SendFill_b_Checksum_de:
  :call SendByte_b_checksum_de
  dec c
  jr nz,:-
ret


/**
* @param none
* @clobber a,b
*/
SendMagicBytes:
  ld b,$88
  call SendByte_b
  ld b,$33
  call SendByte_b
ret

; Draws a the value of a byte (two nybbles) to the tilemap in settings
; Assumes we're outside of an interrupt, in VBlank.
; @param h: byte to display to tilemap
; @param de: address in tilemap to display to. Moves from low address to high address. 
; @clobbers a,de
; @returns de+1
MACRO PRINTERDEBUG_DRAW_BYTE_TO_TILEMAP
  ; Load first nybble of our variable into low nybble of a. This is the high nybble if not flipped, low nybble if flipped.
  IF (!SCREEN_FLIP_H) 
    swap a
  ENDC
  and a,$0F
  add a,UI_ICONS_BASE_ID ; Convert it to the tile ID of the corresponding hex character.
  ld [de],a ; Put high nybble in lower tilemap address
  inc de  ; Move to next tile in tilemap
  ; Load second nybble of our variable into low nybble of a. This is the low nybble if not flipped, high nybble if flipped.
  ld a,h 
  IF (SCREEN_FLIP_H)
    swap a 
  ENDC
  and a,$0F
  add a,UI_ICONS_BASE_ID
  ld [de],a

ENDM


/**
* Displays two bytes (the currently printing photo number and the step number) on the Settings screen.
* Also displays the step number within the SendTransaction
*This assumes there are no interrupts happening (specifically the VBlank interrupt) and will take up to 16+1 frames
*@clobber a,b,de,hl
*/
PrinterDebug_DisplayCurrentPrintingPhoto:
  call WaitForVBlankStart
  ;draw the results to the screen -- if we're in Settings, tilemap 9C00, pulling from VRAM1's tiles. Use TILE IDs $6x to draw numbers, $00 to draw text
  ;The area should be overwritten once we get back to the main loop.
  SETTINGS_PUT_TILEMAP_ADDR_IN_R16 16,(NUM_SETTINGS+1),de
  
  ;Draw Picture Counter, then StepNumber
  IF (!SCREEN_FLIP_H)
    ld a,[PrintAll_PictureCounter]
    ld h,a
    PRINTERDEBUG_DRAW_BYTE_TO_TILEMAP
    inc de
    ld a,[InTransaction_StepNumber]
    ld h,a
    PRINTERDEBUG_DRAW_BYTE_TO_TILEMAP
  ELSE
    ld a,[InTransaction_StepNumber]
    ld h,a
    PRINTERDEBUG_DRAW_BYTE_TO_TILEMAP
    inc de
    ld a,[PrintAll_PictureCounter]
    ld h,a    
    PRINTERDEBUG_DRAW_BYTE_TO_TILEMAP PrintAll_PictureCounter
  ENDC
ret

/**
* Displays two bytes on the Settings screen, then waits for some frames and a button press.
*This assumes there are no interrupts happening (specifically the VBlank interrupt) and will take up to 16+1 frames
*@param h: keepalive result, 0 for a success, other for fail
*@param l: status register
*@clobber a,b,de
*/
PrinterDebug_DisplayResult:
  call WaitForVBlankStart
  ;draw the results to the screen -- if we're in Settings, tilemap 9C00, pulling from VRAM1's tiles. Use TILE IDs $6x to draw numbers, $00 to draw text
  ;$9C30 is the right 4 tiles and should be overwritten once we get back to the main loop.
    SETTINGS_PUT_TILEMAP_ADDR_IN_R16 16,NUM_SETTINGS,de  ;ignores flip

  IF (!SCREEN_FLIP_H)
    PRINTERDEBUG_DRAW_BYTE_TO_TILEMAP ;draw h:keepalive packet
    inc de
    ld h,l ; load l into h
    PRINTERDEBUG_DRAW_BYTE_TO_TILEMAP ;draw l:status byte
  ELSE
    ld a,h ; swap h and l if flipped
    ld h,l 
    ld l,a
    PRINTERDEBUG_DRAW_BYTE_TO_TILEMAP ;draw l:status byte
    inc de
    ld h,l ; load l into h
    PRINTERDEBUG_DRAW_BYTE_TO_TILEMAP ;draw h:keepalive packet
  ENDC
  
  ;Wait 16 frames to display it
  ld b,$10
  call Printer_Delay_b_Frames

  ;Wait for a button press
  xor a
  ld [joypad_active],a
  :call GetInputROM
  ld a,[joypad_active]
  and a
  jr z,:-
  ; End display result 

ret


/*
* Delay for b frames.
* Only use when interrupts are disabled; it checks for VBlank, so if an ISR denies it VBlank, it may infinite loop.
* @param b: number of frames to delay for. b=0 is 256 frames.
*/
Printer_Delay_b_Frames:
  .displayWait
  call WaitForVBlankStart
  dec b
  jr nz,.displayWait
ret



/**
* Converts a PacketError in b to a TransactionError in b and tests whether it's empty.
* If there is no error, z flag will be set, otherwise it is unset.
* @param b: PacketError bitfield
* @return b: TransactionError bitfield
**/
PacketErrorToTransactionError:
; If packet keepalive error, checksum error, or status error, return that error in b as-is.
; Optimization Note: At the time of writing, Packet Error values are a subset of Transaction Error values -- They can be returned as-is.
; The following check for packet errors can be replaced by "ld a,b / and a / ret"
; However, the below code works even if the values of the types change.
  xor a
  bit PACKET_ERR_BIT_STATUS,b
  jr z,:+
    set TRANSACTION_ERR_BIT_STATUS,a
  :bit PACKET_ERR_BIT_CHECKSUM,b
  jr z,:+
    set TRANSACTION_ERR_BIT_CHECKSUM,a
  :bit PACKET_ERR_BIT_KEEPALIVE,b
  jr z,:+
    set TRANSACTION_ERR_BIT_KEEPALIVE,a
  :
  ld b,a
  and a, a
ret




;---------------------------Transfer Functions-------------------------------
/**
* Sends a group of packets to the printer to print a single photo with no frame with the transfer protocol.
* Prior to calling this, ensure no captures are running and switch to the appropriate SRAM bank for the photo.
* @arg h: HIGH address of image data, either A0 or B0
* @return d: 0 in d if keepalive packet is $80/81
* @return e: status byte of the last packet
*/
SendTransaction_TransferPhoto:
  ; Don't check for Printer presence?
  xor a ;1c 
  ld [InTransaction_StepNumber],a ;Step 0, send init packet ;4c
  ;Clear buffer with an Init packet before sending packet.
  push hl ; Push and pop the pointer to our image data. ;4c
  call SendPacket_Init
  pop hl ;3c
  ;Don't handle errors

  ;Only send one giant data packet of 3584 bytes (16x14 tiles)
  ;(According to the Photo! readme)
  ld a,$01 ;2c
  ld [InTransaction_StepNumber],a ;Step 1, send all lines ;4c
  call SendPacket_TransferData
  ;Don't error check
  call PrinterDebug_DisplayCurrentPrintingPhoto

  ld a,$03 ; Step 3: Data packet send has ended ;2c
  ld [InTransaction_StepNumber],a ;4c
ret


/*
*@param h: HIGH address of image data, either A0 or B0
*@return none: we don't do error checking.
*/
SendPacket_TransferData:


  call SendMagicBytes
  ld de,$00 ;initialize checksum
  ld b,PRINTER_CMD_TRANSFER
  call SendByte_b ;Send command
  ld b,PRINTER_COMPRESSION_OFF
  call SendByte_b
  ;Length of packet is 128x112x2bpp/8bitsperbyte = decimal 3584 or $E000
  ld b,$00 ;send low byte of length
  call SendByte_b
  ld b,$0E ;send high byte of length
  call SendByte_b

  REPT $10
  call SendPacketFragment_OneLine_Transfer
  ENDR
  ;Checksum is zero
  ld b,00
  call SendByte_b
  ld b,00
  call SendByte_b
  ;Send keepalive byte
  ld b,$00
  call SendByte_b
  ldh a,[rSB]
  ;Request printer status
  ld b,$00
  call SendByte_b
  ldh a,[rSB]
ret

/**
* Sends one line of tiles in the Transfer Data packet format (16 tiles)
*@param h: high byte of the line of 16 tiles to send
*@return hl: original address plus $100 (one line of Game Boy Camera tiles)
*/
SendPacketFragment_OneLine_Transfer:
  ;Send first line of 16 image tiles -- $100 bytes
  ld l,$00 ;2c2b
  ld c,$FF ;2c2b
  call SendMulti_hl_c
ret

ActionTransferAll::
  di

  ld a,30
  ld [PrintAll_ToPrint],a
  
  ld a,[PrintAll_ToPrint]
  and a
  jr z,.allPhotosFinished ;if number of photos to print, determined by the state vector, is 0, finish up early.

  xor a
  ld [PrintAll_PictureCounter],a ;Initialize photo counter to 0

.printPhotoLoop
  ;get Slot Number from PictureCounter -- in this case, PictureCounter represents the Gallery Number
  ;Switch to SRAM bank that holds State Vector
  xor a
  ld [rRAMB],a

  ;Set hl and rRAMB to point to our photo. hl = $A000 if ID is even, $B000 if odd
  ld a,[PrintAll_PictureCounter]
  ;Trampoline-call StateVector_GetImageSlot, which returns the slot number in d.
  ld d,a ; argument d is the Gallery Number to find
  ld hl, StateVector_GetImageSlot
  ld e, SAVE_RAMBANK; WRAM bank which contains the callee.
  call Trampoline_hl_e
  
  ;Check whether d==FF. If it is, skip to .printPhotoLoopCondition without actually printing the image.
  ld a,d
  cp a,$FF
  jr z,.printPhotoLoopCondition ;if d==$FF, jump to the loop condition without transferring the photo.
  ;If the returned value was a valid slot number, print it.

  ld hl,$A000 ;Set hl to $A000 if slot number is even (default case)
  bit 0,a
  jr z,.evenSlotID ;if SLOT number is even, address starts at $A0, else $B0
    ld h,$B0 ;slot number is odd, location is B0
  .evenSlotID

  ;with a=slot ID of photo,
  srl a ;a = SlotID >> 1
  inc a ;a =(SlotID>>1)+1
  ld [rRAMB], a ;switch SRAM bank to the appropriate SRAM bank

  call SendTransaction_TransferPhoto

  .printPhotoLoopCondition ; Increments the Picture counter and determines whether to continue looping.
  ;Display to user: printing photo and step within the sendtransaction without asking for input
  call PrinterDebug_DisplayCurrentPrintingPhoto

  ;Increment PictureCounter, then finish when it is equal to the number of photos to print
    ;Don't touch de (return value from the photo print) -- hl is OK, since our next photo has a very different address anyway.
    ld a,[PrintAll_PictureCounter]
    inc a 
    ld [PrintAll_PictureCounter],a ;increment counter and store it back
    ld b,a ;store counter in b
    ld a,[PrintAll_ToPrint]
    cp a,b ;cp ToPrint, (ID of next photo to print)
    jr z,.allPhotosFinished ;if all photos are finished printing, finish.


  ; If B button is pressed
  call GetInputROM
  ldh a,[joypad_state]
  and a,JOYPAD_B_MASK ; I forget if this is active-high like joypad_active or active-low like the actual registers.

  jr z, .printPhotoLoop  ;If no errors, loop not finished, and B button not pressed, jump back to the beginning.


  .allPhotosFinished
  call PrinterDebug_DisplayCurrentPrintingPhoto

  ;Redraw Settings screen
  ld e,UI_RAMBANK
  ld hl,InitSettingsGfx 
  call Trampoline_hl_e

  xor a
  ldh [rIF],a
reti


/* Patches the sections of code which puts a value in SC for printing. If fast printing is enabled, $83 is loaded in. If not, $81.
* Assumes it's in the same WRAM bank as the code to be patched, so should be trampoline called.
* @clobber a, b
*/
PatchCode_PrintSpeed::
  ld b,SCF_START | SCF_SOURCE ; Used if Setting_Print_Speed is 0
  ld a,[Setting_Print_Speed]
  and a ; check whether Setting_Print_Speed is 0
  jr z,:+;If it is, don't change b.
    set 1,b
  :
  ld a,b ;a is now holding our value to write to SC to start printing.
  ld [PatchCode_PrintSpeed_Location1+1],a ;change the n8 in a ld a,n8
  ld [PatchCode_PrintSpeed_Location2+1],a

ret

ENDL