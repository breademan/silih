INCLUDE "src/general.inc"
INCLUDE "src/hardware.inc"

SECTION "Printer SECTION",ROM0[$1000 + ($1000*PRINTER_RAMBANK)]
Printer_Storage::
LOAD "Printer LOAD", WRAMX [$D000]

ActionPrintAll::
  di
  ;Use the state vector to figure out which photos are in which slot
  ;TODO: Stubbed to just point to 01:A000
  ;Set hl to the address of our photo start and switch to the appropriate SRAM bank.
  ;Switch to RAMbank 01:
  ld a,30 ;Initialize picture counter -- TODO: Set it to however many pictures you actually have in active slots
  ld [PrintAll_ToPrint],a ;TODO: replace this too
  
  ld a,[PrintAll_ToPrint]
  and a
  jr z,.allPhotosFinished ;if number of photos to print, determined by the state vector, is 0, finish up early.

  xor a
  ld [PrintAll_PictureCounter],a ;Initialize photo counter to 0

.printPhotoLoop

  ;Set hl and rRAMB to the appropriate numbers. hl = $A000 if ID is even, $B000 if odd
  ld a,[PrintAll_PictureCounter]  ;TODO: We're not looking at an even-or-odd image NUMBER 
                                  ;(which tells you nothing about the address without looking up which slot it takes up in the SV)
                                  ;but for an even or odd image SLOT
                                  ;Therefore, after loading the PictureCounter, we should call a function that returns which slot the photo number is located in
                                  ;return it in register a
                                  ;For testing, just ignore active/inactive values and pretend the slot number IS the photo number, so just print slots in order
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

  ;Error Checking
  ;If d!=0 (keepalive byte wrong) break the whole loop and don't attempt to print more pictures.
    ld a,d
    and a
    jr nz,.keepaliveFail
    ;If we receive some kind of error that implies we can't continue (top 4 bytes), don't attempt to print more pictures.
    ld a,e
    and a,%11110000
    jr nz,.generalError  
  ;TODO: if checksum error, attempt resend a few times without incrementing the counter and checking it's the end.
  ;TODO: this is an infinite loop. Add a retry counter local variable, initialize it at the start of PrintAll, and test it here
    ;ld a,e
    ;bit 0,a
    ;jr z,.checksumGood
    ;.checksumBad
    ; ;increment retry counter,check if you've passed the threshold for retries, and terminate early if so.
    ;jr .printPhotoLoop

  .checksumGood
  
  ;TODO: If no bad checksum, reset the retry counter
  ;
  
  ;TODO: Display to user: printing photo (X), without asking for input
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
  
  ;If no errors and loop not finished, jump back to the beginning.
  jr .printPhotoLoop


  .generalError
  .keepaliveFail
  .allPhotosFinished
  ld h,d
  ld l,e
  call PrinterDebug_DisplayResult
  xor a
  ldh [rIF],a
reti

;These local variables for ActionPrintAll exist because pushing/popping after each print makes register handling and conditionals more complicated.
PrintAll_PictureCounter: db ;local variable that holds the ID of the current photo to print. Always starts at 0. Iterates through photo IDs $00-$1D
PrintAll_ToPrint: db ;local variable that holds the number of photos to print


ActionPrintActive:
  di

  xor a
  ldh [rIF],a
reti

/**
* Sends a group of packets to the printer to print a single photo with no frame.
* Prior to calling this, ensure no captures are running and switch to the appropriate SRAM bank for the photo.
* @arg h: HIGH address of image data, either A0 or B0
* @return d: 0 in d if keepalive packet is $80/81
* @return e: status byte of the last packet
*/
SendTransaction_PrintPhoto_NoFrame:
  ;TODO: Ensure printer is connected by getting printer status.
  ;TODO: clear buffer with an Init packet before sending packet
 ;Send 7 data packets
  ld c,$07 ;Counter for number of packets data packets to send
  : push bc
    call SendPacket_TwoLines_NoFrame
    pop bc
    ;Error checking: if d is not zero, return and pass it up to the display function.
    ld a,d
    cp a,$00
    ret nz ;if keepalive response is wrong, exit early
    ;Error checking: if e is not what is expected, return and pass it up to the display function.
    ;For TwoLine packets, we expect a status byte with only bit 3 possibly set, and with bit 2 set at the end of the transaction.
    ld a,e
    and a,%11110011 ;ignore unprocessed data and image data full statuses
    ret nz ;if z, all is good. Else return early.
    dec c
  jr nz,:-
  ;Send empty data packet before printing -- if we don't do this, the printer will send a packet error.
  
  call SendPacket_EmptyData
  ld d,h
  ld e,l

  ;Error checking: if d is not zero, return and pass it up to the display function.
  ld a,d
  and a
  ret nz ;if keepalive response is wrong, exit early
  ;Error checking: if e is not what is expected, return and pass it up to the display function.
  ;For TwoLine packets, we expect a status byte with only bit 3 possibly set, and with bit 2 set at the end of the transaction.
  ld a,e
  and a,%11110001 ;ignore unprocessed data, image data full, and currently printing statuses
  ret nz ;if z, all is good. Else return early.


  ;Send a print command
  call SendPacket_Print
  ;Error checking: if d is not zero, return and pass it up to the display function.
  ld a,d
  and a
  ret nz ;if keepalive response is wrong, exit early
  ;Error checking: if e is not what is expected, return and pass it up to the display function.
  ;For TwoLine packets, we expect a status byte with only bit 3 possibly set, and with bit 2 set at the end of the transaction.
  ld a,e
  and a,%11110001 ;ignore unprocessed data, image data full, and currently printing statuses
  ret nz ;if z, all is good. Else return early.

  .waitForPrintStart
  ;TODO easy out without timeout: if user presses B, return

  ;Wait for print command to start, with timeout of ?how many? tries
  call SendTransaction_DetectPrinter ;this returns its results in hl instead of de. 
  ld d,h   ;For compatability with the rest of the return values in this function, put the results in de
  ld e,l
  
  ;Error checking: if d is not zero, return and pass it up to the display function.
  ld a,d
  and a
  ret nz ;if keepalive response is wrong, exit early
  ;Error checking: if e is not what is expected, return and pass it up to the display function.
  ld a,e
  and a,%11110001 ;ignore unprocessed data, image data full, and currently printing statuses
  ret nz ;if z, all is good. Else return early.

  ;For waiting for a printer, bit 2 (image data full) MAY be set, but for this borderless photo we don't fill up the buffer, so it shouldn't be set.
  ;TODO: Should we wait for the printer to START printing (bit 1 (printing) set, bit 3 'ready to print' unset from previous set value), then wait for it to STOP?
  ;or should we skip the check for printing START and check only for printing STOPPED (bit 1 unset, bit 3 unset)
  ;The very fast Pico Printer might shut off the "currently printing" flag before we get a chance to read it, leading to an infinite loop of waiting for print to start when it already has.
  bit 1,e
  ;if not printing, loop until it is
  ;TODO: Add a WAIT_FOR_PRINT_START timeout
  jr z,.waitForPrintStart

.waitForPrintEnd
  ;TODO: Easy out with no timeout: if user presses B, exit without timeout

  ;Wait for print command to finish, with timeout of ?how many? seconds
  call SendTransaction_DetectPrinter ;this returns its results in hl instead of de. 
  ld d,h   ;For compatability with the rest of the return values in this function, put the results in de
  ld e,l
  
  ;Error checking: if d is not zero, return and pass it up to the display function.
  ld a,d
  and a
  ret nz ;if keepalive response is wrong, exit early
  ;Error checking: if e is not what is expected, return and pass it up to the display function.
  ld a,e
  and a,%11110001 ;ignore unprocessed data, image data full, and currently printing statuses
  ret nz ;if z, all is good. Else return early.

  ;For waiting for a printer, bit 2 (image data full) MAY be set, but for this borderless photo we don't fill up the buffer, so it shouldn't be set.
  ;Wait for bit 1 unset
  ;Todo: add timeout, if the printer somehow ends up getting "stuck" printing
  bit 1,e
  ;if not printing, loop until it is
  ;TODO: Add a WAIT_FOR_PRINT_START timeout
  jr nz,.waitForPrintEnd

ret

/**
* @return h: 0 in h if keepalive packet is $80/81
* @return l: status byte of the last packet
*/
SendPacket_EmptyData:
  call SendMagicBytes
  ld hl,EmptyDataPacket
  ld c,(EmptyDataPacket.end-EmptyDataPacket)-1
  call SendMulti_hl_c
  ;Send keepalive byte
  ld b,$00
  call SendByte_b
  ldh a,[rSB]
  ld h,$01 ;h will b 1 if no keepalive, 0 if keepalive
  cp a,$81
  jr nz,:+
    ld h,$00
  :cp a,$80
  jr nz,:+
    ld h,$00
  :;Request printer status
  ld b,$00
  call SendByte_b
  ldh a,[rSB]
  ld l,a ;l = status byte of printer
ret
EmptyDataPacket: ;Does not include keepalive or status byte
  db $04,$00,$00,$00,$04,$00
.end


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
  ld d,$01 ;d will be 1 if no keepalive, 0 if keepalive
  cp a,$81
  jr nz,:+
    ld d,$00
  :cp a,$80
  jr nz,:+
    ld d,$00
  :;Request printer status
  ld b,$00
  call SendByte_b
  ldh a,[rSB]
  ld e,a ;e = status byte of printer
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
  ld d,$01 ;d will be 1 if no keepalive, 0 if keepalive
  cp a,$81
  jr nz,:+
    ld d,$00
  :cp a,$80
  jr nz,:+
    ld d,$00
  :;Request printer status
  ld b,$00
  call SendByte_b
  ldh a,[rSB]
  ld e,a ;e = status byte of printer
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
* @return h: 0 in h if keepalive packet is $80/81
* @return l: status byte of the last packet
*/
SendTransaction_DetectPrinter:
  call SendMagicBytes
  ld hl,PrinterDetectionSequence
  ld c,(PrinterDetectionSequence.end-PrinterDetectionSequence)-1
  call SendMulti_hl_c
  ;Send keepalive byte
  ld b,$00
  call SendByte_b
  ldh a,[rSB]
  ld h,$01 ;h will b 1 if no keepalive, 0 if keepalive
  cp a,$81
  jr nz,:+
    ld h,$00
  :cp a,$80
  jr nz,:+
    ld h,$00
  :;Request printer status
  ld b,$00
  call SendByte_b
  ldh a,[rSB]
  ld l,a ;l = status byte of printer
ret
PrinterDetectionSequence: ;Does not include keepalive or status byte
  db $0F,$00,$00,$00,$0F,$00
.end

ActionDetectPrinter::
  di
  call SendTransaction_DetectPrinter
  call PrinterDebug_DisplayResult
  xor a
  ldh [rIF],a
reti

/**
* Displays two bytes on the Settings screen, then waits for 16 frames and a button press.
*This assumes there are no interrupts happening (specifically the VBlank interrupt) and will take up to 16+1 frames
*@param h: keepalive result, 0 for a success, other for fail
*@param l: status register
*@clobber a,b,de
*/
PrinterDebug_DisplayResult:
  ;wait for VBlank -- mode 0 HBlank, then mode 1(VBlank)
  :ldh a,[rSTAT] ;Wait for HBlank
  and a,%00000011
  jr nz,:-
  ;wait for VBlank
  :ldh a,[rSTAT]
  and a,%00000011
  cp a,$01
  jr nz,:-
  ;draw the results to the screen -- if we're in Settings, tilemap 9C00, pulling from VRAM1's tiles. Use TILE IDs $6x to draw numbers, $00 to draw text
  ;$9C30 is the right 4 tiles and should be overwritten once we get back to the main loop.
  ld de,$9C30  ;ignores flip
  ;h: keepalive packet: 0 (ok) or 1 (bad)
  ld a,h ;high nybble
  swap a
  and a,$0F
  add a,$60
  ld [de],a
  inc de
  ld a,h ;low nybble
  and a,$0F
  add a,$60
  ld [de],a
  inc de
  ;l: printer status
  ld a,l ;high nybble
  swap a
  and a,$0F
  add a,$60
  ld [de],a
  inc de
  ld a,l;low nybble
  and a,$0F
  add a,$60
  ld [de],a

  ;Wait 16 frames to display it
  ld b,$10
  .displayWait
  :ldh a,[rSTAT] ;Wait for HBlank
  and a,%00000011
  jr nz,:-
  ;wait for VBlank
  :ldh a,[rSTAT]
  and a,%00000011
  cp a,$01
  jr nz,:-
  dec b
  jr nz,.displayWait

  ;Wait for a button press
  xor a
  ld [joypad_active],a
  :call GetInputROM
  ld a,[joypad_active]
  and a
  jr z,:-
  ; End display result 

ret

/**
* Sends a byte over serial
* @param b: byte to send over serial
* @clobber a
* Waits after the byte is sent
*/
SendByte_b:
  : ldh a,[rSC]  ; Make sure nothing is being transmitted
    bit 7, a
  jr nz,:-
  ld a,b ;load data into rSB
  ldh [rSB],a
  ;start transmission at normal speed
  ;TODO: decide between normal and high speed later
  ld a, SCF_START | SCF_SOURCE
  ldh [rSC],a
  ;Wait for transmission to end
  :ldh a,[rSC]
  bit 7,a
  jr nz,:-
ret

/**
* Sends a byte over serial, and adds it to the running checksum de
* @param b: byte to send over serial
* @param de: checksum (big endian) to send 
* @clobber a
* Waits after the byte is sent
*/
SendByte_b_checksum_de:
  : ldh a,[rSC]  ; Make sure nothing is being transmitted
    bit 7, a
  jr nz,:-
  ld a,b ;load data into rSB
  ldh [rSB],a
  ;start transmission at normal speed
  ;TODO: decide between normal and high speed later
  ld a, SCF_START | SCF_SOURCE
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
  inc c
  : ld a,[hli]
    ld b,a
    call SendByte_b
    dec c
  jr nz,:-
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

/**
* Displays two bytes on the Settings screen, then waits for 16 frames and a button press.
*This assumes there are no interrupts happening (specifically the VBlank interrupt) and will take up to 16+1 frames
*@clobber a,b,de,hl
*/
PrinterDebug_DisplayCurrentPrintingPhoto:
  ;wait for VBlank -- mode 0 HBlank, then mode 1(VBlank)
  :ldh a,[rSTAT] ;Wait for HBlank
  and a,%00000011
  jr nz,:-
  ;wait for VBlank
  :ldh a,[rSTAT]
  and a,%00000011
  cp a,$01
  jr nz,:-
  ;draw the results to the screen -- if we're in Settings, tilemap 9C00, pulling from VRAM1's tiles. Use TILE IDs $6x to draw numbers, $00 to draw text
  ;$9C50 is the right 4 tiles and should be overwritten once we get back to the main loop.
  ld de,$9C50  ;ignores flip
  ld a,[PrintAll_PictureCounter]
  ld h,a ;temporarily store the picture number in h
  ;h: keepalive packet: 0 (ok) or 1 (bad)
  ld a,h ;high 
  swap a
  and a,$0F
  add a,$60
  ld [de],a
  inc de
  ld a,h ;low nybble
  and a,$0F
  add a,$60
  ld [de],a
  inc de

  ;Wait 16 frames to display it
  ld b,$10
  .displayWait
  :ldh a,[rSTAT] ;Wait for HBlank
  and a,%00000011
  jr nz,:-
  ;wait for VBlank
  :ldh a,[rSTAT]
  and a,%00000011
  cp a,$01
  jr nz,:-
  dec b
  jr nz,.displayWait

ret


ENDL