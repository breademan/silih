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
  ld hl,$A000
  ld a,$01
  ld [$4000], a ;switch SRAM bank to 01

  call SendTransaction_PrintPhoto_NoFrame
  ld h,d
  ld l,e
  call PrinterDebug_DisplayResult
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
  ;TODO: clear buffer with an Init packet
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
  ;Send empty data packet before printing
  call SendPacket_EmptyData
  ;Error checking: if d is not zero, return and pass it up to the display function.
  ld d,h
  ld e,l
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

ENDL