;Code to do with saving and reading from the cart SRAM, except for the camera captures
;All of these function will need to be set to SRAM bank 0


DEF STATEVECTOR_START_ADDR EQU $A000+$11B2
DEF STATEVECTOR_LAST_ADDR EQU $A000+$11CF ;last address in the state vector
DEF CHECKSUM_MAGIC_START EQU $A000+$11D0
DEF CHECKSUM_MAGIC_END EQU $A000+$11D4
DEF CHECKSUM_SUM_ADDR EQU $A000 + $11D5
DEF CHECKSUM_XOR_ADDR EQU $A000 + $11D6

DEF STATEVECTOR_START_ADDR_ECHO EQU $A000+$11D7
DEF STATEVECTOR_LAST_ADDR_ECHO EQU $A000+$11F4
DEF CHECKSUM_MAGIC_START_ECHO EQU $A000+$11F5
DEF CHECKSUM_MAGIC_END_ECHO EQU $A000+$11F9
DEF CHECKSUM_SUM_ADDR_ECHO EQU $A000+$11FA
DEF CHECKSUM_XOR_ADDR_ECHO EQU $A000+$11FB


;Writes a byte to the state vector located in $11B2-CF
;Updates the checksums in $11D5 (sum) and $11D6 (xor)
; Updating the backup echo checksums requires we do all this over again or it will be buggy-- just copy the vector wholesale at the end of each multi-byte_transaction.
;Args   a = value of new byte
;       hl = address of byte to change
;clobbers ALL registers except C, so we may need to pop/push register state beforehand
;push/pops make it only clobber af
StateVector_WriteByte:
;d = new byte value
;e = old byte value
;b = new checksum SUM
push bc
push de

ld d, a ;d = new byte value
ld a,[hl]
ld e, a ; e = old byte value
ld a, d
ld [hl],a ; write new byte value
sub a,e
ld b,a
ld a,[CHECKSUM_SUM_ADDR]
add a,b
ld [CHECKSUM_SUM_ADDR], a

ld a,[CHECKSUM_XOR_ADDR]
xor a, e ; a = old_checksum xor old_byte
xor a, d ; a= new checksum
ld [CHECKSUM_XOR_ADDR], a ;write new checksum xor

pop de
pop bc
ret


;Iterates through the state vector (using repeated calls to FindByDisplayNumber). When it finds a display number that is greater than the missing number and less than 1E,
; it decrements it by 1.
;arg c: value that is missing from the SV
;clobbers c, hl, a
StateVector_FixMissingVal:
    ;c: persistent storage for missing value
    ;hl: addr of current element in state vector
    ld hl, STATEVECTOR_START_ADDR
.loop: 
    ld a,[hl]
    cp a,$FF;if SV val is $FF (empty slot), skip it
    jp z,:+ 
    cp a, c ; carry if SV val < missing; no-carry if SV val >= missing
    jr c, :+
    dec a ;SV value > missing: decrement it by one
    call StateVector_WriteByte

    :inc l
    ld a,LOW(STATEVECTOR_LAST_ADDR)+1         ; check whether next element is out-of-bounds
    cp a,l ; if (h)l==last address+1 (z), finish
    jp nz,.loop


    call StateVector_Backup ;finish the transaction by writing to the backup state vector.
    ret

    ;b= starting max value. FixAll will not check for missing values above this
StateVector_FixAll:
.any_missing_loop:
    call StateVector_FindMissing ;compare the retval from this (a) to the max returned in
    ;if missing value is less than max, fix it, decrement the max by 1, and loop. Missing value should never be equal to max.
    cp a,b ; carry if missing val < max
    jr nc, :+
    ld c,a ; FixMissingVal takes c as the argument, so we should fix this
    call StateVector_FixMissingVal
    dec b
    jr nz, .any_missing_loop ;loop back if we may still have some missing values below max
    :ret


;returns a as the value missing from the state vector. 
;If none are missing but the state vector isn't full, it returns max+1 (up to 1E)
;clobbers a,hl
StateVector_FindMissing:
  xor a ; a = value to compare to/"outer counter" goes from 0 to (max-1)
  .loop:
  ld hl,STATEVECTOR_START_ADDR
  REPT $1E
    ;5 cycles per check -- $96 cycles total -- 4 bytes per check: $78 bytes
    ;$96 cycles * $1E loops means $1194 = 4500 M-cycles at worst, if the camera is full. One frame is 17556 M-cycles, so this may be a problem.
    ;We can avoid this by... pretty much only calling it at the start, when we don't care about startup speed as much. 
    ;Whenever we delete, we immediately fix the missing pic using the ID of the deleted photo, thus obviating the need to call this.
    cp a, [hl]
    jr z, .add_and_continue
    inc l
  ENDR
    ret ;at the end, if none of the slots contain this number, then it is missing. Return it as the value.

    .add_and_continue: ;if we got here, it means that the value in a was found in the state vector. Check if we just checked the max value a=1D. If so, return $1E)
  inc a
  cp a,$1E
  jp nz,.loop
  ret




StateVector_Init:
  call StateVector_CountUsedAndFree ;puts the number of used or free slots in an HRAM variable
  call StateVector_GetMax ;returns the max value in the state vector in b
  call StateVector_FixAll ;uses the value in b as the starting max value
  ret

;Finds the max display number in the state vector
;args null
;returns max in b
StateVector_GetMax:
  ld b,$00 ;running max
  ld hl, STATEVECTOR_START_ADDR
  ld c, $1E
  .loop:
    ld a,[hli]
    cp a, $FF
    jp z, .loopcheck ;skip if $FF
    cp a, b
    jp c, .loopcheck ;if running_max > value, (carry) then skip
    ld b,a

  .loopcheck:
    dec c
    jp nz,.loop
  ret

;iterates through the state vector and counts the number of values <1E (used) and >=1E (free)
;We can derive one from the other, assuming consistency, but for now we'll count both in their own global variables, SAVE_SLOTS _FREE and _USED
;As a side effect, also returns SAVE_SLOTS_USED in a
StateVector_CountUsedAndFree:
  ld hl, STATEVECTOR_START_ADDR
  ld b,$00 ; b = used count
  ld c, b  ; c = free count

  .loop:ld a, [hli]
  cp a,$1E ; if carry, it's used.
  jr c,.used
  .free: inc c
  jr .check
  .used: inc b
  .check: ld a, LOW(STATEVECTOR_LAST_ADDR)+1
  cp a, l
  jr nz, .loop ;if next address is within bounds, loop

  ; Write back used and free
  ld a,c
  ld [SAVE_SLOTS_FREE], a
  ld a,b
  ld [SAVE_SLOTS_USED], a

  ;Add the initial free value to the vertical UI by calling 
  ld hl, SAVE_SLOTS_FREE
  IF SCREEN_FLIP_H
  ld de, UIBuffer_Vertical+3
  ELSE
  ld de, UIBuffer_Vertical+2
  ENDC
  call UpdateByteInTilemap
  ret

StateVector_Backup:
  ret


  ;Finds a free slot, populates it with USED, increments USED, decrements FREE, and returns the slot index of that free slot, plus 1.
  ;Returns 0 if no free slots are found. However, this should not be called if FREE is zero
  ;Assumes there are no missing display numbers
  ;clobber hl,a,c
StateVector_FindAndFillFreeSlot:
  ld hl,STATEVECTOR_START_ADDR
  :ld a,[hli]
  inc a
  jr z, .found ;if a == FF
  ld a,l
  cp a,LOW(STATEVECTOR_LAST_ADDR)+1
  jp nz,:-
  ld c,$00
  ret   ;if l==lastelement+1,
  .found
  ld a, l
  sub a,LOW(STATEVECTOR_START_ADDR)
  ld c, a ; c = bank number to fill with data

  dec l ;since we used an hli, we want to decrement hl to get back to the index we're looking at
  ld a, [SAVE_SLOTS_USED]
  call StateVector_WriteByte
  
  ld hl, SAVE_SLOTS_FREE ;assumes FREE and USED variables are contiguous
  dec [hl] ;decrement number of free slots
  inc hl ; can be made into an 8-bit inc if we can guarantee FREE and USED don't cross a byte boundary
  inc [hl] ;increment number of used slots


  ret