; This loader is a modified version of https://github.com/Lorenzooone/force-gb-mode

; There is still some unneccesary code here that doesn't make sense for launching a CGB payload, such as the SGB part, but it works well enough.

INCLUDE "src/hardware.inc"             ; system defines

SECTION "Init", ROM0[$0] ; This is where it starts executing once the program is copied to VRAM

    xor a
    ldh  [rLCDC],a
    jp  vram_start

    SECTION "Entrypoint", ROM0[$100]
    nop
    jp  launch
    
    SECTION "Launcher", ROM0[$150]
launch:
    xor a
    ldh  [rLCDC],a ; disable displaying anything. Disabling display when LY is less than $90 should cause problems on DMG, 
                  ; but it's always at 90 when DMG 
    
    ld hl,$C000 ;start of WRAM
    ld de,PayloadStorage ;start of payload stored in ROM
.copy_payload_wram0 ; for now, only copy C000-CFFF. Later, we may extend it to cover both banks
    ld  a,[de]
    inc de
    ld  [hl+],a
    bit 4,h
    jr z,.copy_payload_wram0

    ;counter for WRAM bank -- finishes when RAMbank becomes 7 (7 is a real RAM bank, but we don't want to have to ROMbank switch)
        ;Quits when d is $80 aka d's bit 7 is set
    ld de, $2000 ; de: source data -- each RAMbank is $1000 in size
    xor a
    ldh [rSVBK],a     ;This relies on SVBK reading zero if zero is written -- if it returns 1, you need to increment _after_ an inner loop
.copy_payload_banked_outer
 
    ld hl, $D000 ; hl: start of destination WRAM bank

    ldh a,[rSVBK]
    inc a
    ldh [rSVBK], a

.copy_payload_banked_inner
        ld a, [de]
        inc de
        ld [hli],a
        bit 4, h;terminate loading into this WRAM bank when H goes from D0 to E0 - 
        jp nz, .copy_payload_banked_inner
    ;When we finish copying the rambank, check if d is $80
    bit 7,d
    jr z,.copy_payload_banked_outer
    
    ld  hl,$8000 ; start of VRAM
    ld  de,vram_data ; source of VRAM code
.copy_code_to_vram ; copies the data in $0000 to $0FFF to $8000-87FF (VRAM)
    ld  a,[de]
    inc de
    ld  [hl+],a
    bit 3,h ;stop at $8800
    jr z, .copy_code_to_vram

    ld  de,GraphicsData ; source of VRAM code
.copy_graphics_to_vram ; copies the data in $0000 to $0FFF to $8800-8FFF (VRAM)
    ld  a,[de]
    inc de
    ld  [hl+],a
    bit 4,h ;stop at $9000 coming from $8800
    jr z, .copy_graphics_to_vram
    xor a ; In the original version, we jump to the VRAM equivalent of $0000, which xors a for SCY/X. Since we're dropping that part, we need to xor a here.
    jp  vram_start


    SECTION "Start",ROM0[$200]         ; start vector, followed by header data applied by rgbfix.exe
vram_data:
LOAD "VRAM LOAD", VRAM [$8000]
vram_start: ; VRAM code start
	ldh	[rSCX],a
	ldh	[rSCY],a
    ld  sp,$FFFE                       ; setup stack
    ld  a,$80
    ldh  [rBCPS],a
    ldh  [rOCPS],a
    ldh  [rOBP0],a
    ldh  [rOBP1],a
    ldh  [$FF4C],a                      ; set as GBC+DMG

.init_palette
    ld  b,$10 ; 2 palettes
    ld  hl,palette
.palette_loop_obj
    ld  a,[hl+]
    ldh  [rOCPD],a
    dec b
    jr  nz,.palette_loop_obj
    ld  b,$10
.palette_loop_bg
    ld  a,[hl+]
    ldh  [rBCPD],a
    dec b
    jr  nz,.palette_loop_bg
    ld  a,$FC
    ldh  [rBGP],a

.init_arrangements
    ld hl,emptyTile
    ld b,[hl]
    ld de,$0240
    ld hl,$9C00
.arrangements_loop
    ld  a,b
    ld [hl+],a
    dec de
    ld  a,d
    or  a,e
    jr  nz,.arrangements_loop

    xor a
    ld  hl,$FE00
    ld  c,$A0
.blank_oam_start
    ld  [hl+],a
    dec c
    jr nz,.blank_oam_start
    ld  hl,sgb_data
    call .sgbpackettransfer
    ld  a,$10                          ; read P15 - returns a, b, select, start
    ldh  [rP1],a

.copy_to_hram
    ld  hl,hram_code
.copy_to_hram_exec
    ld  c,$80
    ld  b,$7F
.copy_hram_loop
    ld  a,[hl+]
    ld  [$ff00+c],a
    inc c
    dec b
    jr  nz,.copy_hram_loop

.jump_to_hram
    jp  $FF80
    
.copy_to_hram2 ; Called after header is verified and user presses A:
            ; Blanks OAM, copies the start_comunication code into HRAM and executes it
    xor a
    ld  hl,$FE00
    ld  c,$A0
.blank_oam
    ld  [hl+],a
    dec c
    jr nz,.blank_oam
    ld  hl,.start_comunication
    jr  .copy_to_hram_exec

; From https://imanoleasgames.blogspot.com/2016/12/games-aside-1-super-game-boy.html
; Super Game Boy packet transfer
; @entrada  HL: Packet address
.sgbpackettransfer:
    push    bc
    xor     a
    ldh      [rP1],  a               ; Initial pulse (Start write). P14 = LOW and P15 = LOW
    ld      a,  P1F_4 | P1F_5
    ldh      [rP1],  a               ; P14 = HIGH and P15 = HIGH between pulses
    ld      b,  16                  ; Number of bytes per packet
.sgbpackettransfer_1:
    ld      e,  8                   ; Bits per byte
    ld      a,  [hl+]
    ld      d,  a                   ; Next byte of the packet
.sgbpackettransfer_2:
    bit     0,  d
    ld      a,  P1F_4               ; P14 = HIGH and P15 = LOW (Write 1)
    jr      nz, .sgbpackettransfer_3
    ld      a,  P1F_5               ; P14 = LOW and P15 = HIGH (Write 0)
.sgbpackettransfer_3:
    ldh      [rP1],  a               ; We send one bit
    ld      a,  P1F_4 | P1F_5
    ldh      [rP1],  a               ; P14 = HIGH and P15 = HIGH between pulses
    rr      d                       ; We rotate the register so that the next bit goes to position 0
    dec     e
    jr      nz, .sgbpackettransfer_2; We jump while there are bits left to be sent
    dec     b
    jr      nz, .sgbpackettransfer_1; We jump while there are bytes left to be sent
    ld      a,  P1F_5
    ldh      [rP1],  a               ; Bit 129, stop bit (Write 0)
    ld      a,  P1F_4 | P1F_5
    ldh      [rP1],  a               ; P14 = HIGH and P15 = HIGH between pulses
    pop     bc
    ret
     
.start_comunication
    ld  a,$02
    ldh  [$FF70],a ;WRAM Bank 2
    ld  a,$1
    ldh  [$FF4F],a ;VRAM bank 1
    ld  de,rHDMA1
    ld  hl,hdma_data
    ld  c,$5
.hdma_transfer1
    ld  a,[hl+]
    ld  [de],a
    inc de
    dec c
    jr  nz,.hdma_transfer1
    ld  de,rHDMA1
    ld  hl,hdma_data2
    ld  c,$5
.hdma_transfer2
    ld  a,[hl+]
    ld  [de],a
    inc de
    dec c
    jr  nz,.hdma_transfer2
    xor a
    ldh  [$FF4F],a
    ld  de,rHDMA1
    ld  hl,hdma_data
    ld  c,$5
.hdma_transfer3
    ld  a,[hl+]
    ld  [de],a
    inc de
    dec c
    jr  nz,.hdma_transfer3

    ld  a,$04
    ldh  [$FF4C],a                      ; set as DMG
    ld  a,$01
    ldh  [rOPRI],a                      ; set as DMG
    ld  b,$1
    ld  a,$11
    ldh  [$FF50],a                      ; set as DMG ; disable boot ROM
    ld  a,$91
    ldh  [rLCDC],a
;    ld a, $11 ; Signals to a game that it's in CGB mode; however, if this is build with CGB headers and the ROM inits as CGB, it's fine.
    jp  $C000; Jump to payload in WRAM 
    
hram_code:
    ;ld  a,(_VRAM+testSprite)/$100
    ;ld  [$FF46],a
    ld  a,LCDCF_ON | LCDCF_BG8000 | LCDCF_BG9C00 | LCDCF_OBJ8 | LCDCF_OBJON | LCDCF_WINOFF | LCDCF_BGON
    ldh  [rLCDC],a
.main_loop
.inner_loop
    xor a
    ldh  [rIF],a ; Clear outstanding interrupt flags
    inc a
    ldh  [rIE],a ; Enable VBlank interrupt
.wait_interrupt
    ldh  a,[rIF]
    and a,$1
    jr  z,.wait_interrupt ; Wait for Vblank interrupt flag
    
.check_logo
    ld  hl,$0104                       ; Start of the Nintendo logo
    ld  b,$30                          ; Nintendo logo's size
    ld  de,logoData
.check_logo_loop
    call $FF80+.wait_VRAM_accessible-hram_code
    ld  a,[de]
    cp  [hl]
    jr  nz,.failure
    inc de
    inc hl
    dec b
    jr  nz,.check_logo_loop
    
.check_header
    ld  b,$19
    ld  a,b
.check_header_loop
    add [hl]
    inc l
    dec b
    jr  nz,.check_header_loop
    add [hl]
    jr  nz,.failure

.success
    ld  a,$1
    call $FF80+.change_arrangements-hram_code ; call change_arrangements with a=1
    ldh  a,[rP1]                        ; read input
    cpl
    and a,PADF_A|PADF_B|PADF_START
    jr  z,.main_loop
    call $FF80+.wait_VRAM_accessible-hram_code
    xor a
    ldh  [rLCDC],a
    jp  vram_start.copy_to_hram2
    
.failure
    xor a
    call $FF80+.change_arrangements-hram_code ; Call change_arrangements with a=0
    jr  .main_loop
    
.wait_VRAM_accessible
    push hl
    ld  hl,rSTAT
.wait
    bit 1,[hl]                         ; Wait until Mode is 0/1 -- HBlank or VBlank
    jr  nz,.wait
    pop hl
    ret

.change_arrangements ; a = 0? source data=waitArrangements ; a = 1? confirmedArrangements
    and a,$1
    jr  z,.load_waiting_arrangements
    
    ld  de,confirmedArrangements
    jr  .chosen_arrangements
    
.load_waiting_arrangements
    ld  de,waitArrangements
    
.chosen_arrangements
    ld  b,$C0                          ; Arrangements' size
    ld  hl,$9C00+$C0
.change_arrangements_loop
    call $FF80+.wait_VRAM_accessible-hram_code
    ld   a,[de]
    add  a,$80
    ld   [hl+],a
    inc  de
    dec  b
    jr   nz,.change_arrangements_loop
    ret
    
.end_hram_code
ASSERT (.end_hram_code - hram_code) < ($7E - ($2 * $3)) ; calling functions consumes a bit of the available space

logoData:
INCBIN "src/loader/logo.bin"

emptyTile:
DB $67+$80
waitArrangements:   
INCBIN "src/loader/ui_arrangements_wait.bin"
confirmedArrangements:
INCBIN "src/loader/ui_arrangements_confirmed.bin"

palette:
INCBIN "src/loader/palette.bin"

hdma_data:
DB $D3,$00,$98,$A0,$12
hdma_data2:
DB $D3,$00,$80,$00,$40

sgb_data:
DB $71,$04,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; supresses further SGB commands

ENDL

SECTION "Graphics",ROM0[$800] ;the arrangement expects the tiledata to be located in VRAM start + $800, but since we load data from $200 up into VRAM,
GraphicsData:
    ;The graphics data ends up offset from the regular data by 200
LOAD "Graphics LOAD", VRAM[$8800]
INCBIN "src/loader/ui_graphics.bin"
ENDL