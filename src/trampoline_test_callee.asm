INCLUDE "src/general.inc"

SECTION "Trampoline Callee SECTION",ROM0[$1000 + ($1000*TEST_CALLEE_RAMBANK)]
Test_Callee_Storage::
    LOAD "Trampoline Callee LOAD", WRAMX [$D000]


Trampoline_test_callee::
    ld hl, sp+6 ; Arguments on the stack for a trampoline function are located starting at +6
    ld a, [hli]
    ld b, a
    ld a, [hli]
    ld c, a

    ld a, [hli]
    ld l, [hl]
    ld h, a
ret
      

    ENDL