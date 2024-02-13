DEF TEST_CALLER_RAMBANK EQU 2
SECTION "Trampoline Caller SECTION",ROM0[$1000 + ($1000*TEST_CALLER_RAMBANK)]
Test_Caller_Storage::
    LOAD "Trampoline Caller LOAD", WRAMX [$D000]

    ;Loads DEADBEEF into arg0 and arg1 and banked calls Trampoline_test_callee
Trampoline_test_caller:

    ld de, $DEAD;pass parameters via stack
    push de ;push arg 0
    ld de, $BEEF
    push de ;push arg 1
    ld hl, Trampoline_test_callee ; addr of the callee
    ld e, TEST_CALLEE_RAMBANK ;bank which the callee is in
    call Trampoline_hl_e
    add sp, (2+2)
    ret 
    

    ENDL