SECTION "Trampoline Callee SECTION",ROM0[$1000 + ($1000*TEST_CALLEE_RAMBANK)]
Test_Callee_Storage::
    LOAD "Trampoline Callee LOAD", WRAMX [$D000]


Trampoline_test_callee:
    add sp,6 ;Add 6 so we can access the function arguments on the stack
    pop hl ;hl = arg1
    pop bc ;bc = arg0
  
  
    add sp,-(6+2+2) ;subtract (6 + 2*POPs) to get back to the top of stack
  
    ret
      

    ENDL