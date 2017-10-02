/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_IS_EQUAL_STRING:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    CMP(IMM(T_STRING),INDD(FPARG(2), 0));
    JUMP_NE(L_not_string_is_equal_string);
    
    CMP(IMM(T_STRING),INDD(FPARG(3), 0));
    JUMP_NE(L_not_string_is_equal_string);
    
    MOV(R1, FPARG(2)); //ADDR TO THE FIRST STRING.
    MOV(R2, FPARG(3)); //ADDR TO THE 2ND STRING.
    
    //STR.1.LENGTH == STR.2.LENGTH?
    CMP(INDD(R1, 1), INDD(R2, 1)); 
    JUMP_NE(L_return_false_is_equal_string);
    
    MOV(R3, INDD(R1, 1)); //R3 = N = LLENGTH.
    CMP(IMM(0), R3);
    JUMP_EQ(L_return_true_is_equal_string);
    ADD(R3, 2);
    MOV(R4, IMM(2)); //R4 = i = the first char in the string.
    L_loop_i_lt_length_is_equal_string:
        CMP(INDD(R1, R4), INDD(R2, R4));
        JUMP_NE(L_return_false_is_equal_string);
        INCR(R4);
    CMP(R4, R3);
    JUMP_LT(L_loop_i_lt_length_is_equal_string);
        
    L_return_true_is_equal_string:
        MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_is_equal_string);
    
    L_return_false_is_equal_string:
        MOV(R0, IMM(SOB_FALSE));
    JUMP(L_exit_is_equal_string);
    
    L_not_string_is_equal_string:
        SHOW("ERROR ARG IS NOT A string isEqualString",FPARG(2));
        SHOW("ERROR ARG IS NOT A string isEqualString",FPARG(3));
        JUMP(L_error_wrong_type_of_argument);
        
    L_exit_is_equal_string:
    POPALL;
    POP(FP);
    RETURN;