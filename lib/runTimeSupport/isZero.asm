/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_IS_ZERO:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_INTEGER), INDD(FPARG(2), 0));
    JUMP_EQ(L_skip_fraction_test_is_zero);
    
    CMP(IMM(T_FRACTION), INDD(FPARG(2), 0));
    JUMP_NE(L_wrong_type_of_argument_is_zero);
    
    L_skip_fraction_test_is_zero:
    
    CMP(IMM(0),INDD(FPARG(2), 1));
    JUMP_NE(L_not_zero);
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_zero);
    L_not_zero:
        MOV(R0, IMM(SOB_FALSE));
        JUMP(L_exit_zero);
        
    L_wrong_type_of_argument_is_zero:
        SHOW("Error in zero? parameter is not number", INDD(FPARG(2),0));
        JUMP(L_error_wrong_type_of_argument);
/*        MOV(R0, IMM(SOB_FALSE)); 
            maybe need to change to check number and not integer.
*/
    L_exit_zero:
    POP(FP);
    RETURN;