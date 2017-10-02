/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_CHAR_TO_INTEGER:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_CHAR),INDD(FPARG(2), 0));
    JUMP_NE(L_not_char_char_to_integer);
    
    PUSH(INDD(FPARG(2),1));
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    
    JUMP(L_exit_char_to_integer);
    
    L_not_char_char_to_integer:
        SHOW("error arg is not char char->integer", FPARG(2));
        JUMP(L_error_wrong_type_of_argument);
        
    L_exit_char_to_integer:
    POP(FP);
    RETURN;