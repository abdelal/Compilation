/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_INTEGER_TO_CHAR:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_INTEGER),INDD(FPARG(2), 0));
    JUMP_NE(L_not_integer_integer_to_char);
    
    PUSH(INDD(FPARG(2),1));
    CALL(MAKE_SOB_CHAR);
    DROP(1);
    
    JUMP(L_exit_integer_to_char);
    
    L_not_integer_integer_to_char:
        SHOW("error arg is not integer integer->char", FPARG(2));
        JUMP(L_error_wrong_type_of_argument);
        
    L_exit_integer_to_char:
    POP(FP);
    RETURN;