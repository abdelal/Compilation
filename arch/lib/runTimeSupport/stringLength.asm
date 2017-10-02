/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_STRING_LENGTH:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_STRING),INDD(FPARG(2), 0));
    JUMP_NE(L_not_string_length);
    PUSH(INDD(FPARG(2), 1));
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    JUMP(L_exit_string_length);
    L_not_string_length:
        SHOW("error in string-length argument is not a string", R0);
        JUMP(L_error_wrong_type_of_argument);
    L_exit_string_length:
    POP(FP);
    RETURN;