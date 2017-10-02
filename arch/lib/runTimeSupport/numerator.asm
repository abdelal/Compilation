/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_NUMERATOR:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_FRACTION),INDD(FPARG(2), 0));
    JUMP_NE(L_not_fraction_numerator);
    PUSH(INDD(FPARG(2), 1));
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    JUMP(L_exit_numerator);
    L_not_fraction_numerator:
        CMP(IMM(T_INTEGER), INDD(FPARG(2), 0));
        JUMP_EQ(L_integer_numerator);
        SHOW("error in numerator? argument is not a fraction", R0);
        JUMP(L_error_wrong_type_of_argument);
    L_integer_numerator:
        PUSH(INDD(FPARG(2), 1));
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
    L_exit_numerator:
    POP(FP);
    RETURN;