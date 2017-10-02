/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_DENOMINATOR:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_FRACTION),INDD(FPARG(2), 0));
    JUMP_NE(L_not_fraction_denominator);
    PUSH(INDD(FPARG(2), 2));
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    JUMP(L_exit_denominator);
    L_not_fraction_denominator:
        CMP(IMM(T_INTEGER), INDD(FPARG(2), 0));
        JUMP_EQ(L_integer_denominator);
        SHOW("error in denominator? argument is not a fraction", R0);
        JUMP(L_error_wrong_type_of_argument);
    L_integer_denominator:
        PUSH(IMM(1));
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
    L_exit_denominator:
    POP(FP);
    RETURN;