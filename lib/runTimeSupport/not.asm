/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_NOT:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_BOOL),INDD(FPARG(2), 0));
    JUMP_NE(L_not_true);
    CMP(IMM(0), INDD(FPARG(2), 1));
    JUMP_NE(L_not_true);
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_not);
    L_not_true:
        MOV(R0, IMM(SOB_FALSE));
    L_exit_not:
    POP(FP);
    RETURN;