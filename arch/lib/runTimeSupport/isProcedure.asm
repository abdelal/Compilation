/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_IS_PROCEDURE:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_CLOSURE),INDD(FPARG(2), 0));
    JUMP_NE(L_not_procedure);
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_procedure);
    L_not_procedure:
        MOV(R0, IMM(SOB_FALSE));
    L_exit_procedure:
    POP(FP);
    RETURN;