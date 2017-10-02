/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_IS_INTEGER:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_INTEGER),INDD(FPARG(2), 0));
    JUMP_NE(L_not_integer);
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_integer);
    L_not_integer:
        MOV(R0, IMM(SOB_FALSE));
    L_exit_integer:
    POP(FP);
    RETURN;