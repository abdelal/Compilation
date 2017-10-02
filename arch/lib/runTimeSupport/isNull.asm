/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_IS_NULL:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_NIL),INDD(FPARG(2), 0));
    JUMP_NE(L_not_null);
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_null);
    L_not_null:
        MOV(R0, IMM(SOB_FALSE));
    L_exit_null:
    POP(FP);
    RETURN;