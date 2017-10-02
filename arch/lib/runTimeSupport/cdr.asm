/* cdr.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_CDR:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_PAIR),INDD(FPARG(2), 0));
    JUMP_NE(L_not_pair_cdr);
    MOV(R0, INDD(FPARG(2), 2));
    JUMP(L_exit_pair_cdr);
    L_not_pair_cdr:
        SHOW("error in cdr argument is not a pair", R0);
        JUMP(L_error_wrong_type_of_argument);
    L_exit_pair_cdr:
    POP(FP);
    RETURN;