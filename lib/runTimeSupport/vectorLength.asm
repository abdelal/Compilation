/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_VECTOR_LENGTH:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_VECTOR),INDD(FPARG(2), 0));
    JUMP_NE(L_not_vector_length);
    PUSH(INDD(FPARG(2), 1));
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    JUMP(L_exit_vector_length);
    L_not_vector_length:
        SHOW("error in vector-length argument is not a vector", R0);
        JUMP(L_error_wrong_type_of_argument);
    L_exit_vector_length:
    POP(FP);
    RETURN;