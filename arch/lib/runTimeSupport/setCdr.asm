/* setcdr.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */

  MY_SETCDR:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(2), FPARG(1));
    JUMP_NE(L_wrong_number_of_agruments_setcdr);
    CMP(IMM(T_PAIR),INDD(FPARG(2), 0));
    JUMP_NE(L_not_pair_setcdr);
    //need to set the cdr to the new value.
    
    MOV(INDD(FPARG(2), 2), FPARG(3)); //setting the cdr to be the value.
    
    JUMP(L_exit_setcdr);
    L_not_pair_setcdr:
        SHOW("error in cdr argument is not a pair", R0);
        JUMP(L_error_wrong_type_of_argument);
    L_wrong_number_of_agruments_setcdr:
        SHOW("error wrong number of args (not 2) in set-cdr!" , FPARG(1));
        JUMP(L_error_lambda_args_count);
    L_exit_setcdr:
    MOV(R0, IMM(SOB_VOID));
    POP(FP);
    RETURN;