/* setcar.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */

  MY_SETCAR:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(2), FPARG(1));
    JUMP_NE(L_wrong_number_of_agruments_setcar);
    CMP(IMM(T_PAIR),INDD(FPARG(2), 0));
    JUMP_NE(L_not_pair_setcar);
    //need to set the car to the new value.
    
    MOV(INDD(FPARG(2), 1), FPARG(3)); //setting the car to be the value.
    
    JUMP(L_exit_setcar);
    L_not_pair_setcar:
        SHOW("error in car argument is not a pair", R0);
        JUMP(L_error_wrong_type_of_argument);
    L_wrong_number_of_agruments_setcar:
        SHOW("error wrong number of args (not 2) in set-car!" , FPARG(1));
        JUMP(L_error_lambda_args_count);
    L_exit_setcar:
    MOV(R0, IMM(SOB_VOID));
    POP(FP);
    RETURN;