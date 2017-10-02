/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_CONS:
    PUSH(FP);
    MOV(FP, SP);
    //check the number of arguments  2.
    CMP(IMM(2), FPARG(1));
    JUMP_NE(L_wrong_number_of_arguments_cons);
    
    PUSH(FPARG(3));
    PUSH(FPARG(2));
    CALL(MAKE_SOB_PAIR);
    DROP(2);
    
    JUMP(L_exit_cons);
    
    L_wrong_number_of_arguments_cons:
        SHOW("error in cons wrong number of args (not 2)", FPARG(1));
        JUMP(L_error_lambda_args_count);
        
    L_exit_cons:
    POP(FP);
    RETURN;