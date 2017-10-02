/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_LESS_THAN:
    PUSH(FP);
    MOV(FP, SP);
    CMP(FPARG(1), IMM(1));
    JUMP_LT(L_error_wrong_number_of_arguments_less_than);
    JUMP_EQ(L_1_argument_make_true_less_than);

    MOV(R1, FPARG(1)); //R1 = n (number of args).
    ADD(R1, IMM(2));
    MOV(R2, IMM(2));  //R2 = 2 first arg.
    
    CMP(IMM(T_INTEGER), INDD(FPARG(R2), 0));
    JUMP_NE(L_check_if_arg_is_fraction_less_than);
    MOV(R3, INDD(FPARG(R2), 1)); //R3 = N = I
    MOV(R4, IMM(1)); //R4 = D = 1.
    JUMP(L_skip_if_arg_is_fraction_check_less_than);
    
    L_check_if_arg_is_fraction_less_than:
    CMP(IMM(T_FRACTION), INDD(FPARG(R2), 0));
    JUMP_NE(L_error_wrong_type_of_argument_less_than);
    MOV(R3, INDD(FPARG(R2), 1)); //R3 = N = FRAC.N
    MOV(R4, INDD(FPARG(R2), 2)); //R4 = D = FRAC.D
    
    L_skip_if_arg_is_fraction_check_less_than:
    INCR(R2);
    L_loop_i_lt_n_less_than:    
        CMP(IMM(T_INTEGER), INDD(FPARG(R2), 0));
        JUMP_NE(L_check_if_arg_is_fraction_less_than2);
        MOV(R5, INDD(FPARG(R2), 1)); //R5 = N = I
        MOV(R6, IMM(1)); //R6 = D = 1.
        JUMP(L_skip_if_arg_is_fraction_check_less_than2);
        
        L_check_if_arg_is_fraction_less_than2:
        CMP(IMM(T_FRACTION), INDD(FPARG(R2), 0));
        JUMP_NE(L_error_wrong_type_of_argument_less_than);
        MOV(R5, INDD(FPARG(R2), 1)); //R5 = N = FRAC.N
        MOV(R6, INDD(FPARG(R2), 2)); //R6 = D = FRAC.D
        
        L_skip_if_arg_is_fraction_check_less_than2:
        //compare the numbers.
        MOV(R7, R3); //R7 = N1
        MUL(R7, R6); //R7 = N1*D2
        MOV(R8, R5); // R8 = N2
        MUL(R8, R4); // R8 = N2*D1
        CMP(R7, R8); //ARGi < ARG(i+1) ?
        JUMP_GE(L_return_false_less_than);
        
        MOV(R3, R5); //R3 = ARG(i+1).N
        MOV(R4, R6); //R4 = ARG(i+1).D
        INCR(R2);
    CMP(R2,R1);
    JUMP_LT(L_loop_i_lt_n_less_than);
    
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_less_than);
    
    L_return_false_less_than:
        MOV(R0, IMM(SOB_FALSE));
        
    JUMP(L_exit_less_than);

        
    L_error_wrong_number_of_arguments_less_than:
        SHOW("error in < wrong number of arguments (lt 1)", FPARG(1));
        JUMP(L_error_lambda_args_count);
    
    L_error_wrong_type_of_argument_less_than:
        SHOW("error in < wrong type of arguments ", INDD(FPARG(2),0));
        JUMP(L_error_wrong_type_of_argument);
        
    L_1_argument_make_true_less_than:
        CMP(IMM(T_INTEGER), INDD(FPARG(2), 0));
        JUMP_NE(L_check_if_fraction_less_than);
        JUMP(L_skip_fraction_check_less_than);
        
        L_check_if_fraction_less_than:
        CMP(IMM(T_FRACTION), INDD(FPARG(2), 0));
        JUMP_NE(L_error_wrong_type_of_argument_less_than);
        
        L_skip_fraction_check_less_than:
        MOV(R0, IMM(SOB_TRUE));
        
    L_exit_less_than:
    POP(FP);
    RETURN;