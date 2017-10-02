/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_MAKE_STRING:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    //CHECK 1/2 ARGUMENTS
    CMP(FPARG(1), IMM(1));
    JUMP_LT(L_WRONG_NUMBER_OF_ARGUMENT_MAKE_STRING);

    
    //CHECK ARG 1 is 0 or greater
    CMP(IMM(T_INTEGER), INDD(FPARG(2),0));
    JUMP_NE(L_not_integer_make_string);
    
    CMP(INDD(FPARG(2),1), IMM(0));
    JUMP_LT(L_error_out_of_bound_make_string);
    JUMP_EQ(L_make_empty_string_make_string);
    
    CMP(IMM(1), FPARG(1));
    JUMP_EQ(L_no_char_to_make_string);
    
    //CHECK ARG 2 is a char.
    CMP(IMM(T_CHAR), INDD(FPARG(3), 0));
    JUMP_NE(L_not_char_make_string);
    
    MOV(R3, INDD(FPARG(3),1)); //R3 = CHAR.
    JUMP(L_have_char_make_string);
    L_no_char_to_make_string:
        MOV(R3, 0);
    L_have_char_make_string:
    MOV(R1, INDD(FPARG(2),1)); //R1 = LENGHT
    MOV(R2, IMM(0)); //R2 = i.
    L_loop_make_string: //while i < length
        PUSH(R3);
        INCR(R2);
    CMP(R2, R1);
    JUMP_LT(L_loop_make_string);
    L_make_empty_string_make_string:
    PUSH(INDD(FPARG(2),1));
    CALL(MAKE_SOB_STRING);
    DROP(1);
    DROP(INDD(FPARG(2),1));
    
    
    

    JUMP(L_exit_make_string);
    
    L_WRONG_NUMBER_OF_ARGUMENT_MAKE_STRING:
        SHOW("ERROR WRONG NUMBER OF ARGUMENT IN make_string (NOT 1 or 2)", FPARG(1));
        JUMP(L_error_lambda_args_count);
    
    L_not_integer_make_string:
        SHOW("error in make_string argument 2 is not a integer", FPARG(2));
        JUMP(L_error_wrong_type_of_argument);
        
    L_not_char_make_string:
        SHOW("error in make_string argument 2 is not a char", FPARG(3));
        JUMP(L_error_wrong_type_of_argument);
        
    L_error_out_of_bound_make_string:
        SHOW("error in make_string argument 1 is not gt 0 ", FPARG(2));
        JUMP(L_error_wrong_type_of_argument);
        
    L_exit_make_string:
    POPALL;
    POP(FP);
    RETURN;