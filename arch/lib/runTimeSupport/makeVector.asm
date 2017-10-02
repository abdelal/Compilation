/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_MAKE_VECTOR:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    //CHECK 1/2 ARGUMENTS
    CMP(FPARG(1), IMM(1));
    JUMP_LT(L_WRONG_NUMBER_OF_ARGUMENT_MAKE_VECTOR);

    
    //CHECK ARG 1 is 0 or greater
    CMP(IMM(T_INTEGER), INDD(FPARG(2),0));
    JUMP_NE(L_not_integer_make_vector);
    
    CMP(INDD(FPARG(2),1), IMM(0));
    JUMP_LT(L_error_out_of_bound_make_vector);
    JUMP_EQ(L_make_empty_vector_make_vector);
    
    CMP(IMM(1), FPARG(1));
    JUMP_EQ(L_no_char_to_make_vector);
    
    //CHECK ARG 2 is a char.
   // CMP(IMM(T_CHAR), INDD(FPARG(3), 0));
    //JUMP_NE(L_not_char_make_vector);
    
   // MOV(R3, INDD(FPARG(3),1)); //R3 = CHAR.
    MOV(R3, FPARG(3));
    JUMP(L_have_char_make_vector);
    L_no_char_to_make_vector:
        PUSH(IMM(0));
        CALL(MAKE_SOB_INTEGER);
        DROP(1)
        MOV(R3, R0);
    L_have_char_make_vector:
    MOV(R1, INDD(FPARG(2),1)); //R1 = LENGHT
    MOV(R2, IMM(0)); //R2 = i.
    L_loop_make_vector: //while i < length
        PUSH(R3);
        INCR(R2);
    CMP(R2, R1);
    JUMP_LT(L_loop_make_vector);
    L_make_empty_vector_make_vector:
    PUSH(INDD(FPARG(2),1));
    CALL(MAKE_SOB_VECTOR);
    DROP(1);
    DROP(INDD(FPARG(2),1));
    
    
    

    JUMP(L_exit_make_vector);
    
    L_WRONG_NUMBER_OF_ARGUMENT_MAKE_VECTOR:
        SHOW("ERROR WRONG NUMBER OF ARGUMENT IN make_vector (NOT 1 or 2)", FPARG(1));
        JUMP(L_error_lambda_args_count);
    
    L_not_integer_make_vector:
        SHOW("error in make_vector argument 2 is not a integer", FPARG(2));
        JUMP(L_error_wrong_type_of_argument);
        
    L_not_char_make_vector:
        SHOW("error in make_vector argument 2 is not a char", FPARG(3));
        JUMP(L_error_wrong_type_of_argument);
        
    L_error_out_of_bound_make_vector:
        SHOW("error in make_vector argument 1 is not gt 0 ", FPARG(2));
        JUMP(L_error_wrong_type_of_argument);
        
    L_exit_make_vector:
    POPALL;
    POP(FP);
    RETURN;