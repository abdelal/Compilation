/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_VECTORREF:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    //CHECK 2 ARGUMENTS
    CMP(IMM(2), FPARG(1));
    JUMP_NE(L_WRONG_NUMBER_OF_ARGUMENT_VECTORREF);
    //CHECK ARG 1 IS VECTOR
    CMP(IMM(T_VECTOR), INDD(FPARG(2),0));
    JUMP_NE(L_not_vector_vectorref);
    //CHECK ARG 2 IS A INTEGER BETWEEN 0 TO VECTOR LENGTH-1.
    CMP(IMM(T_INTEGER), INDD(FPARG(3), 0));
    JUMP_NE(L_not_integer_vectorref);
    
    CMP(INDD(FPARG(3), 1), IMM(0));
    JUMP_LT(L_integer_out_of_bound_vectorref);
    
    CMP(INDD(FPARG(3), 1), INDD(FPARG(2), 1));
    JUMP_GE(L_integer_out_of_bound_vectorref);
    
    MOV(R1, FPARG(2)); // R1 = POINTER TO THE VECTOR.
    MOV(R2, INDD(FPARG(3),1)); //R2 =get POSITION.
    ADD(R2, IMM(2));
    MOV(R0, INDD(R1,R2));

    JUMP(L_exit_vectorref);
    
    L_WRONG_NUMBER_OF_ARGUMENT_VECTORREF:
        SHOW("ERROR WRONG NUMBER OF ARGUMENT IN VECTOR-ref (NOT 2)", FPARG(1));
        JUMP(L_error_lambda_args_count);
        
    L_not_vector_vectorref:
        SHOW("error in vector-ref argument 1 is not a vector", FPARG(2));
        JUMP(L_error_wrong_type_of_argument);
    
    L_not_integer_vectorref:
        SHOW("error in vector-ref argument 2 is not a integer", FPARG(3));
        JUMP(L_error_wrong_type_of_argument);
    
    L_integer_out_of_bound_vectorref:
        SHOW("error in vector-ref argument 2 is not between 0 - length-1", FPARG(3));
        JUMP(L_error_wrong_type_of_argument);
        
    L_exit_vectorref:
    POPALL;
    POP(FP);
    RETURN;