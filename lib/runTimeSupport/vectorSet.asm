/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_VECTORSET:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    //CHECK 3 ARGUMENTS
    CMP(IMM(3), FPARG(1));
    JUMP_NE(L_WRONG_NUMBER_OF_ARGUMENT_VECTORSET);
    //CHECK ARG 1 IS VECTOR
    CMP(IMM(T_VECTOR), INDD(FPARG(2),0));
    JUMP_NE(L_not_vector_vectorset);
    //CHECK ARG 2 IS A INTEGER BETWEEN 0 TO VECTOR LENGTH-1.
    CMP(IMM(T_INTEGER), INDD(FPARG(3), 0));
    JUMP_NE(L_not_integer_vectorset);
    
    CMP(INDD(FPARG(3), 1), IMM(0));
    JUMP_LT(L_integer_out_of_bound_vectorset);
    
    CMP(INDD(FPARG(3), 1), INDD(FPARG(2), 1));
    JUMP_GE(L_integer_out_of_bound_vectorset);
    
    MOV(R1, FPARG(2)); // R1 = POINTER TO THE VECTOR.
    MOV(R2, INDD(FPARG(3),1)); //R2 =ASSIGNMENT POSITION.
    ADD(R2, IMM(2));
    MOV(R3, FPARG(4)); // R3 = POINTER TO THE NEW VALUE.
    MOV(INDD(R1,R2), R3);

    JUMP(L_exit_vectorset);
    
    L_WRONG_NUMBER_OF_ARGUMENT_VECTORSET:
        SHOW("ERROR WRONG NUMBER OF ARGUMENT IN VECTOR-SET! (NOT 3)", FPARG(1));
        JUMP(L_error_lambda_args_count);
        
    L_not_vector_vectorset:
        SHOW("error in vector-set! argument 1 is not a vector", FPARG(2));
        JUMP(L_error_wrong_type_of_argument);
    
    L_not_integer_vectorset:
        SHOW("error in vector-set! argument 2 is not a integer", FPARG(3));
        JUMP(L_error_wrong_type_of_argument);
    
    L_integer_out_of_bound_vectorset:
        SHOW("error in vector-set! argument 2 is not between 0 - length-1", FPARG(3));
        JUMP(L_error_wrong_type_of_argument);
        
    L_exit_vectorset:
    MOV(R0,IMM(SOB_VOID));
    POPALL;
    POP(FP);
    RETURN;