/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_STRING_REF:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    //CHECK 2 ARGUMENTS
    CMP(IMM(2), FPARG(1));
    JUMP_NE(L_WRONG_NUMBER_OF_ARGUMENT_STRING_REF);
    //CHECK ARG 1 IS string
    CMP(IMM(T_STRING), INDD(FPARG(2),0));
    JUMP_NE(L_not_string_stringref);
    //CHECK ARG 2 IS A INTEGER BETWEEN 0 TO string LENGTH-1.
    CMP(IMM(T_INTEGER), INDD(FPARG(3), 0));
    JUMP_NE(L_not_integer_stringref);
    
    CMP(INDD(FPARG(3), 1), IMM(0));
    JUMP_LT(L_integer_out_of_bound_stringref);
    
    CMP(INDD(FPARG(3), 1), INDD(FPARG(2), 1));
    JUMP_GE(L_integer_out_of_bound_stringref);
    
    
    MOV(R1, FPARG(2)); // R1 = POINTER TO THE string.
    MOV(R2, INDD(FPARG(3),1)); //R2 =get POSITION.
    ADD(R2, IMM(2));

    PUSH(INDD(R1,R2));
    CALL(MAKE_SOB_CHAR);
    DROP(1);

    JUMP(L_exit_stringref);
    
    L_WRONG_NUMBER_OF_ARGUMENT_STRING_REF:
        SHOW("ERROR WRONG NUMBER OF ARGUMENT IN string-ref (NOT 2)", FPARG(1));
        JUMP(L_error_lambda_args_count);
        
    L_not_string_stringref:
        SHOW("error in string-ref argument 1 is not a string", FPARG(2));
        JUMP(L_error_wrong_type_of_argument);
    
    L_not_integer_stringref:
        SHOW("error in string-ref argument 2 is not a integer", FPARG(3));
        JUMP(L_error_wrong_type_of_argument);
        
    L_integer_out_of_bound_stringref:
        SHOW("error in string-ref argument 2 is not between 0 - length-1", FPARG(3));
        JUMP(L_error_wrong_type_of_argument);
        
    L_exit_stringref:
    POPALL;
    POP(FP);
    RETURN;