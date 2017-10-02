/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_STRING_SET:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    //CHECK 3 ARGUMENTS
    CMP(IMM(3), FPARG(1));
    JUMP_NE(L_WRONG_NUMBER_OF_ARGUMENT_STRING_SET);
    //CHECK ARG 1 IS string
    CMP(IMM(T_STRING), INDD(FPARG(2),0));
    JUMP_NE(L_not_string_stringset);
    //CHECK ARG 2 IS A INTEGER BETWEEN 0 TO string LENGTH-1.
    CMP(IMM(T_INTEGER), INDD(FPARG(3), 0));
    JUMP_NE(L_not_integer_stringset);
    
    CMP(INDD(FPARG(3), 1), IMM(0));
    JUMP_LT(L_integer_out_of_bound_stringset);
    
    CMP(INDD(FPARG(3), 1), INDD(FPARG(2), 1));
    JUMP_GE(L_integer_out_of_bound_stringset);
    //need to check the set arg is char.
    CMP(IMM(T_CHAR), INDD(FPARG(4), 0));
    JUMP_NE(L_not_char_stringset);
    
    MOV(R1, FPARG(2)); // R1 = POINTER TO THE string.
    MOV(R2, INDD(FPARG(3),1)); //R2 =ASSIGNMENT POSITION.
    ADD(R2, IMM(2));
    
    MOV(R3, INDD(FPARG(4),1)); // R3 = value of the char
    MOV(INDD(R1,R2), R3);

    JUMP(L_exit_stringset);
    
    L_WRONG_NUMBER_OF_ARGUMENT_STRING_SET:
        SHOW("ERROR WRONG NUMBER OF ARGUMENT IN string-SET! (NOT 3)", FPARG(1));
        JUMP(L_error_lambda_args_count);
        
    L_not_string_stringset:
        SHOW("error in string-set! argument 1 is not a string", FPARG(2));
        JUMP(L_error_wrong_type_of_argument);
    
    L_not_integer_stringset:
        SHOW("error in string-set! argument 2 is not a integer", FPARG(3));
        JUMP(L_error_wrong_type_of_argument);
        
    L_not_char_stringset:
        SHOW("error in string-set! argument 3 is not a char", FPARG(4));
        JUMP(L_error_wrong_type_of_argument);
        
    L_integer_out_of_bound_stringset:
        SHOW("error in string-set! argument 2 is not between 0 - length-1", FPARG(3));
        JUMP(L_error_wrong_type_of_argument);
        
    L_exit_stringset:
    MOV(R0,IMM(SOB_VOID));
    POPALL;
    POP(FP);
    RETURN;