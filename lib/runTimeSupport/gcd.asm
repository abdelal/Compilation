/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_GCD:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    CMP(IMM(2), FPARG(1));
    JUMP_NE(L_wrong_number_of_arguments_gcd);
    MOV(R1, FPARG(2));
    CMP(T_INTEGER, INDD(R1,0));
    JUMP_NE(L_argument_is_not_integer_gcd);
    
    MOV(R2,FPARG(3));
    CMP(T_INTEGER, INDD(R2,0));
    JUMP_NE(L_argument_is_not_integer_gcd);
    
    MOV(R1, INDD(R1, 1));
    PUSH(R1);
    CALL(ABS);
    DROP(1);
    MOV(R1, R0);
    MOV(R2, INDD(R2, 1));
    PUSH(R2);
    CALL(ABS);
    DROP(1);
    MOV(R2, R0);
    
    L_loop_gcd:
        MOV(R3, R1);
        REM(R3,R2);
        CMP(IMM(0), R3);
        JUMP_EQ(L_exit_gcd);
        
        MOV(R1, R2);
        MOV(R2, R3);
        JUMP(L_loop_gcd);
    
    L_wrong_number_of_arguments_gcd:
        SHOW("GCD get wrong number of args (not 2)", FPARG(1));
        JUMP(L_error_lambda_args_count);
    
    L_argument_is_not_integer_gcd:
        SHOW("GCD argument is not integer", FPARG(2));
        JUMP(L_error_wrong_type_of_argument);
    L_exit_gcd:
        PUSH(R2);
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
    POPALL;
    POP(FP);
    RETURN;