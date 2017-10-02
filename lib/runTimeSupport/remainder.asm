/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_REMAINDER:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    CMP(IMM(2), FPARG(1));
    JUMP_NE(L_error_exit_remainder);
    
    CMP(IMM(T_INTEGER), INDD(FPARG(2), 0));
    JUMP_NE(L_error_exit_remainder2);
    CMP(IMM(T_INTEGER), INDD(FPARG(3), 0));
    JUMP_NE(L_error_exit_remainder2);
    
    MOV(R1, INDD(FPARG(2),1)); //R1 = N.
    MOV(R2, INDD(FPARG(3),1)); //R2 = D.
    MOV(R3, R1);
    DIV(R3, R2); // R3 = DIV.
    MUL(R3, R2); //R3 = DIV*D.
    SUB(R1, R3); // R1 = REMAINDER = N - DIV*D.
    PUSH(R1);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    JUMP(L_exit_remainder);
    
    L_error_exit_remainder:
        SHOW("error in remainder wrong number of args (not 2)", R0);
        JUMP(L_error_lambda_args_count);
    
    L_error_exit_remainder2:
        SHOW("error in remainder arguments are not integers", R0);
        JUMP(L_error_wrong_type_of_argument);
        
    L_exit_remainder:
    POPALL;
    POP(FP);
    RETURN;