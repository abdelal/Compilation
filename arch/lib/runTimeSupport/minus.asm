/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_MINUS:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    CMP(IMM(0), FPARG(1));
    JUMP_EQ(L_return_minus_zero);
    
    CMP(IMM(1), FPARG(1));
    JUMP_EQ(L_return_minus_number);
    
    MOV(R1, FPARG(1)); // R1 = i =n the number of arguments.
    SUB(R1, IMM(1)); //n-1
    CMP(T_INTEGER, INDD(FPARG(2),0));
    JUMP_NE(L_1st_argument_fraction);
    MOV(R2, INDD(FPARG(2), 1)); // R2 = sum = first integer.
    JUMP(L_skip_fraction_minus);
    L_1st_argument_fraction:
    CMP(T_FRACTION, INDD(FPARG(2),0));
    JUMP_NE(L_not_number_minus);
    MOV(R2, INDD(FPARG(2),1)); // R2 = sum = first fraction NOMINATOR.
    MOV(R5, INDD(FPARG(2), 2)); // R5 = DNOMINATOR
    MOV(R3, IMM(3)); //counter -> (2ND)first arg = 3.
    MOV(R4, IMM(0)); //initiate.
    JUMP(L_i_gt_zero_fraction_minus);
    L_skip_fraction_minus:
    MOV(R3, IMM(3)); //counter -> (2ND)first arg = 3.
    MOV(R4, IMM(0)); //initiate.
    MOV(R5, IMM(1)); //initiate.
    L_i_gt_zero_minus: //while i > 0.
        CMP(IMM(T_INTEGER), INDD(FPARG(R3),0));
        JUMP_NE(L_not_integer_minus);
        SUB(R2, INDD(FPARG(R3),1));
        INCR(R3);
        DECR(R1);
    CMP(R1, IMM(0));
    JUMP_GT(L_i_gt_zero_minus);
    PUSH(R2);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    JUMP(L_exit_minus);
    
    //from now on R2 = N is the NUMERATOR and R5 = D is the DENOMINATOR.
    L_i_gt_zero_fraction_minus:
        CMP(IMM(T_INTEGER), INDD(FPARG(R3),0));
        JUMP_NE(L_not_integer_minus);
        MOV(R4, INDD(FPARG(R3),1)); //R4 = integer value
        MUL(R4, R5); //R4 = I*D.
        SUB(R2, R4); // (I*D-N).
        MOV(R5, R5); // D.
        JUMP(L_continue_minus);
        
        L_not_integer_minus:
            CMP(IMM(T_FRACTION), INDD(FPARG(R3),0));
            JUMP_NE(L_not_number_minus);
            //here we have 2 fractions.
            //R2 = N1, R5 = D1.
            //INDD(FPARG(R3),1) = N2.
            //INDD(FPARG(R3),2) = D2.
            MOV(R4, INDD(FPARG(R3), 1)); //R4 = N2.
            MUL(R4, R5); //R4 = N2*D1.
            MUL(R2, INDD(FPARG(R3), 2)); //R2 = N1*D2.
            SUB(R2, R4); //R2 = N = N1*D2-N2*D1.
            MUL(R5, INDD(FPARG(R3), 2)); //D = D1*D2.
        L_continue_minus:
        INCR(R3);
        DECR(R1);
    CMP(R1, IMM(0));
    JUMP_GT(L_i_gt_zero_fraction_minus);
    
    L_sum_is_fraction_minus:
    //check if fraction can be reduce to integer.
    
    
    
    PUSH(R5);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    PUSH(R0);
    PUSH(R2);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    PUSH(R0);
    PUSH(IMM(2));
    PUSH(IMM(666));   //SAVTA 
    CALL(MY_REMAINDER);
    DROP(4);
    CMP(INDD(R0, 1), IMM(0));
    JUMP_NE(L_make_sob_fraction_minus);
    //if #t MAKE_SOB_INTEGER.
    DIV(R2,R5);
    PUSH(R2);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    JUMP(L_exit_minus);
    //else make_sob_fraction.
    L_make_sob_fraction_minus:
        //CHECKING IF WE CAN LETSAMTSEM.
        PUSH(R5);
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
        PUSH(R0);
        PUSH(R2);
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
        PUSH(R0);
        PUSH(IMM(2));
        PUSH(IMM(666));   //SAVTA
        CALL(MY_GCD);
        DROP(4);
        PUSH(INDD(R0,1));
        CALL(ABS);
        DROP(1);
        //DIV(R2, INDD(R0,1));
        //DIV(R5, INDD(R0,1));
        DIV(R2, R0);
        DIV(R5, R0);
        PUSH(IMM(3));
        CALL(MALLOC);
        DROP(1)
        MOV(INDD(R0, 0), IMM(T_FRACTION));
        MOV(INDD(R0, 1), R2); //NOMENATOR.
        MOV(INDD(R0, 2), R5); //DENOMINATOR.
    JUMP(L_exit_minus);
    
    L_not_number_minus:
        SHOW("error in minus argument is not a number", R0);
        JUMP(L_error_wrong_type_of_argument);
    
    L_return_minus_zero:
        SHOW("MINUS CAN GET 0 ARGUMENTS", FPARG(1));
        JUMP(L_error_lambda_args_count);
        
    L_return_minus_number:
        MOV(R1,FPARG(2));
        CMP(T_INTEGER, INDD(R1, 0));
        JUMP_NE(L_1_argument_fraction_minus);
        
        MOV(R1, INDD(R1,1));
;         MOV(R2, IMM(0));
;         SUB(R2, R1);
        PUSH(-R1);
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
        JUMP(L_exit_minus);
        
        L_1_argument_fraction_minus:
        CMP(T_FRACTION, INDD(R1,0));
        JUMP_NE(L_not_number_minus);
        
        MOV(R2, INDD(R1,1));
        MOV(R1, INDD(R1, 2));
        PUSH(R1);
        PUSH(-R2);
        CALL(MAKE_SOB_FRACTION);
        DROP(2);
        JUMP(L_exit_minus);
        
    L_exit_minus:
    POPALL;
    POP(FP);
    RETURN;