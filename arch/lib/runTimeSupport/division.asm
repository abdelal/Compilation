/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_DIVISION:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    CMP(FPARG(1), IMM(1));
    JUMP_LT(L_error_wrong_number_of_arguments_division);
    
    MOV(R1, FPARG(1)); // R1 = i =n the number of arguments.
    DECR(R1);
    CMP(IMM(T_INTEGER), INDD(FPARG(2), 0));
    JUMP_NE(L_check_if_first_is_fraction_division);
    MOV(R2, INDD(FPARG(2), 1)); //R2 = N = I.
    MOV(R5, IMM(1)); //R5 = D = 1.
    JUMP(L_skip_fraction_check_division);
    
    L_check_if_first_is_fraction_division:
    CMP(IMM(T_FRACTION), INDD(FPARG(2), 0));
    JUMP_NE(L_error_wrong_type_of_argument);
    MOV(R2, INDD(FPARG(2), 1)); //R2 = N = FRAC.N
    MOV(R5, INDD(FPARG(2), 2)); //R5 = D = FRAC.D
    
    L_skip_fraction_check_division:
    CMP(FPARG(1), IMM(1));
    JUMP_EQ(L_return_1_divided_by_first_division);
    MOV(R3, IMM(3)); //counter -> 2ND arg = 3.
    MOV(R4, IMM(0)); //initiate.
    L_i_gt_zero_division: //while i > 0.
        CMP(IMM(T_INTEGER), INDD(FPARG(R3),0));
        JUMP_NE(L_not_integer_division);
        MOV(R6, INDD(FPARG(R3), 1));
        CMP(R6, IMM(0)); // negative?
        JUMP_GE(L_skip_negative_code_division);
        MOV(R2, -R2);
        MOV(R6, -R6);
        L_skip_negative_code_division:
        MOV(R2, R2); //R2 = N.
        MUL(R5, R6); //R5 = D*I.
        INCR(R3);
        DECR(R1);
    CMP(R1, IMM(0));
    JUMP_GT(L_i_gt_zero_division);
    JUMP(L_sum_is_fraction_division);
    
    L_return_1_divided_by_first_division:
        //CALCULATING 1/FIRST ARG.
        CMP(R2, IMM(0)); //NOMINATOR NEGATIVE?
        JUMP_GE(L_skip_1_arg_negative_code_division);
        MOV(R2, -R2);
        MOV(R5, -R5);
        L_skip_1_arg_negative_code_division:
        MOV(R1, R2); //R1 = TMP R2
        MOV(R2, R5); //R2 = DENOMINATOR
        MOV(R5, R1); //R5 = NOMINAOTR
    JUMP(L_sum_is_fraction_division);
    
    //from now on R2 = N is the NUMERATOR and R5 = D is the DENOMINATOR.
    L_i_gt_zero_fraction_division:
        CMP(IMM(T_INTEGER), INDD(FPARG(R3),0));
        JUMP_NE(L_not_integer_division);
        MOV(R4, INDD(FPARG(R3),1)); //R4 = integer value
        CMP(R4, IMM(0)); //R4 NEGATIVE ?
        JUMP_GE(L_skip_integer_and_fractions_code_division);
        MOV(R4, -R4);
        MOV(R2, -R2);
        L_skip_integer_and_fractions_code_division:
        MUL(R5, R4); //R5 = D*I.
        MOV(R2, R2); //R2 = N.
        JUMP(L_continue_division);
        
        L_not_integer_division:
            CMP(IMM(T_FRACTION), INDD(FPARG(R3),0));
            JUMP_NE(L_not_number_division);
            //here we have 2 fractions.
            //R2 = N1, R5 = D1.
            //INDD(FPARG(R3),1) = N2.
            //INDD(FPARG(R3),2) = D2.
            MOV(R4, INDD(FPARG(R3), 1)); //R4 = N2.
            MOV(R6, INDD(FPARG(R3), 2)); //R6 = D2.
            CMP(R4, IMM(0)); //NOMINAOTR NEGATIVE? 
            JUMP_GE(L_skip_negative_fraction_integer_code_division);
            MOV(R4, -R4);
            MOV(R6, -R6);
            L_skip_negative_fraction_integer_code_division:
            MUL(R2, R6); //R2 = N1*D2
            MUL(R5, R4); //R5 = D1 * N2

        L_continue_division:
        INCR(R3);
        DECR(R1);
    CMP(R1, IMM(0));
    JUMP_GT(L_i_gt_zero_fraction_division);
    
    L_sum_is_fraction_division:
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
    JUMP_NE(L_make_sob_fraction_division);
    //if #t MAKE_SOB_INTEGER.
    DIV(R2,R5);
    PUSH(R2);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    JUMP(L_exit_division);
    //else make_sob_fraction.
    L_make_sob_fraction_division:
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
        DIV(R2, INDD(R0,1));
        DIV(R5, INDD(R0,1));
        PUSH(IMM(3));
        CALL(MALLOC);
        DROP(1)
        MOV(INDD(R0, 0), IMM(T_FRACTION));
        MOV(INDD(R0, 1), R2); //NOMENATOR.
        MOV(INDD(R0, 2), R5); //DENOMINATOR.
    JUMP(L_exit_division);
    
    L_not_number_division:
        SHOW("error in division argument is not a number", R0);
        JUMP(L_error_wrong_type_of_argument);

    L_error_wrong_number_of_arguments_division:
        SHOW("error in division wrong number of args (less than 1)", FPARG(1));
        JUMP(L_error_lambda_args_count);
        
    L_exit_division:
    POPALL;
    POP(FP);
    RETURN;