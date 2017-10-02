

  MY_APPLY:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    
    CMP(IMM(3), FPARG(1));
    JUMP_NE(L_wrong_number_of_arguments_my_apply);

    MOV(R3, INDD(FPARG(2),1)); //R3 = LENGTH
    MOV(R1, FPARG(3)); //R1 = POINTER TO THE LIST
    MOV(R2, IMM(0)); //R2 = i = 0
    CMP(IMM(0), R3); //0 ARGS ?
    JUMP_EQ(L_skip_args_push_my_apply);
    L_loop_i_lt_length_my_apply: //WHILE i < length
        //need to push the value of the car pair.
        PUSH(INDD(R1, 1)); //PUSHING CAR PAIR
        MOV(R1, INDD(R1, 2)); //R1 = CDR PAIR
        INCR(R2); //i++
        
    CMP(R2, R3); //i < length? 
    JUMP_LT(L_loop_i_lt_length_my_apply);
    
    //FLIPS ARGUMENTS
    MOV(R2, IMM(0));
    MOV(R4, SP);
    DECR(R4); //R4 = SP -1
    L_loop_i_lt_length_flip_arguments_my_apply:
        PUSH(STACK(R4));
        DECR(R4); //R4--
        INCR(R2); //i++
    CMP(R2, R3); // i < length ?
    JUMP_LT(L_loop_i_lt_length_flip_arguments_my_apply);
    
    
    PUSH(R3); //PUSH m = length.
    PUSH(INDD(FPARG(4), 1)); //PUSHING PROC ENV.
    CALLA(INDD(FPARG(4), 2)); //CALLING THE PROC THAT APPLY GOT.
    DROP(1); //DROP THE ENV
    POP(R3); //R3 = m
    DROP(R3); //DROPING m =? LENGTH ITEMS
    DROP(INDD(FPARG(2),1)); //DROP AGAIN M ARGUMENTS.
    JUMP(L_exit_apply);
    
    L_skip_args_push_my_apply:
    
    PUSH(R3); //PUSH m = length.
    PUSH(INDD(FPARG(4), 1)); //PUSHING PROC ENV.
    CALLA(INDD(FPARG(4), 2)); //CALLING THE PROC THAT APPLY GOT.
    DROP(1); //DROP THE ENV
    POP(R3); //R3 = m
    DROP(R3); //DROPING m =? LENGTH ITEMS
    
    JUMP(L_exit_apply);
    
    L_wrong_number_of_arguments_my_apply:
        SHOW("error in my_apply wrong number of args (not 3)", FPARG(1));
        JUMP(L_error_lambda_args_count);
        
    L_exit_apply:
    
    POPALL;
    POP(FP);
    RETURN

  MY_CAR:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_PAIR),INDD(FPARG(2), 0));
    JUMP_NE(L_not_pair_car);
    MOV(R0, INDD(FPARG(2), 1));
    JUMP(L_exit_pair_car);
    L_not_pair_car:
        SHOW("error in car argument is not a pair", R0);
        JUMP(L_error_wrong_type_of_argument);
    L_exit_pair_car:
    POP(FP);
    RETURN
  MY_CDR:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_PAIR),INDD(FPARG(2), 0));
    JUMP_NE(L_not_pair_cdr);
    MOV(R0, INDD(FPARG(2), 2));
    JUMP(L_exit_pair_cdr);
    L_not_pair_cdr:
        SHOW("error in cdr argument is not a pair", R0);
        JUMP(L_error_wrong_type_of_argument);
    L_exit_pair_cdr:
    POP(FP);
    RETURN

  MY_CHAR_TO_INTEGER:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_CHAR),INDD(FPARG(2), 0));
    JUMP_NE(L_not_char_char_to_integer);
    
    PUSH(INDD(FPARG(2),1));
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    
    JUMP(L_exit_char_to_integer);
    
    L_not_char_char_to_integer:
        SHOW("error arg is not char char->integer", FPARG(2));
        JUMP(L_error_wrong_type_of_argument);
        
    L_exit_char_to_integer:
    POP(FP);
    RETURN


  MY_CONS:
    PUSH(FP);
    MOV(FP, SP);
    //check the number of arguments  2.
    CMP(IMM(2), FPARG(1));
    JUMP_NE(L_wrong_number_of_arguments_cons);
    
    PUSH(FPARG(3));
    PUSH(FPARG(2));
    CALL(MAKE_SOB_PAIR);
    DROP(2);
    
    JUMP(L_exit_cons);
    
    L_wrong_number_of_arguments_cons:
        SHOW("error in cons wrong number of args (not 2)", FPARG(1));
        JUMP(L_error_lambda_args_count);
        
    L_exit_cons:
    POP(FP);
    RETURN

  MY_DENOMINATOR:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_FRACTION),INDD(FPARG(2), 0));
    JUMP_NE(L_not_fraction_denominator);
    PUSH(INDD(FPARG(2), 2));
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    JUMP(L_exit_denominator);
    L_not_fraction_denominator:
        CMP(IMM(T_INTEGER), INDD(FPARG(2), 0));
        JUMP_EQ(L_integer_denominator);
        SHOW("error in denominator? argument is not a fraction", R0);
        JUMP(L_error_wrong_type_of_argument);
    L_integer_denominator:
        PUSH(IMM(1));
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
    L_exit_denominator:
    POP(FP);
    RETURN


  MY_DIVISION:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    CMP(FPARG(1), IMM(1));
    JUMP_LT(L_error_wrong_number_of_arguments_division);
    
    MOV(R3, FPARG(1)); // R3 = i =n the number of arguments.
    DECR(R3);
    CMP(IMM(T_INTEGER), INDD(FPARG(2), 0));
    JUMP_NE(L_check_if_first_is_fraction_division);
    MOV(R1, INDD(FPARG(2), 1)); //R1 = N = I.
    MOV(R5, IMM(1)); //R5 = D = 1.
    JUMP(L_skip_fraction_check_division);
    
    L_check_if_first_is_fraction_division:
    CMP(IMM(T_FRACTION), INDD(FPARG(2), 0));
    JUMP_NE(L_error_wrong_type_of_argument);
    MOV(R1, INDD(FPARG(2), 1)); //R1 = N = FRAC.N
    MOV(R5, INDD(FPARG(2), 2)); //R5 = D = FRAC.D
    
    L_skip_fraction_check_division:
    CMP(FPARG(1), IMM(1));
    JUMP_EQ(L_return_1_divided_by_first_division);
    MOV(R2, IMM(3)); //counter -> 2ND arg = 3.
    MOV(R4, IMM(0)); //initiate.
    L_i_gt_zero_division: //while i > 0.
        CMP(IMM(T_INTEGER), INDD(FPARG(R2),0));
        JUMP_NE(L_not_integer_division);
        MOV(R6, INDD(FPARG(R2), 1));
        CMP(R6, IMM(0)); // negative?
        JUMP_GE(L_skip_negative_code_division);
        MOV(R1, -R1);
        MOV(R6, -R6);
        L_skip_negative_code_division:
        MOV(R1, R1); //R1 = N.
        MUL(R5, R6); //R5 = D*I.
        INCR(R2);
        DECR(R3);
    CMP(R3, IMM(0));
    JUMP_GT(L_i_gt_zero_division);
    JUMP(L_sum_is_fraction_division);
    
    L_return_1_divided_by_first_division:
        CMP(R1, IMM(0)); //NOMINATOR NEGATIVE?
        JUMP_GE(L_skip_1_arg_negative_code_division);
        MOV(R1, -R1);
        MOV(R5, -R5);
        L_skip_1_arg_negative_code_division:
        MOV(R3, R1); //R3 = TMP R1
        MOV(R1, R5); //R1 = DENOMINATOR
        MOV(R5, R3); //R5 = NOMINAOTR
    JUMP(L_sum_is_fraction_division);
    
    L_i_gt_zero_fraction_division:
        CMP(IMM(T_INTEGER), INDD(FPARG(R2),0));
        JUMP_NE(L_not_integer_division);
        MOV(R4, INDD(FPARG(R2),1)); //R4 = integer value
        CMP(R4, IMM(0)); //R4 NEGATIVE ?
        JUMP_GE(L_skip_integer_and_fractions_code_division);
        MOV(R4, -R4);
        MOV(R1, -R1);
        L_skip_integer_and_fractions_code_division:
        MUL(R5, R4); //R5 = D*I.
        MOV(R1, R1); //R1 = N.
        JUMP(L_continue_division);
        
        L_not_integer_division:
            CMP(IMM(T_FRACTION), INDD(FPARG(R2),0));
            JUMP_NE(L_not_number_division);

            MOV(R4, INDD(FPARG(R2), 1)); //R4 = N2.
            MOV(R6, INDD(FPARG(R2), 2)); //R6 = D2.
            CMP(R4, IMM(0)); //NOMINAOTR NEGATIVE? 
            JUMP_GE(L_skip_negative_fraction_integer_code_division);
            MOV(R4, -R4);
            MOV(R6, -R6);
            L_skip_negative_fraction_integer_code_division:
            MUL(R1, R6); //R1 = N1*D2
            MUL(R5, R4); //R5 = D1 * N2

        L_continue_division:
        INCR(R2);
        DECR(R3);
    CMP(R3, IMM(0));
    JUMP_GT(L_i_gt_zero_fraction_division);
    
    L_sum_is_fraction_division:
    //check if fraction can be reduce to integer.
    
    
    
    PUSH(R5);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    PUSH(R0);
    PUSH(R1);
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
    DIV(R1,R5);
    PUSH(R1);
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
        PUSH(R1);
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
        PUSH(R0);
        PUSH(IMM(2));
        PUSH(IMM(666));   //SAVTA
        CALL(MY_GCD);
        DROP(4);
        DIV(R1, INDD(R0,1));
        DIV(R5, INDD(R0,1));
        PUSH(IMM(3));
        CALL(MALLOC);
        DROP(1)
        MOV(INDD(R0, 0), IMM(T_FRACTION));
        MOV(INDD(R0, 1), R1); //NOMENATOR.
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
    RETURN

  MY_NUMERIC_EQUAL:
    PUSH(FP);
    MOV(FP, SP);
    CMP(FPARG(1), IMM(1));
    JUMP_LT(L_error_wrong_number_of_arguments_numeric_equal);
    JUMP_EQ(L_1_argument_make_true_numeric_equal);

    MOV(R3, FPARG(1)); //R3 = n (number of args).
    ADD(R3, IMM(2));
    MOV(R1, IMM(2));  //R1 = 2 first arg.
    
    CMP(IMM(T_INTEGER), INDD(FPARG(R1), 0));
    JUMP_NE(L_check_if_arg_is_fraction_numeric_equal);
    MOV(R2, INDD(FPARG(R1), 1)); //R2 = N = I
    MOV(R4, IMM(1)); //R4 = D = 1.
    JUMP(L_skip_if_arg_is_fraction_check_numeric_equal);
    
    L_check_if_arg_is_fraction_numeric_equal:
    CMP(IMM(T_FRACTION), INDD(FPARG(R1), 0));
    JUMP_NE(L_error_wrong_type_of_argument_numeric_equal);
    MOV(R2, INDD(FPARG(R1), 1)); //R2 = N = FRAC.N
    MOV(R4, INDD(FPARG(R1), 2)); //R4 = D = FRAC.D
    
    L_skip_if_arg_is_fraction_check_numeric_equal:
    INCR(R1);
    L_loop_i_lt_n_numeric_equal:    
        CMP(IMM(T_INTEGER), INDD(FPARG(R1), 0));
        JUMP_NE(L_check_if_arg_is_fraction_numeric_equal2);
        MOV(R5, INDD(FPARG(R1), 1)); //R5 = N = I
        MOV(R6, IMM(1)); //R6 = D = 1.
        JUMP(L_skip_if_arg_is_fraction_check_numeric_equal2);
        
        L_check_if_arg_is_fraction_numeric_equal2:
        CMP(IMM(T_FRACTION), INDD(FPARG(R1), 0));
        JUMP_NE(L_error_wrong_type_of_argument_numeric_equal);
        MOV(R5, INDD(FPARG(R1), 1)); //R5 = N = FRAC.N
        MOV(R6, INDD(FPARG(R1), 2)); //R6 = D = FRAC.D
        
        L_skip_if_arg_is_fraction_check_numeric_equal2:
        //compare the numbers.
        MOV(R7, R2); //R7 = N1
        MUL(R7, R6); //R7 = N1*D2
        MOV(R8, R5); // R8 = N2
        MUL(R8, R4); // R8 = N2*D1
        CMP(R7, R8); //ARGi == ARG(i+1) ?
        JUMP_NE(L_return_false_numeric_equal);
        
        MOV(R2, R5); //R2 = ARG(i+1).N
        MOV(R4, R6); //R4 = ARG(i+1).D
        INCR(R1);
    CMP(R1,R3);
    JUMP_LT(L_loop_i_lt_n_numeric_equal);
    
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_numeric_equal);
    
    L_return_false_numeric_equal:
        MOV(R0, IMM(SOB_FALSE));
        
    JUMP(L_exit_numeric_equal);

        
    L_error_wrong_number_of_arguments_numeric_equal:
        SHOW("error in = wrong number of arguments (lt 1)", FPARG(1));
        JUMP(L_error_lambda_args_count);
    
    L_error_wrong_type_of_argument_numeric_equal:
        SHOW("error in = wrong type of arguments ", INDD(FPARG(2),0));
        JUMP(L_error_wrong_type_of_argument);
        
    L_1_argument_make_true_numeric_equal:
        CMP(IMM(T_INTEGER), INDD(FPARG(2), 0));
        JUMP_NE(L_check_if_fraction_numeric_equal);
        JUMP(L_skip_fraction_check_numeric_equal);
        
        L_check_if_fraction_numeric_equal:
        CMP(IMM(T_FRACTION), INDD(FPARG(2), 0));
        JUMP_NE(L_error_wrong_type_of_argument_numeric_equal);
        
        L_skip_fraction_check_numeric_equal:
        MOV(R0, IMM(SOB_TRUE));
        
    L_exit_numeric_equal:
    POP(FP);
    RETURN

  MY_GCD:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    CMP(IMM(2), FPARG(1));
    JUMP_NE(L_wrong_number_of_arguments_gcd);
    MOV(R3, FPARG(2));
    CMP(T_INTEGER, INDD(R3,0));
    JUMP_NE(L_argument_is_not_integer_gcd);
    
    MOV(R1,FPARG(3));
    CMP(T_INTEGER, INDD(R1,0));
    JUMP_NE(L_argument_is_not_integer_gcd);
    
    MOV(R3, INDD(R3, 1));
    PUSH(R3);
    CALL(ABS);
    DROP(1);
    MOV(R3, R0);
    MOV(R1, INDD(R1, 1));
    PUSH(R1);
    CALL(ABS);
    DROP(1);
    MOV(R1, R0);
    
    L_loop_gcd:
        MOV(R2, R3);
        REM(R2,R1);
        CMP(IMM(0), R2);
        JUMP_EQ(L_exit_gcd);
        
        MOV(R3, R1);
        MOV(R1, R2);
        JUMP(L_loop_gcd);
    
    L_wrong_number_of_arguments_gcd:
        SHOW("GCD get wrong number of args (not 2)", FPARG(1));
        JUMP(L_error_lambda_args_count);
    
    L_argument_is_not_integer_gcd:
        SHOW("GCD argument is not integer", FPARG(2));
        JUMP(L_error_wrong_type_of_argument);
    L_exit_gcd:
        PUSH(R1);
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
    POPALL;
    POP(FP);
    RETURN


  MY_GREATER_THAN:
    PUSH(FP);
    MOV(FP, SP);
    CMP(FPARG(1), IMM(1));
    JUMP_LT(L_error_wrong_number_of_arguments_greater_than);
    JUMP_EQ(L_1_argument_make_true_greater_than);

    MOV(R3, FPARG(1)); //R3 = n (number of args).
    ADD(R3, IMM(2));
    MOV(R1, IMM(2));  //R1 = 2 first arg.
    
    CMP(IMM(T_INTEGER), INDD(FPARG(R1), 0));
    JUMP_NE(L_check_if_arg_is_fraction_greater_than);
    MOV(R2, INDD(FPARG(R1), 1)); //R2 = N = I
    MOV(R4, IMM(1)); //R4 = D = 1.
    JUMP(L_skip_if_arg_is_fraction_check_greater_than);
    
    L_check_if_arg_is_fraction_greater_than:
    CMP(IMM(T_FRACTION), INDD(FPARG(R1), 0));
    JUMP_NE(L_error_wrong_type_of_argument_greater_than);
    MOV(R2, INDD(FPARG(R1), 1)); //R2 = N = FRAC.N
    MOV(R4, INDD(FPARG(R1), 2)); //R4 = D = FRAC.D
    
    L_skip_if_arg_is_fraction_check_greater_than:
    INCR(R1);
    L_loop_i_lt_n_greater_than:    
        CMP(IMM(T_INTEGER), INDD(FPARG(R1), 0));
        JUMP_NE(L_check_if_arg_is_fraction_greater_than2);
        MOV(R5, INDD(FPARG(R1), 1)); //R5 = N = I
        MOV(R6, IMM(1)); //R6 = D = 1.
        JUMP(L_skip_if_arg_is_fraction_check_greater_than2);
        
        L_check_if_arg_is_fraction_greater_than2:
        CMP(IMM(T_FRACTION), INDD(FPARG(R1), 0));
        JUMP_NE(L_error_wrong_type_of_argument_greater_than);
        MOV(R5, INDD(FPARG(R1), 1)); //R5 = N = FRAC.N
        MOV(R6, INDD(FPARG(R1), 2)); //R6 = D = FRAC.D
        
        L_skip_if_arg_is_fraction_check_greater_than2:
        //compare the numbers.
        MOV(R7, R2); //R7 = N1
        MUL(R7, R6); //R7 = N1*D2
        MOV(R8, R5); // R8 = N2
        MUL(R8, R4); // R8 = N2*D1
        CMP(R7, R8); //ARGi > ARG(i+1) ?
        JUMP_LE(L_return_false_greater_than);
        
        MOV(R2, R5); //R2 = ARG(i+1).N
        MOV(R4, R6); //R4 = ARG(i+1).D
        INCR(R1);
    CMP(R1,R3);
    JUMP_LT(L_loop_i_lt_n_greater_than);
    
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_greater_than);
    
    L_return_false_greater_than:
        MOV(R0, IMM(SOB_FALSE));
        
    JUMP(L_exit_greater_than);

        
    L_error_wrong_number_of_arguments_greater_than:
        SHOW("error in > wrong number of arguments (lt 1)", FPARG(1));
        JUMP(L_error_lambda_args_count);
    
    L_error_wrong_type_of_argument_greater_than:
        SHOW("error in > wrong type of arguments ", INDD(FPARG(2),0));
        JUMP(L_error_wrong_type_of_argument);
        
    L_1_argument_make_true_greater_than:
        CMP(IMM(T_INTEGER), INDD(FPARG(2), 0));
        JUMP_NE(L_check_if_fraction_greater_than);
        JUMP(L_skip_fraction_check_greater_than);
        
        L_check_if_fraction_greater_than:
        CMP(IMM(T_FRACTION), INDD(FPARG(2), 0));
        JUMP_NE(L_error_wrong_type_of_argument_greater_than);
        
        L_skip_fraction_check_greater_than:
        MOV(R0, IMM(SOB_TRUE));
        
    L_exit_greater_than:
    POP(FP);
    RETURN

  MY_INTEGER_TO_CHAR:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_INTEGER),INDD(FPARG(2), 0));
    JUMP_NE(L_not_integer_integer_to_char);
    
    PUSH(INDD(FPARG(2),1));
    CALL(MAKE_SOB_CHAR);
    DROP(1);
    
    JUMP(L_exit_integer_to_char);
    
    L_not_integer_integer_to_char:
        SHOW("error arg is not integer integer->char", FPARG(2));
        JUMP(L_error_wrong_type_of_argument);
        
    L_exit_integer_to_char:
    POP(FP);
    RETURN


  MY_IS_BOOLEAN:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_BOOL),INDD(FPARG(2), 0));
    JUMP_NE(L_not_boolean);
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_boolean);
    L_not_boolean:
        MOV(R0, IMM(SOB_FALSE));
    L_exit_boolean:
    POP(FP);
    RETURN

  MY_IS_CHAR:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_CHAR),INDD(FPARG(2), 0));
    JUMP_NE(L_not_char);
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_char);
    L_not_char:
        MOV(R0, IMM(SOB_FALSE));
    L_exit_char:
    POP(FP);
    RETURN


  MY_IS_EQUAL:
    PUSH(FP);
    MOV(FP, SP);
    
    CMP(IMM(2), FPARG(1));
    JUMP_NE(L_error_wrong_number_of_arguments);
    
   
    CMP(INDD(FPARG(2), 0), INDD(FPARG(3), 0)); 
    JUMP_NE(L_return_false_is_equal);
    
    MOV(R3, FPARG(2)); // R3 = ARG1
    MOV(R1, FPARG(3)); // R1 = ARG2

    CMP(T_INTEGER, INDD(R3, 0));
    JUMP_EQ(L_compare_2nd_value_is_equal);
    CMP(T_CHAR, INDD(R3, 0));
    JUMP_EQ(L_compare_2nd_value_is_equal);
    CMP(T_SYMBOL, INDD(R3, 0));
    JUMP_EQ(L_compare_2nd_value_is_equal);
    
    CMP(T_FRACTION, INDD(R3, 0));
    JUMP_EQ(L_compare_2nd_and_3rd_is_equal);
    
    //compare by address.
    CMP(T_VOID, INDD(R3, 0));
    JUMP_EQ(L_compare_address_is_equal);
    CMP(T_NIL, INDD(R3, 0));
    JUMP_EQ(L_compare_address_is_equal);
    CMP(T_BOOL, INDD(R3, 0));
    JUMP_EQ(L_compare_address_is_equal);
    CMP(T_VECTOR, INDD(R3, 0));
    JUMP_EQ(L_compare_address_is_equal);
    CMP(T_STRING, INDD(R3, 0));
    JUMP_EQ(L_compare_address_is_equal);
    CMP(T_PAIR, INDD(R3, 0));
    JUMP_EQ(L_compare_address_is_equal);
    CMP(T_CLOSURE, INDD(R3, 0));
    JUMP_EQ(L_compare_address_is_equal);
    
    L_compare_2nd_and_3rd_is_equal:
        CMP(INDD(R3, 1), INDD(R1, 1));
        JUMP_NE(L_return_false_is_equal);
        CMP(INDD(R3, 2), INDD(R1, 2));
        JUMP_NE(L_return_false_is_equal);
        JUMP_EQ(L_return_true_is_equal);
        
    L_compare_2nd_value_is_equal:
        CMP(INDD(R3, 1), INDD(R1, 1));
        JUMP_EQ(L_return_true_is_equal);
        JUMP_NE(L_return_false_is_equal);
    
    L_compare_address_is_equal:
        CMP(R3, R1);
        JUMP_EQ(L_return_true_is_equal);
        JUMP_NE(L_return_false_is_equal);
        
    L_return_false_is_equal:
        MOV(R0, IMM(SOB_FALSE));
    JUMP(L_exit_is_equal);
    
    L_return_true_is_equal:
        MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_is_equal);

    L_error_wrong_number_of_arguments:
        SHOW("error in eq? wrong number of arguments", FPARG(1));
        JUMP(L_error_lambda_args_count);
        
    L_exit_is_equal:
    POP(FP);
    RETURN


  MY_IS_EQUAL_STRING:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    CMP(IMM(T_STRING),INDD(FPARG(2), 0));
    JUMP_NE(L_not_string_is_equal_string);
    
    CMP(IMM(T_STRING),INDD(FPARG(3), 0));
    JUMP_NE(L_not_string_is_equal_string);
    
    MOV(R3, FPARG(2)); //ADDR TO THE FIRST STRING.
    MOV(R1, FPARG(3)); //ADDR TO THE 2ND STRING.
    
    //STR.1.LENGTH == STR.2.LENGTH?
    CMP(INDD(R3, 1), INDD(R1, 1)); 
    JUMP_NE(L_return_false_is_equal_string);
    
    MOV(R2, INDD(R3, 1)); //R2 = N = LLENGTH.
    CMP(IMM(0), R2);
    JUMP_EQ(L_return_true_is_equal_string);
    ADD(R2, 2);
    MOV(R4, IMM(2)); //R4 = i = the first char in the string.
    L_loop_i_lt_length_is_equal_string:
        CMP(INDD(R3, R4), INDD(R1, R4));
        JUMP_NE(L_return_false_is_equal_string);
        INCR(R4);
    CMP(R4, R2);
    JUMP_LT(L_loop_i_lt_length_is_equal_string);
        
    L_return_true_is_equal_string:
        MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_is_equal_string);
    
    L_return_false_is_equal_string:
        MOV(R0, IMM(SOB_FALSE));
    JUMP(L_exit_is_equal_string);
    
    L_not_string_is_equal_string:
        SHOW("ERROR ARG IS NOT A string isEqualString",FPARG(2));
        SHOW("ERROR ARG IS NOT A string isEqualString",FPARG(3));
        JUMP(L_error_wrong_type_of_argument);
        
    L_exit_is_equal_string:
    POPALL;
    POP(FP);
    RETURN


  MY_IS_FRACTION:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_FRACTION),INDD(FPARG(2), 0));
    JUMP_NE(L_not_fraction);
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_fraction);
    L_not_fraction:
        MOV(R0, IMM(SOB_FALSE));
    L_exit_fraction:
    POP(FP);
    RETURN

  MY_IS_INTEGER:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_INTEGER),INDD(FPARG(2), 0));
    JUMP_NE(L_not_integer);
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_integer);
    L_not_integer:
        MOV(R0, IMM(SOB_FALSE));
    L_exit_integer:
    POP(FP);
    RETURN

  MY_IS_NULL:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_NIL),INDD(FPARG(2), 0));
    JUMP_NE(L_not_null);
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_null);
    L_not_null:
        MOV(R0, IMM(SOB_FALSE));
    L_exit_null:
    POP(FP);
    RETURN


  MY_IS_PAIR:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_PAIR),INDD(FPARG(2), 0));
    JUMP_NE(L_not_pair);
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_pair);
    L_not_pair:
        MOV(R0, IMM(SOB_FALSE));
    L_exit_pair:
    POP(FP);
    RETURN

  MY_IS_PROCEDURE:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_CLOSURE),INDD(FPARG(2), 0));
    JUMP_NE(L_not_procedure);
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_procedure);
    L_not_procedure:
        MOV(R0, IMM(SOB_FALSE));
    L_exit_procedure:
    POP(FP);
    RETURN


  MY_IS_STRING:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_STRING),INDD(FPARG(2), 0));
    JUMP_NE(L_not_string);
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_string);
    L_not_string:
        MOV(R0, IMM(SOB_FALSE));
    L_exit_string:
    POP(FP);
    RETURN

  MY_IS_SYMBOL:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_SYMBOL),INDD(FPARG(2), 0));
    JUMP_NE(L_not_symbol);
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_symbol);
    L_not_symbol:
        MOV(R0, IMM(SOB_FALSE));
    L_exit_symbol:
    POP(FP);
    RETURN


  MY_IS_VECTOR:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_VECTOR),INDD(FPARG(2), 0));
    JUMP_NE(L_not_vector);
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_vector);
    L_not_vector:
        MOV(R0, IMM(SOB_FALSE));
    L_exit_vector:
    POP(FP);
    RETURN

  MY_IS_ZERO:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_INTEGER), INDD(FPARG(2), 0));
    JUMP_EQ(L_skip_fraction_test_is_zero);
    
    CMP(IMM(T_FRACTION), INDD(FPARG(2), 0));
    JUMP_NE(L_wrong_type_of_argument_is_zero);
    
    L_skip_fraction_test_is_zero:
    
    CMP(IMM(0),INDD(FPARG(2), 1));
    JUMP_NE(L_not_zero);
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_zero);
    L_not_zero:
        MOV(R0, IMM(SOB_FALSE));
        JUMP(L_exit_zero);
        
    L_wrong_type_of_argument_is_zero:
        SHOW("Error in zero? parameter is not number", INDD(FPARG(2),0));
        JUMP(L_error_wrong_type_of_argument);
/*        MOV(R0, IMM(SOB_FALSE)); 
            maybe need to change to check number and not integer.
*/
    L_exit_zero:
    POP(FP);
    RETURN
  MY_LESS_THAN:
    PUSH(FP);
    MOV(FP, SP);
    CMP(FPARG(1), IMM(1));
    JUMP_LT(L_error_wrong_number_of_arguments_less_than);
    JUMP_EQ(L_1_argument_make_true_less_than);

    MOV(R3, FPARG(1)); //R3 = n (number of args).
    ADD(R3, IMM(2));
    MOV(R1, IMM(2));  //R1 = 2 first arg.
    
    CMP(IMM(T_INTEGER), INDD(FPARG(R1), 0));
    JUMP_NE(L_check_if_arg_is_fraction_less_than);
    MOV(R2, INDD(FPARG(R1), 1)); //R2 = N = I
    MOV(R4, IMM(1)); //R4 = D = 1.
    JUMP(L_skip_if_arg_is_fraction_check_less_than);
    
    L_check_if_arg_is_fraction_less_than:
    CMP(IMM(T_FRACTION), INDD(FPARG(R1), 0));
    JUMP_NE(L_error_wrong_type_of_argument_less_than);
    MOV(R2, INDD(FPARG(R1), 1)); //R2 = N = FRAC.N
    MOV(R4, INDD(FPARG(R1), 2)); //R4 = D = FRAC.D
    
    L_skip_if_arg_is_fraction_check_less_than:
    INCR(R1);
    L_loop_i_lt_n_less_than:    
        CMP(IMM(T_INTEGER), INDD(FPARG(R1), 0));
        JUMP_NE(L_check_if_arg_is_fraction_less_than2);
        MOV(R5, INDD(FPARG(R1), 1)); //R5 = N = I
        MOV(R6, IMM(1)); //R6 = D = 1.
        JUMP(L_skip_if_arg_is_fraction_check_less_than2);
        
        L_check_if_arg_is_fraction_less_than2:
        CMP(IMM(T_FRACTION), INDD(FPARG(R1), 0));
        JUMP_NE(L_error_wrong_type_of_argument_less_than);
        MOV(R5, INDD(FPARG(R1), 1)); //R5 = N = FRAC.N
        MOV(R6, INDD(FPARG(R1), 2)); //R6 = D = FRAC.D
        
        L_skip_if_arg_is_fraction_check_less_than2:
        //compare the numbers.
        MOV(R7, R2); //R7 = N1
        MUL(R7, R6); //R7 = N1*D2
        MOV(R8, R5); // R8 = N2
        MUL(R8, R4); // R8 = N2*D1
        CMP(R7, R8); //ARGi < ARG(i+1) ?
        JUMP_GE(L_return_false_less_than);
        
        MOV(R2, R5); //R2 = ARG(i+1).N
        MOV(R4, R6); //R4 = ARG(i+1).D
        INCR(R1);
    CMP(R1,R3);
    JUMP_LT(L_loop_i_lt_n_less_than);
    
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_less_than);
    
    L_return_false_less_than:
        MOV(R0, IMM(SOB_FALSE));
        
    JUMP(L_exit_less_than);

        
    L_error_wrong_number_of_arguments_less_than:
        SHOW("error in < wrong number of arguments (lt 1)", FPARG(1));
        JUMP(L_error_lambda_args_count);
    
    L_error_wrong_type_of_argument_less_than:
        SHOW("error in < wrong type of arguments ", INDD(FPARG(2),0));
        JUMP(L_error_wrong_type_of_argument);
        
    L_1_argument_make_true_less_than:
        CMP(IMM(T_INTEGER), INDD(FPARG(2), 0));
        JUMP_NE(L_check_if_fraction_less_than);
        JUMP(L_skip_fraction_check_less_than);
        
        L_check_if_fraction_less_than:
        CMP(IMM(T_FRACTION), INDD(FPARG(2), 0));
        JUMP_NE(L_error_wrong_type_of_argument_less_than);
        
        L_skip_fraction_check_less_than:
        MOV(R0, IMM(SOB_TRUE));
        
    L_exit_less_than:
    POP(FP);
    RETURN


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
    
    MOV(R2, INDD(FPARG(3),1)); //R2 = CHAR.
    JUMP(L_have_char_make_string);
    L_no_char_to_make_string:
        MOV(R2, 0);
    L_have_char_make_string:
    MOV(R3, INDD(FPARG(2),1)); //R3 = LENGHT
    MOV(R1, IMM(0)); //R1 = i.
    L_loop_make_string: //while i < length
        PUSH(R2);
        INCR(R1);
    CMP(R1, R3);
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
    RETURN


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
    
   // MOV(R2, INDD(FPARG(3),1)); //R2 = CHAR.
    MOV(R2, FPARG(3));
    JUMP(L_have_char_make_vector);
    L_no_char_to_make_vector:
        PUSH(IMM(0));
        CALL(MAKE_SOB_INTEGER);
        DROP(1)
        MOV(R2, R0);
    L_have_char_make_vector:
    MOV(R3, INDD(FPARG(2),1)); //R3 = LENGHT
    MOV(R1, IMM(0)); //R1 = i.
    L_loop_make_vector: //while i < length
        PUSH(R2);
        INCR(R1);
    CMP(R1, R3);
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
    RETURN

  MY_MINUS:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    CMP(IMM(0), FPARG(1));
    JUMP_EQ(L_return_minus_zero);
    
    CMP(IMM(1), FPARG(1));
    JUMP_EQ(L_return_minus_number);
    
    MOV(R3, FPARG(1)); // R3 = i =n the number of arguments.
    SUB(R3, IMM(1)); //n-1
    CMP(T_INTEGER, INDD(FPARG(2),0));
    JUMP_NE(L_1st_argument_fraction);
    MOV(R1, INDD(FPARG(2), 1)); // R1 = sum = first integer.
    JUMP(L_skip_fraction_minus);
    L_1st_argument_fraction:
    CMP(T_FRACTION, INDD(FPARG(2),0));
    JUMP_NE(L_not_number_minus);
    MOV(R1, INDD(FPARG(2),1)); // R1 = sum = first fraction NOMINATOR.
    MOV(R5, INDD(FPARG(2), 2)); // R5 = DNOMINATOR
    MOV(R2, IMM(3)); //counter -> (2ND)first arg = 3.
    MOV(R4, IMM(0)); //initiate.
    JUMP(L_i_gt_zero_fraction_minus);
    L_skip_fraction_minus:
    MOV(R2, IMM(3)); //counter -> (2ND)first arg = 3.
    MOV(R4, IMM(0)); //initiate.
    MOV(R5, IMM(1)); //initiate.
    L_i_gt_zero_minus: //while i > 0.
        CMP(IMM(T_INTEGER), INDD(FPARG(R2),0));
        JUMP_NE(L_not_integer_minus);
        SUB(R1, INDD(FPARG(R2),1));
        INCR(R2);
        DECR(R3);
    CMP(R3, IMM(0));
    JUMP_GT(L_i_gt_zero_minus);
    PUSH(R1);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    JUMP(L_exit_minus);
    
    L_i_gt_zero_fraction_minus:
        CMP(IMM(T_INTEGER), INDD(FPARG(R2),0));
        JUMP_NE(L_not_integer_minus);
        MOV(R4, INDD(FPARG(R2),1)); //R4 = integer value
        MUL(R4, R5); //R4 = I*D.
        SUB(R1, R4); // (I*D-N).
        MOV(R5, R5); // D.
        JUMP(L_continue_minus);
        
        L_not_integer_minus:
            CMP(IMM(T_FRACTION), INDD(FPARG(R2),0));
            JUMP_NE(L_not_number_minus);
            //here we have 2 fractions.
            //R1 = N1, R5 = D1.
            //INDD(FPARG(R2),1) = N2.
            //INDD(FPARG(R2),2) = D2.
            MOV(R4, INDD(FPARG(R2), 1)); //R4 = N2.
            MUL(R4, R5); //R4 = N2*D1.
            MUL(R1, INDD(FPARG(R2), 2)); //R1 = N1*D2.
            SUB(R1, R4); //R1 = N = N1*D2-N2*D1.
            MUL(R5, INDD(FPARG(R2), 2)); //D = D1*D2.
        L_continue_minus:
        INCR(R2);
        DECR(R3);
    CMP(R3, IMM(0));
    JUMP_GT(L_i_gt_zero_fraction_minus);
    
    L_sum_is_fraction_minus:

    
    
    
    PUSH(R5);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    PUSH(R0);
    PUSH(R1);
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
    DIV(R1,R5);
    PUSH(R1);
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
        PUSH(R1);
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
        //DIV(R1, INDD(R0,1));
        //DIV(R5, INDD(R0,1));
        DIV(R1, R0);
        DIV(R5, R0);
        PUSH(IMM(3));
        CALL(MALLOC);
        DROP(1)
        MOV(INDD(R0, 0), IMM(T_FRACTION));
        MOV(INDD(R0, 1), R1); //NOMENATOR.
        MOV(INDD(R0, 2), R5); //DENOMINATOR.
    JUMP(L_exit_minus);
    
    L_not_number_minus:
        SHOW("error in minus argument is not a number", R0);
        JUMP(L_error_wrong_type_of_argument);
    
    L_return_minus_zero:
        SHOW("MINUS CAN GET 0 ARGUMENTS", FPARG(1));
        JUMP(L_error_lambda_args_count);
        
    L_return_minus_number:
        MOV(R3,FPARG(2));
        CMP(T_INTEGER, INDD(R3, 0));
        JUMP_NE(L_1_argument_fraction_minus);
        
        MOV(R3, INDD(R3,1));
;         MOV(R1, IMM(0));
;         SUB(R1, R3);
        PUSH(-R3);
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
        JUMP(L_exit_minus);
        
        L_1_argument_fraction_minus:
        CMP(T_FRACTION, INDD(R3,0));
        JUMP_NE(L_not_number_minus);
        
        MOV(R1, INDD(R3,1));
        MOV(R3, INDD(R3, 2));
        PUSH(R3);
        PUSH(-R1);
        CALL(MAKE_SOB_FRACTION);
        DROP(2);
        JUMP(L_exit_minus);
        
    L_exit_minus:
    POPALL;
    POP(FP);
    RETURN

  MY_MULTIPY:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    CMP(IMM(0), FPARG(1));
    JUMP_EQ(L_return_multipy_one);
    
    MOV(R3, FPARG(1)); // R3 = i =n the number of arguments.
    MOV(R1, IMM(1)); // R1 = mul = 1.
    MOV(R2, IMM(2)); //counter -> first arg = 2.
    MOV(R4, IMM(0)); //initiate.
    MOV(R5, IMM(1)); //initiate.
    L_i_gt_zero_multipy: //while i > 0.
        CMP(IMM(T_INTEGER), INDD(FPARG(R2),0));
        JUMP_NE(L_not_integer_multipy);
        MUL(R1, INDD(FPARG(R2),1));
        INCR(R2);
        DECR(R3);
    CMP(R3, IMM(0));
    JUMP_GT(L_i_gt_zero_multipy);
    PUSH(R1);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    JUMP(L_exit_multipy);
    
    //from now on R1 = N is the NUMERATOR and R5 = D is the DENOMINATOR.
    L_i_gt_zero_fraction_multipy:
        CMP(IMM(T_INTEGER), INDD(FPARG(R2),0));
        JUMP_NE(L_not_integer_multipy);
        MOV(R4, INDD(FPARG(R2),1)); //R4 = integer value
        MUL(R1, R4); //R1 = N*I.
        MOV(R5, R5); // D= D.
        JUMP(L_continue_multipy);
        
        L_not_integer_multipy:
            CMP(IMM(T_FRACTION), INDD(FPARG(R2),0));
            JUMP_NE(L_not_number_multipy);

            MOV(R4, INDD(FPARG(R2), 1)); //R4 = N2.
          
            MUL(R1, R4); //R1 = N1 * N2
            MUL(R5, INDD(FPARG(R2), 2)); // R5 = D1*D2.
        L_continue_multipy:
        INCR(R2);
        DECR(R3);
    CMP(R3, IMM(0));
    JUMP_GT(L_i_gt_zero_fraction_multipy);
    
    L_sum_is_fraction_multipy:
    //check if fraction can be reduce to integer.
    
    
    
    PUSH(R5);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    PUSH(R0);
    PUSH(R1);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    PUSH(R0);
    PUSH(IMM(2));
    PUSH(IMM(666));   //SAVTA 
    CALL(MY_REMAINDER);
    DROP(4);
    CMP(INDD(R0, 1), IMM(0));
    JUMP_NE(L_make_sob_fraction_multipy);
    //if #t MAKE_SOB_INTEGER.
    DIV(R1,R5);
    PUSH(R1);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    JUMP(L_exit_multipy);
    //else make_sob_fraction.
    L_make_sob_fraction_multipy:
        //CHECKING IF WE CAN LETSAMTSEM.
        PUSH(R5);
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
        PUSH(R0);
        PUSH(R1);
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
        PUSH(R0);
        PUSH(IMM(2));
        PUSH(IMM(666));   //SAVTA
        CALL(MY_GCD);
        DROP(4);
        DIV(R1, INDD(R0,1));
        DIV(R5, INDD(R0,1));
        PUSH(IMM(3));
        CALL(MALLOC);
        DROP(1)
        MOV(INDD(R0, 0), IMM(T_FRACTION));
        MOV(INDD(R0, 1), R1); //NOMENATOR.
        MOV(INDD(R0, 2), R5); //DENOMINATOR.
    JUMP(L_exit_multipy);
    
    L_not_number_multipy:
        SHOW("error in multipy argument is not a number", R0);
        JUMP(L_error_wrong_type_of_argument);

    L_return_multipy_one:
        PUSH(IMM(1));
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
    L_exit_multipy:
    POPALL;
    POP(FP);
    RETURN


  MY_NOT:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_BOOL),INDD(FPARG(2), 0));
    JUMP_NE(L_not_true);
    CMP(IMM(0), INDD(FPARG(2), 1));
    JUMP_NE(L_not_true);
    MOV(R0, IMM(SOB_TRUE));
    JUMP(L_exit_not);
    L_not_true:
        MOV(R0, IMM(SOB_FALSE));
    L_exit_not:
    POP(FP);
    RETURN

  MY_NUMERATOR:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_FRACTION),INDD(FPARG(2), 0));
    JUMP_NE(L_not_fraction_numerator);
    PUSH(INDD(FPARG(2), 1));
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    JUMP(L_exit_numerator);
    L_not_fraction_numerator:
        CMP(IMM(T_INTEGER), INDD(FPARG(2), 0));
        JUMP_EQ(L_integer_numerator);
        SHOW("error in numerator? argument is not a fraction", R0);
        JUMP(L_error_wrong_type_of_argument);
    L_integer_numerator:
        PUSH(INDD(FPARG(2), 1));
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
    L_exit_numerator:
    POP(FP);
    RETURN

  MY_PLUS:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    CMP(IMM(0), FPARG(1));
    JUMP_EQ(L_return_plus_zero);
    
    MOV(R3, FPARG(1)); // R3 = i =n the number of arguments.
    MOV(R1, IMM(0)); // R1 = sum = 0.
    MOV(R2, IMM(2)); //counter -> first arg = 2.
    MOV(R4, IMM(0)); //initiate.
    MOV(R5, IMM(1)); //initiate.
    L_i_gt_zero_plus: //while i > 0.
        CMP(IMM(T_INTEGER), INDD(FPARG(R2),0));
        JUMP_NE(L_not_integer_plus);
        ADD(R1, INDD(FPARG(R2),1));
        INCR(R2);
        DECR(R3);
    CMP(R3, IMM(0));
    JUMP_GT(L_i_gt_zero_plus);
    PUSH(R1);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    JUMP(L_exit_plus);
    

    L_i_gt_zero_fraction_plus:
        CMP(IMM(T_INTEGER), INDD(FPARG(R2),0));
        JUMP_NE(L_not_integer_plus);
        MOV(R4, INDD(FPARG(R2),1)); //R4 = integer value
        MUL(R4, R5); //R4 = I*D.
        ADD(R1, R4); // (I*D+N).
        MOV(R5, R5); // D.
        JUMP(L_continue_plus);
        
        L_not_integer_plus:
            CMP(IMM(T_FRACTION), INDD(FPARG(R2),0));
            JUMP_NE(L_not_number_plus);
            //here we have 2 fractions.
            //R1 = N1, R5 = D1.
            //INDD(FPARG(R2),1) = N2.
            //INDD(FPARG(R2),2) = D2.
            MOV(R4, INDD(FPARG(R2), 1)); //R4 = N2.
            MUL(R4, R5); //R4 = N2*D1.
            MUL(R1, INDD(FPARG(R2), 2)); //R1 = N1*D2.
            ADD(R1, R4); //R1 = N = N1*D2+N2*D1.
            MUL(R5, INDD(FPARG(R2), 2)); //D = D1*D2.
        L_continue_plus:
        INCR(R2);
        DECR(R3);
    CMP(R3, IMM(0));
    JUMP_GT(L_i_gt_zero_fraction_plus);
    
    L_sum_is_fraction_plus:
    
    
    PUSH(R5);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    PUSH(R0);
    PUSH(R1);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    PUSH(R0);
    PUSH(IMM(2));
    PUSH(IMM(666));   //SAVTA 
    CALL(MY_REMAINDER);
    DROP(4);
    CMP(INDD(R0, 1), IMM(0));
    JUMP_NE(L_make_sob_fraction_plus);
    DIV(R1,R5);
    PUSH(R1);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    JUMP(L_exit_plus);
    //else make_sob_fraction.
    L_make_sob_fraction_plus:
        //CHECKING IF WE CAN LETSAMTSEM.
        PUSH(R5);
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
        PUSH(R0);
        PUSH(R1);
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
        PUSH(R0);
        PUSH(IMM(2));
        PUSH(IMM(666));   //SAVTA
        CALL(MY_GCD);
        DROP(4);
        DIV(R1, INDD(R0,1));
        DIV(R5, INDD(R0,1));
        PUSH(IMM(3));
        CALL(MALLOC);
        DROP(1)
        MOV(INDD(R0, 0), IMM(T_FRACTION));
        MOV(INDD(R0, 1), R1); //NOMENATOR.
        MOV(INDD(R0, 2), R5); //DENOMINATOR.
    JUMP(L_exit_plus);
    
    L_not_number_plus:
        SHOW("error in plus argument is not a number", R0);
        JUMP(L_error_wrong_type_of_argument);

    L_return_plus_zero:
        PUSH(IMM(0));
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
    L_exit_plus:
    POPALL;
    POP(FP);
    RETURN


  MY_REMAINDER:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    CMP(IMM(2), FPARG(1));
    JUMP_NE(L_error_exit_remainder);
    
    CMP(IMM(T_INTEGER), INDD(FPARG(2), 0));
    JUMP_NE(L_error_exit_remaindeR1);
    CMP(IMM(T_INTEGER), INDD(FPARG(3), 0));
    JUMP_NE(L_error_exit_remaindeR1);
    
    MOV(R3, INDD(FPARG(2),1)); //R3 = N.
    MOV(R1, INDD(FPARG(3),1)); //R1 = D.
    MOV(R2, R3);
    DIV(R2, R1); // R2 = DIV.
    MUL(R2, R1); //R2 = DIV*D.
    SUB(R3, R2); // R3 = REMAINDER = N - DIV*D.
    PUSH(R3);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    JUMP(L_exit_remainder);
    
    L_error_exit_remainder:
        SHOW("error in remainder wrong number of args (not 2)", R0);
        JUMP(L_error_lambda_args_count);
    
    L_error_exit_remaindeR1:
        SHOW("error in remainder arguments are not integers", R0);
        JUMP(L_error_wrong_type_of_argument);
        
    L_exit_remainder:
    POPALL;
    POP(FP);
    RETURN

  MY_SETCAR:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(2), FPARG(1));
    JUMP_NE(L_wrong_number_of_agruments_setcar);
    CMP(IMM(T_PAIR),INDD(FPARG(2), 0));
    JUMP_NE(L_not_pair_setcar);
    //need to set the car to the new value.
    
    MOV(INDD(FPARG(2), 1), FPARG(3)); //setting the car to be the value.
    
    JUMP(L_exit_setcar);
    L_not_pair_setcar:
        SHOW("error in car argument is not a pair", R0);
        JUMP(L_error_wrong_type_of_argument);
    L_wrong_number_of_agruments_setcar:
        SHOW("error wrong number of args (not 2) in set-car!" , FPARG(1));
        JUMP(L_error_lambda_args_count);
    L_exit_setcar:
    MOV(R0, IMM(SOB_VOID));
    POP(FP);
    RETURN
  MY_SETCDR:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(2), FPARG(1));
    JUMP_NE(L_wrong_number_of_agruments_setcdr);
    CMP(IMM(T_PAIR),INDD(FPARG(2), 0));
    JUMP_NE(L_not_pair_setcdr);
    
    MOV(INDD(FPARG(2), 2), FPARG(3)); 
    
    JUMP(L_exit_setcdr);
    L_not_pair_setcdr:
        SHOW("error in cdr argument is not a pair", R0);
        JUMP(L_error_wrong_type_of_argument);
    L_wrong_number_of_agruments_setcdr:
        SHOW("error wrong number of args (not 2) in set-cdr!" , FPARG(1));
        JUMP(L_error_lambda_args_count);
    L_exit_setcdr:
    MOV(R0, IMM(SOB_VOID));
    POP(FP);
    RETURN

  MY_STRING_LENGTH:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_STRING),INDD(FPARG(2), 0));
    JUMP_NE(L_not_string_length);
    PUSH(INDD(FPARG(2), 1));
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    JUMP(L_exit_string_length);
    L_not_string_length:
        SHOW("error in string-length argument is not a string", R0);
        JUMP(L_error_wrong_type_of_argument);
    L_exit_string_length:
    POP(FP);
    RETURN

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
    
    
    MOV(R3, FPARG(2)); // R3 = POINTER TO THE string.
    MOV(R1, INDD(FPARG(3),1)); //R1 =get POSITION.
    ADD(R1, IMM(2));

    PUSH(INDD(R3,R1));
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
    RETURN
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
    
    MOV(R3, FPARG(2)); // R3 = POINTER TO THE string.
    MOV(R1, INDD(FPARG(3),1)); //R1 =ASSIGNMENT POSITION.
    ADD(R1, IMM(2));
    
    MOV(R2, INDD(FPARG(4),1)); // R2 = value of the char
    MOV(INDD(R3,R1), R2);

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
    RETURN

  MY_STRING_TO_SYMBOL:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    
    CMP(IMM(T_STRING),INDD(FPARG(2), 0));
    JUMP_NE(L_not_string_string_to_symbol);

    MOV(R3, INDD(8, 0)); //the first node of the linked list.
    MOV(R1, FPARG(2)); //R1 = THE INPUT STRING.
    L_loop_linked_list_string_to_symbol:
        //CHECK IF THE NODE IS NOT_DEFINE.
        CMP(INDD(R3, 0), NOT_DEFINE);
        JUMP_EQ(L_dont_compare_strings_string_to_symbol);
        
        PUSH(R1);
        PUSH(INDD(R3, 0));
        PUSH(2);    //n
        PUSH(666); //SAVTA
        CALL(MY_IS_EQUAL_STRING);
        DROP(4);
        
        CMP(R0, IMM(SOB_TRUE));
        JUMP_EQ(L_return_string_addrs_string_to_symbol);
        
        
        L_dont_compare_strings_string_to_symbol:
        CMP(INDD(R3,1), T_NIL);
        JUMP_EQ(L_make_new_symbol_node_string_to_symbol);
        
        MOV(R3, INDD(R3, 1)); //R3 POINT TO THE NEXT NODE.
    JUMP(L_loop_linked_list_string_to_symbol);
        
    L_return_string_addrs_string_to_symbol:
        MOV(R2, INDD(R3, 0)); //R2 = POINTER TO THE STRING.
        PUSH(2);
        CALL(MALLOC);
        DROP(1);
        MOV(INDD(R0, 0), T_SYMBOL);
        MOV(INDD(R0, 1), R2);
        
    JUMP(L_exit_string_to_symbol);
    
    L_make_new_symbol_node_string_to_symbol:
  
        PUSH(2);
        CALL(MALLOC);
        DROP(1);
        MOV(INDD(R0, 0), R1);
        MOV(INDD(R0, 1), T_NIL);
        MOV(INDD(R3,1), R0);
 
        PUSH(2);
        CALL(MALLOC);
        DROP(1);
        MOV(INDD(R0, 0), T_SYMBOL);
        MOV(INDD(R0, 1), R1);
    
    JUMP(L_exit_string_to_symbol);
    
    L_not_string_string_to_symbol:
        SHOW("ERROR ARG IS NOT A string string->symbol",FPARG(2));
        JUMP(L_error_wrong_type_of_argument);
        
    L_exit_string_to_symbol:
    POPALL;
    POP(FP);
    RETURN


  MY_SYMBOL_TO_STRING:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_SYMBOL),INDD(FPARG(2), 0));
    JUMP_NE(L_not_symbol_symbol_to_string);

    MOV(R3, INDD(FPARG(2),1)); //R3= POINTER TO THE STRING.
    MOV(R1, INDD(R3,1)); //R1 = STRING LENGTH
    CMP(R1, IMM(0));
    JUMP_EQ(L_string_length_0_synbol_to_string);
    ADD(R1, IMM(2));
    MOV(R2, IMM(2));// R2= i = 2 first char.
    
    L_loop_symbol_to_string: //while i < length
        PUSH(INDD(R3,R2));
        INCR(R2);
    CMP(R2, R1);
    JUMP_LT(L_loop_symbol_to_string);
    L_string_length_0_synbol_to_string:
    PUSH(INDD(R3,1));
    CALL(MAKE_SOB_STRING);
    DROP(1);
    DROP(INDD(R3,1));

    
    JUMP(L_exit_symbol_to_string);
    
    L_not_symbol_symbol_to_string:
        SHOW("ERROR ARG IS NOT A SYMBOL symbol->string",FPARG(2));
        JUMP(L_error_wrong_type_of_argument);
        
    L_exit_symbol_to_string:
    POP(FP);
    RETURN

  MY_VECTOR:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    
    MOV(R3, FPARG(1)); // R3 = NUMBER OF ARGS.
    CMP(R3, IMM(0));
    JUMP_EQ(L_ZERO_ARGUMENTS_VECTOR);
    ADD(R3, IMM(2));
    MOV(R1, IMM(2)); //R1 = i = 2 (first arg).
    L_LOOP_VECTOR: //WHILE i < R3
        PUSH(FPARG(R1));
        INCR(R1);
    CMP(R1, R3);
    JUMP_LT(L_LOOP_VECTOR);

    L_ZERO_ARGUMENTS_VECTOR:
    PUSH(FPARG(1));
    CALL(MAKE_SOB_VECTOR);
    DROP(1);
    DROP(FPARG(1));
    

    POPALL;
    POP(FP);
    RETURN

  MY_VECTOR_LENGTH:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_VECTOR),INDD(FPARG(2), 0));
    JUMP_NE(L_not_vector_length);
    PUSH(INDD(FPARG(2), 1));
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    JUMP(L_exit_vector_length);
    L_not_vector_length:
        SHOW("error in vector-length argument is not a vector", R0);
        JUMP(L_error_wrong_type_of_argument);
    L_exit_vector_length:
    POP(FP);
    RETURN


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
    
    MOV(R3, FPARG(2)); // R3 = POINTER TO THE VECTOR.
    MOV(R1, INDD(FPARG(3),1)); //R1 =get POSITION.
    ADD(R1, IMM(2));
    MOV(R0, INDD(R3,R1));

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
    RETURN


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
    
    MOV(R3, FPARG(2)); // R3 = POINTER TO THE VECTOR.
    MOV(R1, INDD(FPARG(3),1)); //R1 =ASSIGNMENT POSITION.
    ADD(R1, IMM(2));
    MOV(R2, FPARG(4)); // R2 = POINTER TO THE NEW VALUE.
    MOV(INDD(R3,R1), R2);

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
    RETURN
