/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_IS_EQUAL:
    PUSH(FP);
    MOV(FP, SP);
    
    CMP(IMM(2), FPARG(1));
    JUMP_NE(L_error_wrong_number_of_arguments);
    
    //Compare the type of the aguments
    CMP(INDD(FPARG(2), 0), INDD(FPARG(3), 0)); 
    JUMP_NE(L_return_false_is_equal);
    
    MOV(R1, FPARG(2)); // R1 = ARG1
    MOV(R2, FPARG(3)); // R2 = ARG2
    
    //always true.
   // CMP(T_VOID, INDD(R1, 0));
   // JUMP_EQ(L_return_true_is_equal);
    
  //  CMP(T_NIL, INDD(R1, 0));
   // JUMP_EQ(L_return_true_is_equal);
    
    //compare by value.
    CMP(T_INTEGER, INDD(R1, 0));
    JUMP_EQ(L_compare_2nd_value_is_equal);
    CMP(T_CHAR, INDD(R1, 0));
    JUMP_EQ(L_compare_2nd_value_is_equal);
    CMP(T_SYMBOL, INDD(R1, 0));
    JUMP_EQ(L_compare_2nd_value_is_equal);
    
    CMP(T_FRACTION, INDD(R1, 0));
    JUMP_EQ(L_compare_2nd_and_3rd_is_equal);
    
    //compare by address.
    CMP(T_VOID, INDD(R1, 0));
    JUMP_EQ(L_compare_address_is_equal);
    CMP(T_NIL, INDD(R1, 0));
    JUMP_EQ(L_compare_address_is_equal);
    CMP(T_BOOL, INDD(R1, 0));
    JUMP_EQ(L_compare_address_is_equal);
    CMP(T_VECTOR, INDD(R1, 0));
    JUMP_EQ(L_compare_address_is_equal);
    CMP(T_STRING, INDD(R1, 0));
    JUMP_EQ(L_compare_address_is_equal);
    CMP(T_PAIR, INDD(R1, 0));
    JUMP_EQ(L_compare_address_is_equal);
    CMP(T_CLOSURE, INDD(R1, 0));
    JUMP_EQ(L_compare_address_is_equal);
    
    L_compare_2nd_and_3rd_is_equal:
        CMP(INDD(R1, 1), INDD(R2, 1));
        JUMP_NE(L_return_false_is_equal);
        CMP(INDD(R1, 2), INDD(R2, 2));
        JUMP_NE(L_return_false_is_equal);
        JUMP_EQ(L_return_true_is_equal);
        
    L_compare_2nd_value_is_equal:
        CMP(INDD(R1, 1), INDD(R2, 1));
        JUMP_EQ(L_return_true_is_equal);
        JUMP_NE(L_return_false_is_equal);
    
    L_compare_address_is_equal:
        CMP(R1, R2);
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
    RETURN;