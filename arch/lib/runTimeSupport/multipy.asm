/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_MULTIPY:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    CMP(IMM(0), FPARG(1));
    JUMP_EQ(L_return_multipy_one);
    
    MOV(R1, FPARG(1)); // R1 = i =n the number of arguments.
    MOV(R2, IMM(1)); // R2 = mul = 1.
    MOV(R3, IMM(2)); //counter -> first arg = 2.
    MOV(R4, IMM(0)); //initiate.
    MOV(R5, IMM(1)); //initiate.
    L_i_gt_zero_multipy: //while i > 0.
        CMP(IMM(T_INTEGER), INDD(FPARG(R3),0));
        JUMP_NE(L_not_integer_multipy);
        MUL(R2, INDD(FPARG(R3),1));
        INCR(R3);
        DECR(R1);
    CMP(R1, IMM(0));
    JUMP_GT(L_i_gt_zero_multipy);
    PUSH(R2);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    JUMP(L_exit_multipy);
    
    //from now on R2 = N is the NUMERATOR and R5 = D is the DENOMINATOR.
    L_i_gt_zero_fraction_multipy:
        CMP(IMM(T_INTEGER), INDD(FPARG(R3),0));
        JUMP_NE(L_not_integer_multipy);
        MOV(R4, INDD(FPARG(R3),1)); //R4 = integer value
        MUL(R2, R4); //R2 = N*I.
        MOV(R5, R5); // D= D.
        JUMP(L_continue_multipy);
        
        L_not_integer_multipy:
            CMP(IMM(T_FRACTION), INDD(FPARG(R3),0));
            JUMP_NE(L_not_number_multipy);
            //here we have 2 fractions.
            //R2 = N1, R5 = D1.
            //INDD(FPARG(R3),1) = N2.
            //INDD(FPARG(R3),2) = D2.
            MOV(R4, INDD(FPARG(R3), 1)); //R4 = N2.
          
            MUL(R2, R4); //R2 = N1 * N2
            MUL(R5, INDD(FPARG(R3), 2)); // R5 = D1*D2.
        L_continue_multipy:
        INCR(R3);
        DECR(R1);
    CMP(R1, IMM(0));
    JUMP_GT(L_i_gt_zero_fraction_multipy);
    
    L_sum_is_fraction_multipy:
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
    JUMP_NE(L_make_sob_fraction_multipy);
    //if #t MAKE_SOB_INTEGER.
    DIV(R2,R5);
    PUSH(R2);
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
    RETURN;