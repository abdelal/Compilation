/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_APPLY:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    
    //check the number of arguments  2.
    CMP(IMM(3), FPARG(1));
    JUMP_NE(L_wrong_number_of_arguments_my_apply);

    MOV(R1, INDD(FPARG(2),1)); //R1 = LENGTH
    MOV(R2, FPARG(3)); //R2 = POINTER TO THE LIST
    MOV(R3, IMM(0)); //R3 = i = 0
    CMP(IMM(0), R1); //0 ARGS ?
    JUMP_EQ(L_skip_args_push_my_apply);
    L_loop_i_lt_length_my_apply: //WHILE i < length
        //need to push the value of the car pair.
        PUSH(INDD(R2, 1)); //PUSHING CAR PAIR
        MOV(R2, INDD(R2, 2)); //R2 = CDR PAIR
        INCR(R3); //i++
        
    CMP(R3, R1); //i < length? 
    JUMP_LT(L_loop_i_lt_length_my_apply);
    
    //FLIPS ARGUMENTS
    MOV(R3, IMM(0));
    MOV(R4, SP);
    DECR(R4); //R4 = SP -1
    L_loop_i_lt_length_flip_arguments_my_apply:
        PUSH(STACK(R4));
        DECR(R4); //R4--
        INCR(R3); //i++
    CMP(R3, R1); // i < length ?
    JUMP_LT(L_loop_i_lt_length_flip_arguments_my_apply);
    
    
    PUSH(R1); //PUSH m = length.
    PUSH(INDD(FPARG(4), 1)); //PUSHING PROC ENV.
    CALLA(INDD(FPARG(4), 2)); //CALLING THE PROC THAT APPLY GOT.
    DROP(1); //DROP THE ENV
    POP(R1); //R1 = m
    DROP(R1); //DROPING m =? LENGTH ITEMS
    DROP(INDD(FPARG(2),1)); //DROP AGAIN M ARGUMENTS.
    JUMP(L_exit_apply);
    
    L_skip_args_push_my_apply:
    
    PUSH(R1); //PUSH m = length.
    PUSH(INDD(FPARG(4), 1)); //PUSHING PROC ENV.
    CALLA(INDD(FPARG(4), 2)); //CALLING THE PROC THAT APPLY GOT.
    DROP(1); //DROP THE ENV
    POP(R1); //R1 = m
    DROP(R1); //DROPING m =? LENGTH ITEMS
    
    JUMP(L_exit_apply);
    
    L_wrong_number_of_arguments_my_apply:
        SHOW("error in my_apply wrong number of args (not 3)", FPARG(1));
        JUMP(L_error_lambda_args_count);
        
    L_exit_apply:
    
    POPALL;
    POP(FP);
    RETURN;