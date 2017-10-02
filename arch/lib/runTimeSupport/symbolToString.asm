/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_SYMBOL_TO_STRING:
    PUSH(FP);
    MOV(FP, SP);
    CMP(IMM(T_SYMBOL),INDD(FPARG(2), 0));
    JUMP_NE(L_not_symbol_symbol_to_string);

    MOV(R1, INDD(FPARG(2),1)); //R1= POINTER TO THE STRING.
    MOV(R2, INDD(R1,1)); //R2 = STRING LENGTH
    CMP(R2, IMM(0));
    JUMP_EQ(L_string_length_0_synbol_to_string);
    ADD(R2, IMM(2));
    MOV(R3, IMM(2));// R3= i = 2 first char.
    
    L_loop_symbol_to_string: //while i < length
        PUSH(INDD(R1,R3));
        INCR(R3);
    CMP(R3, R2);
    JUMP_LT(L_loop_symbol_to_string);
    L_string_length_0_synbol_to_string:
    PUSH(INDD(R1,1));
    CALL(MAKE_SOB_STRING);
    DROP(1);
    DROP(INDD(R1,1));

    
    JUMP(L_exit_symbol_to_string);
    
    L_not_symbol_symbol_to_string:
        SHOW("ERROR ARG IS NOT A SYMBOL symbol->string",FPARG(2));
        JUMP(L_error_wrong_type_of_argument);
        
    L_exit_symbol_to_string:
    POP(FP);
    RETURN;