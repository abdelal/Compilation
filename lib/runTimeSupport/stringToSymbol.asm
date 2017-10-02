/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_STRING_TO_SYMBOL:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    
    CMP(IMM(T_STRING),INDD(FPARG(2), 0));
    JUMP_NE(L_not_string_string_to_symbol);

    MOV(R1, INDD(8, 0)); //the first node of the linked list.
    MOV(R2, FPARG(2)); //R2 = THE INPUT STRING.
    L_loop_linked_list_string_to_symbol:
        //CHECK IF THE NODE IS NOT_DEFINE.
        CMP(INDD(R1, 0), NOT_DEFINE);
        JUMP_EQ(L_dont_compare_strings_string_to_symbol);
        
        PUSH(R2);
        PUSH(INDD(R1, 0));
        PUSH(2);    //n
        PUSH(666); //SAVTA
        CALL(MY_IS_EQUAL_STRING);
        DROP(4);
        
        CMP(R0, IMM(SOB_TRUE));
        JUMP_EQ(L_return_string_addrs_string_to_symbol);
        
        
        L_dont_compare_strings_string_to_symbol:
        CMP(INDD(R1,1), T_NIL);
        JUMP_EQ(L_make_new_symbol_node_string_to_symbol);
        
        MOV(R1, INDD(R1, 1)); //R1 POINT TO THE NEXT NODE.
    JUMP(L_loop_linked_list_string_to_symbol);
        
    L_return_string_addrs_string_to_symbol:
        MOV(R3, INDD(R1, 0)); //R3 = POINTER TO THE STRING.
        PUSH(2);
        CALL(MALLOC);
        DROP(1);
        MOV(INDD(R0, 0), T_SYMBOL);
        MOV(INDD(R0, 1), R3);
        
    JUMP(L_exit_string_to_symbol);
    
    L_make_new_symbol_node_string_to_symbol:
    //MAKE THE NEW NODE.
        PUSH(2);
        CALL(MALLOC);
        DROP(1);
        MOV(INDD(R0, 0), R2);
        MOV(INDD(R0, 1), T_NIL);
        MOV(INDD(R1,1), R0);
        
    //MAKE THE NEW SYMBOL
        PUSH(2);
        CALL(MALLOC);
        DROP(1);
        MOV(INDD(R0, 0), T_SYMBOL);
        MOV(INDD(R0, 1), R2);
    
    JUMP(L_exit_string_to_symbol);
    
    L_not_string_string_to_symbol:
        SHOW("ERROR ARG IS NOT A string string->symbol",FPARG(2));
        JUMP(L_error_wrong_type_of_argument);
        
    L_exit_string_to_symbol:
    POPALL;
    POP(FP);
    RETURN;