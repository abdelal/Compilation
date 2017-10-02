/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */


  MY_VECTOR:
    PUSH(FP);
    MOV(FP, SP);
    PUSHALL;
    
    MOV(R1, FPARG(1)); // R1 = NUMBER OF ARGS.
    CMP(R1, IMM(0));
    JUMP_EQ(L_ZERO_ARGUMENTS_VECTOR);
    ADD(R1, IMM(2));
    MOV(R2, IMM(2)); //R2 = i = 2 (first arg).
    L_LOOP_VECTOR: //WHILE i < R1
        PUSH(FPARG(R2));
        INCR(R2);
    CMP(R2, R1);
    JUMP_LT(L_LOOP_VECTOR);

    L_ZERO_ARGUMENTS_VECTOR:
    PUSH(FPARG(1));
    CALL(MAKE_SOB_VECTOR);
    DROP(1);
    DROP(FPARG(1));
    

    POPALL;
    POP(FP);
    RETURN;