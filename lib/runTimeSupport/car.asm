/* car.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */

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
    RETURN;