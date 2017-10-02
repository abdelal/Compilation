/* scheme/make_sob_integer.asm
 * Takes an integer, and place the corresponding Scheme object in R0
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 MAKE_SOB_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_FRACTION);
  MOV(INDD(R0, 1), FPARG(0));
  MOV(INDD(R0, 2), FPARG(1));
  POP(FP);
  RETURN;
