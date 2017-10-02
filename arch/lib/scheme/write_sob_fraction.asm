/* scheme/write_sob_integer.asm
 * Take a pointer to a Scheme integer object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 WRITE_SOB_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  PUSHALL;
  MOV(R0, FPARG(0));
  MOV(R1, INDD(R0, 2));
  MOV(R0, INDD(R0, 1));
  PUSH(R0);
  CALL(WRITE_INTEGER);
  DROP(1);
  OUT(IMM(2), '/');
  PUSH(R1);
  CALL(WRITE_INTEGER);
  DROP(1);
  POPALL;
  POP(FP);
  RETURN;
