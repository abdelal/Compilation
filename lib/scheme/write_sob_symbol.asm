/* scheme/write_sob_string.asm
 * Take a pointer to a Scheme string object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 WRITE_SOB_SYMBOL:
  //MOV(R1, STARG(0));   
  //MOV(R0, INDD(R1,1));
  MOV(R0, INDD(STARG(0),1));
  PUSH(R0);
  CALL(WRITE_SOB2);
  DROP(1);
  RETURN;

