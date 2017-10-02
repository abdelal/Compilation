
(define file->string
 (lambda (in-file)
  (let ((in-port (open-input-file in-file)))
   (letrec ((run (lambda ()
                    (let ((ch (read-char in-port)))
                     (if (eof-object? ch)
                         (begin (close-input-port in-port) '())
                         (cons ch (run)))))))
    (list->string
     (run))))))

(define nLine "\n")

(define faddr
 (lambda (fvar table)
    (if (equal? (cadadr (car table)) fvar)
      (caar table)
      (faddr fvar (cdr table)))))


; '( (key . val) ... (key . val) )
; where each elem could have more than one key.
; this func returns the elem with i-th key. (- i 1) because list-ref is zero based
(define assoc-i
  (lambda (key lst i)
    (cond ((null? lst) #f)
          ((and (< (- i 1) (length (car lst))) (equal? (list-ref (car lst) (- i 1)) key)) (car lst))
          (else (assoc-i key (cdr lst) i)))))

; returns the elem from assocList which it's 2 member is equal to key
(define assoc-2
  (lambda (key lst)
    (assoc-i key lst 2)))

; get the address of elem from table
(define get-const-addr-from-tableFromAssocList
  (lambda (elem assocList)
    (car (assoc-2 elem assocList))))

(define get-const-addr
  (lambda (pe const-table)
    (let ((e (make-tagged-const (cadr pe))))
          (cond ((symbol? (cadr pe))
                  `(,(car e) ,(get-const-addr-from-table (cadr e) const-table)))

                ((pair? (cadr pe))
                  `(,(car e) ,@(repr-tagged-const (cadr e) const-table)))
                
                ((vector? (cadr pe))
                  `(,(car e) ,(cadr e) ,@(repr-tagged-const (get-vector-items e) const-table)))
                
                (else e)))))

(define dropLastElem
  (lambda (lst)
    (reverse (cdr (reverse lst)))))

(define getLastElem
  (lambda (lst)
    (list-ref lst (- (length lst) 1))))


(define lableCounter
  (lambda (lable)
    (let ((counter -1))
      (lambda ()
        (set! counter (+ counter 1))
        (string-append lable (number->string counter))))))

;==========================lables===============================
(define ^ditLable (lableCounter "L_if3_dit"))
(define ^difLable (lableCounter "L_if3_dif"))
(define ^if3ELable (lableCounter "L_if3_Exit"))
(define ^printR0ELable (lableCounter "L_printR0_Exit"))
(define ^orELable (lableCounter "L_or_Exit"))
(define ^applicNotProcLable (lableCounter "L_applic_not_proc"))
(define ^applicNoErrLable (lableCounter "L_applic_no_err"))
(define ^tcApplicLoopLabel (lableCounter "L_tc_applic_loop"))
(define ^tcApplicEndLoopLabel (lableCounter "L_tc_applic_end_loop"))
(define ^lambdaCreateClosureLabel (lableCounter "L_lambda_create_closure"))
(define ^lambdaBodyCodeLabel (lableCounter "L_lambda_body_code"))
(define ^lambdaBodyCodeGenLabel (lableCounter "L_lambda_body_code_gen"))
(define ^lambdaExitLabel (lableCounter "L_lamdba_exit"))
(define ^lambdaLoopLabel (lableCounter "L_lamdba_loop"))
(define ^lambdaEndLoopLabel (lableCounter "L_lamdba_end_loop"))
(define ^lambdaOldEnvLabel (lableCounter "L_lambda_copy_old_env"))
(define ^lambdaCopyParamsLabel (lableCounter "L_lambda_copy_param"))
(define ^lambdaStackCorrectionLabel (lableCounter "L_lambda_stack_correction"))
(define ^lambdaEmptyRestLabel (lableCounter "L_lambda_emp_r"))
(define ^lambdaOptFixFPARGLabel (lableCounter "L_lambda_opt_fix_fparg"))
;===============================================================

(define generateSOBError
  (lambda ()
    (string-append
      "  JUMP(ErrExit);" nLine)))


;  ___________________
; | A        ______   |
; |         | B    |  |
; |         |______|  |
; |___________________|

(define ^code-gen-lambda
  (lambda (type)
    (lambda (sexpr const-table fvar-table nested)
      (let (;================== labels ========================
            (lambda_body_code (^lambdaBodyCodeLabel))
            (lambda_body_code_gen (^lambdaBodyCodeLabel))
            (label-exit (^lambdaExitLabel))
            (copy_old_env (^lambdaCopyParamsLabel))
            (copy_old_env_loop (^lambdaLoopLabel))
            (end_copy_old_env_loop (^lambdaEndLoopLabel))
            (copy_params_loop (^lambdaLoopLabel))
            (end_copy_params_loop (^lambdaEndLoopLabel))
            (copy_params_to_env (^lambdaCopyParamsLabel))
            (opt_args_list_loop (^lambdaLoopLabel))
            (opt_args_list_end_loop (^lambdaEndLoopLabel))
            ;===================================================
            (body (getLastElem sexpr))
            (extEnvSize (+ nested 1))
            (extEnvSizeStr (number->string (+ nested 1))))
        (string-append
         "  /* ========== lambda ========== */" nLine
         ; =============== A =====================
         copy_old_env":" nLine
         "  MOV(R1,FPARG(0)); //R1 = old env (before expansion)" nLine
         "  PUSH("extEnvSizeStr"); //allocate memory to extended the environment" nLine
         "  CALL(MALLOC);" nLine
         "  DROP(1);" nLine
         "  MOV(R2,R0); //allocated memory is now in R2" nLine
         "  CMP(FP,2);" nLine
         "  JUMP_LE("copy_params_to_env");" nLine
         nLine
         "  /* loop over old env in R1 and copy it to ext env in R2. R2[0] is reserved for the environment expansion */" nLine
         "  MOV(R3, 0);" nLine
         "  MOV(R4, 1); " nLine
         copy_old_env_loop":" nLine nLine

         "  CMP(R3, "(number->string nested)");" nLine
         "  JUMP_GE("end_copy_old_env_loop");" nLine nLine

         "  MOV(INDD(R2,R4), INDD(R1,R3));" nLine
         "  INCR(R3);" nLine
         "  INCR(R4);" nLine
         "  JUMP("copy_old_env_loop");" nLine nLine

         end_copy_old_env_loop":" nLine nLine

         copy_params_to_env":" nLine
         "  /* copy params from the stack to ext env (they turn from pvars to bvars)*/" nLine nLine

         "  MOV(R3,FPARG(1)); // R3 = n" nLine
         "  PUSH(R3); // allocate mem for R2[0]" nLine
         "  CALL(MALLOC);" nLine
         "  DROP(1);" nLine
         "  MOV(R1, R0);//allocated memory is now in R1 => R1 = R2[0][numOfParams-str]  " nLine
         "  MOV(R5,0);" nLine

         copy_params_loop":" nLine nLine 

         "  CMP(R5,R3);" nLine
         "  JUMP_GE("end_copy_params_loop");" nLine nLine

         "  MOV(R4,2);" nLine
         "  ADD(R4,R5); // r4 = i+2" nLine
         "  MOV(INDD(R1,R5),FPARG(R4)); // R1[i] = FPARG[i+2]" nLine
         "  INCR(R5);" nLine
         "  JUMP("copy_params_loop");" nLine nLine

         end_copy_params_loop":" nLine
         "  MOV(INDD(R2,0), R1); //R2[0] points to the args in env" nLine
         
         nLine
         (^lambdaCreateClosureLabel)":" nLine
         "  PUSH(3);" nLine
         "  CALL(MALLOC);" nLine
         "  DROP(1);" nLine
         "  MOV(INDD(R0,0), T_CLOSURE);" nLine
         "  MOV(INDD(R0,1), R2); //env" nLine
         "  MOV(INDD(R0,2), LABEL("lambda_body_code")); //body" nLine
         "  JUMP("label-exit");" nLine
         
         ; =============== B =====================
         nLine
         lambda_body_code":" nLine
         "  PUSH(FP);" nLine
         "  MOV(FP,SP);" nLine nLine
         (if (eq? type 'simple)
            (string-append "  /*  ========= lambda-simple ========= */"nLine)

            (let ((emp_rst (^lambdaEmptyRestLabel)))
             (string-append
             "  /*  ========= not lambda-simple ========= */" nLine
              (^lambdaStackCorrectionLabel)":" nLine
              
              (if (eq? type 'var)

                (string-append
                  "  MOV(R1, IMM(SOB_NIL));" nLine
                  "  MOV(R2, FPARG(1)); // R2 = i " nLine
                  "  CMP(R2,IMM(0)); // check if empty arg" nLine
                  "  JUMP_EQ("emp_rst");" nLine
                  "  INCR(R2);" nLine nLine

                  "/* loop over the args */" nLine
                  opt_args_list_loop ":" nLine nLine

                  "     PUSH(R1);" nLine
                  "     PUSH(FPARG(R2));" nLine
                  "     CALL(MAKE_SOB_PAIR);" nLine
                  "     DROP(2);" nLine
                  "     MOV(R1,R0);" nLine
                  "     MOV(FPARG(R2), R1);" nLine
                  "     DECR(R2);" nLine
                  "  CMP(R2,IMM(1));" nLine
                  "  JUMP_GT("opt_args_list_loop");" nLine
                  "  JUMP("lambda_body_code_gen");" nLine
                  "/* finish looping over the args */" nLine nLine
                
                  emp_rst ":" nLine
                  "  PUSH(FPARG(-2));" nLine
                  "  MOV(FPARG(-2), FPARG(-1)); // fix the place ret addr " nLine
                  "  MOV(FPARG(-1), FPARG(0)); // fix the place of env" nLine
                  "  MOV(R1, FPARG(1));" nLine
                  "  ADD(R1 , IMM(1)); // 1 becuse the null " nLine
                  "  MOV(FPARG(0), R1); // fix the place of env" nLine
                  "  MOV(FPARG(1), IMM(SOB_NIL)); // put nil in the bottom of the stack" nLine
                  "  MOV(FP, SP); //fix the fp to point to the start of the frame" nLine
                  )

                (let ((paramsSizeStr (number->string (length (cadr sexpr))))
                     (fix_fparg (^lambdaOptFixFPARGLabel)))
                  (string-append
              "     MOV(R1, IMM(SOB_NIL));" nLine
              "     MOV(R2, FPARG(1)); // R2 = i" nLine
              "     MOV(R3,IMM("paramsSizeStr"));" nLine
              
              "     CMP(R2,R3);" nLine
              "     JUMP_EQ("emp_rst"); // empty rest" nLine
              
              "     CMP(R2,R3);" nLine
              "     JUMP_LT(ErrExit);" nLine

              "     ADD(R2,IMM(1));" nLine
              "     ADD(R3,IMM(1));" nLine nLine

              "/* loop over the args */" nLine
              opt_args_list_loop ":" nLine nLine

              "     PUSH(R1);" nLine
              "     PUSH(FPARG(R2));" nLine
              "     CALL(MAKE_SOB_PAIR);" nLine
              "     DROP(2);" nLine
              "     MOV(R1,R0);" nLine
              "     MOV(FPARG(R2), R1);" nLine
              "     DECR(R2);" nLine
              "  CMP(R2,R3);" nLine
              "  JUMP_GT("opt_args_list_loop");" nLine
              "  JUMP("lambda_body_code_gen");" nLine


              nLine
              "/* empty rest */" nLine
              emp_rst ":" nLine nLine
              "  PUSH(FPARG(-2)); // fix the pos of old-fp" nLine
              "  MOV(FPARG(-2), FPARG(-1)); // fix the pos of ret-adddr" nLine
              "  MOV(FPARG(-1), FPARG(0)); // fix the pos of enf" nLine
              "  MOV(R1, FPARG(1));" nLine
              "  ADD(R1 , IMM(1)); // adding 1 for the nil" nLine
              "  MOV(FPARG(0), R1); // fix the pos of new n" nLine 

              "  MOV(R4, FPARG(1));" nLine
              "  INCR(R4); // R4 = n+1" nLine
              "  MOV(R1, IMM(1));" nLine
              "  MOV(R5, R1);" nLine 
              "  ADD(R5, IMM(1));" nLine nLine

              "//while i < n+2" nLine
              fix_fparg":" nLine nLine
              "     MOV(FPARG(R1), FPARG(R5)); // FPARG[i] = FPARG[i+1] "nLine
              "     INCR(R1);" nLine
              "     INCR(R5);" nLine
              "  CMP(R1, R4);" nLine
              "  JUMP_LT("fix_fparg");" nLine


              "  MOV(FPARG(R4), IMM(SOB_NIL)); // put mill in the butoom of the stack" nLine
              "  MOV(FP, SP); // update the fp to point on the start of the frame" nLine
          ))

                ))))

         #;(if (or (eq? type 'opt) (eq? type 'var))
           (let* ((emp_rst (^lambdaEmptyRestLabel))
                  (params (cadr sexpr))
                  (paramsSizeStr (number->string (length params))))
             (string-append
             "  /*  ========= not lambda-simple ========= */" nLine
              (^lambdaStackCorrectionLabel)":" nLine
              "  MOV(R2, FPARG(1));" nLine
              "  ADD(R2, IMM(1));" nLine
              "  MOV(R1, FPARG(R2));" nLine
              "  /* Creating a list of optional arguments */" nLine
              "  MOV(R6, FPARG(1)); //posision of the first arg" nLine
              "  MOV(R7,"paramsSizeStr"); // posision of the last arg" nLine
              "  ADD(R7,IMM(1));" nLine nLine

              opt_args_list_loop":" nLine nLine

              "  CMP(R6,R7); // check if r6 reached the last arg" nLine
              "  JUMP_LE("opt_args_list_end_loop");" nLine nLine

              "  PUSH(R1); // list of opt args till now" nLine
              "  MOV(R2, FPARG(R6)); // r2 = args[r6]" nLine
              "  PUSH(R2);" nLine
              "  CALL(MAKE_SOB_PAIR);" nLine
              "  DROP(2);" nLine
              "  MOV(R1,R0); // r1 = (prev(r1) , r2)" nLine nLine

              "  DECR(R6);" nLine
              "  JUMP("opt_args_list_loop");" nLine nLine

              opt_args_list_end_loop":" nLine
              nLine
              "  MOV(R2, SP);" nLine
              "  SUB(R2, IMM(5));" nLine
              "  SUB(R2,IMM("paramsSizeStr")) // r2 = position before param0 on stack;" nLine
              "  MOV(STACK(R2), R1); // put respectly opt list in the botton" nLine
              "  /* ======= done with stack-correction ======= " nLine 
              nLine)) 

            (string-append "  /*  ========= lambda-simple ========= */"nLine)
           )

 
          (string-append
            "  /* ========== code-gen of body ========== */" nLine
            lambda_body_code_gen ":" nLine
            (code-gen body const-table fvar-table extEnvSize)
            " /* ======= done code-gen for body ======= */" nLine)


         nLine
         "  POP(FP);" nLine
         "  RETURN;" nLine
         label-exit":" nLine)))))

 (define applic-matual-code
  (lambda (args nested const-table fvar-table proc openMsg applicNotProcLable)
    (let ((numOfArgs (number->string (length args)))
      (argsCode (apply string-append 
                            (map
                              (lambda (arg)
                                (string-append
                                  (code-gen arg const-table fvar-table nested)
                                  "  PUSH(R0);" nLine))
                              (reverse args)))) ; apply code-gen on args from end to beginning
                  
      (procCode (code-gen proc const-table fvar-table nested)))

              (string-append
               openMsg nLine
               ;"  PUSH(SOB_NIL); //push null to save a space for a potential empty list of arguments." nLine
               argsCode
               "  PUSH("numOfArgs"); // push num of arg" nLine
               procCode
               "  CMP(INDD(R0,0), IMM(T_CLOSURE));" nLine 
               "  JUMP_NE("applicNotProcLable");" nLine
               "  PUSH(INDD(R0,1)); // push env to stack" nLine
               ))))

(define code-gen-const
 (lambda (sexpr const-table fvar-table nested)
  (let ((addr (number->string 
                          (get-const-addr-from-tableFromAssocList 
                                        (get-const-addr sexpr const-table)
                                        const-table))))
    (string-append
      "  /* ========== const ========== */" nLine
      "  MOV(R0,"addr"); //the address from the symbol table" nLine
      "  /* ======= done const ======== */" nLine
    ))))

(define code-gen-fvar
 (lambda (sexpr const-table fvar-table nested)
  (let ((addr (number->string (faddr (cadr sexpr) fvar-table))))
    (string-append 
        "  /* ========== fvar ========== */" nLine
        "   MOV(R0, INDD("addr",1));" nLine
        "  /* ======= done fvar ======== */"  nLine
    ))))

(define code-gen-pvar
  (lambda (sexpr const-table fvar-table nested)
    (with  sexpr
      (lambda (tag var minor)
        (let ((minorStr (number->string (+ minor 2)))) ; 2 is for env and n in the stack
          (string-append
           "  /* ========== pvar ========== */" nLine
           "  MOV(R0, FPARG("minorStr"));" nLine
           "  /* ======= done pvar ======== */" nLine
          ))))))

(define code-gen-bvar
  (lambda (sexpr const-table fvar-table nested)
    (with sexpr
      (lambda (tag var major minor)
        (let ((minorStr (number->string minor))
            (majorStr (number->string major)))
          (string-append
           "  /* ========== bvar ========== */" nLine
           "  MOV(R0, FPARG(0)); // get to the env " nLine
           "  MOV(R0, INDD(R0,"majorStr")); // go major steps " nLine
           "  MOV(R0, INDD(R0,"minorStr")); // go minor steps and we got the value" nLine
           "  /* ======= done bvar ======== */" nLine
           ))))))

(define code-gen-if3
 (lambda (sexpr const-table fvar-table nested)
  (with sexpr
    (lambda (if3 test dit dif)
      (let ((testCisc (code-gen test const-table fvar-table nested))
            (ditCisc (code-gen dit const-table fvar-table nested))
            (difCisc (code-gen dif const-table fvar-table nested))
            (trueLable (^ditLable))
            (elseLable (^difLable))
            (exitLable (^if3ELable)))
        (string-append
         "  /* ========== if3 ========== */" nLine 
         testCisc nLine 
         "  CMP(R0,SOB_FALSE);" nLine
         "  JUMP_EQ(" elseLable ");" nLine
         trueLable ":" nLine
         ditCisc nLine
         "  JUMP(" exitLable ");" nLine
         elseLable ":" nLine
         difCisc nLine
         exitLable ":" nLine
         "  /* ======= done if3 ======== */"
         nLine))))))

(define code-gen-or
 (lambda (sexpr const-table fvar-table nested)
  (with sexpr
    (lambda (tag pes)
      (let* ((exitLable (^orELable))
            (regConds 
              (apply string-append
                    (map (lambda (e)
                           (string-append
                              (code-gen e const-table fvar-table nested)
                              "  CMP(R0,SOB_FALSE);" nLine
                              "  JUMP_NE("exitLable");" nLine))
                         (dropLastElem pes))))
            (lastCond (code-gen (getLastElem pes) const-table fvar-table nested)))
        
        (string-append
         "  /* ========== or ========== */" nLine
         regConds
         lastCond
         exitLable ":" nLine
         "  /* ======= done or ======== */" 
         nLine))))))

(define code-gen-def
 (lambda (sexpr const-table fvar-table nested)
  (with sexpr
    (lambda (def a e)
      (let ((fvarAddrStr (number->string (faddr (cadr a) fvar-table)))
        (valueCode (code-gen e const-table fvar-table nested)))
        (string-append
         "  /* ========== (define a e) ========== " nLine
         "  /* code-gen of e */" nLine
         valueCode
         "  /* set the value of e to a */" nLine
         "  MOV(ADDR("fvarAddrStr"),FVAR);" nLine
         "  MOV(ADDR("fvarAddrStr"+1),R0);" nLine
         "  MOV(R0,SOB_VOID); //return void" nLine 
         "  /* ======= done (define a e) ======== */" nLine
         ))))))

(define code-gen-set
 (lambda (sexpr const-table fvar-table nested)
  (with sexpr
    (lambda (tag var value)
      (let ((type (car var))
          (name (cadr var))
            (value-code-gen (code-gen value const-table fvar-table nested)))

      (string-append
        "  /* ======= set ========== */" nLine
        value-code-gen
        
        (cond
         ((eq? type 'pvar)
          (let ((minor (number->string (+ (caddr var) 2))))
            (string-append
              "  MOV(FPARG("minor"),R0); // minor = reg_minor+2" nLine
            )))
          
          ((eq? type 'bvar)
            (let ((major (number->string (caddr var)))
                  (minor (number->string (cadddr var))))
              (string-append
                "  MOV(R1, FPARG(0)); // R1 = env" nLine 
                "  MOV(R1, INDD(R1,"major"));" nLine
                "  ADD(R1," minor");" nLine
                "  MOV(ADDR(R1), R0);" nLine   
              )))

          ((eq? type 'fvar)
            (let ((addr (number->string (faddr name fvar-table))))
              (string-append
                "  MOV(ADDR("addr"),FVAR);" nLine
                "  MOV(ADDR("addr"+1),R0);" nLine
              )))

           (else (error "code-gen-set" "mismatch type!")))

        "  MOV(R0,IMM(SOB_VOID));" nLine

        "  /* ======= done set ======== */" nLine
          ))))))

(define code-gen-applic
 (lambda (sexpr const-table fvar-table nested)
  (with sexpr
    (lambda (tag proc args)
      (let* ((applicNotProcLable (^applicNotProcLable))
            (applicNoErrLable (^applicNoErrLable))
            (openMsg "  /* ========== applic ========== */")
            (matual (applic-matual-code args nested const-table fvar-table proc openMsg applicNotProcLable))
            )
        (string-append
         matual
         "  CALLA(INDD(R0,2)); // run the code" nLine
         "  DROP(1); // drop the env from the stack" nLine
         "  POP(R1); // R1 = numOfArgs" nLine
         "  DROP(R1); // drop m+1 parameters (numOfArgs)" nLine
         "  JUMP("applicNoErrLable");" nLine
         applicNotProcLable ":" nLine
         (generateSOBError)
         applicNoErrLable":"
         " /* ======== done applic ======== */" nLine
         ))))))

(define code-gen-tc-applic
 (lambda (sexpr const-table fvar-table nested)
  (with sexpr
    (lambda (tag proc args)
      (let* ((tcApplicLoopLabel (^tcApplicLoopLabel))
            (tcApplicEndLoopLabel (^tcApplicEndLoopLabel))
            (applicNotProcLable (^applicNotProcLable))
            (openMsg "  /* ======== tc-applic ======== */")
            (matual (applic-matual-code args nested const-table fvar-table proc openMsg applicNotProcLable))
          )
        (string-append
         matual
         "  PUSH(FPARG(-1)); // push to stack the old ret addr" nLine
         "  MOV(R1,FPARG(-2)); //save old FP in R1" nLine
         "  MOV(R2, FPARG(1)); // R@ = num of prev args" nLine
         "  MOV(R4 ,SP);" nLine
         "  SUB(R4, 3);" nLine
         "  MOV(R3, STACK(R4)); // R3 = new num of args" nLine
         "  MOV(R6, R2);" nLine
         "  INCR(R6);" nLine
         "  MOV(R7, R4);" nLine
         "  SUB(R7, R3);" nLine
                  
         "  MOV(R4, IMM(0));" nLine
         "  MOV(R5, R3);" nLine
         "  ADD(R5, IMM(3));" nLine
         tcApplicLoopLabel ":" nLine nLine

         "  CMP(R4, R5);" nLine
         "  JUMP_GE("tcApplicEndLoopLabel");" nLine nLine
         
         "  MOV(FPARG(R6), STACK(R7));" nLine nLine

         "  INCR(R4);" nLine
         "  INCR(R7);" nLine
         "  DECR(R6);" nLine nLine

         "  JUMP("tcApplicLoopLabel");" nLine nLine

         tcApplicEndLoopLabel ":" nLine
         "  ADD(R2, 4);" nLine 
         "  DROP(R2);// get new sp" nLine
         "  MOV(FP, R1);// update fp to be the old fp" nLine
         "  JUMPA(INDD(R0,2)); //jump to the body of the procidure" nLine
         applicNotProcLable ":" nLine
         (generateSOBError)
         " /* ======= done tc-applic ====== */" nLine
         ))))))

(define code-gen-box-set
 (lambda (sexpr const-table fvar-table nested)
  (with sexpr
    (lambda (tag var value)
      (let ((type (car var))
          (value-code-gen (code-gen value const-table fvar-table nested)))
        (string-append
        "  /* ======= box-set ========== */" nLine
        value-code-gen
        "  MOV(R1, R0);" nLine
        (cond 

          ((eq? type 'pvar)
            (let ((minor (number->string (+ (caddr var) 2))))
              (string-append
                "  MOV(R0, FPARG("minor")); // minor = reg_minor+2" nLine
              )))

          ((eq? type 'bvar)
            (let ((major (number->string (caddr var)))
                  (minor (number->string(cadddr var))))
              (string-append
                "  MOV(R0, FPARG(0)); // R0 = env" nLine
                "  MOV(R0, INDD(R0," major "));" nLine
                "  MOV(R0, INDD(R0,"minor"));" nLine
              )))

          (else (error "code-gen-set case box-get" "mismatch type!")))

        "  MOV(IND(R0),R1);" nLine
        "  MOV(R0,IMM(SOB_VOID));" nLine
        "  /* ======= done box-set ======== */" nLine
    ))))))

(define code-gen-box-get
 (lambda (sexpr const-table fvar-table nested)
  (with sexpr
    (lambda (tag var)
        (let ((type (car var))
               (name (cadr var)))
            (string-append
            "  /* ========== box-get ========== */" nLine
                (cond
                    ((eq? type 'pvar)
                        (let ((minor (number->string (+ 2 (caddr var)))))
                            (string-append
                                "  MOV(R0, FPARG("minor"));" nLine
                            )))
                    ((eq? type 'bvar)
                        (let ((major (number->string (caddr var)))
                              (minor (number->string (cadddr var))))
                            (string-append
                                "  MOV(R0, FPARG(0)); // R0 = env" nLine
                                "  MOV(R0, INDD(R0," major "));" nLine
                                "  MOV(R0, INDD(R0,"minor"));" nLine
                        )))
                    ((eq? type 'fvar)
                        (let ((addr (number->string (+ 1 (faddr name fvar-table)))))
                            (string-append
                                "  MOV(R0, ADDR("addr"));" nLine
                            )))
                    (else (error "code-gen in box-get" "mosmatch type.")))

            "  MOV(R0, IND(R0)); // unboxing" nLine 
            "  /* ======= done box-get ======== */" nLine
            ))))))

(define code-gen-box
 (lambda (sexpr const-table fvar-table nested)
  (with sexpr
    (lambda (tag var)
        (let ((minor (number->string (+ (caddr var) 2))))
      (string-append
        "  /* ========== box ========== */" nLine
        "  MOV(R1, FPARG(" minor"));" nLine
        "  PUSH(IMM(1));" nLine
        "  CALL(MALLOC);" nLine
        "  DROP(1);" nLine                        
        "  MOV(FPARG(" minor"), R0); // minor = reg_minor+2" nLine
        "  MOV(ADDR(R0), R1);" nLine
        "  /* ======= done box ======== */" nLine
        ))))))

(define code-gen-seq
 (lambda (sexpr const-table fvar-table nested)
    (with sexpr
      (lambda (tag pes)
        (let ((eisCode (apply string-append
               (map (lambda (ei)
                      (code-gen ei const-table fvar-table nested))
                    pes))))
          (string-append
            "  /* ========== seq ========== */" nLine 
              eisCode nLine
              "  /* ======= done seq ======== */"
              nLine))))))

(define code-gen
 (lambda (sexpr const-table fvar-table nested)
    (let ((tag (car sexpr))
          (params `(,sexpr ,const-table ,fvar-table ,nested)))
      (cond 
        ((eq? tag 'const)         (apply code-gen-const params))
        ((eq? tag 'fvar)          (apply code-gen-fvar params))
        ((eq? tag 'pvar)          (apply code-gen-pvar params))
        ((eq? tag 'bvar)          (apply code-gen-bvar params))
        ((eq? tag 'if3)           (apply code-gen-if3 params))     
        ((eq? tag 'or)            (apply code-gen-or params))                                         
        ((eq? tag 'def)           (apply code-gen-def params))
        ((eq? tag 'set)           (apply code-gen-set params))
        ((eq? tag 'applic)        (apply code-gen-applic params))
        ((eq? tag 'tc-applic)     (apply code-gen-tc-applic params))
        ((eq? tag 'box-set)       (apply code-gen-box-set params))
        ((eq? tag 'box-get)       (apply code-gen-box-get params))
        ((eq? tag 'box)           (apply code-gen-box params))
        ((eq? tag 'seq)           (apply code-gen-seq params))
        ((eq? tag 'lambda-simple) (apply (^code-gen-lambda 'simple) params))
        ((eq? tag 'lambda-opt)    (apply (^code-gen-lambda 'opt) params))
        ((eq? tag 'lambda-var)    (apply (^code-gen-lambda 'var) params))
        
        (else (error "code-gen - " "no such tag."))
        ))))
 

;;ancher 
 
(define get-symbols-addrs
  (lambda (consts)
  (let ((addr (lambda() (get-symbols-addrs (cdr consts)))))
      (if (null? consts) 
        `()
      (if (eq? (car (car consts)) 'T_SYMBOL)
      `(,(cadr (car consts))
                    ,@(addr))
          (addr))))))


 ;ancher
(define buildSymbTable
 (lambda (const-table memoryLc)
    (let* ((symbols (get-symbols-addrs (map cadr const-table))))
    (letrec ((buildSymbTable-acc (lambda (linked-list memoryLc symbols)
        (if  (null? symbols) `(,@linked-list
                                 ,`(,memoryLc
                                    ,`(NOT_DEFINE
                                        T_NIL)))  
      (if (not(null? (cdr symbols)))
        (buildSymbTable-acc `(,@linked-list
                                                    ,`(,memoryLc
                                                       ,`(,(car symbols)
                                                          ,(+ memoryLc 2))))
                                                   (+ memoryLc 2)
                                                   (cdr symbols))
     `(,@linked-list 
                                       ,`(,memoryLc
                                          ,`(,(car symbols)
                                             T_NIL))))))))
            (buildSymbTable-acc `() memoryLc symbols)
     ))))

(define build-set
 (lambda (lst)
        (if (null? lst) 
          '()
          `(,(car lst) ,@(build-set (filter (lambda (e) (not (equal? e (car lst)))) (cdr lst)))))))
 

(define genglobtbl
  (lambda ( csttble exp)
  (build-fvar-table
      (build-set `(,@((get-tagged-by 'fvar) exp)
                                     zero? vector? boolean? char? integer? null? pair? string? symbol? procedure? fraction? numerator denominator vector-length string-length car cdr not + remainder - set-car! set-cdr! vector-set! vector-ref vector symbol->string integer->char char->integer string-set! string-ref make-vector make-string cons * / < > = eq? string->symbol my_apply
                   ))
      (get-free-loc csttble) )

  ))


;---------------------------------------------------------------------------------
(define prntR0Val
 (lambda ()
    (let ((exit (^printR0ELable)))
      (string-append
        "   CMP(R0, IMM(SOB_VOID));" nLine
        "   JUMP_EQ("exit");" nLine
        "   PUSH(R0);"  nLine
        "   CALL(WRITE_SOB);" nLine
        "   DROP(1);" nLine
        "   OUT(IMM(2), '\\n');" nLine
        exit":" nLine
      )))) 

(define fraction?
 (lambda (num)
    (and (number? num) (or (not (eq? (denominator num) 1)) (not (eq? (denominator num) -1))))))
 
(define make-tagged-const
 (lambda (const)
    (cond ((eq? const (void)) `(T_VOID))        
          ((null? const) `(T_NIL))        
          ((char? const) `(T_CHAR ,const))
          ((integer? const) `(T_INTEGER ,const))      
          ((boolean? const) `(T_BOOL ,(if (equal? const #t) 1 0)))
          ((pair? const) `(T_PAIR ,(tag-list-elems `(,(car const) ,(cdr const)))))
          ((symbol? const) `(T_SYMBOL ,(make-tagged-const (symbol->string const))))
          ((fraction? const) `(T_FRACTION ,(numerator const) ,(denominator const)))
          ((vector? const) `(T_VECTOR ,(vector-length const) ,(tag-list-elems (vector->list const))))
          ((string? const) `(T_STRING ,(string-length const) ,@(map char->integer (string->list const))))   
          (else `(T_CLOSURE ,const)))))
 
(define tag-list-elems
 (lambda (lst)
  (cond ((null? lst) '())
        ((not (list? lst)) lst)
        (else `(,(make-tagged-const (car lst))
                ,@(tag-list-elems (cdr lst)))))))
 
(define get-tagged-by
  (lambda (tag)
    (lambda (lst)
      (if (or (null? lst) (not (list? lst))) 
        '()
        (if (eq? (car lst) tag) 
          (cdr lst)
          `(,@((get-tagged-by tag) (car lst)) ,@((get-tagged-by tag) (cdr lst))))))))


(define mapping-size
 (lambda (item)
  (let ((tag (car item)))
    (if (or (eq? tag 'T_VOID) (eq? tag 'T_NIL))
        1
    (if (or (eq? tag 'T_INTEGER) (eq? tag 'FVAR) (eq? tag 'T_SYMBOL) (eq? tag 'T_BOOL) (eq? tag 'T_CHAR) (eq? tag 'SYMBOL_TABLE))
        2
    (if (or (eq? tag 'T_PAIR) (eq? tag 'T_FRACTION) (eq? tag 'FVAR_TABLE)) 
        3
    (if (eq? tag 'T_VECTOR) 
        (+ 2 (cadr item))
    (if (eq? tag 'T_STRING) 
        (+ 2 (cadr item))
        0))))))))

(define get-const-addr-from-table
 (lambda (const const-table)
    (cond ((null? const-table) 0)
          ((equal? const (cadar const-table)) (caar const-table))
           (else (get-const-addr-from-table const (cdr const-table))))))

(define const-tags
  (list 'T_VOID 'T_NIL 'T_INTEGER 'T_INTEGER 'T_CHAR 'T_FRACTION 'T_BOOL 'T_SYMBOL 'T_STRING 'T_PAIR 'T_VECTOR 'T_CLOSURE))

(define is-tagged-const?
   (lambda (item)
     (cond ((or (null? item) (not (list? item))) #f)
           ((member (car item) const-tags) #t)
           (else #f))))

(define get-struct-items
  (lambda (item)
    (if (not (null? (cdr item)))
        (cadr item)
        '())))

(define get-vector-items
  (lambda (vec)
    (get-struct-items (cdr vec))))

(define get-pair-items
   (lambda (const-pair)
    (get-struct-items const-pair)))
 
(define tagged-vec?
 (lambda (const)
  (and (eq? (car const) 'T_VECTOR)
         (list? (get-vector-items const))
         (or (zero? (cadr const))
             (is-tagged-const? (car (get-vector-items const)))))))

(define tagged-pair?
 (lambda (const)
  (and (eq? (car const) 'T_PAIR)
       (list? (get-pair-items const))
       (is-tagged-const? (car (get-pair-items const))))))
 
(define repr-tagged-const
 (lambda (const const-table)
    (if (not (null? const))

      (if (is-tagged-const? (car const))
        (let ((tag (caar const))
              (func (lambda (rep)
                        (repr-tagged-const `(,@(cdr const) ,(get-const-addr-from-table rep const-table))
                                              const-table))))
          (cond 

            ((eq? tag 'T_SYMBOL)
              (func `(,tag ,(get-const-addr-from-table (cadar const) const-table))))

            ((eq? tag 'T_PAIR)
                      (func `(,tag ,@(repr-tagged-const (get-pair-items (car const)) const-table))))
            
            ((eq? tag 'T_VECTOR)
                      (func `(,tag 
                              ,(cadar const) 
                              ,@(repr-tagged-const (get-vector-items (car const)) const-table))))
                                                          
            (else (func (car const)))))

          const)

       '()
 )))

(define get-free-loc
 (lambda (ctable . rest)
  (let ((lastElem (getLastElem (if (or (null? rest) (null? (car rest)))
                                  ctable
                                  (car rest)))))
    (+ (car lastElem) (mapping-size (cadr lastElem))))))

(define sym-str?
 (lambda (const)
   (let ((valid-struct
            (lambda (item tag)
              (and (eq? (car item) tag)
                   (list? (cdr item))))))
      (and (valid-struct const 'T_SYMBOL)
           (list? (cadr const))
           (eq? (caadr const) 'T_STRING)))))
 
(define build-const-table
 (lambda (items const-table mem)
    (if (null? items) 
      const-table

      (let ((fist-item (car items))
            (rest-items (cdr items)))
        (cond 
          ((sym-str? fist-item)
            (let* ((new-ctable (build-const-table `(,(cadr fist-item)) const-table mem))
                   (free-loc (get-free-loc new-ctable))
                   (new-symbol `(,(car fist-item) ,(get-const-addr-from-table (cadr fist-item) new-ctable))))

             (build-const-table `(,new-symbol ,@rest-items) new-ctable free-loc)))

          
          ((tagged-pair? fist-item)
            (let* ((pair-items (get-pair-items fist-item))
                   (new-ctable (build-const-table pair-items const-table mem))
                   (free-loc (get-free-loc new-ctable))
                   (new-pair `(,(car fist-item) ,@(repr-tagged-const pair-items new-ctable))))
          
              (build-const-table `(,new-pair ,@rest-items) new-ctable free-loc)))


          ((and (tagged-vec? fist-item)
                (not (null? (cddr fist-item))))
            (let* ((vec-items (get-vector-items fist-item))
                   (new-ctable (build-const-table vec-items const-table mem))
                   (free-loc (get-free-loc new-ctable))
                   (vec-repr (if (zero? (cadr fist-item)) 
                                    '()
                                    (repr-tagged-const vec-items new-ctable)))
                  (new-vector `(,(car fist-item) ,(cadr fist-item) ,@vec-repr)))

              (build-const-table `(,new-vector ,@rest-items) new-ctable free-loc)))
              

          ((member fist-item (map cadr const-table))
            (build-const-table rest-items const-table mem))


          (else 
            (build-const-table rest-items `(,@const-table ,`(,mem ,fist-item)) (+ mem (mapping-size fist-item)))))
 ))))
 
(define get-consts-table
 (lambda (pe)
  (let ((base-consts
           `(,`(,1 ,`(T_VOID))
             ,`(,2 ,`(T_NIL))
             ,`(,3 ,`(T_BOOL 1))
             ,`(,5 ,`(T_BOOL 0))
             ,`(,7 ,`(FVAR_TABLE 0 NOT_DEFINE))
             ,`(,9 ,`(SYMBOL_TABLE NOT_DEFINE))
            )))
    (build-const-table pe base-consts 12))))

 
(define build-fvar-table
 (lambda (items mem)
    (letrec ((build-fvar-table-rec
                (lambda (items mem table)
                  (if (not (null? items))
                      (build-fvar-table-rec (cdr items) (+ mem 2) `(,@table ,`(,mem ,`(FVAR ,(car items)))))
                      table))))
      
      (build-fvar-table-rec items mem '()))))


(define gen-ctable-cisc
 (lambda (ctable mem)
  (if (or (null? ctable) (not (list? ctable))) 
      ""
      (let ((mem-str (number->string mem))
            (const (car ctable))
            (rest (cdr ctable))
            (amem (+ 1 mem)))
        (if (symbol? const)
            (string-append
              " MOV(ADDR("mem-str"),"(symbol->string const)");"nLine
              (gen-ctable-cisc rest amem))
        (if (number? const)
            (string-append
              " MOV(ADDR("mem-str"),IMM("(number->string const)"));"nLine
              (gen-ctable-cisc rest amem))
        (if (char? const)
            (string-append
              " MOV(ADDR("mem-str"),"(number->string (char->integer const))");"nLine
              (gen-ctable-cisc rest amem)))))))))

 
(define string-elems-lst
    (lambda (lst)
      (if (null? lst) ""
      (if (null? (cdr lst)) (car lst)           
      (string-append (car lst) (string-elems-lst (cdr lst)))))))


(define parse-string-to-sexper
 (lambda (str)
  (if (not (eq? "" str))
    (<Sexpr> (string->list str)
              (lambda (sexpr rest)
                `(,sexpr ,@(parse-string-to-sexper (list->string rest))))
              `() )

    `()
    )))


(define parseexp
  (lambda (input)
    (let ((apply-all-hws
             (lambda (sexprStr)
              (annotate-tc
                (pe->lex-pe
                  (box-set
                    (remove-applic-lambda-nil
                      (eliminate-nested-defines
                        (parse sexprStr)))))))))
      (map apply-all-hws
           (parse-string-to-sexper 
              (if (equal? ";; empty file" (file->string input))
                  ""
                  (file->string input)))))))

(define output
  (lambda (opfile)
    (if (file-exists? opfile)
                     (begin (delete-file opfile)
                            (open-output-file opfile))
                     (open-output-file opfile))))


(define lodPrims
  (lambda (global-variable-table)
    (let ((user-proc-str
             (lambda (proc label ftable)
                    (string-append
                        " MOV(ADDR("(number->string (faddr proc ftable))"),FVAR);" nLine
                        " PUSH(LABEL("label"));" nLine
                        " PUSH(IMM(2));" nLine
                        " CALL(MAKE_SOB_CLOSURE);" nLine
                        " DROP(2);" nLine
                        " MOV(ADDR(1+"(number->string (faddr proc ftable))"), R0);" nLine))))
      (string-append            
        (user-proc-str 'zero? "MY_IS_ZERO" global-variable-table)
        (user-proc-str 'vector? "MY_IS_VECTOR" global-variable-table)
        (user-proc-str 'boolean? "MY_IS_BOOLEAN" global-variable-table)
        (user-proc-str 'char? "MY_IS_CHAR" global-variable-table)
        (user-proc-str 'integer? "MY_IS_INTEGER" global-variable-table)
        (user-proc-str 'null? "MY_IS_NULL" global-variable-table)
        (user-proc-str 'pair? "MY_IS_PAIR" global-variable-table)
        (user-proc-str 'string? "MY_IS_STRING" global-variable-table)
        (user-proc-str 'symbol? "MY_IS_SYMBOL" global-variable-table)
        (user-proc-str 'procedure? "MY_IS_PROCEDURE" global-variable-table)
        (user-proc-str 'fraction? "MY_IS_FRACTION" global-variable-table)
        (user-proc-str 'numerator "MY_NUMERATOR" global-variable-table)
        (user-proc-str 'denominator "MY_DENOMINATOR" global-variable-table)
        (user-proc-str 'vector-length "MY_VECTOR_LENGTH" global-variable-table)
        (user-proc-str 'string-length "MY_STRING_LENGTH" global-variable-table)
        (user-proc-str 'car "MY_CAR" global-variable-table)
        (user-proc-str 'cdr "MY_CDR" global-variable-table)
        (user-proc-str 'not "MY_NOT" global-variable-table)
        (user-proc-str '+ "MY_PLUS" global-variable-table)
        (user-proc-str 'remainder "MY_REMAINDER" global-variable-table)
        (user-proc-str '- "MY_MINUS" global-variable-table)
        (user-proc-str 'set-car! "MY_SETCAR" global-variable-table)
        (user-proc-str 'set-cdr! "MY_SETCDR" global-variable-table)
        (user-proc-str 'vector-set! "MY_VECTORSET" global-variable-table)
        (user-proc-str 'vector-ref "MY_VECTORREF" global-variable-table)
        (user-proc-str 'vector "MY_VECTOR" global-variable-table)
        (user-proc-str 'symbol->string "MY_SYMBOL_TO_STRING" global-variable-table)
        (user-proc-str 'integer->char "MY_INTEGER_TO_CHAR" global-variable-table)
        (user-proc-str 'char->integer "MY_CHAR_TO_INTEGER" global-variable-table)
        (user-proc-str 'string-set! "MY_STRING_SET" global-variable-table)
        (user-proc-str 'make-string "MY_MAKE_STRING" global-variable-table)
        (user-proc-str 'string-ref "MY_STRING_REF" global-variable-table)
        (user-proc-str 'make-vector "MY_MAKE_VECTOR" global-variable-table)
        (user-proc-str 'cons "MY_CONS" global-variable-table)
        (user-proc-str '* "MY_MULTIPY" global-variable-table)
        (user-proc-str '/ "MY_DIVISION" global-variable-table)
        (user-proc-str '< "MY_LESS_THAN" global-variable-table)
        (user-proc-str '> "MY_GREATER_THAN" global-variable-table)
        (user-proc-str '= "MY_NUMERIC_EQUAL" global-variable-table)
        (user-proc-str 'eq? "MY_IS_EQUAL" global-variable-table)
        (user-proc-str 'string->symbol "MY_STRING_TO_SYMBOL" global-variable-table)
        (user-proc-str 'my_apply "MY_APPLY" global-variable-table)))))

(define flat-list 
  (lambda (lst)
    (cond ((null? lst) '())
    ((pair? lst) (append (flat-list (car lst))
                         (flat-list (cdr lst))))
    (else (list lst)))))

(define compile-scheme-file
 (lambda (input-file output-file)
    (let* ( (sexprs  (parseexp input-file))
        (runTimeSupport_sexpr (parseexp "runtime-support.scm" ))
            (aexps `(,@runTimeSupport_sexpr ,@sexprs))
            (const-table (get-consts-table (tag-list-elems ((get-tagged-by 'const) aexps))))
        (global-variable-table (genglobtbl const-table aexps ) )
          (memlocation (get-free-loc const-table  global-variable-table))
      (symbol_linked_list (buildSymbTable const-table memlocation ))
            (out (output output-file))
            (pstCodeGen (lambda (sexpr)(string-append (code-gen sexpr const-table global-variable-table -1) (prntR0Val))))          
        (glblConstsSize (length (flat-list(map cadr global-variable-table))))
        (make-runTimeSupport (lambda (sexpr)
            (code-gen sexpr const-table  global-variable-table -1)))
      (SymbSize (flat-list (map cadr symbol_linked_list)))
      )
     (display (string-append
                "/* ======================== The prologue ======================== */" nLine
                "#include <stdio.h>" nLine
                "#include <stdlib.h>" nLine
                "#include \"arch/cisc.h\"" nLine
                "int main() {" nLine
                "   START_MACHINE;" nLine
                "   JUMP(CONTINUE);" nLine
                "#include \"arch/system.lib\"" nLine
                "#include \"arch/char.lib\"" nLine
                "#include \"arch/string.lib\"" nLine
                "#include \"arch/io.lib\"" nLine
                "#include \"arch/scheme.lib\"" nLine
                "#include \"arch/math.lib\"" nLine
                "#include \"arch/runTimeSupport.lib\"" nLine
                "   CONTINUE:" nLine
                " PUSH(IMM(" (number->string (length (flat-list (map cadr const-table))))"));" nLine
                " CALL(MALLOC);" nLine
                " DROP(1);" nLine
                (gen-ctable-cisc (flat-list (map cadr const-table)) 1)
                " PUSH(IMM(" (number->string glblConstsSize) "));" nLine
                " CALL(MALLOC);" nLine
                " DROP(1);" nLine
                  (if (= glblConstsSize 0)
                     ""
                    (string-append
                        "//update size of globaltable." nLine
                        "   MOV(ADDR(10),IMM("(number->string  glblConstsSize) "));" nLine
                        "//updating address for constanttable " nLine
                        "   MOV(ADDR(11),IMM("(number->string  (get-free-loc const-table)) "));" nLine
                        ))
                " PUSH(IMM(" (number->string (length SymbSize)) "));" nLine
                " CALL(MALLOC);" nLine
                " DROP(1);" nLine
                "   MOV(ADDR(8),IMM("(number->string (get-free-loc const-table global-variable-table))"));" nLine
                (gen-ctable-cisc SymbSize (get-free-loc const-table global-variable-table))
              (lodPrims global-variable-table)
                (string-elems-lst (map make-runTimeSupport runTimeSupport_sexpr))
                (string-elems-lst (map pstCodeGen sexprs))
                "/* ======================== The epilogue ======================== */" nLine
                " STOP_MACHINE;" nLine
                " return 0;" nLine
                "ErrExit:" nLine
                "   return 1;" nLine
                "}" nLine
                )
                out)
     (close-output-port out)
     )

 ))
 