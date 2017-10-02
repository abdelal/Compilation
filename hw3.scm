; GENERAL


(define valid-structure 
  (lambda (app)
    (lambda (pe) 
            (and (pair? pe) (eq? (car pe) app)))))

(define gFirst (lambda (first rest) first))
(define gRest (lambda (first rest) rest))


(define seq? (valid-structure 'seq))
(define set? (valid-structure 'set))

(define tagged-var? (valid-structure 'var))

(define lambda-simple? (valid-structure 'lambda-simple))
(define lambda-opt? (valid-structure 'lambda-opt))
(define lambda-var? (valid-structure 'lambda-var))


(define lambda?
  (lambda (expr)
         (or (lambda-simple? expr)
             (lambda-opt? expr)
             (lambda-var? expr))))

(define get-lambda-body
  (lambda (expr)
    (if (lambda? expr)
      (let ((type (car expr)))
        (cond
          ((equal? 'lambda-opt type) (cadddr expr))
          ((or (equal? 'lambda-simple type) (equal? 'lambda-var type)) (caddr expr)))))))


;============================part 3============================;

(define foo
  (lambda (pes ret-ds+es)
    (if (null? pes) (ret-ds+es `() '())
        (foo (cdr pes)
             (lambda (ds es)
               (cond 
                 ((eq? (caar pes) 'def ) (ret-ds+es (cons (cdar pes) ds) es))
                 ((eq? (caar pes) 'seq)  (foo (cadar pes) (lambda (ds1 es1) 
                                                            (ret-ds+es (append ds1 ds)
                                                                       (append es1 es)))))
                 (else  (ret-ds+es ds (cons (car pes ) es))))
               )))))

(define eliminate-map
  (lambda (exp)
    (if (null? exp) exp
    (map (lambda (v) (cons 'set (eliminate-nested-defines v))) 
          exp))))

(define build-lambda-body
  (lambda (defs rest)
    (let ((eliminated (eliminate-map defs)))
        (if (or (null? rest) (null? defs))
            (if (null? (cdr eliminated))
                (car eliminated)
                eliminated)
            `(seq ,`(,@eliminated ,@(map eliminate-nested-defines rest)))))))


(define build-lambda
  (lambda (defs rest)
    `(applic ,`(lambda-simple ,(map cadar defs) ,(build-lambda-body defs rest))
             ,(make-list (length defs) (list 'const #f)))))


(define pre-foo
  (lambda (pe g)
    (foo `(,pe) g)))


(define eliminate-nested-defines
  (letrec ((nested-def
            (lambda (pe)   
              (let ((def? (valid-structure 'def))
                    (contains-def? (lambda () (not (null? (pre-foo pe gFirst))))))
                
                (if (null? pe)
                  '()
                  (if (and (or (seq? pe) (def? pe)) (contains-def?))
                      (build-lambda (pre-foo pe gFirst) (pre-foo pe gRest))
                      (if (list? (car pe))
                        (if (null? (cdr pe))
                          `(,(nested-def (car pe)))
                          (cons (nested-def (car pe)) (nested-def (cdr pe))))     
                        `(,(car pe) ,@(nested-def (cdr pe))))))
                ))))
    
    (lambda (pe) ; maybe need to check more cases 
      `(,(car pe) ,@(nested-def (cdr pe))))))


;============================part 4============================;

(define remove-applic-lambda-nil
  (lambda (pe)
    (let ((applic? (lambda (expr)
                         (and ((valid-structure 'applic) expr)
                              (lambda-simple? (cadr expr))
                              (null? (cadadr expr )) 
                              (or (null? (caddr expr)) (equal? (caddr expr) `(,'())))))))

      (if (or (null? pe) (not (list? pe)) (tagged-var? pe)) 
        pe

        (if (applic? pe)
          (let ((lambda-body (get-lambda-body (cadr pe))))
            (if ((valid-structure 'seq) lambda-body)
              `(seq ,@(map remove-applic-lambda-nil (cdr lambda-body)))
              (remove-applic-lambda-nil lambda-body)))

          (if (pair? (car pe))
            (if (null? (cdr pe))
              `(,(remove-applic-lambda-nil (car pe)))
              `(,(remove-applic-lambda-nil (car pe)) ,@(remove-applic-lambda-nil (cdr pe))))

            `(,(car pe) ,@(remove-applic-lambda-nil (cdr pe))))))
      )
    ))


;============================part 5============================;

(define lambda-simple-var?
  (lambda (var params)
    (and  (pair? params) (member var params))))

(define notFreeVar?
  (lambda (var params)
    (or (lambda-simple-var? var params) 
        (equal? var params))))

(define seqfy 
  (lambda (s)
    (cond ((null? s) '())
          ((null? (cdr s)) (car s))
          (else `(seq  ,s)))))

(define ^set-box
  (lambda (v) 
    `(set ,`(var ,v) ,`(box (var ,v)))))

(define gen-box1
  (lambda (expr v rest un)
    (cons
        (car expr)
        (cons (cadr expr)
              (list (caddr expr)
                    (if (eq? un #t)
                      `(seq ,`(,(^set-box v) ,@rest))
                      `(seq ,`(,(^set-box v) ,rest))))))))
(define gen-box2
  (lambda (expr v)
    (cons
        (car expr)
        (cons (cadr expr)
            `(,(seqfy (cons (^set-box v) (cddr expr))))))))

(define boxInjection
  (lambda (v lst)
    (let* ((injectBox (lambda (expr)
                        (if (lambda-opt? expr)
                              (if (pair? (cdddr expr))
                                (if (seq? (cadddr expr))
                                  (gen-box1 expr v (cadr (cadddr expr)) #t)
                                  (gen-box1 expr v (cadr (cddr expr)) #f))

                                (gen-box2 expr v)
                              )
                                         
                              (if (and (pair? (cddr expr)) (seq? (caddr expr)))
                                (cons
                                  (car expr)
                                  (list (cadr expr)
                                        `(,(caaddr expr) ,`(,(^set-box v) ,@(car (cdaddr expr))))))
                                (gen-box2 expr v)))))

           (declared? (lambda (expr)
                             (if (lambda? expr)
                                  (notFreeVar? v (cadr expr))
                                 #f)))
           ) 
      (if (or (null? lst) (not (pair? lst)))
        lst
        (if (list? (car lst))
          (if (null? (cdr lst))
            (list (boxInjection v (car lst)))
            (cons (boxInjection v (car lst)) (boxInjection v (cdr lst))))
          (if (declared? lst)
            (injectBox lst)
            (if (declared? (car lst))
              (cons (injectBox (car lst)) (boxInjection v (cdr lst)))
              (cons (car lst) (boxInjection v (cdr lst)))))))
            )))

(define retFirstIfTrue
  (lambda (first second)
    (if first first second)))

(define occVar?
  (lambda (v expr)
    (if (and (tagged-var? expr) (equal? v (cadr expr))) v #f)))

(define nestedBvar
  (lambda (expr)
    (lambda(v)
      (if (or (null? expr) (not (list? expr)))
        #f
        (if (occVar? v expr)
          v
          (if (set? expr)
            (retFirstIfTrue ((nestedBvar (cadr expr)) v) ((nestedBvar (caddr expr)) v))
            (if (lambda? expr)
              (if (notFreeVar? v  (cadr expr))
                #f
                ((nestedBvar (caddr expr)) v))
              (if (list? (car expr))
                (if (null? (cdr expr))
                  ((nestedBvar (car expr)) v)
                  (retFirstIfTrue ((nestedBvar (car expr)) v) ((nestedBvar (cdr expr)) v)))
                ((nestedBvar (cdr expr)) v)))))))))


(define filtering
  (lambda (lst)
        (filter (lambda (x) x) lst))) 

(define sharedElemnts
  (lambda (l1 l2)
    (let ((l2-member? (lambda (x) (if (member x l2) x))))
      (if (and (pair? l1) (pair? l2))
        (filtering (map l2-member? l1))
        '()
        ))))

;====================================================================================================================
;====================================================================================================================
;====================================================================================================================
;====================================================================================================================
;====================================================================================================================
;====================================================================================================================
;====================================================================================================================

(define check-getset-occurence 
  (lambda ( input  indx)
    (lambda( var-name)
      (let ((call (lambda (subinput)( (check-getset-occurence  subinput indx) var-name)     ))
        )
      (cond
        ;base case
        ( (or (null? input) (not (list? input )))
         #f)
        
        ;set case
        ((and(= indx 1) (set? input) (equal? `(var ,var-name) (cadr input)))
         var-name )
           ((and(= indx 2) (occVar? var-name input) var-name ))
           ( (and  (= indx 2)(equal? (car input) 'set)(not(null? (cdr input)))   )
          ((check-getset-occurence  (caddr input)indx) var-name  ))
        
        ;lambda cases
        ( (and (lambda? input)     (notFreeVar? var-name  (cadr input))  )
          #f)
        ( (and (lambda? input) (not(notFreeVar? var-name  (cadr input))) )
          (if (= indx 0)
          ( (nestedBvar (caddr input)) var-name)
          ( (check-getset-occurence  (caddr input) indx) var-name)

          ))
        
        ;continue searching
        ( (and (list? (car input))  (null? (cdr input))) 
          ((check-getset-occurence (car input) indx)var-name ))   
        ( (and (list? (car input)) (not(null? (cdr input))))   
          (retFirstIfTrue ((check-getset-occurence  (car input) indx) var-name ) ((check-getset-occurence  (cdr input) indx) var-name )) )     
        ( else
          ((check-getset-occurence  (cdr input) indx) var-name )))

      ))))
 




(define get-need-box-vars
  (lambda (parms body)
    (let* (
           (makefun (lambda (fun indx) (if (pair? parms) (filtering (map (fun body indx) parms )) ((check-getset-occurence body 0) parms) )  ))
           (common (sharedElemnts  (makefun check-getset-occurence 0) (makefun check-getset-occurence 1)))
           (Needsboxing (sharedElemnts common (makefun check-getset-occurence 2))) 
           )
      (if (null? Needsboxing)
         #f
          Needsboxing))))


(define fnd&rpls
(lambda (arg1 arg2 lst name indx)

    (cond
      ;base case
      ((or (null? lst) (not (list? lst )) (and (lambda? lst) (notFreeVar? name  (cadr lst)))
        (and (= indx 2)  (lambda? lst) (notFreeVar? name  (cadr lst)))  (and (= indx 1) (lambda? lst) (notFreeVar? (cadr arg1) (cadr lst))) )
       lst)
     ((or(and (= indx 2) (equal? (car lst) arg1) (equal? (cadadr lst) name) ) (and (= indx 1)(equal? (car lst) arg1)))
       (cons arg2 (fnd&rpls arg1 arg2 (cdr lst) name indx)))
    ((and (= indx 1) (equal? (car lst) 'set)(not(null? (cdr lst))))
       (cons(car lst) (cons (cadr lst) (fnd&rpls arg1 arg2 (cddr lst) name indx))))
      ;keep going
      ((and (list? (car lst))  (null? (cdr lst))) 
        (list (fnd&rpls arg1 arg2 (car lst) name indx)))
      ((and (list? (car lst)) (not(null? (cdr lst))))
         (cons (fnd&rpls arg1 arg2 (car lst) name indx) (fnd&rpls arg1 arg2 (cdr lst) name indx)))
      (else 
       (cons (car lst) (fnd&rpls arg1 arg2 (cdr lst)  name indx))
         ))))

(define replace-occur
  (lambda (lst var-name indx)
    (if (lambda? lst)
     (if  (if (= indx 2)
          `(,(car lst) ,(cadr lst)  ,(fnd&rpls `set `box-set (caddr lst)  var-name indx)) 
          `(,(car lst) ,(cadr lst)  ,(fnd&rpls `(var ,var-name) `(box-get ,`(var ,var-name)) (caddr lst) "" indx )) 
          )
     (if (equal? 'lambda-opt    (car lst))
            (if (= indx 2)
          `(,(car lst) ,(cadr lst) ,(caddr lst) ,(fnd&rpls `set `box-set (cadddr lst) var-name indx)) 
          `(,(car lst) ,(cadr lst) ,(caddr lst) ,(fnd&rpls `(var ,var-name) `(box-get ,`(var ,var-name)) (cadddr lst) "" indx))
          )
             (if (= indx 2)
             `(,(car lst) ,(cadr lst),(fnd&rpls `set `box-set (caddr lst)  var-name 2))
             `(,(car lst) ,(cadr lst),(fnd&rpls `(var ,var-name) `(box-get ,`(var ,var-name)) (caddr lst) "" indx)) 

                    )))
        (if (= indx 2)
        (fnd&rpls `set `box-set  lst  var-name indx)
        (fnd&rpls `(var ,var-name) `(box-get ,`(var ,var-name)) lst "" indx )))))




(define vars-needBxing?
(lambda (expr)
    (if (not(pair? expr))
      #f
    (get-need-box-vars   
         (if (equal? 'lambda-opt    (car expr)) 
          (append (cadr expr)(list (caddr expr)) )
    ( if(equal? 'lambda-simple (car expr)) 
          (cadr expr) 
          (list (cadr expr))
           ))  
         (if (equal? 'lambda-opt    (car expr)) 
          (cadddr expr) 
          (caddr expr) )  
    
       ))))

(define returnval
  (lambda (expr needBxing )
    (if needBxing
     (cond
         ;lambda opt case
        ( (and (pair? expr) (equal? 'lambda-opt  (car expr))
               `(,(car expr) ,(cadr expr) ,(caddr expr) ,(box-set (get-lambda-body expr)) )))
        
        ;lambda case
        ( (lambda? expr)
          `(,(car expr) ,(cadr expr) ,(box-set (get-lambda-body expr)) ))
        ))))




(define strtBxing
  (lambda(expr)
    (let* ((needBxing (vars-needBxing? expr))
          (retrnedCall (returnval expr needBxing))
          (appliedFun  (lambda (lst var-name)(boxInjection var-name (replace-occur (replace-occur  lst var-name 1) var-name 2)))))

(if (or (null? expr) (not (list? expr ))) 
  expr
  (if needBxing
        (fold-left appliedFun retrnedCall (reverse needBxing))
      (cond
      ( (and (pair? expr) (equal? 'lambda-opt  (car expr))
             `(,(car expr) ,(cadr expr) ,(caddr expr) ,(box-set (get-lambda-body expr)) )))
      ( (lambda? expr)
        `(,(car expr) ,(cadr expr) ,(box-set (get-lambda-body expr)) ))
      (else
       (box-set expr))
      ))))))



(define box-set
  (lambda (expr)
(if (or (null? expr) (not (list? expr ))) 
  expr
 
     (if (and (list? (car expr)) (not(null? (cdr expr))))
        (cons (box-set (car expr)) (box-set (cdr expr)))
      (if (lambda? expr)
         (strtBxing expr)
         (if (and (list? (car expr))  (null? (cdr expr)))
        (list (box-set (car expr)))
             (cons (strtBxing(car expr)) (box-set (cdr expr)))
      ))))))


;============================part 6============================;

(define gen-fun3
(lambda(fun pe)
    (with pe
      fun)))

(define gen-min
  (lambda (fun x s)
    (if (= s 1)
      (lambda ( mrg min )
      (fun -1 (+ x min))
      )
    (lambda (mrg min)
    (fun  (+ 1 mrg) min))
      )))


(define defined?
(lambda (pe)
  (or(eq? pe 'or) (eq? pe 'def) (eq? pe 'if3) (eq? pe 'set)(eq? pe 'box-set) 
   (eq? pe 'lambda-simple) (eq? pe 'lambda-var)  (eq? pe 'lambda-opt) (eq? pe 'seq)) 
  ))


(define PrnchSrch
  (lambda (a s ret-min ret-nf)

    (let*((fun1  (gen-min ret-min 1 1))
      )
    (letrec ((runfun (lambda() (PrnchSrch a (cdr s)fun1 ret-nf))))
   ( if (null? s) 
    (ret-nf)
    (if (eq? (car s) a)
      (ret-min -1 0)
      (runfun)
    )
    )))))



(define GlblSrch
  (lambda (a env mag+mn ret-nf)
     (let* (
      (fun1 (lambda ( mrg min)(mag+mn 0 min)))
      (fun2 (gen-min mag+mn 0 2))
      (fun4 (lambda () (GlblSrch a (cdr env) fun2 ret-nf )))
      (runfun (lambda() (PrnchSrch a (car env) fun1 fun4 )))
      )
          (if (null? env) 
        (ret-nf)
          (runfun)
           ))))


(define mainFun
(lambda (pe params env)
  (let* (
    (delrun (lambda(rov ) (mainFun rov params env)))
    (gen-fun   (lambda () 
         (if (eq? (car pe) 'lambda-simple)
        (lambda ( _ arglst body )
         `(lambda-simple ,arglst , (mainFun body arglst (cons params env))))
        (if (eq? (car pe) 'lambda-opt)
          (lambda ( _ arg opt body )
            (let ((arglst(append arg `(,opt))))
               `(lambda-opt ,arg, opt , (mainFun body arglst (cons params env)))))
          (if (eq? (car pe) 'applic) 
                (lambda  (_ par body )
                         `(applic  ,(mainFun par params env) ,`(,@(mainFun body params env))))
            (if (eq? (car pe) 'lambda-var)
           (lambda ( _ arglst body )
                           `(lambda-var ,arglst , (mainFun body `(,arglst) (cons params env))))

    ))))))
    (fun1  (lambda ( _ v )
      (let* (
          (fun2 (lambda (mrg min) `(pvar ,v ,min )))
          (fun3  (lambda (mrg min)`(bvar , v ,mrg ,min)))
          (fun4  (lambda () `(fvar ,v)))
          (fun5  (lambda () (GlblSrch v env  fun3 fun4)))
          (fun6  (PrnchSrch v params  fun2  fun5 ))
          )
           fun6 ))))
  (if (null? pe)pe
    (if (or(eq? (car pe) 'lambda-simple) (eq? (car pe) 'lambda-opt) (eq? (car pe) 'applic) (eq? (car pe) 'lambda-var) )
    (gen-fun3 (gen-fun) pe)

    (if (eq? (car pe) 'var)
      (gen-fun3 fun1 pe)
        (if (not(list? (car pe)))
          `(,(car pe) ,@(delrun (cdr pe)))
        (if  (not(null? (cdr pe)))  
          `(,(delrun (car pe))  ,@(delrun (cdr pe))  )
                 `(,(delrun (car pe))) ))))))))


  (define pe->lex-pe
    (lambda (pe)
      (mainFun pe '() '() )
    ))


;============================part 7============================;

(define list-head
  (lambda (lst n)
    (if (= n 0)
        '()
        (cons (car lst) (list-head (cdr lst) (- n 1)))))) 

(define mainfun
  (lambda (pe tp?)
   (let(
    (funlambda (lambda (name)  
            (if (or (eq? name 'lambda-var) (eq? name 'lambda-simple))
              (lambda ( _ arglst body )`(,name ,arglst , (mainfun body #t)))
            (if (eq? name 'lambda-opt)
              (lambda ( _ arglst opt body )`(lambda-opt ,arglst ,opt , (mainfun body #t)))
            (if (or (eq? name 'or) (eq? name 'seq))
              (lambda ( _ el)`(,name ,`( ,@(map (lambda (pe2) (mainfun pe2 #f)) (list-head el (- (length el ) 1 )) ) ,(mainfun  (list-ref el (- (length el ) 1 )) tp?))))
            (if (or(eq? name 'set)(eq? name 'box-set))
              (lambda ( _ var val )`(,name ,var , (mainfun val #f)))
            (if (eq? name 'def) 
              (lambda ( _ var val) `(def ,(mainfun var #f)  ,(mainfun val #f)))
            (if (eq? name 'if3) 
              (lambda ( _ test dit dif)`(if3 ,(mainfun test #f ) ,(mainfun dit tp?) ,(mainfun dif tp? )))
          ))))))))
  )
 (if (defined? (car pe))
      (gen-fun3 (funlambda (car pe)) pe)
            (if (eq? (car pe) 'applic) 
               (let ((proc (mainfun (cadr pe) #f))   
                      (args (map (lambda (pe) (mainfun pe #f))  (caddr pe))))
                       (if tp?  
                      `(tc-applic ,proc ,args)  
                      `(applic ,proc ,args)))
             pe)))))

(define annotate-tc
   (lambda (pe)
      (mainfun pe #f)))

