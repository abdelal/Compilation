 (load "pattern-matcher.scm")


(define simple-const? 
  (lambda (x) 
   (or (null? x) (void-object? x) (vector? x) (boolean? x) (char? x) (number? x) (string? x))))

(define *reserved-words*
  '(and begin cond define do else if lambda let let* 
    letrec or quasiquote unquote unquote-splicing quote set!))

(define not-*reserved-words*
  (lambda (v)
    (not (member v *reserved-words* ))))
    ;(not (member v `(,*reserved-words*)))))

(define var? 
  (lambda (v) 
    (and (symbol? v)
   (not-*reserved-words* v))))

(define emptylist?
  (lambda (v)
    (if (equal? v '()) #t #f)))



(define *void-object* 
  (void) )


(define void-object?
  (lambda (v)
    (equal? v *void-object*)))


(define  val? '())

(define mitdefine?
  (lambda (lst)
    (or (list? lst)
  (and (list? lst)
       (var? (car lst))))))


(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
     (eq? (car e) tag)
     (pair? (cdr e))
     (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
   (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
     simple-sexprs-predicates)
    (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
      (pair? e)
      (symbol? e)
      (vector? e))
  `',e
  e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
  (cadr e)
  e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
   (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
      (lambda (e)
        (cond ((unquote? e) (cadr e))
        ((unquote-splicing? e)
         (error 'expand-qq
           "unquote-splicing here makes no sense!"))
        ((pair? e)
         (let ((a (car e))
         (b (cdr e)))
           (cond ((unquote-splicing? a)
            `(append ,(cadr a) ,(expand-qq b)))
           ((unquote-splicing? b)
            `(cons ,(expand-qq a) ,(cadr b)))
           (else `(cons ,(expand-qq a) ,(expand-qq b))))))
        ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
        ((or (null? e) (symbol? e)) `',e)
        (else e))))
     (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
     (optimizer
      (compose-patterns
       (pattern-rule
        `(append ,(? 'e) '())
        (lambda (e) (optimize-qq-expansion e)))
       (pattern-rule
        `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
        (lambda (c1 c2 e)
    (let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
          (e (optimize-qq-expansion e)))
      (optimize-qq-expansion `(append ,c ,e)))))
       (pattern-rule
        `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
        (lambda (c1 c2)
    (let ((c (quotify (append (unquotify c1) (unquotify c2)))))
      c)))
       (pattern-rule
        `(append ,(? 'e1) ,(? 'e2))
        (lambda (e1 e2)
    (let ((e1 (optimize-qq-expansion e1))
          (e2 (optimize-qq-expansion e2)))
      `(append ,e1 ,e2))))
       (pattern-rule
        `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
        (lambda (c1 c2 e)
    (let ((c (quotify (list (unquotify c1) (unquotify c2))))
          (e (optimize-qq-expansion e)))
      (optimize-qq-expansion `(append ,c ,e)))))
       (pattern-rule
        `(cons ,(? 'e1) ,(? 'e2))
        (lambda (e1 e2)
    (let ((e1 (optimize-qq-expansion e1))
          (e2 (optimize-qq-expansion e2)))
      (if (and (const? e1) (const? e2))
          (quotify (cons (unquotify e1) (unquotify e2)))
          `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))



                                                     
(define identify-lambda
        (lambda (argl ret-simple ret-opt ret-var)
    (cond
     ((null? argl) (ret-simple '()))
     ((var? argl) (ret-var argl))
     (else (identify-lambda (cdr argl)
         (lambda (s) (ret-simple `(,(car argl) ,@s)))
         (lambda (s opt) (ret-opt `(,(car argl) ,@s) opt))
         (lambda (var) (ret-opt `(,(car argl)) var)))))))

(define cond-else-form?
  (lambda (x) 
    (and (equal? (car x) 'else) 
      (not (null? (cdr x))))))

(define begin-expr?
  (lambda (lst)
    (and (pair? lst) (equal? (car lst) 'begin))))

(define clean-begin 
    (lambda (lst) 
        (fold-left
            (lambda (clean rest)
                (append clean
                    (if (begin-expr? rest)
                        (clean-begin (cdr rest))
                        (list rest))))
            '() lst)))

(define items-var?
  (lambda (lst)
    (andmap 
      (lambda (item) (var? (car item)))
      lst)))

(define let-var?
  (lambda (var)
    (and (list? var)
      ;(andmap list? var)
      (andmap pair? var)
      (items-var? var))))

(define quasi?
    (lambda (x) 
        (equal? x 'quasiquote)))

(define car-map
  (lambda (lst)
    (map (lambda(x)(car x)) lst)))

(define cadr-map
  (lambda (lst)
    (map (lambda(x)(cadr x)) lst)))

(define only-vars?
  (lambda (x)
    (if (or (null? x) (var? x))
      #t
      (if (pair? x) 
        (and (var? (car x)) (only-vars? (cdr x)))))))


(define has-duplications?
  (lambda (lst)
    (if (or (null? lst) (emptylist? lst))
      #f
        (if (member (car lst) (cdr lst)) 
          #t
          (has-duplications? (cdr lst))))))
          
(define no-duplications?
        (lambda (lst)
            (if (not (list? lst))
              #t
                (not (has-duplications? lst)))))


(define parse
  (let ((run 
   (compose-patterns
  
  ; CONST
    (pattern-rule
     (? 'c simple-const?)
     (lambda (c)
       `(const ,c)))

    (pattern-rule
     `(quote ,(? 'c))
     (lambda (c)
       `(const ,c)))
    
  ; VAR
    (pattern-rule
     (? 'v var?)
     (lambda (v)
       `(var ,v)))
    
  ; IF without else
    (pattern-rule
     `(if ,(? 'test) ,(? 'dit))
     (lambda (test dit)
       `(if3 ,(parse test)
       ,(parse dit)
       (const ,*void-object*))))

  ; IF reg form
    (pattern-rule
     `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
     (lambda (test dit dif)
       `(if3 ,(parse test)
       ,(parse dit)
       ,(parse dif))))
    
  ; APPLICATIONS 
      (pattern-rule 
      `( ,(? 'expr (lambda (v) (not-*reserved-words* v))) . ,(? 'rest ))
      (lambda (expr rest)
      `(applic ,(parse expr)
         ,(map parse rest))))

  ; DISJUNCTION
    (pattern-rule
      `(or ,(? 'expr) . ,(? 'rest))
        (lambda (expr rest)
      (if (emptylist? rest)
          (parse expr)
      `(or (,(parse expr)
      ,@(map parse rest))))))
    (pattern-rule
      `(or)
        (lambda () (parse '#f)))
         
  ; LAMBDA    
    (pattern-rule
      `(lambda ,(? 'args only-vars? no-duplications?) ,(? 'rest) . ,(? 'body list?))
        (lambda (args rest body) 
     `(,@(identify-lambda
                  args
                  (lambda (vars) `(lambda-simple ,vars))
                  (lambda (vars opt) `(lambda-opt ,vars ,opt))
                  (lambda (vars) `(lambda-var ,vars))) ,(parse `(begin ,rest ,@body)))))
    
  ; DEFINE mit
    (pattern-rule
    `(define (,(? 'var) . ,(? 'vars)) ,(? 'expr) . ,(? 'exprs))
     (lambda (var vars expr exprs)
       (parse `(define ,var ,`(lambda ,vars  ,expr .,exprs)))))

  ; DEFINE reg
    (pattern-rule
     `(define ,(? 'var)  ,(? 'first) . ,(? 'rest))
     (lambda (var first rest)       
       (if (null? rest) 
     `(def ,(parse var) ,(parse first))
     `(def ,(parse var) ,(parse (beginify (cons first rest)))))))

  ; SET!
    (pattern-rule
      `(set! ,(? 'var) ,(? 'exp))
      (lambda (var exp) `(set ,(parse var) ,(parse exp) )))
  
  ; BEGIN
     (pattern-rule
      `(begin ,(? 'expr). ,(? 'exprs list? not-*reserved-words*))
      (lambda (first exprs)
        (if (null? exprs)
        (parse first)
        `(seq ,(map parse (clean-begin (cons first exprs)))))))

  ; BEGIN empty
    (pattern-rule
      `(begin)
      (lambda () (parse *void-object*)))

  ; AND
    (pattern-rule 
          `(and ,(? 'first) . ,(? 'rest))
          (lambda (first rest)
            (if (null? rest)
                  (parse first)
                  ;else make IF expantion 
                  (parse `(if ,first 
                        (and ,@rest) 
                        #f)))))

    ; AND empty
       (pattern-rule 
          `(and)
          (lambda () (parse '#t)))
   

    ; COND
        (pattern-rule
          `(cond ,(? 'first) . ,(? 'rest))
          (lambda (first rest)
            (if (null? rest)
              (if (cond-else-form? first)
                ;make BEGIN expantion
                (parse `(begin ,@(cdr first)))
                ;else make IF & BEGIN expantion 
                    (parse `(if ,(car first) (begin ,@(cdr first)))))
              ;else make IF & BEGIN expantion 
                  (parse `(if ,(car first) (begin ,@(cdr first)) (cond ,@rest))))))


    ; LET
    (pattern-rule
        `(let ((,(? 'var var?) ,(? 'val)) . ,(? 'rest let-var?)) ,(? 'expr) . ,(? 'exprs))
          (lambda (var val rest expr exprs)
                  (let ((params `(,var ,@(car-map rest)))
                (vals `(,val ,@(cadr-map rest))))
                  (parse `((lambda ,params ,expr . ,exprs) ,@vals)))))

  ; LET empty
    (pattern-rule
        `(let () ,(? 'body) . ,(? 'exprs))
        (lambda (body exprs)
          (parse `((lambda () ,body . ,exprs)))))

  ; LET* (added cheking null? rest)
    (pattern-rule
     `(let* ((,(? 'var var?) ,(? val?)) . ,(? 'rest)) . ,(? 'exprs))
     (lambda (var val rest exprs)
        (if (null? rest)
                (parse `(let ((,var ,val)) . ,exprs))
          (parse `(let ((,var ,val))
                (let* ,rest . ,exprs))))))

  ; LET* empty (changed Mayer's)
    (pattern-rule
     `(let* () ,(? 'expr ) . ,(? 'exprs list?))
     (lambda (expr exprs) 
      (parse `(,`(lambda () ,expr . ,exprs)))))
   
  ; LETREC empty
        (pattern-rule 
          `(letrec () ,(? 'expr) . ,(? 'exprs list?))
            (lambda (expr exprs)
              (parse `(let () ,`(let () ,expr . ,exprs)) )))

  ; LETREC // maybe split to let* and letrec
        (pattern-rule
            `(letrec ((,(? 'var var?) ,(? 'val)) . ,(? 'rest-bigings let-var?)) ,(? 'expr) . ,(? 'exprs))
            (lambda (var val rest-bigings expr exprs)
                (letrec ((vals `(,val  ,@(cadr-map rest-bigings))) ; list of values
                      (vars-init (map (lambda (x) `(,x #f)) `(,var  ,@(car-map rest-bigings)))) ; list of vars
                      (vars-bidings  ; set function for ret addr (enables the rec)
                        (lambda (vars vals) 
                          (if (null? vars)
                            '() 
                          (cons (list 'set! (car (car vars)) (car vals)) 
                            (vars-bidings (cdr vars) (cdr vals)))))))
                  ; build the let expantions
                  (if (null? exprs)
                    (parse `(let ,vars-init ,(beginify `(,@(vars-bidings vars-init vals)
                                        (let () ,expr)))))

                    (parse `(let ,vars-init ,(beginify `(,@(vars-bidings vars-init vals)
                                        (let () ,(beginify (cons expr exprs)))))))))))

        ; QUASI 
          (pattern-rule
                `(,(? 'qexp quasi?) ,(? 'expr))
                    (lambda (qexp expr)
                        (parse (expand-qq expr))))

    ;; add more rules here
    )))
    (lambda (e)
      (run e
     (lambda ()
       (error 'parse
        (format "Unknown form: ~s" e)))))))

(define beginify
  (lambda (s)
    (cond
      ((null? s) *void-object*)
      ((null? (cdr s)) (cdr s))
      (else `(begin ,@s)))))

