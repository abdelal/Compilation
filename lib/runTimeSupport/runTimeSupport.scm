
(define (cdar x) (cdr (car x)))
(define (caar x) (car (car x)))
(define (cddr x) (cdr (cdr x)))
(define (cadr x) (car (cdr x)))
(define (caddr x) (car (cdr (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (caaar x) (car (car (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (caadr x) (car (car (cdr x))))
(define (caaddr x) (car (caddr x)))
(define (cdddar x) (cdr (cddar x)))
(define (caaaar x) (car (caaar x)))
(define (caaadr x) (car (caadr x)))
(define (caadar x) (car (cadar x)))
(define (cadaar x) (car (cdaar x)))
(define (caddar x) (car (cddar x)))
(define (cadadr x) (car (cdadr x)))
(define (cdaaar x) (cdr (caaar x)))
(define (cddaar x) (cdr (cdaar x)))
(define (cddadr x) (cdr (cdadr x)))
(define (cadddr x) (car (cdddr x)))
(define (cdadar x) (cdr (cadar x)))
(define (cdaadr x) (cdr (caadr x)))




(define foldHlp
 (lambda (func acom varList)
              (if (and (pair? varList)
                    (null? (cdr varList)))
					 (car varList)
                    (func (car varList) (foldHlp func acom (cdr varList))))))

	(define  HelpFun 
	(lambda (fstlist sndList)
           (if(null? fstlist) sndList
     		 (cons (car fstlist)
      (HelpFun (cdr fstlist) sndList)))))

(define (append . args)
		(if (null? args)
		args
		(if (null? (cdr args))
		(car args)
		(if (not(or (pair? (car (cdr args))) (null? (car (cdr args)))))
				(HelpFun (car args) (car (cdr args)))	
        (foldHlp  (lambda (sList other)
                                (HelpFun sList other))
                `()
            args)))))



(define mAppend2 (lambda (exp)
          (cond 
            ((null? exp) `()) ;;avoiding error
            ((not (pair? exp)) exp)
            ((null? (car exp)) `())
            ((not (pair? (car exp))) ;;solo arg. 
             (append (list (car exp))
                     (mAppend2 (cdr exp))))
            (else (car exp)))))

 
(define apply
 (lambda (fun . args)
        (let ((args (mAppend2 args)))
            (letrec ((length (lambda (items acc)
                                (if (null? items) acc
                                     (length (cdr items) (+ acc 1))))))
                    (my_apply (length args 0) args fun)
                           ))))
 

(define submap (lambda (funvar args1)
                    (if (null? args1) `()
                        (let ((x (funvar (car args1))))
                            (cons x
                                (submap funvar (cdr args1)))))))


(define map
 (lambda (funvar varList . more_list)

        (let ((nls (cons varList more_list)))
            (if (not(null? (car nls))) 
                (cons (apply funvar (submap (lambda (x) 
                                        (if (not (null? x)) (car x)
                                            `()))
                                    nls))
                    (apply map funvar (submap (lambda (x)
                                            (if (not (null? x)) (cdr x)
                                                `()))
                                        nls)))
					`()
))))
 
 
(define number?
 (lambda (e)
    (or (integer? e)
        (fraction? e))
))

(define rational? number?)

(define list
 (lambda x
        x

 ))
 
 
 
 
 
 
 
 
 
 
 
 
