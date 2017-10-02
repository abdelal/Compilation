
(define mk15lst
 (lambda (n)
    (if (= n 0) `(0)
        `(,@(mk15lst (- n 1)),n))))

(define ad2lst
 (lambda (wlst add)
    (if (null? add) wlst
          (if(not(member (car add) wlst)) 
            (ad2lst `(,@wlst ,(car add)) (cdr add))
            (ad2lst wlst (cdr add))))))

(define exists?
 (lambda (vs LV-out)
    (if(null? vs) #f
          (if (member (car vs) LV-out) #t
           (exists? (cdr vs)
                            LV-out)))))


(define FMxReg
 (lambda (aSM max_reg)
    (if (null? aSM) max_reg
           (if(not (list? aSM)) max_reg
           (if(number? aSM) (max max_reg aSM)
           (max (FMxReg (car aSM) max_reg)
                     (FMxReg (cdr aSM) max_reg)))))))



(define deadCMD?
 (lambda (cmRGs)
  (let ((crg (caddr (car cmRGs))))
    (if (not( null? crg))
      (not (exists? crg (cadr cmRGs)))
     #t  ))))
 


(define rmvitma
 (lambda (wlst xs)
  (letrec ( (rmvitm (lambda (wlst x)
    (if (member x wlst)
        (if (eq? (car wlst) x)
         (cdr wlst)
               `(,(car wlst),@(rmvitm (cdr wlst) x))) wlst)))) 
   (if (null? xs) 
    wlst
      (rmvitma (rmvitm wlst (car xs)) (cdr xs)))) ))



 
(define ^livenesslst-acc
 (lambda (aSM rgs)
    (if (null? aSM) 
      `()
      `(,`(,(car aSM) ,rgs),@(^livenesslst-acc (cdr aSM) 
    (ad2lst (rmvitma rgs (caddar aSM)) (cadar  aSM)))))))




(define rmvded
 (lambda (aSM-live-in-out)
    (if(null? aSM-live-in-out) `()
          (if (not(deadCMD? (car aSM-live-in-out)))
          `(,(car aSM-live-in-out)
                  ,@(rmvded (cdr aSM-live-in-out)))
                     (rmvded (cdr aSM-live-in-out)))))) 


(define rmvAsm
 (lambda (aSM-live-in-out)
    (let ( (aSM-iter (rmvded aSM-live-in-out))
        (Acommnd (lambda (aSM-live-in-out)(map car aSM-live-in-out))))
        (if (not(equal? aSM-live-in-out aSM-iter))
            (rmvAsm (reverse (^livenesslst-acc (reverse (Acommnd aSM-iter)) (mk15lst (FMxReg (Acommnd aSM-iter) 15)))))
            (Acommnd aSM-iter)))))

 
 (define remww
 (lambda (aSM)
    (if(not (null? aSM))
      (rmvAsm  (reverse (^livenesslst-acc (reverse aSM) (mk15lst (FMxReg aSM 15)))))
     `())))