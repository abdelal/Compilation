(load "pc.scm")

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
	 (new (*parser (char #\newline))
	      (*parser <end-of-input>)
	      (*disj 2)
	      done))) (new (*parser (char #\;))
	 
	 (*parser <any-char>)
	 (*parser <end-of-line-comment>)
	 *diff *star

	 (*parser <end-of-line-comment>)
	 (*caten 3)
	 done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <Sexpr>))
       (*caten 2)
       done))


(define <comment>
  (disj <line-comment>
	<sexpr-comment>))

(define <skip>
  (disj <comment>
	<whitespace>))

(define <line-comment-ie>
  (new (*parser (word "#;"))
       (*delayed (lambda () ie))
       (*caten 2)
       done))


(define <comment-ie>
  (disj <line-comment-ie>
	<whitespace>))

(define <skip-ie>
  (disj <comment-ie>
	<whitespace>))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
	   (*parser <p>)
	   (*parser <wrapper>)
	   (*caten 3)
	   (*pack-with
	    (lambda (_left e _right) e))
	   done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))
(define ^<skipped-ie*> (^^<wrapped> (star <skip-ie>)))



(define <digit>
  (range #\0 #\9))

(define <digit+>
  (range #\1 #\9))



(define <SymbolNotANumber>
  (new (*delayed(lambda() <infixSymbol>))
       (*delayed(lambda() <NumberNotSymbol>))
       *diff

   done
       ))

(define <Natural>
  (new (*parser (char #\0))*star
       (*parser <digit+>)
       (*caten 2)
       (*pack-with(lambda(a b) b))
       (*parser <digit>) *star
       (*caten 2)
       (*pack-with
	(lambda (a s)
	  (string->number
	   (list->string
	    `(,a ,@s)))))

       (*parser (char #\0))*plus
       (*pack (lambda (_) 0))
       (*disj 2)
       done))


(define <Integer>
  (new (*parser (char #\+))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
	(lambda (++ n) n))

       (*parser (char #\-))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
	(lambda (-- n) (- n)))

       (*parser <Natural>)

       (*disj 3)
       done))


(define <Fraction>
  (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*guard (lambda (n) (not (zero? n))))
       (*caten 3)
       (*pack-with
	(lambda (num div den)
	  (/ num den)))
       done))

(define <Number>
  (new  (*parser <Fraction>)
        (*parser <Integer>) 
	(*disj 2)
       done))

(define <NumberNotSymbol>
  (new (*parser <Number>)
       (*delayed (lambda() <SymbolChar>))
       (*delayed (lambda() <math>))
       *diff
       (*parser <digit>)
       *diff
   *not-followed-by
   done))

(define <eNumberNotSymbol>
  (new
   (*parser <Number>)
   (*delayed (lambda() <SymbolChar>))
     (*parser <digit>)
   *diff
   *not-followed-by
   done))


(define <Boolean>
  (new (*parser (word-ci "#t"))
       (*pack
	(lambda (_) #t))

       (*parser (word-ci "#f"))
       (*pack
	(lambda (_) #f))

       (*disj 2)
       done))

(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word-ci str))
	 (*pack (lambda (_) ch))
	 done)))

(define <StringMetaChar>
  (new (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
        (*parser (^<meta-char> "\\l" (integer->char 955)))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page)) ; formfeed
       
       (*disj 7)
       done))


(define <CharPrefix>
  (new (*parser (char #\#))
       (*parser (char #\\))
       (*caten 2)

      done))



(define <VisableChar>
  (const (lambda (ch)
	   (if (> (char->integer ch)  32)
	       #t
	       #f
	       ))))

(define <alpha>
  (new (*parser (range #\a #\z))
       (*parser (range #\A #\Z))
       (*disj 2)
       (*parser <digit>)
       (*disj 2)
       done))


(define <VisibleSimpleChar>
  (new(*parser <VisableChar>)
    (*parser <alpha>)
     *plus
      *not-followed-by
      
      
	done))


(define <HexChar>
  (new (*parser <digit>)
     (*parser (range-ci #\a #\f))
       (*disj 2)
  	 done))



(define <HexUnicodeChar>
  (new (*parser (char #\x))
       (*parser <HexChar>)
       *plus
        (*guard (lambda (hexchar) 
        (and (>=(string->number (list->string hexchar) 16) 0)
	     (< (string->number (list->string hexchar) 16) 1114112))))
       
       (*pack (lambda(hex_chars)
               (integer->char (string->number
             (list->string (cons #\# (cons #\x hex_chars )))))
                 )
              )
       (*caten 2)
       (*pack-with (lambda (a s) s))
       done))


(define <StringHexChar>
  (new  (*parser (char #\\))
        (*parser (char #\x))
	(*caten 2)	
	(*parser <HexChar>)
        *star
	(*guard (lambda (ls) 
	(and (>=(string->number (list->string ls) 16) 0)
	     (< (string->number (list->string ls) 16) 1114112))))

	       (*pack (lambda(hex_chars)
               (integer->char (string->number
             (list->string (cons #\# (cons #\x hex_chars )))))
                 )
              )
	   (*parser (char #\;))
	   
	(*caten 3)
	(*pack-with (lambda (prefix hexChar suffix)
	 hexChar))

	
	
      done ))

(define <NamedChar>
  (new (*parser (^<meta-char> "lambda" (integer->char 955)))
       (*parser (^<meta-char> "newline" #\newline))
       (*parser (^<meta-char> "return" #\return))
       (*parser (^<meta-char> "tab" #\tab))
       (*parser (^<meta-char> "page" #\page))
       (*parser (^<meta-char> "space" #\space))
       (*parser (^<meta-char> "nul" #\nul ))
       (*disj 7)

   done))



(define <Char>
  (new  (*parser <CharPrefix>)
        (*parser <HexUnicodeChar>)
        (*parser <NamedChar>)
        (*parser <VisibleSimpleChar>)
  
        (*disj 3)

       (*caten 2)
       (*pack-with
        (lambda (a s) s))
 
       done))




(define <SymbolChar>
  (new 
       (*parser <digit>)
       (*parser (range #\a #\z))
       (*parser (range #\A #\Z))
       (*pack (lambda (ch)(integer->char (+ (char->integer ch) 32))))
       (*parser (char #\!) )
       (*parser (char #\$ ) )
       (*parser (char #\^ ) )
       (*parser (char #\* ) )
       (*parser (char #\-) )
       (*parser (char #\_) )
       (*parser (char #\=) )
       (*parser (char #\+) )
       (*parser (char #\<) )
       (*parser (char #\>) )
       (*parser (char #\?) )
       (*parser (char #\/) )
       (*disj 15)

       
     done))


(define <Symbol>
  (new (*parser <SymbolChar>)
       *plus
   
           (*pack
       (lambda (ch) (string->symbol (list->string ch))))
       done))

(define <StringLiteralChar>
  (new (*parser <any-char>)
       (*parser (char #\\))
       *diff
   done))


(define <StringChar>
  (new  (*parser <StringMetaChar>)
	  (*parser <StringHexChar>)
	(*parser <StringLiteralChar>)
    
       (*disj 3)
      
       done))

  (define <String>
  (new (*parser (char #\"))
       (*parser <StringChar>)
       (*parser (char #\"))
       *diff
       *star
       (*parser (char #\"))
       (*caten 3)

     (*pack-with
      (lambda (od chars cd )
	(list->string chars)))

       
       done))





(define <ProperList>
  (new (*parser (char #\())
       (*parser (delay (lambda() <Sexpr>)))
       *star
       (*parser (char #\)))
       (*caten 3)
       (*pack-with (lambda (lb sexprz rb)
                   sexprz))
       done))




(define <Vector>
  (new (*parser (char #\#))
       (*parser (char #\())
       (*caten 2)
       (*parser (delay (lambda() <Sexpr>)))
       *star
       (*parser (char#\) ))
       (*caten 3)
       (*pack-with (lambda (lb sexprz rb)
                   (list->vector sexprz)))
       done))



(define <ImproperList>
  (new (*parser (char #\())
       (*parser (delay (lambda() <Sexpr>)))
       *plus
       (*parser (char #\.))
       (*parser (delay (lambda() <Sexpr>)))
       (*parser (char #\)))
       (*caten 5)

      (*pack-with (lambda (lb sexpr1 dot sexpr2 rb)
                (append sexpr1 sexpr2)))
       done))


(define <Quoted>
  (new (*parser (char #\'))
       (*parser (delay (lambda() <Sexpr>)))
       (*caten 2)
        (*pack-with (lambda (w1 sexprz) (list 'quote sexprz)))
       done))



(define <QuasiQuoted>
  (new (*parser (char #\`))
       (*parser (delay (lambda() <Sexpr>)))
       (*caten 2)
       (*pack-with (lambda (q sexprz) (list 'quasiquote sexprz)))
       done))



(define <Unquoted>
  (new (*parser (char #\,))
       (*parser (delay (lambda() <Sexpr>)))
       (*caten 2)
      (*pack-with (lambda (q sexprz) (list 'unquote sexprz)))
       done))


(define <UnquoteAndSpliced>
  (new (*parser (char #\,))
       (*parser (char #\@))
       (*caten 2)
       (*parser (delay (lambda() <Sexpr>)))
       (*caten 2)
      (*pack-with (lambda (q sexprz) (list 'unquote-splicing sexprz)))
       done))


(define <PowerSymbol>
  (new (*parser (char #\^))
      
       (*parser (char #\*))
       (*parser (char #\*))
       (*caten 2) 
        
       (*disj 2)

             
   done))



(define <InfixPrefixExtensionPrefix>
  (new (*parser (char #\#))
       (*parser (char #\#))
       (*caten 2)

       (*parser (char #\#))
       (*parser (char #\%))
       (*caten 2)
     
       
       (*disj 2)


   done))

(define <math>
  (new (*parser <PowerSymbol>)
       (*parser (char #\/))
       (*parser (char #\*))
       (*parser (char #\+))
       (*parser (char #\-))
       (*disj 4)
       (*disj 2)
    
   done))

(define <infixSymbol>
  (new (*parser <SymbolChar>)
       (*parser <math>)
       *diff
       *plus
       (*pack
       (lambda (ch) (string->symbol (list->string ch))))
      
  done))
 

(define <InfixExtension>
   (^<skipped-ie*>
  (new (*parser <InfixPrefixExtensionPrefix>)
       (*delayed (lambda() ie))

       (*caten 2)
             (*pack-with (lambda (a exp) exp))
   done)))




(define <infixParen>
  (new (*parser (char #\( ))
        (*delayed (lambda() ie))
       (*parser (char #\) ))
       (*caten 3)
       (*pack-with (lambda (p1 exp p2) exp))
   done))



(define <infixSexprEscape>
  (new (*parser <InfixPrefixExtensionPrefix>)
       (*delayed (lambda() <Sexpr>))
       (*caten 2)
       (*pack-with
	(lambda (pre sexpr) sexpr))
   done))



(define arglist
  (^<skipped-ie*>
  (new (*delayed (lambda() basic))
       (*parser (char #\,))
       (*delayed (lambda() basic))
       (*caten 2)
       (*pack-with
	(lambda (sign exp) exp))
       *star
       (*caten 2)
       (*pack-with
	(lambda (arg1 cont)
	  (fold-left (lambda(arg1 cont) (;append
					 cons arg1 cont)) (list arg1) cont)))
	 ; (cons arg1 cont)))
       (*parser <epsilon>)
       (*disj 2)
       done)))


(define func
  
  (new (*delayed (lambda () basic))
       (*parser(char #\())
       ;------------------------------------------------arglist----------------------------------------------------
        (*parser <skip>) *star
	(*caten 2)
	 (*pack-with (lambda(par space)
                    par))
	 (*delayed (lambda() ie))
	 (*caten 2)
	  (*pack-with (lambda(pra expr)
                    expr))
        (*parser(char #\,))
        (*delayed (lambda() ie))
        (*caten 2) 
        (*pack-with (lambda(comma expr)
                    expr))
        *star 
        (*parser(char #\)))
	(*caten 2)
	 (*pack-with (lambda(expr pra)
                    expr))
        (*caten 2)
      (*pack-with
	(lambda ( expr1 expr2 )
	  (if (and  (not(null? expr1)) (not(null? expr2)))  (append (cons expr1 expr2)) 
	  (if  (and (null? expr2) (not(null? expr1))) (list expr1)
 	       (list)
        
	   ))))
      *plus
      ;------------------------------------------------arglist----------------------------------------------------
      (*parser(char #\())
      (*parser <skip>) *star
      (*caten 2)
      	 (*pack-with (lambda(bra space)
                    bra))
        (*parser(char #\)))
        (*caten 2)
        (*pack-with (lambda(p1 p2)
        (list)))
         *plus
     (*disj 2)
        (*caten 2)            
   

    (*pack-with (lambda (basic cont)
                       (if (null? cont)
                          basic
                        (fold-left
	   (lambda (op elem) (append (list op) elem))
	    basic cont))))
    done))



(define arr
(^<skipped-ie*>
 (new (*parser func)
      (*delayed (lambda () basic))
      (*disj 2)
         (*parser(char #\[))
         (*delayed (lambda() ie))
	 	(*caten 2)
	  
       (*pack-with
	(lambda(bra expr)
	  expr))
       (*parser(char #\]))
       	(*caten 2)
	  
       (*pack-with
	(lambda(expr bra)
	  expr))

          *plus
         (*caten 2)
         (*pack-with (lambda (basic cont)
		       (fold-left
			(lambda(basic cont)
                (if (null? cont)
                        basic
                (append (list 'vector-ref basic  cont))))
			basic cont)))

	 
	done)))



(define pow
  (^<skipped-ie*>
  (new  (*parser func)
	(*parser arr)
         (*delayed (lambda() basic))
	 (*disj 3)
         (*parser <PowerSymbol>)
         (*caten 2)*star
         (*parser arr)
	 (*parser func)
         (*delayed (lambda() basic))
	 (*disj 3)     
             (*caten 2)
             (*pack-with (lambda (cont basic)
                  (fold-left
                    (lambda (basic cont)
                      (if (not(null? (cdr cont)))
                     (append `(expt) (list (car cont) basic))
		        (car basic)))
                        basic (reverse cont)) ))
	     done)))


(define neg
   (^<skipped-ie*>
  (new (*parser pow)
       (*parser (char #\-))
       (*parser pow)
       (*caten 2)
       (*pack-with (lambda (op exp) `(-,exp)))

       (*disj 2)
          done)))



(define muldiv
   (^<skipped-ie*>
  (new (*parser neg)
       (*parser (char #\*))
       (*parser (char #\/))
       (*disj 2)
       (*parser neg)
       (*caten 2)
       (*pack-with
	(lambda (sign cont)
	  (lambda (exp)
	    (list (string->symbol (string sign)) exp cont))))
       *star
       (*caten 2)
       (*pack-with
	(lambda (arg1 cont)
	  (fold-left
	   (lambda (op elem) (elem op))
	   arg1 cont)))
       done)))

(define addsub
    (^<skipped-ie*>
  (new (*parser muldiv)
          (*parser (char #\+))
       (*parser (char #\-))
       (*disj 2)
       (*parser muldiv)
       (*caten 2)
       (*pack-with
	(lambda (sign cont)
	  (lambda (exp)
	    (list (string->symbol (string sign)) exp cont))))
       *star
       (*caten 2)
       (*pack-with
	(lambda (arg1 cont)
	  (fold-left
	   (lambda (op elem) (elem op))
	   arg1 cont)))
       
       done)))

(define basic
    (^<skipped-ie*>
  (new (*parser <SymbolNotANumber>)
        (*parser <Number>)
   	(*parser <infixParen>) 
	(*parser <infixSexprEscape>)
  
      (*disj 4)
      done)))

(define ie
  (^<skipped-ie*>
   (new (*parser addsub)
       done)))



(define <Sexpr>
  (^<skipped*>
  (new (*parser <Boolean>)
       (*parser <Char>)
       (*parser <String>)
       (*parser <eNumberNotSymbol>)
       (*parser <Symbol>)
       (*parser <ProperList>)
       (*parser <ImproperList>)
       (*parser <Vector>)
       (*parser <Quoted>)
       (*parser <QuasiQuoted>)
       (*parser <Unquoted>)
       (*parser <UnquoteAndSpliced>)
       (*parser  <InfixExtension>)
       (*disj 13)
       done)))
