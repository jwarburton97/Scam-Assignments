(define (main) 
	(setPort (open (getElement ScamArgs 1) 'read))
    (define env this)
	(define (iter expr)
		(if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
	)
	(iter (readExpr))
)
(define (curry @) 
	(if (is? (car @) 'CONS)
		
        (define LENGTH (car @))
        (define LENGTH @)
	)
    (define (need) (length (get 'parameters (car LENGTH))))
    (if (< (length (cdr LENGTH)) (need))
        (lambda (@) (curry (append LENGTH @)))
		(apply (car LENGTH) (cdr LENGTH))
    )
)