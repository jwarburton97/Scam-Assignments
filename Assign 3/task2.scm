(define (main) 
	(setPort (open (getElement ScamArgs 1) 'read))
    (define env this)
	(define (iterator expr)
		(if (not (eof?)) (begin (eval expr env) (iterator (readExpr))))
	)
	(iterator (readExpr))
)

(define (replace a b)
	 (define (peek c d)
        (cond
			((null? (cddr d))
				(cond 
					((eq? (car d) c)
						(cadr d)
					)
					(else
						c
					)
				)
			)
            (else
				(cond 
					((eq? (car d) c)
						(cadr d)
					)
					(else
						(peek c (cddr d))
					)
				)
			)
		)
	)

	(define (iterator e)
		(cond
			((null? e) e)
			((object? (car e))e)
			((eq? (car e) 'quote) (set-car! e (peek (car e) b)))
			((eq? (type(car e))'CONS) (iterator (car e)) (iterator (cdr e)))
			(else 
				(set-car! e (peek (car e) b)) (iterator (cdr e)))
		)
	)
	(iterator (get 'parameters a))
	(iterator (cadr (get 'code a)))
)


