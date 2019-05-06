(define (main) 
	(setPort (open (getElement ScamArgs 1) 'read))
    (define env this)
	(define (iter expr)
		(if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
	)
	(iter (readExpr))
)

(define (scoping symbol object)
	(cond
		((local? symbol object) 'bound)
		(else
			(define (scopeIterator symbol2 environment)
				(cond
					((null? environment) 'undefined)
					((local? symbol2 environment) 'free)
					(else
						(scopeIterator symbol2 (environment'__context))
						)
					)
				)
			(scopeIterator symbol (object'__context))
			)
		)
)