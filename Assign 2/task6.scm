(define (main)
	(setPort(open(getElement ScamArgs 1) 'read))
	(setNilDisplay 'nil)
	(define env this)
	(define (iter expr)
		(if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
    )
	(iter (readExpr))
)
(define (Find earned given)
	(map
		(lambda (X)
			(cons earned X)
		)
		given
	)
)
(define (powerSet S)
	(setNilDisplay 'nil)
	(cond
		((null? S) '(()) )
		(else
			(let ( (given (powerSet (cdr S))) (earned (car S)) )
				(merge given (Find earned given))
			)
		)
	)
)
(define (merge a b)
	(cond
		((null? a) b)
		((null? b) a)
		((or (null? (car a)) (< (length (car a)) (length (car b))))
			(cons (car a) (merge (cdr a) b))
		)
		(else
			(cons (car b) (merge a (cdr b)))
		)
	)
)