(define (main) ;Code in main was given from the beastie website.
    (setPort (open (getElement ScamArgs 1) 'read))
    (define env this)
	(define (iter expr)
		(if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
		)
	(iter (readExpr))    
)
(define (range start end raise) ;required range method as outline on beastie
	(define (helper help)
		(cond
			((or
				(and(> raise 0) (< help end)) ;if the step is bigger than 0 and the start is less than the end
				(and(< raise 0) (> help end))) ;or if the step is less than 0 and the start is greater than the end
				(cons help (helper (+ raise help))) ;creates a pair of the values
			)
			(else nil) ;if either of the tests don't pass then return nil
			)
		)
	(helper start)
)
(define (for-loop makeList pass) ; required for-loop method as outlined on beastie
	(cond
		((null? makeList)nil) ;if the list is null return null
		(else (begin
			(pass (car makeList))
			(for-loop (cdr makeList) pass) ;
		))
	)
)