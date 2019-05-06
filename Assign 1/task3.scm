(define(root-n x orig n)
	(define answer (sqroot x orig n))
	(if(good-enough? answer x)
	answer
	(root-n answer orig n)
	)
)
(define (sqroot y x n)
	(/(+(*(- n 1) y)(/ x (^ y(- n 1))))n)
)
(define(good-enough? guess x)
	(if (< (abs (- x guess)) 0.0000000000000001)
		#t
		#f
		)
		)

(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define arg1 (readExpr))
    (define arg2 (readExpr))
    (println "((root-n " arg1 ") " arg2 ") is " (fmt "%.15f" (root-n (real arg2) (real arg2) arg1)))
)