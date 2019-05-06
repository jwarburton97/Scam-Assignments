(define (main) 
	(setPort (open (getElement ScamArgs 1) 'read))
    (define env this)
	(define (iter expr)
		(if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
	)
	(iter (readExpr))
)

(define (sdisplay s n)
	(print "(")
	(sdisplay-Helper s n)
	(print ")")
)
(define (sdisplay-Helper s n)
	(cond
		((= s 0) (print "..."))
		(else 
			(print (stream-car n) ",")
			(sdisplay-Helper (- s 1) (stream-cdr n))
		)
	)
)
(define (same-stream? s t count thresh)
   
        (if(= count 0) 
			#t
		    (if(<= (abs (- (stream-car s) (stream-car t))) thresh) 
				(same-stream? (stream-cdr s) (stream-cdr t) (- count 1) thresh)
				#f
			)
        )
)
(define (integrate a b)
	(define (solver-area x y)
		(*(/(+ x y) 2) b)
	)
	(define (integration c d)
		(cons-stream (+ (stream-car d) (solver-area (stream-car c) (stream-car (stream-cdr c)))) (integration(stream-cdr c)(stream-cdr d)))
	)
(define helper
	(cons-stream(real 0) (integration a helper)))
)

(define(quad a b c d)
	(define (mover f) ; stepper
		(cons-stream (+(stream-car f) (real d)) (mover (stream-cdr f)))
	)
	(define mover-helper ; x
		(cons-stream (real 0) (mover mover-helper))
	)
	(define (solver-square e) ; square
		(* e e)
	)
	
	(define (get-value value);get-f
		(+ (* a (solver-square value)) (* b value) c)
	)
	(define (stream g);f-stream
		(cons-stream (get-value (stream-car g)) (stream (stream-cdr g)))
	)
	(define stream-helper ;f
		(cons-stream (get-value (stream-car mover-helper)) (stream (stream-cdr mover-helper)))
	)
	
)


(define (derivate a b c)
	(define (flip d e)
		(-(*(/ d b) 2) e)
	)
	(define (derivate-helper f g h)
		(let ((i (- (stream-car (stream-cdr f))(stream-car f))))
			(define j (flip i h))
			(cons-stream j (derivate-helper (stream-cdr f) g j))
		)
	)
(cons-stream (real c) (derivate-helper a b c))
)