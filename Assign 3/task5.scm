(define (main) 
	(setPort (open (getElement ScamArgs 1) 'read))
    (define env this)
	(define (iter expr)
		(if (not (eof?)) 
			(begin 
				(eval expr env) 
				(iter (readExpr))
			)
		)
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


(define (add-stream a b)
	(cons-stream
		(+ (stream-car a) (stream-car b))
		(add-stream (stream-cdr a)(stream-cdr b))
	)
)





(define (smush a b)
    (define (trip c d e f)
        
			(if(= f 0) 
				(cons-stream (stream-car c) (trip (stream-cdr c) d e 1))
				(if(= f 1) 
					(cons-stream (stream-car d) (trip c (stream-cdr d) e 2))
					(if(= f 2) 
						(cons-stream (stream-car e) (trip c d (stream-cdr e) 0))
					)
				)
			)
		)
	
	(define (accumulate stream input)
        (define current (b input (stream-car stream)))
        (cons-stream current (accumulate (stream-cdr stream) current))
	)
    (define ones 
		(cons-stream 1 ones)
	)
    (define wholes 
		(cons-stream 0 (add-stream ones wholes))
	)
    (define filler 
		(cons-stream (stream-car a) (accumulate (stream-cdr a) (stream-car a)))
	)
    (cons-stream (stream-car wholes) (trip (stream-cdr wholes) a filler 1))
)


