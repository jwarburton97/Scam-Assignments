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
(define (sdisplay-Helper s n)	;Helper function to print everything correctly
    ;(inspect n)
	(cond
        ((= s 0) (print "..."))
        (else	
	;		(inspect (stream-car n))
			(print (stream-car n) ",")
			(sdisplay-Helper  (- s 1) (stream-cdr n))	
			
        )
	)
)
(define (ints-from n)
    (cons-stream n 
		(ints-from (+ n 1))
	) 
)
(define (stream-map start end)  
	(cons-stream 
		(start (stream-car end)) 
		(stream-map start (stream-cdr end))
	)
)
(define (ramanujan)
 

(define (stream-shuffle s t)
	;(inspect t) debugging to figure out how to get this working
    (define a (+ (^ (car (stream-car s)) 3) (^ (cadr (stream-car s)) 3)))
    (define b (+ (^ (car (stream-car t)) 3) (^ (cadr (stream-car t)) 3)))
	;(inspect a)
    (cond
		((< a b)
			(cons-stream (stream-car s) (stream-shuffle (stream-cdr s) t)))
		(else
			(cons-stream (stream-car t) (stream-shuffle s (stream-cdr t)))
		)
	)
)
(define (similar s lastDup)
	(cond
		((and (= (stream-car s) (stream-car (stream-cdr s))) (not (= (stream-car s) lastDup)))
			(cons-stream (stream-car s) (similar (stream-cdr s) (stream-car s))))
		(else
			(similar (stream-cdr s) lastDup)
		)
	)
)

(define (pairs stream tree)
	;(inspect s) debugging to figure out the value of s 
	(cons-stream
		(list (stream-car tree) (stream-car stream))
			(stream-shuffle (stream-map (lambda (x) (list (stream-car tree) x)) (stream-cdr stream)) (pairs (stream-cdr tree) (stream-cdr stream)))
	)
)
(define sortedPairs 
	(pairs (ints-from 0) (ints-from 0))
)
(define sortedVals 
	(stream-map (lambda (x) (+ (^ (car x) 3) (^ (cadr x) 3))) sortedPairs)
)
(similar sortedVals 0)
)
