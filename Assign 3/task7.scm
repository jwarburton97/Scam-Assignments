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
		(else (print (stream-car n) ",")
			(sdisplay-Helper (- s 1) (stream-cdr n))
		)
	)
)
(define (symbolic-mystery)
	(println "(mystery x) is $\\cos x$")
)
(define (sop op s t) 
	(cons-stream (op (stream-car s) (stream-car t)) (sop op (stream-cdr s) (stream-cdr t)))
)



(define (sref s n) 
	(cond
	((= n 0) (stream-car s))
	(else (sref (stream-cdr s) (- n 1)))
    )
)
(define (stream-map f s) (cons-stream (f (stream-car s)) (stream-map f (stream-cdr s))))
(define (euler-transformation s)
    (define s0 (real (sref s 0)))
    (define s1 (real (sref s 1)))
    (define s2 (real (sref s 2)))
    (cons-stream
        (if (= (+ s0 (* -2.0 s1) s2) 0.0) 
            s1
            (- s2
                (/ (^ (- s2 s1) 2.0) (+ s0 (* -2.0 s1) s2))     
            )
        )
        (euler-transformation (stream-cdr s))
    ) 
)
(define (tableau xf s)
    (cons-stream s (tableau xf (xf s)))
)

(define (mystery x)
    (define alt-ones 
        (cons-stream 1.0 
            (cons-stream -1.0 alt-ones)
        )
    )
    
    (define twos (cons-stream 2.0 twos))
    (define evens (cons-stream 0.0 (sop + twos evens)))
    (define xes (cons-stream (real x) xes))
    (define x-to-power (sop ** xes evens)) ;numerator

    (define (factorial n)
        (define (fact-iter product counter max-count)
            (if (> counter max-count)
                (real product)
                (fact-iter (* counter product)
                    (+ counter 1.0)
                    max-count)))
        (fact-iter 1.0 1.0 (real n))
    )

    (define even-facts (stream-map factorial evens)) ;denominator

    (sop * (sop / x-to-power even-facts) alt-ones)
)
(define (sums s) 
	(cons-stream 
		(stream-car s) 
		(sop + (stream-cdr s) (sums s))
	)
)
(define (ps-mystery x)
    (sums (mystery x))
)

(define (acc-mystery x)
    (euler-transformation (ps-mystery x))  
)
(define (super-mystery x)
    (stream-map stream-car (tableau euler-transformation (ps-mystery x)))
)