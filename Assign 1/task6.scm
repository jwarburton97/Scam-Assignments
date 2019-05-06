(define (w f i)
    (cond 
        ((= i 0) (f i))
        (else (/ (real(- (* (S f (+ i 1)) (S f (- i 1))) (square (S f i)))   )
		(real(+ (- (S f (+ i 1)) (* 2 (S f i))) (S f (- i 1))))))
        )
    )


(define (square n)
    (* n n)
)

(define (S f n)
  (define (iter i store)
    (cond
      ((<= i n) (iter (+ i 1) (+ store (f i))))
      (else store)
      )
    )
  (iter 0 0)
)

(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define f (eval (readExpr) this))
    (define arg2 (readExpr))
    (println "(S " f " " arg2 ") is " (fmt "%.15f" (S f arg2)))
    (println "(w " f " " arg2 ") is " (fmt "%.15f" (w f arg2)))
)