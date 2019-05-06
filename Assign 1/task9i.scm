(define (main)
  (setPort (open (getElement ScamArgs 1) 'read))
  (define arg1 (readExpr))
  (println "(ramanujani " arg1 ") is " (fmt "%.25f" (ramanujani arg1)))
  (println "$4$")
  )

; method that is the whole program
(define (ramanujani x)
	(define (iter count store)
    (cond
		((>= count 0)
			(iter (- count 1) (* (+ count 1) (sqrt (+ (+ 6 count) store))))
		)
		(else
			store
		)
	)
	)
	(iter x 0)
)
; sqrt method
(define (sqrt x)
  (^ x (/ (real 1) 2))
)