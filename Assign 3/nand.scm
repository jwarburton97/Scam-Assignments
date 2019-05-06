(define NAND_GATE_DELAY 6)

(define (nand-gate a b output) 
    (define (nand)
      (let ((value
             (logic (get-signal a) (get-signal b))))
             
          (after-delay NAND_GATE_DELAY
             (lambda() (set-signal! output value))
            )
         )
      )
  (add-action! a nand)
  (add-action! b nand)
  'ok 
)
(define (logic a b)
    (cond 
		((= 2 (+ a b)) 0) 
		(else 1)
	)
     
)