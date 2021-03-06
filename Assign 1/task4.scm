(define (main)
	(setPort (open (getElement ScamArgs 1) 'read))
	(define arg (readExpr))
	(define arg2(readExpr))
	(define arg3(readExpr))
	
	(println "((crazyTriangle " arg " " arg2 ") " arg3 ")")
	((crazyTriangle arg arg2)arg3)
)
(define (crazyTriangle left right)
    (lambda (height) 
        (define (helper source store)           
            (cond 
                ((<= source store))             
                (else 
                (print-spacing store height)
                (print-row store left right)
                (helper source (+ store 1))
                )
            )
        )
        (helper height 0)
    )
)

(define (print-spacing row total)
    (define (iter source) 
        (cond 
            ((= (- total 1) source))           
            (else 
               (print " ")
                (iter (+ source 1))
            )
        )
    )
    (iter row)
)

(define (print-row row left right)
    (define (iter source)
        (cond 
            ((= row 0) (println left))
            ((= source row) (println right))
            (else 
                (print ((pascal-at left right) row source) " ")
                (iter (+ source 1))
            )
        )
    )
    (iter 0)
)


(define (pascal-at left right)
    (lambda (row col)
        (cond 
            ((= row col) right)
            ((= col 0) left)
            (else (+ ((pascal-at left right) (- row 1) (- col 1)) ((pascal-at left right) (- row 1) col)))
        )
    )
)
