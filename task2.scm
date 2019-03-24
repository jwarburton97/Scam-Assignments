(define (main) ;Using the main from task1 since were reading in arguments not values.
	(setPort (open (getElement ScamArgs 1) 'read))
    (define env this)
	(define (iter expr)
		(if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
	)
	(iter (readExpr))
)
(define (curry @) ;Required curry function 
	;checks if the parameter argument is the function or if its used later in a nest
	(if (is? (car @) 'CONS)
		; If it is nested call the nested part LENGTH
        (define LENGTH (car @))
        ; Otherwise call the whole thing LENGTH
        (define LENGTH @)
	)
    ; Define need as the amount of parameters needed for the function in LENGTH
    (define (need) (length (get 'parameters (car LENGTH))))
    ; If length of cdr LENGTH (# of args) is less than needed
    (if (< (length (cdr LENGTH)) (need))
        ; Make a lambda that recalls the function with a more flattened list
        (lambda (@) (curry (append LENGTH @)))
        ; Else, apply the function to the rest of the list.
		(apply (car LENGTH) (cdr LENGTH))
    )
)
