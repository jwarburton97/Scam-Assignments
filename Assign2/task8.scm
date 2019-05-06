(define (main) 
    (setPort (open (getElement ScamArgs 1) 'read))
    (define env this)
	(define (iter expr)
		(if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
		)
	(iter (readExpr))    
)
	(define (node value left right)
        (define (display) (print value))
        this
        )
(define (newBST value)
        (node value nil nil)
        )
(define (displayBST root)
        (define (iter root indent)
            (if (valid? root)
                (begin
                    (iter (root'right) (string+ indent "    "))
                    (print indent)
                    ((root'display))
                    (println)
                    (iter (root'left) (string+ indent "    "))
                    )
                )
            )
        (iter root "")
        )




(define (insertBST tree value) 
	(cond
	((null? tree) (newBST value))
	(	(< value (tree 'value))			
			(if(null? (tree'left))	
					(node (tree'value) (node value nil nil) (tree'right))
					
					(node (tree'value) (insertBST (tree'left) value)(tree'right))
			)
		
		)			
		((> value (tree 'value) )
			(if(null? (tree'right))
				(node (tree'value) (tree'left) (node value nil nil))
				(node (tree'value) (tree'left)(insertBST (tree'right) value))	
			)
		)
	)	
	
)