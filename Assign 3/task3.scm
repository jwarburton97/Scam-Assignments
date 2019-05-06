(define (main) ;Code in main was given from the beastie website.
    (setPort (open (getElement ScamArgs 1) 'read))
    (define env this)
	(define (iter expr)
		(if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
		)
	(iter (readExpr))    
)

(define (deque) 
(define head nil)
(define end nil)
(define size 0)
(define (enqueueFront value)	
	(define queueing (queue value nil nil))
	(cond
		((null? head)
			(begin 
				
				(set 'head queueing)
				(set 'size 1)
				(set 'end queueing)
			)
		)
		(else
			(begin
				((queueing 'set-c) head)
				((head 'set-p) queueing)
				(set 'head queueing)
				(++ size)
			)
		)
	)
	value
)

(define (enqueueBack value)
	(define queueing (queue value nil nil))
	(cond
		((null? head)
			(begin
				
				(set 'head queueing)
				(set 'size 1)
				(set 'end queueing)
            )
		)
		(else
			(begin
				((queueing'set-p) end)
				((end'set-c) queueing)
				(set 'end queueing)
				(++ size)
			)
		)
	)
	value
)
(define (enqueueIndex index value)
	(define (iterator start add input value)
		(cond
			((= add input)
				(begin
					(define child ((start'get-c)))
					(define node (queue value start child))
					((start'set-c) node)
					(cond
						((= input size)
							(set 'end node)
						)
						(else
							((child'set-p) node)
						)
					)
				)
			)
			(else
				(iterator ((start'get-c)) (+ add 1) input value)
			)
		)
	)
	(cond
		((= index 0)
			(enqueueFront value)
		)
		(else
			(begin
				(iterator head 1 index value)
				(++ size)
			)
		)
	)
    value
)
(define (dequeueFront)
	(define value ((head'get-v)))
	(set 'head ((head'get-c)))
	(-- size)
	(cond
		((= size 0)
			(set 'end nil)
		)
		(else
			((head'set-p)nil)
		)
	)
	value
)
(define (dequeueBack)
	(define value ((end'get-v)))
	(set 'end ((end'get-p)))
	(-- size)
	(cond
		((= size 0)
			(set 'head nil)
		)
		(else
			((end'set-c)nil)
		)
	)
	value
)
(define (dequeueIndex index)
	(define (iterator start index add)
		(cond
			((= index add)
				(begin
					(define child ((start 'get-c)))
                    (define parent ((start 'get-p)))
                    (define value ((start 'get-v)))
                    ((child 'set-p) parent)
                    (cond
						((!= add 0)
							((parent 'set-c) child)
						)
                        (else
							(set 'head child)
                        )
					)
                    value
				)
			)
			(else
				(iterator ((start'get-p)) index (- add 1))
			)
		)
	)  
	(cond
		((= index (- size 1))
			(dequeueBack)
		)
		(else
			(begin
				(define value(iterator ((end'get-p)) index (- size 2)))
				(-- size)
				value
			)
		)
	)
)
(define (display)
	(define (iterator start)
		(if (not (null? start))
			(begin
				(print ((start 'get-v)))
				(if(not(null?((start 'get-c))))
					(begin
						(print ",")
						(iterator ((start 'get-c)))
					)
				)
			)
		)
	)
(print "[")
(iterator head)
(print "]")
)
(define (peekFront)
	((head 'get-v))
)
(define (peekBack)
	((end 'get-v))
)

this
)

(define (queue value parent child)
	(define value2 value)
	(define parent2 parent)
	(define child2 child)
	(define (set-p parent3)
		(set 'parent2 parent3)
	)
	(define (set-c child3)
		(set 'child2 child3)
	)
	(define(set-v value3)
		(set 'value2 value3)
	)
	(define (get-p)
		parent2
	)
	(define (get-c)
		child2
	)
	(define (get-v)
		value2
	)
	this
)