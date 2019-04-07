(define (main) ;Code in main was given from the beastie website.
    (setPort (open (getElement ScamArgs 1) 'read))
    (define env this)
	(define (iter expr)
		(if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
		)
	(iter (readExpr))    
)
(define (Stack) (_Stack (list) 0))

(define (_Stack store size) this)

(define (storeEmpty stack)
  (= (length (stack 'store)) 0)
  )

(define (push stack val)
  (_Stack (cons val (stack 'store)) (+ (stack 'size) 1))
  )

(define (pop stack)
  (if (storeEmpty stack)
    nil
   (_Stack (cdr (stack 'store)) (- (stack 'size) 1))
   )
  )

(define (speek stack)
  (if (storeEmpty stack)
    nil
    (car (stack 'store))
    )
  )

(define (ssize stack)
  (stack 'size)
  )

; Implemented using two stacks
; Inbox = stack values are enqueued on
; Outbox = stack values are dequeued from
; Values from the inbox are transferred to the outbox
; if dequeue or qpeek is called
(define (Queue) (_Queue (Stack) (Stack)))
  
(define (_Queue inbox outbox) this)

(define (outboxEmpty queue)
  (= (ssize (queue 'outbox)) 0)
  )

(define (inboxEmpty queue)
  (= (ssize (queue 'inbox)) 0)
  )

(define (enqueue queue val)
  (_Queue (push (queue 'inbox) val) (queue 'outbox))
  )

; Runs in O(length of inbox) if inbox is not empty
; but overall amortized constant time because
; this will only happen on the first time "dequeue" or "qpeek" is called
; after an enqueue
(define (makeOutbox in out)
  (cond 
    ((> (ssize in) 0)
     (define nextVal (speek in))
     (makeOutbox (pop in) (push out nextVal))
     )
    (else
      (_Queue in out)
      )
    )
  )

(define (dequeue queue)
  (cond 
    ((outboxEmpty queue)
     (if (inboxEmpty queue)
       nil
       (dequeue (makeOutbox (queue 'inbox) (queue 'outbox)))
       )
     )
    (else (_Queue (queue 'inbox) (pop (queue 'outbox))))
    )
  )

(define (qpeek queue)
  (cond
    ((outboxEmpty queue) 
     (if (inboxEmpty queue)
       nil
       (qpeek (makeOutbox (queue 'inbox) (queue 'outbox)))
       )
     )
    (else (speek (queue 'outbox)))
    )
  )

(define (qsize queue)
  (+ (ssize (queue 'inbox)) (ssize (queue 'outbox)))
)
