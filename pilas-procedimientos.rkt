#lang eopl
;Andres Felipe Arrechea Saa - 1780023
;Nicolas Jaramillo Mayor - 1840558

;representacion procedimental

(define empty-stack
  (lambda ()
    (lambda (var)
      (if (eq? var #t)
          'empty-stack
          (eopl:error "Empty stack")
          )
      )
    )
  )

(define push
  (lambda (elem st)
    (lambda (switch)
      (if (eq? switch #t)
          elem
          st
          )
      )
    )
  )

(define pop
  (lambda (st)
    (st #f)
    )
  )

(define top
  (lambda (st)
    (st #t)
    )
  )

(define empty-stack?
  (lambda (st)
    (if (eqv? st ((empty-stack) #t))
        #t
        #f
        )
    )
  )

;(define st (push 3 ((empty-stack))))
