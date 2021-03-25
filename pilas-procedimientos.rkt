#lang eopl

;representacion procedimental
(define empty-stack (lambda ()
                        (lambda ()
                          (list 'empty-stack)
                          )))

(define push (lambda ()
               (lambda (elem stack)
                 (cons elem stack))))

(define pop (lambda ()
              (lambda (stack)
                (cdr stack))))


(define top (lambda ()
                   (lambda (stack)
                          (car stack))))

(define empty-stack? (lambda ()              
              (lambda (stack)
                     (if (equal? stack (empty-stack))
                         #t
                         #f
                      )
                     )
              ))
