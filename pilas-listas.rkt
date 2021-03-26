#lang eopl
;Andres Felipe Arrechea Saa - 1780023
;Nicolas Jaramillo Mayor - 1840558

;;representacion lista
(define empty-stack (lambda ()
                      (list 'empty-stack)))

(define push (lambda (elem stack)
               (cons elem stack)))

(define pop (lambda (stack)
               (cdr stack)))

(define top (lambda (stack)
              (car stack)))

(define  empty-stack? (lambda (stack)
                        (if (equal? stack (empty-stack))
                            #t
                            #f)))





