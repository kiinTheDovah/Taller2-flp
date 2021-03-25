#lang eopl

(define zero (lambda () '() ))

(define is-zero? (lambda (lst)
                   (if (null? lst)
                   #t
                   #f
                   )))


(define successor (lambda (lst)
                    (cond
                      [(and (not (is-zero? lst)) (< (car lst) 31)) (cons (+ (car lst) 1 ) (cdr lst))]
                      [(is-zero? lst) (list 1)]
                      [else (cons 0 (successor (cdr lst)))]
                      )
                    )
  )

(define predecessor (lambda (lst)
                    (cond
                      [(is-zero? lst) "error resultado negaticvo"]
                      [(equal? lst (successor (zero))) (zero)]
                      [(and (<= (car lst) 31) (> (car lst) 0)) (cons (- (car lst) 1) (cdr lst)) ]
                      [else  (cons 31 (predecessor (cdr lst)))]
                      
                      )
                    )
)