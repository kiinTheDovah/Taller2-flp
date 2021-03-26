#lang eopl
;Andres Felipe Arrechea Saa - 1780023
;Nicolas Jaramillo Mayor - 1840558

(define-datatype stack stack?
  (empty-stack)
  (stack-with-elements (first-elem elem?)
                       (rest-elems stack?)
                 )
  )


(define elem? (lambda (elem) #t))

(define empty-tack (lambda () (empty-stack)))

;push
(define push
  (lambda (elem stk)
    (stack-with-elements elem stk)
    ))

;pop
(define pop
  (lambda (stk)
    (cases stack stk 
      (stack-with-elements (firt-elem rest-elems) rest-elems)
      (else #f)
      )))

;top
(define top
  (lambda (stk)
    (cases stack stk 
      (stack-with-elements (first-elem rest-elems) first-elem)
      (else #f)
      )))



;empty?
(define empty-stack?
  (lambda (stk)
    (cases stack stk
      (empty-stack () #t)
      (else #f))))


;para probar
(define stack-prueba (push 34 (push #t (push `m (push 435 (empty-stack))))))

