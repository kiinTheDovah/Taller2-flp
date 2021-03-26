#lang eopl
;Andres Felipe Arrechea Saa - 1780023
;Nicolas Jaramillo Mayor - 1840558

(define diff-one
  (lambda ()
    '(one)
    )
  )

(define diff-one?
  (lambda (l)
    (cond
      [(equal? l (diff-one)) #t]
      [(and (diff-one? (cadr l)) (diff-zero? (caddr l))) #t]
      [(and (diff-zero? (cadr l)) (diff-negOne? (caddr l))) #t]
      [else #f]
      )
    )
  )

(define diff-zero
  (lambda ()
    (list 'diff (diff-one) (diff-one))
    )
  )

(define diff-zero?
  (lambda (l)
    (cond
      [(diff-one? l) #f]
      [(equal? l (diff-zero)) #t ]
      [(and (diff-one? (cadr l)) (diff-one? (caddr l))) #t]
      [(and (diff-negOne? (cadr l)) (diff-negOne? (caddr l))) #t]
      [(and (diff-zero? (cadr l)) (diff-zero? (caddr l))) #t]
      [else #f]
    )
    )
  )

(define diff-negOne
  (lambda ()
    '(diff (diff (one) (one)) (one))
    )
  )

(define diff-negOne?
  (lambda (l)
    (cond
      [(equal? l (diff-negOne)) #t]
      [(or (diff-one? l) (diff-zero? l)) #f]
      [(and (diff-zero? (cadr l)) (diff-one? (caddr l)) ) #t]
      [(and (diff-negOne? (cadr l)) (diff-zero? (caddr l)) ) #t]
      [else #f]
    )
    )
  )

(define diff-successor
  (lambda (l)
    (diff (list 'diff l (diff-negOne)))
    )
  )

(define diff-predecessor
  (lambda (l)
    (diff (list 'diff l (diff-one)))
    )
  )

(define diff
  (lambda (l)
    (cond
      [(diff-one? l) (diff-one)]
      [(diff-zero? l) (diff-zero)]
      [(diff-negOne? l) (diff-negOne)]
      [else l]
      )
    )
  )

(define diff-check-val ;returns the decimal value of the represented diff-tree
  (lambda (l)
    (cond
      [(diff-one? l) 1]
      [(diff-zero? l) 0]
      [(diff-negOne? l) -1]
      [else (- (diff-check-val (cadr l)) (diff-check-val (caddr l)))]
      )
    )
  )

(define diff-tree-plus
  (lambda (t1 t2)
    (list 'diff t1 (list 'diff (diff-zero) t2))
    )
  )
