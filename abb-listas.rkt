#lang eopl
;Andres Felipe Arrechea Saa - 1780023
;Nicolas Jaramillo Mayor - 1840558

(define empty-bintree
  (lambda ()
    '()
    )
  )

(define current-element
  (lambda (node)
    (car node)
    )
  )

(define move-to-left-son
  (lambda (node)
    (cadr node)
    )
  )

(define move-to-right-son
  (lambda (node)
    (caddr node)
    )
  )

(define number->bintree
  (lambda (n)
    (list n (empty-bintree) (empty-bintree))
    )
  )

(define empty-bintree?
  (lambda (tree)
    (if (equal? (empty-bintree) tree)
        #t
        #f
        )
    )
  )

(define at-leaf?
  (lambda (tree)
    (if (and
         (equal? (move-to-left-son  tree) (empty-bintree))
         (equal? (move-to-right-son tree) (empty-bintree))
         )
        #t
        #f
        )
    )
  )

(define bintree-with-at-least-one-child?
  (lambda (tree)
    (not (at-leaf? tree))
    )
  )

(define insert-to-left
  (lambda (n tree)
    (list
     (current-element tree)
     (number->bintree n)
     (move-to-right-son tree)
     )
    )
  )

(define insert-to-right
  (lambda (n tree)
    (list
     (current-element tree)
     (move-to-left-son tree)
     (number->bintree n)
     )
    )
  )

(define is-left-lesser?
  (lambda (tree)
    (cond
      [(empty-bintree? (move-to-left-son tree)) #t]
      [(<=
        (current-element(move-to-left-son tree))
        (current-element tree)
        ) #t]
      [else #f]
      )
    )
  )

(define is-right-greater?
  (lambda (tree)
    (cond
      [(empty-bintree? (move-to-right-son tree)) #t]
      [(>=
        (current-element(move-to-right-son tree))
        (current-element tree)
        ) #t]
      [else #f]
      )
    )
  )

(define bintree-order-validation
  (lambda (tree)
    (cond
      [(empty-bintree? tree) #t]
      [(at-leaf? tree) #t]
      [(and
        (is-left-lesser? tree)
        (is-right-greater? tree)
        )
       (and
        (bintree-order-validation (move-to-left-son tree))
        (bintree-order-validation (move-to-right-son tree))
        )
       ]
      [else #f]
      )
    )
  )

(define insert-element-into-bintree
  (lambda (tree n)
    (cond
      [(empty-bintree? tree) (number->bintree n)]
      [(equal? n (current-element tree)) tree]
      [(and
        (at-leaf? tree)
        (< n (current-element tree))
        )
        (insert-to-left n tree)
        ]
      [(and
        (at-leaf? tree)
        (> n (current-element tree))
        )
        (insert-to-right n tree)
        ]
      [(< n (current-element tree))
       (list
        (current-element tree)
        (insert-element-into-bintree (move-to-left-son tree) n)
        (move-to-right-son tree)
        )
       ]
      [else (list
             (current-element tree)
             (move-to-left-son tree)
             (insert-element-into-bintree (move-to-right-son tree) n)
            )
            ]
      )
    )
  )

(define binTree '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
