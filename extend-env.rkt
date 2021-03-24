#lang eopl
(define empty-env
  (lambda ()
    (list 'empty-env)
    )
  )

(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)
    )
  )

(define extend-env**
  (lambda (vars vals env)
    (if (null? vars)
        (list env)
        (cons (list (car vars) (car vals))
              (extend-env** (cdr vars)(cdr vals)  env)
              )
        )
    )
  )

(define extend-env*
  (lambda (vars vals env)
    (if (null? vars)
        (list env)
        (cons 'extend-env*
              (cons (list (car vars) (car vals))
                    (extend-env** (cdr vars)(cdr vals)  env)
                    )
              )
        )
    )
  )

(define apply-env
  (lambda (env search-var)
    (cond
      [(eqv? (car env) 'empty-env)
       (report-no-binding-found search-var)]
      
      [(eqv? (car env) 'extend-env)
       (let
           (
            (saved-var (cadr env))
            (saved-val (caddr env))
            (saved-env (cadddr env))
            )
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var)
             )
         )
       ]

      [else (report-invalid-env env)]
      )
    )
  )

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)
    )
  )

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)
    )
  )

(define check-lvl
  (lambda (e)
    (cond
      [(equal? e (empty-env)) 0]
      [(equal? (car e) (empty-env)) 0]
      [(or
        (equal? (car e) 'extend-env)
        (equal? (car e) 'extend-env*)
        )
       (+ (check-lvl (cdr e)) 1)
       ]
      [(and
        (list? (car e))
        (or
         (equal? (caar e) 'extend-env)
         (equal? (caar e) 'extend-env*)
         )
        )
       (check-lvl (car e))
       ]
      [else (check-lvl (cdr e))]
      )
    )
  )

(define extract-env
 (lambda (e)
   (cond
     [(equal? (car e) 'extend-env) (list (cadr e) (caddr e))]
     [(equal? (car e) 'extend-env*) (extract-env (cdr e))]
     [
        (or
         (equal? (caar e) 'extend-env)
         (equal? (caar e) 'extend-env*)
         (equal? (caar e) 'empty-env*)
         )
      empty
      ]
     [else (cons (car e) (extract-env (cdr e))) ]
     )
   )
 )

(define jump-lvl
  (lambda (e)
    (cond
      [(and
        (list? (car e))
        (or
         (equal? (caar e) 'extend-env)
         (equal? (caar e) 'extend-env*)
         (equal? (caar e) 'empty-env*)
         )
        )
       (car e)
       ]
      [else (jump-lvl (cdr e))]
      )
    )
  )

(define check-env
  (lambda (e n)
    (cond
      [(< (check-lvl e) n)
       (eopl:error 'check-env: "Not possible to search depth on environment")]
      [(equal? n 0) '()]
      [(equal? (check-lvl e) n) (extract-env e)]
      [else (check-env (jump-lvl e) n)]
      )
    )
  )


(define e
  (extend-env 'y 8
    (extend-env* '(x z w) '(1 4 5)
      (extend-env 'a 7
         (empty-env)))))