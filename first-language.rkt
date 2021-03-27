#lang eopl
;Andres Felipe Arrechea Saa - 1780023
;Nicolas Jaramillo Mayor - 1840558
; _________________________________________________________________________
;| <programa>   ::= <expresion>                                            |
;|                  un-program (exp)                                       |
;| <expresion>  ::= <numero>                                               |
;|                  num-lit (n)                                            |
;|              ::= (<expresion> <operacion> <expresion>)                  |
;|                  exp-lit (exp1 op exp2)                                 |
;|              ::= <identificador>                                        |
;|                  variable (id)                                          |
;|              ::= var (identificador = <expresion>)* in <expresion>      |
;|                   declaracion (ids exps cuerpo)                         |
;| <operacion>  :=  + - * /                                                |
;|                  primitiva                                              |
;|_________________________________________________________________________|

(define lexica
'(
  (white-sp
   (whitespace) skip)
  (comment
   ("//" (arbno (not #\newline))) skip)
  (identificador
   (letter (arbno (or letter digit "?"))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
 )
)

(define gramatica
  '(
   (programa (expresion) un-program)
   (expresion (number) num-lit)
   (expresion ("("expresion operacion expresion")") exp-lit)
   (expresion (identificador) variable)
   (expresion ("var" (arbno "("identificador "=" expresion")") "in" expresion) declaracion)
   (operacion ("+") primitiva-sum)
   (operacion ("-") primitiva-res)
   (operacion ("*") primitiva-mul)
   (operacion ("/") primitiva-div)
   )
)


(sllgen:make-define-datatypes lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))

( define scan&parse
   ( sllgen:make-string-parser
     lexica
     gramatica ) )

(define just-scan
  (sllgen:make-string-scanner lexica gramatica))

(define unparse-op
  (lambda (op)
    (cases operacion op
      (primitiva-sum () "+")
      (primitiva-res () "-")
      (primitiva-mul () "*")
      (primitiva-div () "/")
      )
    )
  )

(define unparse-exp-helper
  (lambda (ids exps)
    (if (equal? ids empty) ""
    (string-append
     "("
     (symbol->string (car ids))
     " = "
     (unparse-exp (car exps))
     ") "
     (unparse-exp-helper (cdr ids) (cdr exps))
     ))
    )
  )
(define unparse-exp
  (lambda (exp)
    (cases expresion exp
      (num-lit (n) (number->string n))
      (exp-lit  (exp1 op exp2)
                (string-append
                 "("
                 (unparse-exp exp1)
                 (unparse-op op)
                 (unparse-exp exp2)
                 ")"))
      (variable (id) (symbol->string id))
      (declaracion (ids exps cuerpo)
                    (string-append
                     "var "
                     (unparse-exp-helper ids exps)
                     "in "
                     (unparse-exp cuerpo)
                     
                    ))
      )
    )
  )

(define unparse
  (lambda (exp)
    (cases programa exp
      (un-program (expresion) (unparse-exp expresion))
      )
    )
  )

(define example1
  (un-program
   (exp-lit
    (variable 'x)
    (primitiva-mul)
    (variable 'y)))
  )
(define example2
  (scan&parse
   "var (x = 9) (y = 4) in 5"
   )
  )