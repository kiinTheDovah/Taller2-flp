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