#lang eopl

;; Especificacion Lexica
(define especificacion-lexica
  '(
    (espacio-blaco (whitespace) skip)
    (comentario ("%" (arbno (not #\newline))) skip)
    (identificador (letter (arbno letter digit "?" "#")) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit "." digit (arbno digit)) number)
    (numero ("-" digit "." digit (arbno digit)) number)
    )
  )

;; Especificacion Gramatical
(define especificacion-gramatical
  '(
    (programa (expresion) a-program)
    (expresion (numero) lit-exp)
    (expresion (identificador) var-exp)
    (expresion (primitiva "(" (separated-list expresion ",") ")") prim-exp)
    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mul-prim)
    (primitiva ("/") div-prim)
    (primitiva ("add1") add-prim)
    (primitiva ("sub1") sub-prim)
    )
  )

(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

;; Interpretador
(define eval-program
  (lambda (program)
    program
    )
  )

(define interpretador
  (sllgen:make-rep-loop ">" eval-program
                        (sllgen:make-stream-parser especificacion-lexica especificacion-gramatical)
                        )
  )
(interpretador)