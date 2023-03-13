#lang eopl

(define predicado-numero
  (lambda (predicado n lista)
    (cond
      [(null? lista) '()]
      [(predicado (car lista)) (list (car lista) (predicado-numero predicado n (cdr lista)))]
      [else (predicado-numero predicado n (cdr lista))]
      )
    )
  )

(define contar-lista
  (lambda (lista)
    (cond
      [(null? lista) 0]
      [else (+ 1 (contar-lista (cdr lista)))]
      )
    )
  )

(define lista-ejemplo (list "A"2 3 4 5 6 9 10 11 12 13))
(display (predicado-numero number? 1 lista-ejemplo))
(display (contar-lista (predicado-numero number? 1 lista-ejemplo)))
(display (length (predicado-numero number? 1 lista-ejemplo)))
  