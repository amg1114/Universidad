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
(display (predicado-numero number? 1 '("A" 2 3 4 5 6)))
(display (contar-lista (predicado-numero number? 1 '("A" 2 3 4 5 6))))

;(display (predicado-numero number? 1 '("A" 2 3 4 5 6)))
;(display (length (predicado-numero number? 1 '("A" 2 3 4 5 6))))
  