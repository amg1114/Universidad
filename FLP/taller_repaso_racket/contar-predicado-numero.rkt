#lang eopl

(define filtrar
  (lambda (predicado lista)
    (cond
      [(null? lista) '()]
      [(predicado (car lista)) (cons (car lista) (filtrar predicado (cdr lista)))]
      [else (filtrar predicado (cdr lista))]
      )
    )
  )

(define predicado-numero
  (lambda (predicado lista)
      (length (filtrar predicado lista))
    )
  )

(display (predicado-numero number? '(1 "hola" 2 3 4)))