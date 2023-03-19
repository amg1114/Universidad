#lang eopl

(define predicado-numero
  (lambda (predicado n lista)
    (letrec
        (
         (filtrar
          (lambda (p list)
            (cond
              [(null? list) '()]
              [(predicado (car list)) (cons (car list) (filtrar p (cdr list)))]
              [else (filtrar p (cdr list))]
              )
            )
          )
         )
      (cond
        [(>= (length (filtrar predicado lista)) n) #T]
        [else #F]
        )
      )
    )
  )

#|
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
  (lambda (predicado n lista)
    (cond
      [(= n (length (filtrar predicado lista))) #T]
      [else #F]
      )
    )
  )

|#
(display (predicado-numero number? 3 '(1 "hola" 2 3 4)))
  