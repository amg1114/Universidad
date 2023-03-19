#lang eopl

(define filtrar
  (lambda (predicado lista)
    (cond
      [(null? lista) '()]
      [(and (list? (car lista)) (> (length (filtrar predicado (car lista)) ) 0)) (cons (car lista) (filtrar predicado (cdr lista)))] 
      [(predicado (car lista)) (cons (car lista) (filtrar predicado (cdr lista)))]
      [else (filtrar predicado (cdr lista))]
      )
    )
  )

(define predicado-legendario
  (lambda (lista-predicados lista)
    (cond
      [(or (null? lista-predicados) (null? lista)) '()]
      [else (cons (filtrar (car lista-predicados) lista) (predicado-legendario (cdr lista-predicados) lista))]
      )
    )
  )

(define predicados (cons number? (cons string? '() )))

(display (predicado-legendario (list number? string? (lambda (x) (and (number? x) (> x 3)))) '(1 2 3 4 5 "A" ("C" "D"))))