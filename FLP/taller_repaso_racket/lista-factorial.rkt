#lang eopl

(define factorial
  (lambda (n)
    (cond
      [(= n 0) 1]
      [else (* n (factorial (- n 1)))]
      )
    )
  )

(define lista-factoriales
  (lambda (n)
    (cond
      [(not (number? n)) "Ingrese un numero valido"]
      [(< n 0) '()]
      [else (reverse (list (factorial n) (lista-factoriales (- n 1))))]
     ) 
 )
)

(display (lista-factoriales 5))