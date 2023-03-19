#lang eopl

(define multiplicar
  (lambda (n lista)
    (cond
      [(null? lista) '()]
      [(list? (car lista)) (cons (multiplicar n (car lista)) (multiplicar n (cdr lista)))]
      [else (cons (* n (car lista)) (multiplicar n (cdr lista)))]
      )
    )
  )

(define multiplicacion-onfire
  (lambda (n lista [contador 1])
    (cond
      [(<= n 0) '()]
      [(or (= (- contador n) 1) (null? lista)) '()]
      [else (cons (multiplicar contador lista) (multiplicacion-onfire n lista (+ contador 1)))]
      )
    )
  )
(display (multiplicacion-onfire 3 '(1 (1 2) 3 4)))
      
  