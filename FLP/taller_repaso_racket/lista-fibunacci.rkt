#lang eopl

(define fibunacci
  (lambda (n)
    (cond
      [(or (= n 0) (= n 1)) n]
      [else  (+ (fibunacci (- n 1)) (fibunacci (- n 2)))]
      )
    )
  )

(define lista-fibunacci
  (lambda (n)
    (cond
      [(not (number? n)) "Ingrese un numero valido"]
      [(< n 0) '()]
      [else (reverse (list (fibunacci n) (lista-fibunacci (- n 1))))]
      )
    )
  )


(display (lista-fibunacci 5))