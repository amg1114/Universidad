#lang eopl
#|
JOHAN ALEJANDRO MORENO GIL - 2160052
JUAN DAVID VALENCIA MONTALVO - 2160103
|#

(define punto
   '(0 0 0 0 0 0 0 0) )

(define listapuntos
   '(
      (1 1 2 2 3 3 4 4)
      (9 9 2 3 1 2 3 2)
      (4 2 1 2 3 2 4 0)
      (3 9 1 2 8 2 3 5)
      (-3 3 -4 2 2 2 -3 1)
      (1 2 3 5 4 5 -2 2)
))


(define calcular-distancia
  (lambda (x y)
    (cond
      [(or (null? x) (null? y)) 0]
      [else  (+ (expt (- (car x) (car y)) 2) (calcular-distancia (cdr x) (cdr y)))]
      )
    )
  )

(define distancia-superpro
  (lambda (punto lista)
    (cond
      [(null? lista) '()]
      [else (ordenar (cons (list (car lista) (sqrt (calcular-distancia punto (car lista)))) (distancia-superpro punto (cdr lista))))]
      )
    )
  )


(define ordenar
  (lambda (l)
    (cond
      [(null? l) '()]
      [else
       (ordenar-aux (car l) (ordenar (cdr l)))
       ]
    )
  )
  )

(define ordenar-aux
  (lambda (tupla l)
    (cond
      [(null? l) (list tupla)]
      [(> (cadar l) (cadr tupla)) (cons tupla l)]
      [else
       (cons
        (car l)
        (ordenar-aux tupla (cdr l))
        )
       ]
      )
    )
  )

(display (distancia-superpro punto listapuntos))