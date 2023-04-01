#lang eopl
#|
JOHAN ALEJANDRO MORENO GIL - 2160052
JUAN DAVID VALENCIA MONTALVO - 2160103 
|#

(define arbol_example
  '(10
    (7 (4 3 6) (9 8 ()))
    (15 (12 11 13) (17 16 20))
    )
  )

(define insertar
  (lambda (arbol numero)
    (cond
      [(null? arbol) numero]
      [(and (number? arbol)(< numero arbol)) (list arbol numero '())]
      [(and (number? arbol)(>= numero arbol)) (list arbol '() numero)]
      [(and (list? arbol) (< numero (car arbol)))
       (list (car arbol)
             (insertar (cadr arbol) numero )
             (caddr arbol))]
      [(and (list? arbol) (>= numero (car arbol)))
       (list (car arbol)
             (cadr arbol)
             (insertar (caddr arbol) numero))]
      )
    )
  )

(define insercion-cosmica
  (lambda (arbol  numeros)
    (cond
      [(null? numeros) arbol]
      [else (insercion-cosmica (insertar arbol (car numeros)) (cdr numeros))]
      )
    )
  )

(display (insercion-cosmica arbol_example '(2 3 4 10 22 32 23)))