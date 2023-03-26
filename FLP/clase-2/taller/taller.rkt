#lang eopl

#|

<arbol-b> ::= <numero>
          ::= <simbolo><arbol-b><arbol-n>

|#



(define super-mapeo
  (lambda (funciones arbol)
    (cond
      [(null? funciones) '()]
      [else (cons (mapear-arbol (car funciones) arbol) (super-mapeo (cdr funciones) arbol))]
      )
    )
  )

(define mapear-arbol
  (lambda (funcion arbol)
    (cond
      [(null? arbol) '()] 
      [(number? arbol) (funcion arbol)]
      [else (cons (car arbol)(append (list (mapear-arbol funcion (cadr arbol))) (list (mapear-arbol funcion (caddr arbol)))))]
      )
    )
  )

(define arbol1
  '(
    x
    (y (z 1 2) (p 2 3))
    (w (a (a 9 10) 13) (t 1 2))
    )
  )

(define lf
  (list (lambda (x) (* x 2))
        ;(lambda (x) (+ x 2))
        ;(lambda (x) (/ x 8))
        )
  )
(display "Punto 1")
(newline)
(display (super-mapeo lf arbol1))


#| ----- Punto 2 ----- |#


(define common-subtree
  (lambda (arb1 arb2)
     (cond
       [(or
         (null? arb1)
         (null? arb2))
        '()]
       [(and
         (number? arb1)
         (number? arb2))
        (list 0)]
       [else
         (list
          0
          (common-subtree (cadr arb1) (cadr arb2)); Lado izquierdo
          (common-subtree (caddr arb1) (caddr arb2)))])))

(define arbol2
  '(
    a
    (b (d 1 2) 2)
    (c (a 2 13) (c 1 4))
    )
  )

(define busqueda-increible-arbolea
  (lambda (arb1 arb2 fn)
    (cond
      [(or (null? arb1) (null? arb2)) '()]
      [(and (number? arb1) (number? arb2)) (list (fn arb1 arb2))]
      [(or (and (number? arb1) (list? arb2)) (and (number? arb2) (list? arb1))) '()]
      [else (append (busqueda-increible-arbolea (cadr arb1) (cadr arb2) fn)
                    (busqueda-increible-arbolea (caddr arb1) (caddr arb2) fn)
                         
                  )]
      )
    )
  )

(newline)
(display (busqueda-increible-arbolea arbol1 arbol2 (lambda (x y) (+ x y 2))))