#lang eopl

#|

<lista-S> ::= '()
          ::= <lst><lista-S>
<lst> ::= <sibolo>
      ::= <simbolo><lst>
|#

(define lista-S1 '())
(define lista-S2 '(x))
(define lista-S3 '(x (s k) (s k t) y))
(define lista-S4 '(x (s k) (3 k t) y))

#|---------- PUNTO A ---------|#

(define in-S?
  (lambda (el)
    (cond
      [(not (list? el)) #f]
      [(null? el) #t]
      [else (and (in-LST? (car el)) (in-S? (cdr el)))]
      )
    )
  )

(define in-LST?
  (lambda (el)
    (cond
      [(symbol? el) #t]
      [(list? el) (and (in-LST? (car el)) (in-S? (cdr el)))]
      [else #f]
      )
    )
  )
#|---------- FIN PUNTO A ---------|#

#|---------- PUNTO B ---------|#
(define buscar-simbolo
  (lambda (lista symbol)
      (cond
        [(null? lista) #f]
        [(equal? (car lista) symbol) #t]
        [(list? (car lista)) (or (buscar-simbolo (car lista) symbol) (buscar-simbolo (cdr lista) symbol))]
        [else (buscar-simbolo (cdr lista) symbol)]
        )
    )
  )
#|---------- FIN PUNTO B ---------|#