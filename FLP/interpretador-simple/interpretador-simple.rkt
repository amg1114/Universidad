#lang eopl

;; Especificacion Lexica
(define especificacion-lexica
  '(
    (espacio-blaco (whitespace) skip)
    (comentario ("%" (arbno (not #\newline))) skip)
    (identificador (letter (arbno letter digit "?" "#")) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit "." digit (arbno digit)) number)
    (numero ("-" digit "." digit (arbno digit)) number)
    )
  )

;; Especificacion Gramatical
(define especificacion-gramatical
  '(
    (programa (expresion) a-program)
    (expresion (numero) lit-exp)
    (expresion (identificador) var-exp)
    (expresion (primitiva "(" (separated-list expresion ",") ")") prim-exp)
    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mul-prim)
    (primitiva ("/") div-prim)
    (primitiva ("add1") add-prim)
    (primitiva ("sub1") sub-prim)
    )
  )

;; Interpretador
(define eval-program
  (lambda (prog)
    (cases programa prog
      (a-program (exp) (evaluar-expresion exp ambiente-inicial))
      )
    )
  )
;; Funcion de Trabajo
(define evaluar-expresion
  (lambda (exp ambiente)
    (cases expresion exp
      (lit-exp (dato) dato)
      (var-exp (id) (apply-env ambiente id))
      (else "ok")
      )
    )
  )


(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)


(define interpretador
  (sllgen:make-rep-loop ">" eval-program
                        (sllgen:make-stream-parser especificacion-lexica especificacion-gramatical)
                        )
  )


;; Ambientes
(define-datatype ambiente ambiente?
  (ambiente-vacio )
  (ambiente-extendido
   (identificadores (list-of symbol?))
   (valores (list-of valor?))
   (ambiente-ex ambiente?)
   )
  )
(define valor?
  (lambda (x)
    #t
    )
  )

(define apply-env
  (lambda (env sym)
    (cases ambiente env
      (ambiente-vacio () (eopl:error "Variable " sym " no Encontrada"))
      (ambiente-extendido (lid lval env-old)
                          (letrec
                              (
                               (buscar-sym
                                (lambda (lid lval sym)
                                  (cond
                                    [(null? lid) (apply-env env-old sym)]
                                    [(equal? (car lid) sym) (car lval)]
                                    [else (buscar-sym (cdr lid) (cdr lval) sym)]
                                    )
                                  )
                                )
                               )
                            (buscar-sym lid lval sym)
                            )
                          )
      )
    )
  )

;; Ambiente Inicial
(define ambiente-inicial
  (ambiente-extendido '(x y z) '(1 2 3)
                      (ambiente-extendido '(a b c) '(3 2 1)
                                          (ambiente-vacio)
                                          )
                      )
  )


(interpretador)