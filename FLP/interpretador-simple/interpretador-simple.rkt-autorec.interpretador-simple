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
    (expresion ("true") true-exp)
    (expresion ("false") false-exp)
    (expresion ("if" expresion "then" expresion "else" expresion) if-exp)
    (expresion ("let" (arbno identificador "=" expresion) "in" expresion) let-exp)
    (expresion (primitiva "(" (separated-list expresion ",") ")") prim-exp)
    (primitiva ("+") sum-prim)
    (primitiva ("-") min-prim)
    (primitiva ("*") mul-prim)
    (primitiva ("/") div-prim)
    (primitiva ("add1") add-prim)
    (primitiva ("sub1") sub-prim)
    (primitiva ("==") ig-prim)
    (primitiva (">=") mayi-prim)
    (primitiva ("<=") meni-prim)
    (primitiva (">") may-prim)
    (primitiva ("<") men-prim)
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
      (true-exp () #t)
      (false-exp () #f)
      (prim-exp (prim list-exp)
                (let
                    (
                     (lista-val (map (lambda (x) (evaluar-expresion x ambiente)) list-exp))
                     )
                  (evaluar-primitiva prim lista-val)
                  )
                )
      (if-exp (condicion case-t case-f)
              (if (evaluar-expresion condicion ambiente)
                  (evaluar-expresion case-t ambiente)
                  (evaluar-expresion case-f ambiente)
                  )
              )
      (let-exp (ids rands body)
               (let
                   (
                    (lvalues (map (lambda (x) (evaluar-expresion x ambiente)) rands))
                    )
                 (evaluar-expresion body (ambiente-extendido ids lvalues ambiente))
                 )
               )
      )
    )
  )

;; Evaluar Primitivas
(define evaluar-primitiva
  (lambda (prim lista-val)
    (cases primitiva prim
      (sum-prim () (operar-primitiva lista-val + 0))
      (min-prim ()(operar-primitiva lista-val - 0))
      (div-prim ()(operar-primitiva lista-val / 1))
      (mul-prim ()(operar-primitiva lista-val * 1))
      (add-prim () (+ (car lista-val) 1))
      (sub-prim () (- (car lista-val) 1))
      (ig-prim () (= (car lista-val) (cadr lista-val)))
      (mayi-prim () (>= (car lista-val) (cadr lista-val)))
      (meni-prim () (<= (car lista-val) (cadr lista-val)))
      (may-prim ()(> (car lista-val) (cadr lista-val)))
      (men-prim () (< (car lista-val) (cadr lista-val)))
      )
    )
  )

(define operar-primitiva
  (lambda (lval opr term)
    (cond
      [(null? lval) term]
      [else (opr (car lval) (operar-primitiva (cdr lval) opr term))]
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
  (ambiente-extendido '(x y z) '(4 2 5)
                      (ambiente-extendido '(a b c) '(3 2 1)
                                          (ambiente-vacio)
                                          )
                      )
  )


(interpretador)