#lang eopl


;; Constructuoras

(define empty-env
  (lambda () '(empty-env))
  )

(define extend-env
  (lambda (id val env)
    (list 'extend-env id val env)
    )
  )

;; ------- Observadoras --------
;; Predicados
(define empty-env?
  (lambda (x)
    (equal? (car x) 'empty-env)
    )
  )

(define extend-env?
  (lambda (x)
    (equal? (car x) 'extend-env)
    )
  )

;; Extractoras
(define extend-env->id
  (lambda (env)
    (cadr env)
    )
  )
(define extend-env->value
  (lambda (env)
    (caddr env)
    )
  )
(define extend-env->env
  (lambda (env)
    (cadddr env) 
    )
  )

(define apply-env
  (lambda (sym env)
    (cond
      [(empty-env? env) (eopl:error "Variable " sym "no econtrada")]
      [(and (extend-env? env) (equal? sym (extend-env->id env))) (extend-env->value env)]
      [else (apply-env sym (extend-env->env env))]
     )
    )
  )

;; Area del Programador
(define e 
  (extend-env 'x 3
              (extend-env 'y 5
                          (empty-env)
                          )
              )
  )

