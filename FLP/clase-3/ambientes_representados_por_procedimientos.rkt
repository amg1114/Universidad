#lang eopl

;; Constructuoras

(define empty-env
  (lambda ()
    (lambda (signal)
      (cond
        [(= signal 0) 'empty-env]
        [else (eopl:error "Señal no valida")]
        )
      )
    )
  )

(define extend-env
  (lambda (id val env)
    (lambda (signal)
      (cond
        [(= signal 0) 'extend-env]
        [(= signal 1) id]
        [(= signal 2) val]
        [(= signal 3) env]
        )
      )
    )
  )
        

;; ------- Observadoras --------
;; Predicados
(define empty-env?
  (lambda (x)
    (equal? (x 0) 'empty-env)
    )
  )

(define extend-env?
  (lambda (x)
    (equal? (x 0) 'extend-env)
    )
  )

;; Extractoras
(define extend-env->id
  (lambda (env)
    (env 1)
    )
  )
(define extend-env->value
  (lambda (env)
    (env 2)
    )
  )
(define extend-env->env
  (lambda (env)
    (env 3) 
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

