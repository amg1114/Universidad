#lang eopl

(define binary-to-natural
  (lambda (lista)
    (cond
      [(null? lista) 0]
      [(= (car lista) 0) (binary-to-natural (cdr lista))]
      [else (+ (expt 2 (-(length lista) 1)) (binary-to-natural (cdr lista)))]
      )
    )
  )
              
(display (binary-to-natural '(1 0 0 1 1 0 1 1)))