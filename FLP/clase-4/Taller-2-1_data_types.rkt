#lang eopl

#|
JOHN STIVEN RODAS
JUAN SEBASTIAN MARIN SERNA
JOHAN ALEJANDRO MORENO GIL - 2160052
JUAN DAVID VALENCIA MONTALVO - 2160103
|#

;; Datatype Record
(define-datatype record record?
  (empty-record )
  (non-empty-record (key symbol?) (elm element?) (record record?))
  )

;; Datatype Element
(define-datatype element element?
  (simple-element (simple-item simple-item?))
  (list-element (list-item list-item?))
  )

;; Datatype Item
(define-datatype simple-item simple-item?
  (item-num (datum number?))
  (item-sym (datum symbol?))
  )

(define-datatype list-item list-item?
  (empty-list-item)
  (non-empty-list-item (elm simple-item?) (lst list-item?))
  )

;; Observadores

;; Predicados
(define empty-record?
  (lambda (obj)
    (cases record obj
      (empty-record () #t)
      (else #f)
      )
    )
  )

(define non-empty-record?
  (lambda (obj)
    (cases record obj
      (non-empty-record (k el r) #t)
      (else #f)
      )
    )
  )

(define simple-element?
  (lambda (obj)
    (cases element obj
      (simple-element (datum) #t)
      (else #f)
      )
    )
  )

(define list-element?
  (lambda (obj)
    (cases element obj
      (list-element (datum) #t)
      (else #f)
      )
    )
  )

(define item-num?
  (lambda (obj)
    (cases simple-item obj
      (item-num (datum) #t)
      (else #f)
      )
    )
  )

(define item-sym?
  (lambda (obj)
    (cases simple-item obj
      (item-sym (datum) #t)
      (else #f)
      )
    )
  )

(define empty-list-item?
  (lambda (obj)
    (cases list-item obj
      (empty-list-item () #t)
      (else #f)
      )
    )
  )

(define non-empty-list-item?
  (lambda (obj)
    (cases list-item obj
      (non-empty-list-item (el lst) #t)
      (else #f)
      )
    )
  )

;; Extractores
(define non-empty-record->key
  (lambda (rcd)
    (cases record rcd
      (non-empty-record (k el re) k)
      (else eopl:error "Objeto inválido")
      )
    )
  )

(define non-empty-record->elm
  (lambda (rcd)
    (cases record rcd
      (non-empty-record (k el re) re)
      (else eopl:error "Objeto inválido")
      )
    )
  )

(define non-empty-record->record
  (lambda (rcd)
    (cases record rcd
      (non-empty-record (k el re) re)
      (else eopl:error "Objeto inválido")
      )
    )
  )

(define simple-element->datum
  (lambda (el)
    (cases element el
      (simple-element (d) d)
      (else eopl:error "Objeto inválido")
      )
    )
  )

(define list-element->datum
  (lambda (el)
    (cases element el
      (list-element (d) d)
      (else eopl:error "Objeto inválido")
      )
    )
  )

(define item-num->datum
  (lambda (el)
    (cases simple-item el
      (item-num (d)d)
      (else eopl:error "Objeto inválido")
      )
    )
  )

(define item-sym->datum
  (lambda (el)
    (cases simple-item el
      (item-sym (d)d)
      (else eopl:error "Objeto inválido")
      )
    )
  )

(define list-item->elm
  (lambda (el)
    (cases list-item el
      (non-empty-list-item (el lst) el)
      (else eopl:error "Objeto inválido")
      )
    )
  )

(define list-item->lst
  (lambda (el)
    (cases list-item el
      (non-empty-list-item (el lst) lst)
      (else eopl:error "Objeto inválido")
      )
    )
  )

;; -------------- AREA DEL PROGRAMADOR -----------
(define registro0
  (empty-record)
  )

(define registro1
  (non-empty-record
   'key1
   (simple-element (item-num 1))
   (non-empty-record
    'key2
    (simple-element (item-sym 'xyz))
    (non-empty-record
     'key3
     (simple-element (item-num 2))
     (empty-record)
     )
    )
   )
  )

(define registro2
  (non-empty-record
   'ejemplo2
   (list-element
    (non-empty-list-item
     (item-sym 'a1)
     (non-empty-list-item
      (item-sym 'b1)
      (empty-list-item)
      )
     )
    )
   (empty-record)
   )
  )

(define registro3
  (non-empty-record
   'ejemplo3
   (list-element
    (non-empty-list-item
     (item-num 0)
     (non-empty-list-item
      (item-sym 'x2)
      (empty-list-item)
      )
     )
    )
   (empty-record)
   )
  )


(define registro4
  (non-empty-record
   'ejemplo4
   (list-element
    (non-empty-list-item
     (item-num 0)
     (non-empty-list-item
      (item-num 1)
      (non-empty-list-item
       (item-num 2)
       (empty-list-item)
       )
      )
     )
    )
   (empty-record)
   )
  )

(define registro5
  (non-empty-record
   'ejemplo5_key1
   (simple-element (item-sym 'abc))
   (non-empty-record
    'ejemplo5_key2
    (simple-element (item-sym 'plf))
    (non-empty-record
     'ejemplo5_key3
     (list-element
      (non-empty-list-item
       (item-num 0)
       (non-empty-list-item
        (item-num 1)
        (non-empty-list-item
         (item-num 2)
         (empty-list-item)
         )
        )
       )
      )
     (empty-record)
    )
    )
   )
  )


(define get-numbers
  (lambda (obj)
    (cases record obj
      (empty-record ()'())
      (non-empty-record (k el r)
                        (cases element el
                          (simple-element (d) (if (item-num? d)
                                                  (cons (item-num->datum d) (get-numbers r))
                                                  (get-numbers r)
                                                  )
                                          )
                          (list-element (d) (append
                                             (extraer-aux d item-num? item-num->datum)
                                             (get-numbers r)
                                             )
                                        )
                          )
                        )
      )
    )
  )

(define get-symbols
  (lambda (obj)
    (cases record obj
      (empty-record ()'())
      (non-empty-record (k el r)
                        (cases element el
                          (simple-element (d) (if (item-sym? d)
                                                  (cons (item-sym->datum d) (get-symbols r))
                                                  (get-symbols r)
                                                  )
                                          )
                          (list-element (d) (append
                                             (extraer-aux d item-sym? item-sym->datum)
                                             (get-symbols r)
                                             )
                                        )
                          )
                        )
      )
    )
  )


(define extraer-aux
  (lambda (obj predicado extractor)
    (cases list-item obj
      (empty-list-item () '())
      (non-empty-list-item (elm lst)
                           (if (predicado elm)
                               (cons (extractor elm) (extraer-aux lst predicado extractor))
                               (extraer-aux lst predicado extractor)
                               )
                           )
      )
    )
  )

(display "----- Funciones sobre el TAD ------")
(newline)
(newline)
(display "get-numbers sobre registro 1")
(newline)
(display (get-numbers registro1))
(newline)

(display "get-numbers sobre registro 2")
(newline)
(display (get-numbers registro2))
(newline)

(display "get-numbers sobre registro 3")
(newline)
(display (get-numbers registro3))
(newline)

(display "get-numbers sobre registro 4")
(newline)
(display (get-numbers registro4))
(newline)

(display "get-numbers sobre registro 5")
(newline)
(display (get-numbers registro5))
(newline)

(newline)
(display "------ Probar Get Symbols -----")
(newline)
(newline)

(display "get-symbols sobre registro 1")
(newline)
(display (get-symbols registro1))
(newline)

(display "get-symbols sobre registro 2")
(newline)
(display (get-symbols registro2))
(newline)

(display "get-symbols sobre registro 3")
(newline)
(display (get-symbols registro3))
(newline)

(display "get-symbols sobre registro 4")
(newline)
(display (get-symbols registro4))
(newline)

(display "get-symbols sobre registro 5")
(newline)
(display (get-symbols registro5)) 