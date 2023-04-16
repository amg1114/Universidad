#lang eopl

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

(define list-item->simple-item
  (lambda (el)
    (cases list-item el
      (non-empty-list-item (el lst) el)
      (else eopl:error "Objeto inválido")
      )
    )
  )

(define list-item->list-item
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
    (empty-record)
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
     (item-num '0)
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
     (item-num '0)
     (non-empty-list-item
      (item-num '1)
      (non-empty-list-item
       (item-num '2)
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
   (simple-element (item-sym 'flp))
   (non-empty-record
    'ejemplo5_key2
    (simple-element (item-sym 'plf))
    (empty-record)
    )
   )
  )


;; USO DE OBSERVADORES

(display " ---------- PROBAR PREDICADOS ----------")
(newline)
(newline)

(display "Pureba predicado empty-record? con registro0")
(newline)
(display (empty-record? registro0))
(newline)

(display "Pureba predicado non-empty-record? con registro1")
(newline)
(display (non-empty-record? registro1))
(newline)

(display "Pureba predicado simple-element? (simple-element (item-num 0))")
(newline)
(display (simple-element? (simple-element (item-num 0))))
(newline)

(display "Pureba predicado list-element? (list-element (empty-list-item))")
(newline)
(display (list-element? (list-element (empty-list-item))))
(newline)

(display "Pureba predicado item-num? (item-num 0)")
(newline)
(display (item-num? (item-num 0)))
(newline)

(display "Pureba predicado item-sym? (item-sym 'x)")
(newline)
(display (item-sym? (item-sym 'x)))
(newline)

(display "Pureba predicado empty-list-item? (empty-list-item)")
(newline)
(display (empty-list-item? (empty-list-item)))
(newline)

(display "Pureba predicado non-empty-list-item? (non-empty-list-item (item-num 0) (empty-list-item))")
(newline)
(display (non-empty-list-item? (non-empty-list-item (item-num 0) (empty-list-item))))
(newline)
