#lang rosette

(provide (all-defined-out))


(define-generics cell 
                 (cell-size cell)
                 (cell-offset cell)
                 (cell-value cell)
                 (set-cell-value! cell value))

(define (mcell-size mcell) 1)

(define (set-mcell-value cell value) 
 (set-mcell-value! cell value))

(struct mcell (offset [value #:mutable]) #:transparent
  #:methods gen:cell
  [(define cell-size mcell-size)
   (define (cell-offset cell) mcell-offset)
   (define (cell-value cell) mcell-value)
   (define set-cell-value!  set-mcell-value)])


(define (scell-size scell) 32)

(define (set-scell-value cell value) 
  (set-scell-value! cell value))

(struct scell (offset [value #:mutable]) #:transparent
  #:methods gen:cell
  [(define cell-size scell-size)
   (define (cell-offset cell) scell-offset)
   (define (cell-value cell) scell-value)
   (define (set-cell-value! cell value) set-scell-value)])

(struct store (cell [ls #:mutable]) #:transparent)

(define (find-or-create-cell s offset) 
 (define ls (store-ls s))
 (define result (findf (lambda (c) (equal? offset (cell-offset c))) ls))
 (cond 
  [(equal? result #f) 
   (define c ((store-cell s) offset 0)) 
   (set-store-ls! s (append ls (list c)))
   c]
  [else result]))

(define (update-store! store offset value)
 (define cell (find-or-create-cell store offset))
 (set-cell-value! cell value)
 cell)
