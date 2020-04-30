#lang rosette

(provide (all-defined-out))


(define-generics cell 
                 (cell-size cell)
                 (cell-offset cell)
                 (cell-value cell))

(define (mcell-size mcell) 1)

(struct mcell (offset value) #:transparent
  #:methods gen:cell
  [(define cell-size mcell-size)
   (define cell-offset mcell-offset)
   (define cell-value mcell-value)])

(define (scell-size scell) 32)

(struct scell (offset value) #:transparent
  #:methods gen:cell
  [(define cell-size scell-size)
   (define cell-offset scell-offset)
   (define cell-value scell-value)])
