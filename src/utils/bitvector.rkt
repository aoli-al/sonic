#lang rosette

(provide (all-defined-out))

(define (bitvector->bytes bv)
  (map (lambda (idx) (extract (+ idx 7) idx bv)) 
   (reverse (range 0 (bitvector-size (type-of bv)) 8))))

(define (bytes->bitvector bs) 
 (map (lambda (b) (bv b 8)) bs))
