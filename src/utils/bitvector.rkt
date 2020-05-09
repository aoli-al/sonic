#lang rosette

(provide bitvector->bytes)

(define (bitvector->bytes bv)
  (map (lambda (idx) (extract (+ idx 7) idx bv)) 
   (reverse (range 0 (bitvector-size (type-of bv)) 8))))

