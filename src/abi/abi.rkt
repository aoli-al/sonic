#lang rosette

(require "../utils/utils.rkt")

(define (function-hash func)
  (define func-hash (keccak-256 (string->bytes/utf-8 func)))
  (subbytes func-hash 0 8))

(define (make-symbolic-arguments) '())


(define (make-symbolic-data signature)
  (match signature
    [(regexp #rx"([a-zA-Z_][a-zA-Z_0-9]*)(\\(.*\\))" (list func func-name params)) 
     (bytes-append 
      (function-hash func)
      '())]))

(make-symbolic-data "batch_transfer(uint256)")
