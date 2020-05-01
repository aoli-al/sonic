#lang rosette

(require "../utils/utils.rkt"
         "parser.rkt")

(define (function-hash func)
  (define func-hash (keccak-256 (string->bytes/utf-8 func)))
  (subbytes func-hash 0 8))

(define (make-symbolic-arguments params) 
  (define parsed-params (parse-parameters params))
  parsed-params)

(define (make-symbolic-arguments-recursive params))


(define (make-symbolic-data signature)
  (match signature
    [(regexp #rx"([a-zA-Z_][a-zA-Z_0-9]*)(\\(.*\\))" (list func _ params)) 
     (bytes-append 
      (function-hash func)
      (make-symbolic-arguments params))]))

(make-symbolic-data "batchTransfer(uint256)")
