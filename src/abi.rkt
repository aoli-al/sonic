#lang rosette


(define (function-hash function)
 (system "keccak-256sum"))

(define (make-symbolic-arguments signature)
  (match signature
    [(regexp #rx"([a-zA-Z_][a-zA-Z_0-9]*)(\\(.*\\))" a) a]))

(make-symbolic-arguments "batch_transfer()")
(function-hash "batch_transfer")
