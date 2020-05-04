#lang rosette

(require binaryio
         racket/runtime-path 
         file/sha1)

(provide (all-defined-out))

(define-runtime-path script-path "sha3.py")
(define script-path-string (some-system-path->string script-path))

(define (keccak-256 bs)
  (hex-string->bytes
    (bytes->string/utf-8
      (with-input-from-bytes bs
                             (lambda ()
                               (with-output-to-bytes
                                 (lambda ()
                                   (system (string-append "python3 " script-path-string)))))))))

(define (bytes->bitvector bs) 
 (map (lambda (b) (bv b 8)) bs))

