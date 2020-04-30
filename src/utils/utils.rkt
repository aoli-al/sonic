#lang racket

(require binaryio)
(require racket/runtime-path)

(provide keccak-256)

(define-runtime-path script-path "sha3.py")
(define script-path-string (some-system-path->string script-path))

(define (keccak-256 bs)
  (with-input-from-bytes bs
                         (lambda ()
                           (with-output-to-bytes
                             (lambda ()
                               (system (string-append "python3 " script-path-string)))))))

