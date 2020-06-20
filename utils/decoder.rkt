#lang racket/base

(require 2htdp/batch-io)
(require file/sha1)

(provide read-code)

(define (read-code f)
  (hex-string->bytes
    (read-file f))
)
