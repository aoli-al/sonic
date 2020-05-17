#lang rosette

(require "../utils/utils.rkt"
         "../utils/bitvector.rkt"
         "parser.rkt"
         file/sha1
         megaparsack)

(provide make-symbolic-data)

(define dyn-variable-size 3)

(define (function-hash func)
  (define hash-bytes (keccak-256 (string->bytes/utf-8 func)))
  (bytes->bitvector (bytes->list (subbytes hash-bytes 0 4))))

(define (make-symbolic-arguments params) 
  (define parsed-params (parse-parameters params))
  (make-symbolic-arguments-recursive (parse-result! parsed-params)))


(define (dynamic-type? type)
  (match type 
    [(list 'bytes 'none) #t]
    [(list 'array _ 'none) #t]
    [(list 'array t _) (dynamic-type? t)]
    [else #f]))

(define (make-symbolic-arguments-recursive param)
  (define result (foldr (lambda (cur prev) 
           (match-define (list prev-head prev-tail) prev)
           (match-define (list cur-head cur-tail)
             (match cur 
               [(list 'uint _) (define-symbolic x (bitvector 256))
                               (list (bitvector->bytes x) '())]
               [(list 'int _) (define-symbolic x (bitvector 256))
                              (list (bitvector->bytes x) '())]
               [(list 'array type size) 
                (match size
                  ['none (list 
                           (bitvector->bytes (bv (+ (type-size param) (length prev-tail)) 256))
                           (append (bitvector->bytes (bv dyn-variable-size 256))
                                   (make-symbolic-arguments-recursive (make-list dyn-variable-size type))))]
                  [s (if (dynamic-type? type) 
                       (list 
                         (bitvector->bytes (bv (+ (type-size param) (length prev-tail)) 256))
                         (make-symbolic-arguments-recursive (make-list s type)))
                       (list 
                         (make-symbolic-arguments-recursive (make-list s type))
                         '()))])]))
           (list (append prev-head cur-head) (append prev-tail cur-tail)))
         (list '() '()) param))
  (car result))

(define (type-size type)
  (match type 
    [(list 'uint _) 32]
    [(list 'int _) 32]
    [(list 'array t size)
     (match size 
       ['none 32]
       [v (* v (type-size t))])]))


(define (make-symbolic-data signature)
  (match signature
    [(regexp #rx"([a-zA-Z_][a-zA-Z_0-9]*)(\\(.*\\))" (list func _ params)) 
     (append
       (function-hash func)
       (make-symbolic-arguments params))]))
