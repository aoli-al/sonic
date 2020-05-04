#lang rosette

(require "../utils/utils.rkt"
         "parser.rkt")

(define dyn-variable-size 3)

(define (function-hash func)
  (define func-hash (keccak-256 (string->bytes/utf-8 func)))
  (subbytes func-hash 0 8))

(define (make-symbolic-arguments params) 
  (define parsed-params (parse-parameters params))
  (match parsed-params
    [(list 'success p) (make-symbolic-arguments-recursive p)]
    [else (print "error")]))


(define (dynamic-type? type)
 (match type 
  [(list 'bytes 'none) #t]
  [(list 'array _ 'none) #t]
  [(list 'array t _) (dynamic-type? t)]
  [else #f]))

(define (make-symbolic-arguments-recursive param)
  (foldr (lambda (prev cur) 
           (match-define (list prev-head prev-tail) prev)
           (match-define (list cur-head cur-tail)
             (match cur 
               [(list 'uint _) (define-symbolic x (bitvector 256))
                               (list (bitvector->bits x) '())]
               [(list 'int _) (define-symbolic x (bitvector 256))
                              (list (bitvector->bits x) '())]
               [(list 'array type size) 
                (match size
                  ['none (list 
                           (bitvector->bits (bv (+ (type-size param) (length prev-tail)) 256))
                           (append (bitvector->bits (bv dyn-variable-size 256))
                                   (make-symbolic-arguments-recursive (make-list dyn-variable-size type))))]
                  [s (if (dynamic-type? type) 
                       (list 
                         (bitvector->bits (bv (+ (type-size param) (length prev-tail)) 256))
                         (make-symbolic-arguments-recursive (make-list s type)))
                       (list 
                         (make-symbolic-arguments-recursive (make-list s type))
                         '()))])]))
           (list (append prev-head cur-head) (append prev-tail cur-tail)))
         (list '() '()) param))

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
     (bytes-append 
      (function-hash func)
      (make-symbolic-arguments params))]))

(make-symbolic-data "batchTransfer(uint256)")

