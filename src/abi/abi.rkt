#lang rosette

(require "../utils/utils.rkt"
         "parser.rkt")

(define dyn-variable-size 3)

(define (function-hash func)
  (define func-hash (keccak-256 (string->bytes/utf-8 func)))
  (subbytes func-hash 0 8))

(define (make-symbolic-arguments params) 
  (define parsed-params (parse-parameters params))
  parsed-params)

(define (make-symbolic-arguments-recursive params)
  (match params
    [(list a b ...)
     #""]
    ['()
     #""]))

(define (make-symbolic-argument param)
  (foldr (lambda (prev cur) 
           (match-define (list prev-head prev-tail) prev)
           (match cur 
             [(list 'uint _) (define-symbolic x (bitvector 256))
                             (list (bitvector->bits x) '())]
             [(list 'int _) (define-symbolic x (bitvector 256))
                             (list (bitvector->bits x) '())]
             [(list 'array type size) (match size 
               ['none list ((bv (+ (type-size param) (length prev-tail)) 256))])]))
         ; [(list 'int _) (define-symbolic x (bitvector 256))]
         ; [(list 'array type size)
         ; (define-symbolic x (bitvector 256))]
         ; ))
         (list #"" #"") param))
  ; (match param
    ; [(list 'uint i)
     ; (define-symbolic x (bitvector 256))
     ; x]
    ; [(list 'int i)
     ; (define-symbolic x (bitvector 256))
     ; x]
    ; [(list 'array t size) '()]))

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
