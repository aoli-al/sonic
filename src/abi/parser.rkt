#lang racket

(require megaparsack 
         megaparsack/text
         data/monad
         data/applicative)

(provide parse-parameters)


(define integer-type-name/p 
  (do [name <- (or/p (string/p "int")
                (string/p "uint"))]
    [size <- (or/p (try/p integer/p)
                   (pure 256))]
    (pure (list (string->symbol name) size))))

(define bytes-type-name/p 
  (do (string/p "byte") 
    (or/p (try/p (do (char/p #\s)
                   [size <- (or/p (try/p integer/p)
                                  (pure 'none))]
                   (pure (list 'bytes size))))
          (pure '(bytes 1)))))

(define fixed-type-name/p 
  (do [name <- (or/p (string/p "fixed")
                     (string/p "ufixed"))]
   [bits <- integer/p]
   (char/p #\x)
   [decimal <- integer/p]
   (pure (list (string->symbol name) bits decimal))))


(define elementary-type-name/p
  (or/p (try/p (do [name <- (or/p (string/p "bool")
                                  (string/p "address")
                                  (string/p "string")
                                  (string/p "var"))]
                 (pure (list (string->symbol name)))))
        (try/p fixed-type-name/p)
        (try/p bytes-type-name/p)
        (try/p integer-type-name/p)))


(define (build-array-type-name elem-type indices)
 (define t (list 'array elem-type (car indices)))
 (match (cdr indices)
  ['() t]
  [lst (build-array-type-name t lst)]))
 

(define array-type-name/p
  (do [elem <- elementary-type-name/p]
    [indices <- (many+/p          
                  (do (char/p #\[)
                    [size <- (or/p (try/p integer/p)
                                   (pure 'none))]
                    (char/p #\])
                    (pure size)))]
    (pure (build-array-type-name elem indices))))

(define type-name/p
 (or/p (try/p array-type-name/p)
  elementary-type-name/p))


(define parameters/p
 (do (char/p #\()
  [params <- (many/p type-name/p #:sep (char/p #\,))]
  (char/p #\))
  (pure params)))

(define (parse-parameters params)
 (parse-string parameters/p params))
