#lang racket

(require megaparsack megaparsack/text)
(require data/monad)
(require data/applicative)

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

(define array-type-name/p
  (do [elem <- elementary-type-name/p]
    [indices <- (many+/p          
                  (do (char/p #\[)
                    [size <- (or/p (try/p integer/p)
                                   (pure 'none))]
                    (char/p #\])
                    (pure size)))]
    (pure (list elem indices))))

(define type-name/p
 (or/p array-type-name/p
  elementary-type-name/p))


(parse-string array-type-name/p "uint256[123][123][]")
