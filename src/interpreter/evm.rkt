#lang rosette

(require "core.rkt"
         binaryio)

(provide (all-defined-out))

(define (interpret e)
  (match-define (list inst ops) (fetch e))
  (define mu (environment-machine-state e))
  (define stack (machine-state-stack mu))
  (case inst
    [(push)
     (set! stack (append ops stack))]
     ))


(define (fetch e) 
  (define mu (environment-machine-state e))
  (define t (car (environment-transactions e)))
  (define c (transaction-code t))
  (define pc (machine-state-pc mu))
  (define inst (instruction-from-byte (bytes-ref c pc)))
  (set! pc (+ pc 1))
  (define op-size (instruction-operand-size inst))
  (define pops (instruction-pops inst))
  (define stack (machine-state-stack mu))
  (define end-pos (+ pc op-size))
  (define op 
    (case (list (positive? op-size) (positive? pops))
      ['(#t #f)
       (list (bytes->integer 
               (subbytes c pc end-pos)
               #f))]
      ['(#f #t)
       (set-machine-state-stack! mu (drop stack pops))
       (take stack pops)
       ]
      [else '()]
      ))
  (set-machine-state-pc! mu end-pos)
  (list (instruction-name inst) op))

(define (init-machine-state [gas 300000])
  (machine-state gas 0 '#() 0 '()))

(define (init-a-system-state [code #""])
  (a-system-state 0 '#() code))

(define (init-transaction [code #"\0"]) 
  (transaction 0 0 0 #"" 0 0 code 0 #t))

; (define (init-environment)
  ; (environment (list (init-transaction (read-code "../code.tmp"))) (init-machine-state) #(0 (init-a-system-state))))

; (define e (init-environment))
; (interpret e)

(define (exec code transactions) '())
