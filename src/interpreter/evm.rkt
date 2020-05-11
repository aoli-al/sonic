#lang rosette

(require "../core/core.rkt"
         binaryio)

(provide (all-defined-out))

(define (interpret e mu)
  (match-define (list inst ops) (fetch e))
  (define stack (machine-state-stack mu))
  (case inst
    [(push)
     (set! stack (append ops stack))]
     ))


(define (fetch e mu t) 
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

(define (exec-instruction env mu t inst) 
  (define stack (machine-state-stack mu))
  (match-define (list i ops) inst)
  (match i
    ['push (set! stack (append ops stack))]
    ['mstore (print ops)]
    )
  (set-machine-state-stack! mu stack)
  (if (member i '(stop revert return))
   'stop 'continue))

(define (exec-transaction env t) 
 (define mu (init-machine-state))
 (define run (lambda (env mu t)
  (define inst (fetch env mu t))
  (define res (exec-instruction env mu t inst))
  (when (equal? res 'continue)
   (run env mu t))))
 (run env mu t))

(define (exec env pt) 
  (for ([t pt])
    (exec-transaction env t)))
