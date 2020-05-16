#lang rosette

(require "../core/core.rkt"
         "../core/store.rkt"
         "../utils/bitvector.rkt"
         binaryio)

(provide (all-defined-out))


(define (fetch e mu t) 
  (define c (transaction-code t))
  (define p (machine-state-pc mu))
  (define pc 
    (match p
      [(? integer?) p]
      [(bv _ _) (bitvector->integer p)]))
  (define inst (instruction-from-byte (bytes-ref c pc)))
  (set! pc (+ pc 1))
  (define op-size (instruction-operand-size inst))
  (define pops (instruction-pops inst))
  (define stack (machine-state-stack mu))
  (define end-pos (+ pc op-size))
  (define op 
    (case (list (positive? op-size) (positive? pops))
      ['(#t #f)
       (list (integer->bitvector (bytes->integer 
                                   (subbytes c pc end-pos)
                                   #f) (bitvector 256)))]
      ['(#f #t)
       (set-machine-state-stack! mu (drop stack pops))
       (take stack pops)]
      [else '()]
      ))
  (set-machine-state-pc! mu end-pos)
  (list (instruction-name inst) op))

(define (init-machine-state [gas 300000])
  (machine-state gas 0 (store mcell '()) 0 '()))

(define (exec-instruction env mu t inst) 
  (define stack (machine-state-stack mu))
  (define memory (machine-state-memory mu))
  (define contract (dict-ref (environment-system-state env) (transaction-code-address t)))
  (define binary-op (lambda (ops op converter)
                      (match-define (list x y) ops) 
                      (set! stack (append (list (converter (op x y))) stack))))
  (match-define (list i ops) inst)
  (match i
    ['push (set! stack (append ops stack))]
    ['mstore 
     (match-define (list offset value) ops) 
     (for 
       ([i (in-naturals)]
        [byte (bitvector->bytes value)])
       (update-store! memory (bvadd offset (bv i 256)) byte))]
    ['mload
     (define value
       (apply concat
              (map (lambda (idx) 
                     (cell-value 
                      (find-or-create-cell memory 
                                          (bvadd (car ops) (bv idx 256)))))
                   (range 32))))
     (set! stack (append (list value) stack))]
    ['callvalue (set! stack (append (list (transaction-value t)) stack))]
    ['shr (binary-op (reverse ops) bvlshr (lambda (x) x))]
    ['add (binary-op ops bvadd (lambda (x) x))]
    ['sub (binary-op ops bvsub (lambda (x) x))]
    ['dup (set! stack (append (list (last ops)) ops stack))]
    ['iszero (set! stack (append (list (bool->bitvector (bvzero? (car ops)) 256)) stack))]
    ['jumpi 
     (match-define (list dest con) ops) 
     (set-machine-state-pc! mu (if (bvzero? con) (machine-state-pc mu) dest))]
    ['jumpdest (void)]
    ['pop (void)]
    ['codecopy 
     (match-define (list dest-offset offset len) ops)
     (for ([i (bitvector->integer len)]) 
       (update-store! memory (bvadd dest-offset (bv i 256)) 
                      (bv (bytes-ref (a-system-state-code contract) (+ (bitvector->integer offset) i)) 8)))]
    ['return 
     (match-define (list offset len) ops)
     (set-transaction-return! 
       t
       (map 
         (lambda (i) (cell-value (find-or-create-cell memory (bvadd offset (bv i 256)))))
         (range (bitvector->integer len))))]
    ['calldatasize (set! stack (append (list (bv (length (transaction-input t)) 256)) stack))]
    ['calldataload
     (define input (transaction-input t))
     (define idx (bitvector->integer (car ops)))
     (define data (map 
                    (lambda (i) 
                      (define offset (+ i idx)) 
                      (if (< offset (length input)) (list-ref input (+ i idx)) (bv 0 8))) 
                    (range 32)))
     (set! stack (append (list (apply concat data)) stack))]
    ['lt (match-define (list x y) ops) 
     (set! stack (append (list (bool->bitvector (bvult x y) 256)) stack))]
    ['lt (binary-op ops bvult (lambda (x) (bool->bitvector x 256)))]
    ['eq (binary-op ops bveq (lambda (x) (bool->bitvector x 256)))]
    ['jump (set-machine-state-pc! mu (car ops))]
    ['swap (match-define (list a b ... c) ops)
     (set! stack (append (list c) b (list a) stack))]
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
    (exec-transaction env t)
    (println t)))
