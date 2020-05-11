#lang rosette

(require "interpreter/evm.rkt"
         "core/core.rkt"
         "abi/abi.rkt"
         "utils/decoder.rkt"
         racket/cmdline)

(define (create-contract-account env filename)
  (create-account env #:code (read-code filename)))

(define (create-account 
          env
          #:value [value 0] 
          #:store [store '#()] 
          #:code [code #"\0"])
  (define addr (generate-address))
  (dict-set! (environment-system-state env)
             addr (a-system-state 0 '#() code))
  addr)

(define option/function (make-parameter #f))

(define (parse-command-line-options)
  (command-line
    #:once-each
    ["--function" name
    "Set entry function"
    (option/function name)]
    #:args (filename)
    filename))

(define (main source)
  (define env (environment '() (make-hash)))
  (define code-addr (create-contract-account env source))
  (define addr (create-account env #:value 10000000))
  (define t (transaction code-addr addr 1 
                         (make-symbolic-data (option/function)) addr 0 
                         (a-system-state-code (dict-ref (environment-system-state env) code-addr))
                         0 #t))
  (exec env (list t)))

(let [(filename (parse-command-line-options))]
  (main filename))
