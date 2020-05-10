#lang rosette

(require "interpreter/evm.rkt"
         "core/core.rkt"
         "utils/decoder.rkt"
         racket/cmdline)

(define (create-contract-account filename env)
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
    ; #:once-each
    ; ["--function" name
    ; "Set entry function"
    ; (option/function name)]
    #:args (filename)
    filename))

(define (main source)
  (define env (environment))
  (create-contract-account source))

(let [(filename (parse-command-line-options))]
  (main filename))
