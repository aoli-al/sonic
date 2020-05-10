#lang rosette

(provide (all-defined-out))


(struct instruction (name operand-size pops pushes gas) #:transparent)
(struct transaction (code-address sender-address gas-price input execute-address value code depth write-permission) #:transparent)
(struct machine-state (gas pc memory active-memory stack) #:mutable #:transparent)
(struct a-system-state (value store code) #:mutable #:transparent)
(struct environment (transactions machine-state system-state) #:mutable #:transparent)

(define generate-address
  (let ([n 1])
    (lambda ()
      (set! n (add1 n))
      n)))


(define (instruction-from-byte byte)
  (case byte
    [(#x0)
     (instruction 'stop 0 0 0 0)]
    [(#x1)
     (instruction 'add 0 2 1 3)]
    [(#x2)
     (instruction 'mul 0 2 1 5)]
    [(#x3)
     (instruction 'sub 0 2 1 3)]
    [(#x4)
     (instruction 'div 0 2 1 5)]
    [(#x5)
     (instruction 'sdiv 0 2 1 5)]
    [(#x6)
     (instruction 'mod 0 2 1 5)]
    [(#x7)
     (instruction 'smod 0 2 1 5)]
    [(#x8)
     (instruction 'addmod 0 3 1 8)]
    [(#x9)
     (instruction 'mulmod 0 3 1 8)]
    [(#xa)
     (instruction 'exp 0 2 1 10)]
    [(#xb)
     (instruction 'signextend 0 2 1 5)]
    [(#x10)
     (instruction 'lt 0 2 1 3)]
    [(#x11)
     (instruction 'gt 0 2 1 3)]
    [(#x12)
     (instruction 'slt 0 2 1 3)]
    [(#x13)
     (instruction 'sgt 0 2 1 3)]
    [(#x14)
     (instruction 'eq 0 2 1 3)]
    [(#x15)
     (instruction 'iszero 0 1 1 3)]
    [(#x16)
     (instruction 'and 0 2 1 3)]
    [(#x17)
     (instruction 'or 0 2 1 3)]
    [(#x18)
     (instruction 'xor 0 2 1 3)]
    [(#x19)
     (instruction 'not 0 1 1 3)]
    [(#x1a)
     (instruction 'byte 0 2 1 3)]
    [(#x20)
     (instruction 'sha3 0 2 1 30)]
    [(#x30)
     (instruction 'address 0 0 1 2)]
    [(#x31)
     (instruction 'balance 0 1 1 20)]
    [(#x32)
     (instruction 'origin 0 0 1 2)]
    [(#x33)
     (instruction 'caller 0 0 1 2)]
    [(#x34)
     (instruction 'callvalue 0 0 1 2)]
    [(#x35)
     (instruction 'calldataload 0 1 1 3)]
    [(#x36)
     (instruction 'calldatasize 0 0 1 2)]
    [(#x37)
     (instruction 'calldatacopy 0 3 0 3)]
    [(#x38)
     (instruction 'codesize 0 0 1 2)]
    [(#x39)
     (instruction 'codecopy 0 3 0 3)]
    [(#x3a)
     (instruction 'gasprice 0 0 1 2)]
    [(#x3b)
     (instruction 'extcodesize 0 1 1 20)]
    [(#x3c)
     (instruction 'extcodecopy 0 4 0 20)]
    [(#x3d)
     (instruction 'returndatasize 0 0 1 2)]
    [(#x3e)
     (instruction 'returndatacopy 0 3 0 3)]
    [(#x40)
     (instruction 'blockhash 0 1 1 20)]
    [(#x41)
     (instruction 'coinbase 0 0 1 2)]
    [(#x42)
     (instruction 'timestamp 0 0 1 2)]
    [(#x43)
     (instruction 'number 0 0 1 2)]
    [(#x44)
     (instruction 'difficulty 0 0 1 2)]
    [(#x45)
     (instruction 'gaslimit 0 0 1 2)]
    [(#x50)
     (instruction 'pop 0 1 0 2)]
    [(#x51)
     (instruction 'mload 0 1 1 3)]
    [(#x52)
     (instruction 'mstore 0 2 0 3)]
    [(#x53)
     (instruction 'mstore8 0 2 0 3)]
    [(#x54)
     (instruction 'sload 0 1 1 50)]
    [(#x55)
     (instruction 'sstore 0 2 0 0)]
    [(#x56)
     (instruction 'jump 0 1 0 8)]
    [(#x57)
     (instruction 'jumpi 0 2 0 10)]
    [(#x58)
     (instruction 'getpc 0 0 1 2)]
    [(#x59)
     (instruction 'msize 0 0 1 2)]
    [(#x5a)
     (instruction 'gas 0 0 1 2)]
    [(#x5b)
     (instruction 'jumpdest 0 0 0 1)]
    [(#x60)
     (instruction 'push 1 0 1 0)]
    [(#x61)
     (instruction 'push 2 0 1 0)]
    [(#x62)
     (instruction 'push 3 0 1 0)]
    [(#x63)
     (instruction 'push 4 0 1 0)]
    [(#x64)
     (instruction 'push 5 0 1 0)]
    [(#x65)
     (instruction 'push 6 0 1 0)]
    [(#x66)
     (instruction 'push 7 0 1 0)]
    [(#x67)
     (instruction 'push 8 0 1 0)]
    [(#x68)
     (instruction 'push 9 0 1 0)]
    [(#x69)
     (instruction 'push 10 0 1 0)]
    [(#x6a)
     (instruction 'push 11 0 1 0)]
    [(#x6b)
     (instruction 'push 12 0 1 0)]
    [(#x6c)
     (instruction 'push 13 0 1 0)]
    [(#x6d)
     (instruction 'push 14 0 1 0)]
    [(#x6e)
     (instruction 'push 15 0 1 0)]
    [(#x6f)
     (instruction 'push 16 0 1 0)]
    [(#x70)
     (instruction 'push 17 0 1 0)]
    [(#x71)
     (instruction 'push 18 0 1 0)]
    [(#x72)
     (instruction 'push 19 0 1 0)]
    [(#x73)
     (instruction 'push 20 0 1 0)]
    [(#x74)
     (instruction 'push 21 0 1 0)]
    [(#x75)
     (instruction 'push 22 0 1 0)]
    [(#x76)
     (instruction 'push 23 0 1 0)]
    [(#x77)
     (instruction 'push 24 0 1 0)]
    [(#x78)
     (instruction 'push 25 0 1 0)]
    [(#x79)
     (instruction 'push 26 0 1 0)]
    [(#x7a)
     (instruction 'push 27 0 1 0)]
    [(#x7b)
     (instruction 'push 28 0 1 0)]
    [(#x7c)
     (instruction 'push 29 0 1 0)]
    [(#x7d)
     (instruction 'push 30 0 1 0)]
    [(#x7e)
     (instruction 'push 31 0 1 0)]
    [(#x7f)
     (instruction 'push 32 0 1 0)]
    [(#x80)
     (instruction 'dup 0 1 2 3)]
    [(#x81)
     (instruction 'dup 0 2 3 3)]
    [(#x82)
     (instruction 'dup 0 3 4 3)]
    [(#x83)
     (instruction 'dup 0 4 5 3)]
    [(#x84)
     (instruction 'dup 0 5 6 3)]
    [(#x85)
     (instruction 'dup 0 6 7 3)]
    [(#x86)
     (instruction 'dup 0 7 8 3)]
    [(#x87)
     (instruction 'dup 0 8 9 3)]
    [(#x88)
     (instruction 'dup 0 9 10 3)]
    [(#x89)
     (instruction 'dup 0 10 11 3)]
    [(#x8a)
     (instruction 'dup 0 11 12 3)]
    [(#x8b)
     (instruction 'dup 0 12 13 3)]
    [(#x8c)
     (instruction 'dup 0 13 14 3)]
    [(#x8d)
     (instruction 'dup 0 14 15 3)]
    [(#x8e)
     (instruction 'dup 0 15 16 3)]
    [(#x8f)
     (instruction 'dup 0 16 17 3)]
    [(#x90)
     (instruction 'swap 0 2 2 3)]
    [(#x91)
     (instruction 'swap 0 3 3 3)]
    [(#x92)
     (instruction 'swap 0 4 4 3)]
    [(#x93)
     (instruction 'swap 0 5 5 3)]
    [(#x94)
     (instruction 'swap 0 6 6 3)]
    [(#x95)
     (instruction 'swap 0 7 7 3)]
    [(#x96)
     (instruction 'swap 0 8 8 3)]
    [(#x97)
     (instruction 'swap 0 9 9 3)]
    [(#x98)
     (instruction 'swap 0 10 10 3)]
    [(#x99)
     (instruction 'swap 0 11 11 3)]
    [(#x9a)
     (instruction 'swap 0 12 12 3)]
    [(#x9b)
     (instruction 'swap 0 13 13 3)]
    [(#x9c)
     (instruction 'swap 0 14 14 3)]
    [(#x9d)
     (instruction 'swap 0 15 15 3)]
    [(#x9e)
     (instruction 'swap 0 16 16 3)]
    [(#x9f)
     (instruction 'swap 0 17 17 3)]
    [(#xa0)
     (instruction 'log 0 2 0 375)]
    [(#xa1)
     (instruction 'log 0 3 0 750)]
    [(#xa2)
     (instruction 'log 0 4 0 1125)]
    [(#xa3)
     (instruction 'log 0 5 0 1500)]
    [(#xa4)
     (instruction 'log 0 6 0 1875)]
    [(#xf0)
     (instruction 'create 0 3 1 32000)]
    [(#xf1)
     (instruction 'call 0 7 1 40)]
    [(#xf2)
     (instruction 'callcode 0 7 1 40)]
    [(#xf3)
     (instruction 'return 0 2 0 0)]
    [(#xf4)
     (instruction 'delegatecall 0 6 1 40)]
    [(#xfa)
     (instruction 'staticcall 0 6 1 40)]
    [(#xfd)
     (instruction 'revert 0 2 0 0)]
    [(#xfe)
     (instruction 'invalid 0 0 0 0)]
    [(#xfc)
     (instruction 'addr 0 1 1 0)]
    [(#xff)
     (instruction 'selfdestruct 0 1 0 5000)]
    [else (instruction 'bad-inst 0 0 0 0)]))
