#lang python

import sys
from eth_hash.auto import keccak


data = sys.stdin.read()
sys.stdout.write(keccak(data.encode()).hex())
