module Fibonacci where

data Nat : Set where
  zero : Nat
  suc : Nat → Nat

{-# BUILTIN NATURAL Nat #-}

plus : (x y : Nat) → Nat
plus zero    y = y
plus (suc x) y = suc (plus x y)

record Stream : Set where
  coinductive
  constructor cons
  field head : Nat
        tail : Stream
open Stream public

ones : Stream
head ones = 1
tail ones = ones

plusS : (s t : Stream) → Stream
head (plusS s t) = plus  (head s) (head t)
tail (plusS s t) = plusS (tail s) (tail t)

{-# TERMINATING #-}
fib : Stream
head fib = 0
head (tail fib) = 1
tail (tail fib) = plusS fib (tail fib)

nth : Nat → Stream → Nat
nth zero    s = head s
nth (suc n) s = nth n (tail s)
