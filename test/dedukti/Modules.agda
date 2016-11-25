module Modules (F : Set) where

postulate A : Set

module M (a : A) (x : A) where

  b : A
  b = a

postulate val : A

open M val

c = b
