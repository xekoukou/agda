{-# OPTIONS --rewriting #-}

module Dedukti where

postulate
  _⇒_ : ∀{a}{A : Set a} (l r : A) → Set

{-# BUILTIN REWRITE _⇒_ #-}

postulate
  Unit : Set
  triv : Unit

  Empty : Set

  Bool : Set
  true : Bool
  false : Bool

  set : Set
  El  : set -> Set

  unit : set
  r-unit : El unit ⇒ Unit
  {-# REWRITE r-unit #-}

  empty : set
  r-empty : El empty ⇒ Empty
  {-# REWRITE r-empty #-}

  is-true : Bool → set
  r-true  : is-true true ⇒ unit
  r-false : is-true false ⇒ empty
  {-# REWRITE r-true #-}
  {-# REWRITE r-false #-}
