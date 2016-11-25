module Append where

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

plus : Nat → Nat → Nat
plus zero    y = y
plus (suc x) y = suc (plus x y)

data Vec : Nat → Set where
  vnil  :                        Vec zero
  vcons : ∀{n} → Nat → Vec n → Vec (suc n)

append : ∀{n m} → Vec n → Vec m → Vec (plus n m)
append vnil         ys = ys
append (vcons x xs) ys = vcons x (append xs ys)
