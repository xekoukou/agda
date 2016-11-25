module LargeElim where

data Empty : Set where

data Unit : Set where
  unit : Unit

data Bool : Set where
  true  : Bool
  false : Bool

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

leq : (x y : Nat) → Bool
leq zero y = true
leq (suc x) zero = false
leq (suc x) (suc y) = leq x y

True : Bool → Set
True true = Unit
True false = Empty

refl : ∀ x → True (leq x x)
refl zero = unit
refl (suc x) = refl x

trans : ∀ x y z → True (leq x y) → True (leq y z) → True (leq x z)
trans zero _ _ _ _ = unit
trans (suc x) zero _ () _
trans (suc x) (suc y) zero _ ()
trans (suc x) (suc y) (suc z) p q = trans x y z p q


data Either (A B : Set) : Set where
  inl : A → Either A B
  inr : B → Either A B

total : ∀ x y → Either (True (leq x y)) (True (leq y x))
total zero y = inl unit
total (suc x) zero = inr unit
total (suc x) (suc y) = total x y
