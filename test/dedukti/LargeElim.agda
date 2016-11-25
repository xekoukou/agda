module LargeElim where

data Empty : Set where

data Unit : Set where
  triv : Unit

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

IsTrue : Bool → Set
IsTrue true = Unit
IsTrue false = Empty

refl : ∀ x → IsTrue (leq x x)
refl zero = triv
refl (suc x) = refl x

trans : ∀ x y z → IsTrue (leq x y) → IsTrue (leq y z) → IsTrue (leq x z)
trans zero y z p q = triv
trans (suc x) zero _ () _
trans (suc x) (suc y) zero _ ()
trans (suc x) (suc y) (suc z) p q = trans x y z p q


data set : Set where
  empty unit bool nat : set
  isTrue : Bool → set

El : set → Set
El empty = Empty
El unit = Unit
El bool = Bool
El nat = Nat
El (isTrue b) = IsTrue b

data Either : (a b : set) → Set where
  inl : ∀{a b} → El a → Either a b
  inr : ∀{a b} → El b → Either a b

total : ∀ x y → Either (isTrue (leq x y)) (isTrue (leq y x))
total zero y = inl triv
total (suc x) zero = inr triv
total (suc x) (suc y) = total x y
