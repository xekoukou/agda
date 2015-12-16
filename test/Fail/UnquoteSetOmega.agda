{-# OPTIONS --universe-polymorphism #-}
open import Common.Prelude
open import Common.Level
open import Common.Reflection
open import Common.TC

module UnquoteSetOmega where

`Level : Term
`Level = def (quote Level) []

``Level : Type
``Level = el (lit 0) `Level

-- while building the syntax of ∀ ℓ → Set ℓ (of type Setω) is harmless
`∀ℓ→Setℓ : Term
`∀ℓ→Setℓ = pi (arg (argInfo visible relevant) ``Level) (abs "_" (el (lit 0) (sort (set (var 0 [])))))

-- unquoting it is harmfull
∀ℓ→Setℓ = unquote (give `∀ℓ→Setℓ)
