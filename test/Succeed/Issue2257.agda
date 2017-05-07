-- Andreas, 2016-10-14, issue #2257, reported by m0davis
-- Bisected by Nisse.

{-# OPTIONS --allow-unsolved-metas #-}

{-# OPTIONS -v tc.ip:20 #-}
-- {-# OPTIONS -v tc:30 #-}
-- {-# OPTIONS -v tc.meta.assign.proj:45 #-}

postulate AA : Set

record R : Set₁ where
  foo : Set
  foo = ∀ (f : AA) → {!!}

  field
    bar : Set
