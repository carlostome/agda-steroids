open import RPrelude.Monoid
open import RPrelude.Functor

module RPrelude.Foldable where

  record Foldable (F : Set → Set) : Set₁ where
    field
      foldMap : ∀ {A M : Set} {{MonoidM : Monoid M}} → (A → M) → F A → M
