open import Steroids.Monoid
open import Steroids.Functor

module Steroids.Foldable where

  record Foldable (F : Set → Set) : Set₁ where
    field
      foldMap : ∀ {A M : Set} {{MonoidM : Monoid M}} → (A → M) → F A → M
