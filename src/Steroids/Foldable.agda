open import MinPrelude.Monoid
open import MinPrelude.Functor

module MinPrelude.Foldable where

  record Foldable (F : Set → Set) : Set₁ where
    field
      foldMap : ∀ {A M : Set} {{MonoidM : Monoid M}} → (A → M) → F A → M
