open import RPrelude.Applicative
open import RPrelude.Functor
open import RPrelude.Foldable

module RPrelude.Traversable where

  record Traversable (T : Set → Set) : Set₁ where
    field
      traverse : ∀ {F : Set → Set} {A B : Set} {{AppF : Applicative F}} → (A → F B) → T A → F (T B)
      overlap {{super}} : Functor T
      overlap {{super}} : Foldable T
