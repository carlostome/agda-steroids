open import Steroids.Applicative
open import Steroids.Functor
open import Steroids.Foldable

module Steroids.Traversable where

  record Traversable (T : Set → Set) : Set₁ where
    field
      traverse : ∀ {F : Set → Set} {A B : Set} {{AppF : Applicative F}} → (A → F B) → T A → F (T B)
      overlap {{superFunc}} : Functor T
      overlap {{superFold}} : Foldable T

  open Traversable {{...}} public
