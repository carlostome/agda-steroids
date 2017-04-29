open import RPrelude.Functor

module RPrelude.Applicative where

  record Applicative (F : Set → Set) : Set₁ where
    infixl 4 _<*>_
    field
      pure  : ∀ {A : Set}   → A → F A
      _<*>_ : ∀ {A B : Set} → F (A → B) → F A → F B
      overlap {{super}} : Functor F

  open Applicative {{...}} public
