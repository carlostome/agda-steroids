open import Level
open import Steroids.Functor

module Steroids.Applicative where

  record Applicative {α β} (F : Set α → Set β) : Set (suc α ⊔ β) where
    infixl 4 _<*>_
    field
      pure  : ∀ {A}   → A → F A
      _<*>_ : ∀ {A B} → F (A → B) → F A → F B
      overlap {{super}} : Functor F

  open Applicative {{...}} public
