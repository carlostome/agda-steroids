open import Level
open import Steroids.Applicative

module Steroids.Alternative where

  record Alternative {α β} (F : Set α → Set β) : Set (suc α ⊔ β) where
    infixl 4 _<|>_
    field
      empty : ∀ {A} → F A
      _<|>_ : ∀ {A} → F A → F A → F A
      overlap {{super}} : Applicative F

  open Alternative {{...}} public
