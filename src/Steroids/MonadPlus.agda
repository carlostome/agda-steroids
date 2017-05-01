open import Level
open import Steroids.Monad

module Steroids.MonadPlus where

  record MonadPlus {α β} (M : Set α → Set β) : Set (suc α ⊔ β) where
    field
      mzero : ∀ {A} → M A
      mplus : ∀ {A} → M A → M A → M A
      overlap {{super}} : Monad M

  open MonadPlus {{...}} public
