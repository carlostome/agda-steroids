open import Level

module Steroids.Functor where

  record Functor {α β} (F : Set α → Set β) : Set (suc α ⊔ β)  where
    infixl 4 _<$>_
    field
      fmap : ∀ {A B} → (f : A → B) → F A → F B
    _<$>_ = fmap

  open Functor {{...}} public
