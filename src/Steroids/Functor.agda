module MinPrelude.Functor where

  record Functor (F : Set → Set) : Set₁ where
    infixl 4 _<$>_
    field
      fmap : ∀ {A B : Set} → (A → B) → F A → F B
    _<$>_ = fmap

  open Functor {{...}} public
