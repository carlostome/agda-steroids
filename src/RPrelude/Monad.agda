open import Prelude.Applicative

module Prelude.Monad where

 record Monad (M : Set → Set) : Set₁ where
   infixl 4 _>>=_ 
   field
     _>>=_ : ∀ {A B : Set} → M A → (A → M B) → M B
     overlap {{super}} : Applicative M
