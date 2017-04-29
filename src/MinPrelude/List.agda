open import Data.Maybe
open import Data.List
open import Data.Nat

module MinPrelude.List where

lookup : ∀ {A : Set} → ℕ → List A → Maybe A
lookup n [] = nothing
lookup zero (x ∷ xs)    = just x
lookup (suc n) (x ∷ xs) = lookup n xs
