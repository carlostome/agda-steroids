open import Data.String

module Steroids.Show where

  record Show (A : Set) : Set where
    field
      show′ : A → String

  open Show {{...}} public
  
