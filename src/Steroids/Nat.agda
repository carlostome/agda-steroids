open import Data.Nat
open import Data.Nat.Show
open import MinPrelude.Show

module MinPrelude.Nat where

  instance
    ShowNat : Show ℕ
    Show.show′ ShowNat = show
