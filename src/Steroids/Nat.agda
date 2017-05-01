open import Data.Nat
open import Data.Nat.Show
open import Steroids.Show

module Steroids.Nat where

  instance
    ShowNat : Show ℕ
    Show.show′ ShowNat = show
