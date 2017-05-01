open import Data.Maybe using (Maybe; just; nothing)
open import Data.List
open import Data.Nat
open import Data.Bool
open import Data.String as String using (String)
open import Steroids.Show
open import Steroids.Functor

module Steroids.List where

  instance
    ListFunctor : ∀ {α} → Functor {α} List
    Functor.fmap ListFunctor f [] = []
    Functor.fmap ListFunctor f (x ∷ xs) = f x ∷ fmap f xs

  lookup : ∀ {A : Set} → ℕ → List A → Maybe A
  lookup n [] = nothing
  lookup zero (x ∷ xs)    = just x
  lookup (suc n) (x ∷ xs) = lookup n xs

  showList : ∀ {A : Set} {{ _ : Show A}} → List A → String
  showList [] = "[]"
  showList (x ∷ xs) = concatString ("[" ∷ show′ x ∷ "," ∷ [] ++ (if not (null xs) then (intersperse "," (map show′ xs)) else [ "" ]) ++ [ "]" ])
    where
      concatString : List String → String
      concatString = foldl String._++_ ""

  instance
    ShowList : ∀ {A : Set} {{_ : Show A}} → Show (List A)
    Show.show′ ShowList = showList
