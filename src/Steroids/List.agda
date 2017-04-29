open import Data.Maybe using (Maybe; just; nothing)
open import Data.List
open import Data.Nat
open import Data.String as String using (String)
open import MinPrelude.Show

module MinPrelude.List where

  lookup : ∀ {A : Set} → ℕ → List A → Maybe A
  lookup n [] = nothing
  lookup zero (x ∷ xs)    = just x
  lookup (suc n) (x ∷ xs) = lookup n xs

  showList : ∀ {A : Set} {{ _ : Show A}} → List A → String
  showList [] = "[]"
  showList (x ∷ xs) = concatString ("[" ∷ show′ x ∷ "," ∷ [] ++ intersperse "," (map show′ xs) ++ [ "]" ])
    where
      concatString : List String → String
      concatString = foldl String._++_ ""

  instance
    ShowList : ∀ {A : Set} {{_ : Show A}} → Show (List A)
    Show.show′ ShowList = showList
