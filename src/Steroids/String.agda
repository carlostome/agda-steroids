open import Data.String   using (String; fromList; toList; _++_)
open import Data.List     using (reverse; takeWhile)
open import Data.Bool     using (not)
open import Function      using (_∘′_)
open import Data.Char     using (_==_)
open import Data.List     using (List; foldl)

module Steroids.String where

  strip : String → String
  strip = fromList ∘′ reverse ∘′ takeWhile (not ∘′ (_== '.'))
                  ∘′ reverse ∘′ toList 

  concatString : List String → String
  concatString = foldl _++_ ""

