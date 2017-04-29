module RPrelude.Monoid where

  record Monoid (M : Set) : Set where
    field
      mempty  : M
      mappend : M → M → M
