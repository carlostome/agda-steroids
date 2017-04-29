open import Data.Maybe  using (Maybe; just; nothing)
open import Data.List   using (List; _∷_; [])

open import MinPrelude.Functor
open import MinPrelude.Applicative
open import MinPrelude.Monad

module MinPrelude.Maybe where

  instance
    MaybeFunctor : Functor Maybe
    Functor.fmap MaybeFunctor f (just x) = just (f x)
    Functor.fmap MaybeFunctor f nothing  = nothing

  instance
    MaybeApp : Applicative Maybe
    Applicative.pure MaybeApp  = just
    Applicative._<*>_ MaybeApp (just f) m = fmap f m
    Applicative._<*>_ MaybeApp nothing  m = nothing
    Applicative.super MaybeApp = MaybeFunctor

  instance
    MaybeMonad : Monad Maybe
    Monad._>>=_ MaybeMonad (just x) f = f x
    Monad._>>=_ MaybeMonad nothing  f = nothing
    Monad.super MaybeMonad = MaybeApp

  catMaybes : ∀ {A : Set} → List (Maybe A) → List A
  catMaybes [] = []
  catMaybes (just x ∷ l)  = x ∷ catMaybes l
  catMaybes (nothing ∷ l) = catMaybes l
