open import Level
open import Data.Maybe  using (Maybe; just; nothing)
open import Data.List   using (List; _∷_; [])

open import Steroids.Functor
open import Steroids.Applicative
open import Steroids.Monad

module Steroids.Maybe where

  instance
    MaybeFunctor : ∀ {α} → Functor {α} Maybe
    Functor.fmap MaybeFunctor f (just x) = just (f x)
    Functor.fmap MaybeFunctor f nothing  = nothing

  instance
    MaybeApp : ∀ {α} → Applicative {α} Maybe
    Applicative.pure MaybeApp  = just
    Applicative._<*>_ MaybeApp (just f) m = fmap f m
    Applicative._<*>_ MaybeApp nothing  m = nothing
    Applicative.super MaybeApp = MaybeFunctor

  instance
    MaybeMonad : ∀ {α} → Monad {α} Maybe
    Monad._>>=_ MaybeMonad (just x) f = f x
    Monad._>>=_ MaybeMonad nothing  f = nothing
    Monad.super MaybeMonad = MaybeApp

  catMaybes : ∀ {A : Set} → List (Maybe A) → List A
  catMaybes [] = []
  catMaybes (just x ∷ l)  = x ∷ catMaybes l
  catMaybes (nothing ∷ l) = catMaybes l
