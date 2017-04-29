open import RPrelude.Functor
open import RPrelude.Monad
open import RPrelude.Applicative
open import Reflection

module RPrelude.Reflection where

  instance
    TCFunctor : Functor TC
    Functor.fmap TCFunctor f tc = bindTC tc λ x → returnTC (f x)

  instance
    TCApplicative : Applicative TC
    Applicative.pure  TCApplicative      = returnTC
    Applicative._<*>_ TCApplicative tc a = bindTC tc (λ f → bindTC a (λ x → returnTC (f x)))
    Applicative.super TCApplicative      = TCFunctor

  instance
    TCMonad : Monad TC
    Monad._>>=_ TCMonad = bindTC
    Monad.super TCMonad = TCApplicative

  instance
    ArgFunctor : Functor Arg
    Functor.fmap ArgFunctor f (arg i x) = arg i (f x)
