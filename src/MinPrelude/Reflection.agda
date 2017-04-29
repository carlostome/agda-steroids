open import Data.String
open import MinPrelude
open import Reflection

module MinPrelude.Reflection where

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

  instance
    ArgFoldable : Foldable Arg
    Foldable.foldMap ArgFoldable f (arg i x) = f x

  instance
    ArgTraversable : Traversable Arg
    Traversable.traverse ArgTraversable f (arg i x) = (arg i) <$> f x
    Traversable.superFunc ArgTraversable = ArgFunctor
    Traversable.superFold ArgTraversable = ArgFoldable

  instance
    ShowVisibility : Show Visibility
    Show.show′ ShowVisibility visible   = "v"
    Show.show′ ShowVisibility hidden    = "h"
    Show.show′ ShowVisibility instance′ = "i"

  instance
    ShowRelevance : Show Relevance
    Show.show′ ShowRelevance relevant   = "rr"
    Show.show′ ShowRelevance irrelevant = "ir"

  instance
    ShowArg-info : Show Arg-info
    Show.show′ ShowArg-info (arg-info v r) = "arg-info " ++ " " ++ show′ v ++ " " ++ show′ r

  instance
    ShowArg : ∀ {A : Set} {{_ : Show A}} → Show (Arg A)
    Show.show′ ShowArg (arg i x) = "arg " ++ " " ++ show′ i ++ " " ++ show′ x 
