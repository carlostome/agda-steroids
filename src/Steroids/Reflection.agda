open import Data.String
open import Data.Float
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

  instance
    ShowMeta : Show Meta
    Show.show′ ShowMeta = showMeta

  instance
    ShowName : Show Name
    Show.show′ ShowName = showName

  showLit : Literal → String
  showLit (nat n)   = "nat " ++ show′ n
  showLit (float x) = "float"
  showLit (char c)  = "char"
  showLit (string s) = "string " ++ s
  showLit (name x)   = "name " ++ show′ x
  showLit (meta x)   = "meta " ++ show′ x

  instance
    ShowAbs : ∀ {A : Set} {{ _ : Show A}} → Show (Abs A)
    Show.show′ ShowAbs (abs s x) = "(" ++ s ++ " : " ++ show′ x ++ ")"
  instance
    ShowLiteral : Show Literal
    Show.show′ ShowLiteral = showLit

  instance
    ShowSort : Show Sort
    Show.show′ ShowSort (set t) = "set"
    Show.show′ ShowSort (lit n) = show′ n
    Show.show′ ShowSort unknown = "_"

  instance
    {-# TERMINATING #-}
    ShowTerm : Show Term
    Show.show′ ShowTerm (var x args) = "var " ++ show′ x ++ show′ args
    Show.show′ ShowTerm (con c args) = "con " ++ show′ c ++ show′ args
    Show.show′ ShowTerm (def f args) = "def " ++ show′ f ++ show′ args
    Show.show′ ShowTerm (lam v t) = "λ" ++ show′ t
    Show.show′ ShowTerm (pat-lam cs args) = "pat-lam not implemented yet!"
    Show.show′ ShowTerm (pi a b) = "Π" ++ show′ a ++ ". " ++ show′ b
    Show.show′ ShowTerm (sort s) = "sort " ++ show′ s
    Show.show′ ShowTerm (lit l)  = "lit " ++ show′ l
    Show.show′ ShowTerm (meta x x₁) = "meta " ++ show′ x ++ show′ x₁ 
    Show.show′ ShowTerm unknown     = "_"
