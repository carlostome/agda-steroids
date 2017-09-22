open import Data.Maybe
open import Data.String
open import Data.Float
open import Data.Unit
open import Data.List using (intersperse; [])
open import Function using (_∘′_)
open import Steroids
open import Reflection

module Steroids.Reflection where

  instance
    TCFunctor : ∀ {α} → Functor {α} TC
    Functor.fmap TCFunctor f tc = bindTC tc λ x → returnTC (f x)

  instance
    TCApplicative : ∀ {α} → Applicative {α} TC
    Applicative.pure  TCApplicative      = returnTC
    Applicative._<*>_ TCApplicative tc a = bindTC tc (λ f → bindTC a (λ x → returnTC (f x)))
    Applicative.super TCApplicative      = TCFunctor

  instance
    TCMonad : ∀ {α} → Monad {α} TC
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
    Show.show′ ShowArg (arg i x) = show′ x

  instance
    ShowMeta : Show Meta
    Show.show′ ShowMeta = showMeta

  instance
    ShowName : Show Name
    Show.show′ ShowName = showName

  data UQName : Set where
    uqname : Name → UQName

  instance
    ShowUQName : Show UQName
    Show.show′ ShowUQName (uqname x) = strip (show′  x)

  showLit : Literal → String
  showLit (nat n)   = "nat " ++ show′ n
  showLit (float x) = "float"
  showLit (char c)  = "char"
  showLit (string s) = "string " ++ s
  showLit (name x)   = "name " ++ show′ x
  showLit (meta x)   = "meta " ++ show′ x

  instance
    ShowAbs : ∀ {A : Set} {{ _ : Show A}} → Show (Abs A)
    Show.show′ ShowAbs (abs s x) = show′ x

  instance
    ShowLiteral : Show Literal
    Show.show′ ShowLiteral = showLit

  instance
    ShowSort : Show Sort
    Show.show′ ShowSort (set t) = "set"
    Show.show′ ShowSort (lit n) = show′ n
    Show.show′ ShowSort unknown = "_"

  parens : String → String
  parens s = "(" ++ s ++ ")"

  instance
    {-# TERMINATING #-}
    ShowTerm : Show Term
    Show.show′ ShowTerm (var x [])   = show′ x
    Show.show′ ShowTerm (var x args) = show′ x          ++ " " ++ concatString (intersperse " " (fmap (parens ∘′ show′) args))
    Show.show′ ShowTerm (con c [])   = show′ (uqname c)
    Show.show′ ShowTerm (con c args) = show′ (uqname c) ++ " " ++ concatString (intersperse " " (fmap (parens ∘′ show′) args))
    Show.show′ ShowTerm (def f [])   = show′ (uqname f)
    Show.show′ ShowTerm (def f args) = show′ (uqname f) ++ " " ++ concatString (intersperse " " (fmap (parens ∘′ show′) args))
    Show.show′ ShowTerm (lam v t) = "λ" ++ show′ t
    Show.show′ ShowTerm (pat-lam cs args) = "pat-lam not implemented yet!"
    Show.show′ ShowTerm (pi a b) = "Π" ++ show′ a ++ ". " ++ show′ b
    Show.show′ ShowTerm (sort s) = "sort " ++ show′ s
    Show.show′ ShowTerm (lit l)  = show′ l
    Show.show′ ShowTerm (meta x [])   = show′ x
    Show.show′ ShowTerm (meta x args) = show′ x ++ " " ++ concatString (intersperse " " (fmap (parens ∘′ show′) args))
    Show.show′ ShowTerm unknown     = "_"


  evalTC : ∀ {A : Set} → TC A → Term → TC ⊤
  evalTC tc h = tc >>= quoteTC >>= unify h

  macro
    evalT : ∀ {A : Set} → TC A → Term → TC ⊤
    evalT = evalTC
