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

  record Interface : Set₁ where
    coinductive
    field
      Command  : Set
      Response : (c : Command) → Set

  open Interface

  data TCCommand : Set where
    UnifyC     : Term → Term → TCCommand
    NormaliseC : Term → TCCommand
    NewMetaC    : Term → TCCommand

  TCResponse : TCCommand → Set
  TCResponse (UnifyC x y)   = Maybe ⊤
  TCResponse (NewMetaC x)   = Term
  TCResponse (NormaliseC x) = Term

  TCInterface : Interface
  Interface.Command TCInterface  = TCCommand
  Interface.Response TCInterface = TCResponse

  mutual
    record LazyTC I (A : Set) : Set where
      coinductive
      constructor delay
      field
        force : LazyTC′ I A

    data LazyTC′ I (A : Set) : Set where
      fail′   :  LazyTC′ I A
      catch′  :  (m₁ : LazyTC I A) → (m₂ : LazyTC I A) → LazyTC′ I A
      return′ :  A → LazyTC′ I A
      do′′     :  (c : Command I) (f : Response I c → LazyTC I A) → LazyTC′ I A 

  open LazyTC


  returnLazyTC : ∀ {I} {A : Set} (a : A) → LazyTC I A
  force (returnLazyTC a) = return′ a

  bindLazyTC : ∀ {I} {A B : Set} (m : LazyTC I A) → (k : A → LazyTC I B) → LazyTC I B
  force (bindLazyTC m k) with force m
  ... | fail′        = fail′
  ... | catch′ m₁ m₂ = catch′ (bindLazyTC m₁ k) (bindLazyTC m₂ k)
  ... | return′ x    = force (k x)
  ... | do′′ c f      = do′′ c (λ x → bindLazyTC (f x) k)
  
  failLazyTC : ∀ {I} {A : Set} → LazyTC I A 
  force failLazyTC = fail′

  catchLazyTC : ∀ {I} {A : Set} → LazyTC I A → LazyTC I A → LazyTC I A
  force (catchLazyTC x x₁) = catch′ x x₁

  do′ : ∀ {A} {I} (c : Command I) (f : Response I c → LazyTC I A) → LazyTC I A
  force (do′ c f) = do′′ c f

  instance
    LazyTCFunctor : ∀ {I} → Functor (LazyTC I)
    force (Functor.fmap LazyTCFunctor f x) with force x
    ... | fail′        = fail′
    ... | catch′ m₁ m₂ = catch′ (fmap f m₁) (fmap f m₂)
    ... | return′ y = return′ (f y)
    ... | do′′ c k   = do′′ c (λ y → fmap f (k y))

  instance
    LazyTCApplicative : ∀ {I} → Applicative (LazyTC I)
    Applicative.pure  LazyTCApplicative = returnLazyTC
    Applicative._<*>_ LazyTCApplicative mf ma = bindLazyTC mf (λ f → bindLazyTC ma (λ x → returnLazyTC (f x)))
    Applicative.super LazyTCApplicative = LazyTCFunctor

  instance
    LazyTCMonad : ∀ {I} → Monad (LazyTC I)
    Monad._>>=_ LazyTCMonad = bindLazyTC
    Monad.super LazyTCMonad = LazyTCApplicative


  instance
    LazyTCMonadPlus : ∀ {I} → MonadPlus (LazyTC I)
    MonadPlus.mzero LazyTCMonadPlus = failLazyTC
    MonadPlus.mplus LazyTCMonadPlus = catchLazyTC
    MonadPlus.super LazyTCMonadPlus = LazyTCMonad

  unify′ : Term → Term → LazyTC TCInterface ⊤
  unify′ x x₁ = do′ (UnifyC x x₁) (maybe return mzero)

  normalise′ : Term → LazyTC TCInterface Term
  normalise′ t = do′ (NormaliseC t) return

  newMeta′ : Term → LazyTC TCInterface Term
  newMeta′ t = do′ (NewMetaC t) return
