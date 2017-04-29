open import Function               using (case_of_; id)
open import Data.List              using (List; []; _∷_; foldl)
open import MinPrelude.Functor
open import MinPrelude.Applicative

module MinPrelude.Monad where

  record Monad (M : Set → Set) : Set₁ where
    infixl 4 _>>=_
    field
      _>>=_ : ∀ {A B : Set} → M A → (A → M B) → M B
      overlap {{super}} : Applicative M

    _>>_ : ∀ {A B : Set} → M A → M B → M B
    m >> m₁ = m >>= λ _ → m₁

  open Monad {{...}} public

  return : ∀ {A : Set} {M : Set → Set} {{_ : Monad M}} → A → M A
  return = pure

  join : ∀ {A : Set} {M : Set → Set} {{_ : Monad M}} → M (M A) → M A
  join m = m >>= id

  infix -10 do_
  do_ : ∀ {a} {A : Set a} → A → A
  do x = x

  infixr 0 do-bind do-seq do-let

  syntax do-bind  m (λ x → m₁) = x ← m -| m₁
  syntax do-seq   m m₁         = m ~| m₁
  syntax do-let   m (λ x → m₁) = x := m -| m₁

  do-bind = _>>=_
  do-seq = _>>_
  do-let = case_of_

  infixr 0 caseM_of_
  caseM_of_ = _>>=_

  liftM2 : ∀ {A B C : Set} {M : Set → Set} {{_ : Monad M}} → (A → B → C) → M A → M B → M C
  liftM2 f ma mb = f <$> ma <*> mb

  mapM : ∀ {A B : Set} {M : Set → Set} {{_ : Monad M}} → (A → M B) → List A → M (List B)
  mapM f []       = return []
  mapM f (x ∷ xs) = liftM2 _∷_ (f x) (mapM f xs)

  foldlM : ∀ {A B : Set} {M : Set → Set} {{_ : Monad M}} → (B → A → M B) → B → List A → M B
  foldlM f acc l = foldl (λ mb m → mb >>= (λ b → f b m) ) (return acc) l

