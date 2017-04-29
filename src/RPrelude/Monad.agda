open import Function               using (case_of_)
open import RPrelude.Applicative

module RPrelude.Monad where

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
