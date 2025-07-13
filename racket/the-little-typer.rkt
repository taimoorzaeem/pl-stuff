;; Stuff to load when working with the book, The Little Typer
;; For loading:
;;  $ racket --repl
;;  > (enter! "the-llttle-typer.rkt")

#lang pie

;; Chap 01
;; =======

;; Normal Forms
;; ============
;; Given a type, every expression described by that type has
;; a normal form, which is the most direct way of writing it.
;; If two expressions are the same, then they have identical
;; normal forms, and if they have identical normal forms,
;; then they are the same.


;; Normal Forms and Types
;; ======================
;; Sameness is always according to a type, so normal forms
;; are also determined by a type.


;; The First Commandment of cons
;; =============================
;; Two cons-expressions are the same (Pair A D) if their cars
;; are the same A and their cdrs are the same D. Here, A
;; and D stand for any type.


;; Normal Forms of Types
;; =====================
;; Every expression that is a type has a normal form, which
;; is the most direct way of writing that type. If two expres-
;; sions are the same type, then they have identical normal
;; forms, and if two types have identical normal forms, then
;; they are the same type.

(claim one
  Nat)
(define one
  (add1 zero))


;; Claims before Definitions
;; =========================
;; Using deﬁne to associate a name with an expression re-
;; quires that the expression’s type has previously been
;; associated with the name using claim.


;; Values
;; ======
;; An expression with a constructor at the top is called
;; a value.
;; NOTE: Values are called canonical expressions.


;; Values and Normal Forms
;; =======================
;; Not every value is in normal form. This is because the
;; arguments to a constructor need not be normal. Each
;; expression has only one normal form, but it is sometimes
;; possible to write it as a value in more than one way.
;; NOTE: Finding a value that is the same as some
;;       starting expression is called evaluation.


;; Everything Is an Expression
;; ===========================
;; In Pie, values are also expressions. Evaluation in Pie
;; ﬁinds an expression, not some other kind of thing.


;; The Commandment of zero
;; =======================
;; zero is the same Nat as zero.


;; The Commandment of add1
;; =======================
;; If n is the same Nat as k, then (add1 n) is the same
;; Nat as (add1 k).


;; Definitions Are Forever
;; =======================
;; Once a name has been claimed, it cannot be reclaimed, and
;; once a name has been deﬁined, it cannot be redeﬁined.
