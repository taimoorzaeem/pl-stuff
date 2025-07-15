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


;; Chap 02
;; =======

;; Constructors and Eliminators
;; ============================
;; Constructors build values, and eliminators take
;; apart values built by constructors.


;; Eliminating Functions
;; =====================
;; Applying a function to arguments is the eliminator for
;; functions.
;; NOTE: Consistently replacing a variable with an expression
;;       is sometimes called substitution.


;; NOTE: Renaming variables in a consistent way in a lambda
;;       expressions is often called alpha-conversion.


;; The Initial Law of Application
;; ==============================
;; If f is an (→ Y X) and arg is a Y,
;; then (f arg) is an X.


;; The Initial First Commandment of λ
;; ==================================
;; Two λ-expressions that expect the same number of arguments
;; are the same if their bodies are the same after consistently
;; renaming their variables.


;; The Initial Second Commandment of λ
;; ===================================
;; If f is an (→ Y X), then f is the same
;; (→ Y X) as (λ (y) (f y)), as long as y
;; does not occur in f.
;; NOTE(self): Remember Capture Avoiding Substition?


;; The Law of Renaming Variables
;; =============================
;; Consistently renaming variables can’t change the mean-
;; ing of anything.
;; NOTE(self): Duh?? That's obvious.


;; The Commandment of Neutral Expressions
;; ======================================
;; Neutral expressions that are written identically are
;; the same, no matter their type.


;; The Law and Commandment of Define
;; =================================
;; Following (claim name X) and (deﬁine name expr),
;; if expr is an X, then name is an X and name is
;; the same X as expr.


;; The Second Commandment of cons
;; ==============================
;; If p is a (Pair A D), then it is the same (Pair A D)
;; as (cons (car p) (cdr p)).


;; Names in Definitions
;; ====================
;; In Pie, only names that are not already used, whether
;; for constructors, eliminators, or previous definitions, can
;; be used with claim or deﬁine.


;; Dim Names
;; =========
;; Unused names are written dimly, but they do need to be
;; there.


;; The Law of which-Nat
;; ====================
;; If target is a Nat, base is an X, and step is an
;; (→ Nat X), then (which-Nat target base step) is an X.


;; The First Commandment of which-Nat
;; ==================================
;; If (which-Nat zero base step)
;; is an X , then it is the same X as base.


;; The Second Commandment of which-Nat
;; ===================================
;; If (which-Nat (add1 n) base step)
;; is an X, then it is the same X as (step n).

;; (claim gauss
;;   (-> Nat Nat))
;; (define gauss
;;   (lambda (n)
;;     (which-Nat n
;;     zero
;;     (lambda (n-1)
;;       (add (add1 n-1) (gauss n-1))))))
;; NOTE: It doesn't work because recursion isn't allowed. :)


;; Type Values
;; ===========
;; An expression that is described by a type is a value when
;; it has a constructor at its top. Similarly, an expression
;; that is a type is a value when it has a type constructor
;; at its top.


;; Every U Is a Type
;; =================
;; Every expression described by U is a type, but not every
;; type is described by U.


;; (claim Pear-maker
;;   U)
;; (define Pear-maker
;;   (-> Nat Nat
;;     Pear))
;;
;; (claim elim-Pear
;;   (-> Pear Pear-maker
;;     Pear))
;; (define elim-Pear
;;   (lambda (pear maker)
;;     (maker (car pear) (cdr pear))))


;; (claim pearwise
;;   (-> Pear Pear Pear))
;; (define pearwise
;;   (lambda (anjou bosc)
;;     (elim-Pear anjou
;;      (λ (a1 d1)
;;       (elim-Pear bosc
;;         ( λ ( a2 d2 )
;;           (cons
;;             (+ a1 a2)
;;             (+ d1 d2))))))))


;; Recess
;; ======

;; The Law of the
;; ==============
;; If X is a type and e is an X, then (the X e)
;; is an X.


;; The Commandment of the
;; ======================
;; If X is a type and e is an X, then (the X e)
;; is the same X as e.
