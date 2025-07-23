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


;; Chap 03
;; =======

;; Sameness
;; ========
;; If a “same as” chart could show that two expressions are
;; the same, then this fact can be used anywhere without
;; further justiﬁcation. “Same As” charts are only to help
;; build understanding.


;; Total Function
;; ==============
;; A function that always assigns a value to every possible
;; argument is called a total function.


;; The Law of iter-Nat
;; ===================
;; If target is a Nat, base is an X, and step is an
;; (→ X X), then (iter-Nat target base step) is an X.


;; The First Commandment of iter-Nat
;; =================================
;; If (iter-Nat zero base step)
;; is an X, then it is the same X as base.


;; The Second Commandment of iter-Nat
;; ==================================
;; If (iter-Nat (add1 n) base step) is an X , then it
;; is the same X as (step (iter-Nat n base step)).

(claim step-+
  (-> Nat Nat))
(define step-+
  (lambda (n-1)
    (add1 n-1)))

;; The mysterious + is now defined. This is cool as hell.
(claim +
  (-> Nat Nat Nat))
(define +
  (lambda (n j)
    (iter-Nat n
      j
      step-+)))


;; Define gauss using rec-Nat

(claim step-gauss
  (-> Nat Nat Nat))
(define step-gauss
  (lambda (n-1 gauss_n-1)
    (+ (add1 n-1) gauss_n-1)))

(claim gauss
  (-> Nat Nat))
(define gauss
  (lambda (n)
    (rec-Nat n
      0
      step-gauss)))

;; NOTE: rec-Nat is also called primitive recursion.

;; THIS IS EXTREMELY IMPORTANT
;;========================================================================
;; When they say "recursion is not allowed," they're referring to general
;; recursive definitions where you can call a function on arbitrary arguments,
;; potentially leading to non-termination. This would break the type system's
;; guarantees about totality.
;;
;; However, iter-Nat and rec-Nat are not general recursion - they are
;; eliminators (also called recursion principles) that are built into
;; the type system itself. These are carefully designed to ensure termination.
;;
;; iter-Nat performs primitive recursion - it can only make recursive calls on
;; structurally smaller arguments (the predecessor of a natural number).
;;
;; rec-Nat is similar but allows the recursive step to depend on both the
;; predecessor and the result of the recursive call on that predecessor.
;;
;; The type system ensures these always terminate because:
;;
;; 1. They can only recurse on structurally smaller data
;; 2. Natural numbers are well-founded (you can't have an infinite
;;    descending chain)
;; 3. The recursion follows the exact structure of the data type
;;
;; The eliminators give you the power of recursion while maintaining the type
;; system's safety guarantees. This is a common pattern in dependently typed
;; languages - you get structured, provably terminating recursion rather than
;; arbitrary recursion.
;; =========================================================================

(claim step-*
  (-> Nat Nat Nat Nat))
(define step-*
  (lambda (j n-1 *n-1)
    (+ j *n-1)))

(claim *
  (-> Nat Nat Nat))
(define *
  (lambda (n j)
    (rec-Nat n
    0
    (step-* j))))


;; The Law of rec-Nat
;; ==================
;; If target is a Nat, base is an X, and step is an
;; (→ Nat X X) then (rec-Nat target base step) is an X.


;; The First Commandment of rec-Nat
;; ================================
;; If (rec-Nat zero base step) is an X  then it is the same
;; X as base.


;; The Second Commandment of rec-Nat
;; =================================
;; If (rec-Nat (add1 n) base step) is an X, then it is the
;; same X as (step n (rec-Nat n base step)).


;; Chap 04
;; =======

;; The Intermediate Law of Application
;; ===================================
;; If f is a (Pi ((Y U)) X) and Z is a U, then (f Z) is an X
;; where every Y has been consistently replaced by Z.
;; NOTE(self): Basically lambda expression is applied to values
;;             and Pi expressions are applied to types


;; flip function
(claim flip
  (Pi ((A U)
      (D U))
    (-> (Pair A D)
      (Pair D A))))
(define flip
  (lambda (A D)
    (lambda (p)
      (cons (cdr p) (car p)))))


;; elim-Pair
(claim elim-Pair
  (Pi ((A U)
       (D U)
       (X U))
    (-> (Pair A D)
        (-> A D
            X)
       X)))
(define elim-Pair
  (lambda (A D X)
    (lambda (p f)
      (f (car p) (cdr p)))))


;; kar function
(claim kar
  (-> (Pair Nat Nat)
    Nat))
(define kar
  (lambda (p)
    (elim-Pair
      Nat Nat
      Nat
      p
      (lambda (a d)
        a))))

;; kdr function
(claim kdr
  (-> (Pair Nat Nat)
    Nat))
(define kdr
  (lambda (p)
    (elim-Pair
      Nat Nat
      Nat
      p
      (lambda (a d)
        d))))

;; swap function
(claim swap
  (-> (Pair Nat Atom)
    (Pair Atom Nat)))
(define swap
  (lambda (p)
    (elim-Pair
      Nat Atom
      (Pair Atom Nat)
      p
      (lambda (a d)
        (cons d a)))))


;; general-purpose twin function for any type
;; This encapsulated the power of dependent types
(claim twin
  (Pi ((Y U))
    (-> Y (Pair Y Y))))
(define twin
  (lambda (Y)
    (lambda (x)
      (cons x x))))


;; Chap 05
;; =======

;; The Law of List
;; ===============
;; If E is a type, then (List E) is a type.

;; The Law of nil
;; ==============
;; nil is a (List E), no matter what type E is.

;; The Law of ::
;; =============
;; If e is an E and es is a (List E),
;; then (:: e es) is a (List E).
;; NOTE: Jeez, that's so basic. :'(


;; BTW, this is how you define this in haskell:

;; data List e
;;   | Cons e (List e)
;;   = Nil
;;
;; NOTE: Infact, [e] and (:) is just syntactic sugar on this
;;       and you can even do that by yourself using template haskell.


;; The Law of rec-List
;; ===================
;; If target is a (List E), base is an X, and step is an
;; (-> E (List E) X X), then (rec-List target base step) is an X.


;; The First Commandment of rec-List
;; =================================
;; If (rec-List nil base step) is an X, then it is
;; the same X as base.


;; The Second Commandment of rec-List
;; ==================================
;; If (rec-List (:: e es) base step) is an X, then it is the
;; same X as (step e es (rec-List es base step)).


(claim condiments
  (List Atom))
(define condiments
  (:: 'chives
    (:: 'mayonnaise nil)))


(claim step-length
  (Pi ((E U))
    (-> E (List E) Nat
      Nat)))
(define step-length
  (lambda (E)
    (lambda (e es length_es)
      (add1 length_es))))


(claim length
  (Pi ((E U))
    (-> (List E) Nat)))
(define length
  (lambda (E)
    (lambda (es)
      (rec-List es
        0
        (step-length E)))))


;; List Entry Types
;; ================
;; All the entries in a list must be the same type.
;; NOTE: Duhh??


(claim step-append
  (Pi ((E U))
    (-> E (List E) (List E)
      (List E))))
(define step-append
  (lambda (E)
    (lambda (e es append_es)
      (:: e append_es))))


(claim append
  (Pi ((E U))
    (-> (List E) (List E)
      (List E))))
(define append
  (lambda (E)
    (lambda (start end)
      (rec-List start
        end
        (step-append E)))))


;; Chap 06
;; =======

;; The Law of Vec
;; ==============
;; If E is a type and k is a Nat, then (Vec E k)
;; is a type.


;; The Law of vecnil
;; =================
;; vecnil is a (Vec E zero).


;; The Law of vec::
;; ================
;; If e is an E and es is a (Vec E k),
;; then (vec:: e es) is a (Vec E (add1 k)).


(claim first-of-one
  (Pi ((E U))
    (-> (Vec E 1)
      E)))
(define first-of-one
  (lambda (E)
    (lambda (es)
      (head es))))

;; That's no good, we need a general first function. Here it is:

(claim first
  (Pi ((E U)
       (l Nat))
    (-> (Vec E (add1 l))
      E)))


;; The Law of Pi
;; =============
;; The expression (Pi ((y Y)) X) is a type when Y is
;; a type, and X is a type if y is a Y.

(define first
  (lambda (E l)
    (lambda (es)
      (head es))))


;; Use a More Specific Type
;; ========================
;; Make a function total by using a more specific type to
;; rule out unwanted arguments.


;; The previous definition could also have
;; been written like this:
;;
;; (claim ﬁirst
;;   (Pi ((E U))
;;     (Pi ((l Nat))
;;       (-> (Vec E (add1 l))
;;         E))))
;; (define first
;;   (lambda (E)
;;     (lambda (l)
;;       (lambda (es)
;;         (head es)))))

;; or this:

;; (claim ﬁirst
;;   (Pi ((E U))
;;     (Pi ((l Nat))
;;       (Pi ((es (Vec E (add1 l))))
;;         E))))
;; (define first
;;   (lambda (E)
;;     (lambda (l)
;;       (lambda (es)
;;         (head es )))))


;; -> and Pi
;; =========
;; The type (→ Y X) is a shorter way of writing
;; (Pi ((y Y)) X) when y is not used in X.


;; The Final Law of lambda
;; =======================
;; If x is an X when y is a Y, then (lambda (y) x)
;; is a (Pi ((y Y)) X).


;; The Final Law of Application
;; ============================
;; If f is a (Pi ((y Y)) X) and z is a Y, then (f z) is an
;; X where every y has been consistently replaced by z.


;; The Final First Commandment of lambda
;; =====================================
;; If two λ-expressions can be made the same (Pi ((y Y)) X),
;; by consistently renaming their variables, then they are the same.


;; The Final Second Commandment of lambda
;; ======================================
;; If f is a (Pi ((y Y)) X), and y does not occur in
;; f then f is the same as (lambda (y) (f y)).


(claim rest
  (Pi ((E U)
       (l Nat))
    (-> (Vec E (add1 l))
      (Vec E l))))
(define rest
  (lambda (E l)
    (lambda (es)
      (tail es))))


;; Chap 07
;; =======


;; Dependent Types
;; ===============
;; A type that is determined by something that is not a type
;; is called a dependent type.

;; Motive
;; ======
;; ind-Nat needs an extra argument, called the motive and it
;; can be any (-> Nat U). So motive is a function whose body is U.

;; NOTE: So, basically with dependent types, recursion becomes slightly
;;       tricky because the values we recurse on have different types.
;;       Hence, we need something more powerful like ind-Nat.

;; Use ind-Nat for Dependent Types
;; ===============================
;; Use ind-Nat instead of rec-Nat when the rec-Nat- or ind-Nat-
;; expression’s type depends on the target Nat. The ind-Nat-
;; expression’s type is the motive applied to the target.


;; The Law of ind-Nat
;; ==================
;; If target is a Nat, mot is an (→ Nat U), base is a (mot zero),
;; and step is a (Π ((n-1 Nat)) (→ (mot n-1) (mot (add1 n-1)))),
;; then (ind-Nat target mot base step) is a (mot target).


;; The First Commandment of ind-Nat
;; ================================
;; The ind-Nat-expression (ind-Nat zero mot base step) is the
;; same (mot zero) as base.


;; The Second Commandment of ind-Nat
;; =================================
;; The ind-Nat-expression (ind-Nat (add1 n) mot base step) and
;; (step n (ind-Nat n mot base step)) are the same (mot (add1 n)).


;; Induction on Natural Numbers
;; ============================
;; Building a value for any natural number by giving a value
;; for zero and a way to transform a value for n into a value
;; for n + 1 is called induction on natural numbers.


(claim mot-peas
  (-> Nat
    U))
(define mot-peas
  (lambda (k)
    (Vec Atom k)))

(claim step-peas
  (Pi ((l-1 Nat))
    (-> (mot-peas l-1)
      (mot-peas (add1 l-1)))))
(define step-peas
  (lambda (l-1)
    (lambda (peas_l-1)
      (vec:: 'pea peas_l-1))))

(claim peas
  (Pi ((how-many-peas Nat))
    (Vec Atom how-many-peas)))
(define peas
  (lambda (how-many-peas)
    (ind-Nat how-many-peas
      mot-peas
      vecnil
      step-peas)))
