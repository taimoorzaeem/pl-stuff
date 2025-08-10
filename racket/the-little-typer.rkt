;; Stuff to load when working with the book, The Little Typer
;; For loading:
;;  $ racket --repl
;;  > (enter! "the-llttle-typer.rkt")

#lang pie

;; IMPORTANT DISTINCTION
;; =====================
;; In dependent types, proofs are constructed instead of rewrited to be
;; to prove it. In the little prover, proof were kind of automated using
;; rewriting rules. In the little typer, we construct proofs using 
;; dependent type theory.

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

;; NOTE: Motive just tell the type of what we are trying to prove. Let's
;;       say we are proving something over Nat, which has two cases, zero
;;       and add1, so motive is (-> Nat U). It seems trivial, but it gets
;;       extremely important with more complicated dependent types. Another
;;       reason why this is useful is because some values are same, but
;;       they are not equal, that is, different types but their values are
;;       same.

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


(claim last
  (Pi ((E U)
       (l Nat))
    (-> (Vec E (add1 l))
       E)))


(claim base-last
  (Pi ((E U))
    (-> (Vec E (add1 zero))
      E)))
(define base-last
  (lambda (E)
    (lambda (es)
      (head es))))


;; ind-Nat’s Base Type
;; ===================
;; In ind-Nat, the base’s type is the motive applied to
;; the target zero.


(claim mot-last
  (-> U Nat
    U))
(define mot-last
  (lambda (E k)
    (-> (Vec E (add1 k))
      E)))


;; ind-Nat’s Step Type
;; ===================
;; In ind-Nat, the step must take two arguments: some Nat n
;; and an almost-answer whose type is the motive applied
;; to n. The type of the answer from the step is the motive
;; applied to (add1 n). The step’s type is:
;; (Pi ((n Nat))
;;    (→ (mot n)
;;      (mot (add1 n))))


(claim step-last
  (Pi ((E U)
       (l-1 Nat))
    (-> (mot-last E l-1)
      (mot-last E (add1 l-1)))))
(define step-last
  (lambda (E l-1)
    (lambda (last_l-1)
      (lambda (es)
        (last_l-1 (tail es))))))

(define last
  (lambda (E l)
    (ind-Nat l
      (mot-last E)
      (base-last E)
      (step-last E))))


;; Readable Expressions
;; ====================
;; Getting the right answer is worthless if we do not know
;; that it is correct. Understanding the answer is at least
;; as important as having the correct answer.
;; NOTE: Correctness matters so much.

;; IMPORTANT
;; =========
;; Use TODO keyword in pi expressions and pie will tell
;; the supposed type to be in that expression.

;; Chap 08
;; =======


(claim incr
  (-> Nat
    Nat))
(define incr
  (lambda (n)
    (iter-Nat n
      1
      (+ 1))))


;; The Law of =
;; ============
;; An expression (= X from to) is a type if X is a type,
;; from is an X , and to is an X.


;; Reading FROM and TO as Nouns
;; ============================
;; Because from and to are convenient names,
;; the corresponding parts of an =-expression are
;; referred to as the from and the to.

;; IMPORTANT:
;; - Types can sometimes be read as statements. Statements
;;   are sometimes called propositions.
;; - Truth means that we have evidence. This evidence is
;;   called a proof.

;; The Law of same
;; ===============
;; The expression (same e) is an (= X e e) if an e is an X.


(claim +1=add1
  (Pi ((n Nat))
    (= Nat (+ 1 n) (add1 n))))
(define +1=add1
  (lambda (n)
    (same (add1 n))))


(claim incr=add1
  (Pi ((n Nat))
    (= Nat (incr n) (add1 n))))


;; Neutral Expressions
;; ===================
;; Variables that are not defined are neutral. If the target of
;; an eliminator expression is neutral, then the eliminator
;; expression is neutral.

;; IMPORTANT:
;;   (lambda (x) (f x)) is the same as f.
;;   These are sometimes called eta-long normal forms.

(claim base-incr=add1
  (= Nat (incr zero) (add1 zero)))
(define base-incr=add1
  (same (add1 zero)))


(claim mot-incr=add1
  (-> Nat
    U))
(define mot-incr=add1
  (lambda (k)
    (= Nat (incr k) (add1 k))))


;; "If" and "Then" as Types
;; ========================
;; The expression (-> X Y) can be read as the
;; statement, "if X then Y".


(claim step-incr=add1
  (Pi ((n-1 Nat))
    (-> (= Nat
          (incr n-1)
          (add1 n-1))
      (= Nat
        (add1
          (incr n-1))
        (add1
          (add1 n-1))))))


;; Observation about incr
;; ======================
;; No matter which Nat n is, (incr (add1 n))
;; is the same Nat as (add1 (incr n)).


;; The Law of cong
;; ===============
;; If f is an (-> X Y) and target is an (= X from to),
;; then (cong target f) is an (= Y (f from) (f to)).
;; NOTE: cong is short for congruence.

(define step-incr=add1
  (lambda (n-1)
    (lambda (incr=add1_n-1)
      (cong incr=add1_n-1 (+ 1)))))


(define incr=add1
  (lambda (n)
    (ind-Nat n
      mot-incr=add1
      base-incr=add1
      step-incr=add1)))


;; The Commandment of cong
;; =======================
;; If x is an X, and f is an (-> X Y),
;; then (cong (same x) f) is the same
;; (= Y (f x) (f x)) as (same (f x)).

;; NOTE: The interplay between judging sameness and stating
;;       equality is at the heart of working with dependent
;;       types. This ﬁrst taste only scratches the surface.

;; NOTE: Damn this chapter was heavy. I need to breathe for real.


;; Chap 09
;; =======


;; The Law of replace
;; ==================
;; If target is an (= X from to), mot is an (-> X U), and base
;; is a (mot from) then (replace target mot base) is a (mot to).


(claim double
  (-> Nat
    Nat))
(define double
  (lambda (n)
    (iter-Nat n
      0
      (+ 2))))


(claim twice
  (-> Nat
    Nat))
(define twice
  (lambda (n)
    (+ n n)))

;; You can write statements such as:
;;    "For every Nat n, (twice n) equals (double n)."
;; as a type. i.e:

(claim twice=double
  (Pi ((n Nat))
    (= Nat (twice n) (double n))))

;; This type represents a proposition, which needs a proof, the program we
;; write for this would be its proof. Who knew types can be this expressive!
;;
;; This is a little bit complex and would need some other auxiliary
;; proofs. Let's begin:


(claim add1+=+add1
  (Pi ((n Nat)
       (j Nat))
    (= Nat
      (add1 (+ n j))
      (+ n (add1 j)))))


(claim mot-add1+=+add1
  (-> Nat Nat
    U))
(define mot-add1+=+add1
  (lambda (j k)
    (= Nat
      (add1 (+ k j))
      (+ k (add1 j)))))


(claim step-add1+=+add1
  (Pi ((j Nat)
       (n-1 Nat))
    (-> (mot-add1+=+add1 j n-1)
      (mot-add1+=+add1 j (add1 n-1)))))
(define step-add1+=+add1
  (lambda (j n-1)
    (lambda (add1+=+add1_n-1)
      (cong add1+=+add1_n-1 (+ 1)))))


(define add1+=+add1
  (lambda (n j)
    (ind-Nat n
      (mot-add1+=+add1 j)
      (same (add1 j))
      (step-add1+=+add1 j))))


;; Now we move towards proving twice = double

(claim mot-twice=double
  (-> Nat
    U))
(define mot-twice=double
  (lambda (n)
    (= Nat
      (twice n)
      (double n))))


(claim step-twice=double
  (Pi ((n-1 Nat))
    (-> (mot-twice=double n-1)
      (mot-twice=double (add1 n-1)))))

;; Observation about +
;; ===================
;; No matter which Nats j and k are, (+ (add1 j) k)
;; is the same Nat as (add1 (+ j k)).


(claim mot-step-twice=double
  (-> Nat Nat
    U))
(define mot-step-twice=double
  (lambda (n-1 k)
    (= Nat
      (add1 k)
      (add1 (add1 (double n-1))))))


(define step-twice=double
  (lambda (n-1)
    (lambda (twice=double_n-1)
      (replace (add1+=+add1 n-1 n-1)
        (mot-step-twice=double n-1)
        (cong twice=double_n-1
          (+ 2))))))


(define twice=double
  (lambda (n)
    (ind-Nat n
      mot-twice=double
      (same zero)
      step-twice=double)))


(claim twice=double-of-17
  (= Nat (twice 17) (double 17)))

(claim twice=double-of-17-again
  (= Nat (twice 17) (double 17)))

(define twice=double-of-17
  (twice=double 17))

;; We can do it with congruence as well:
(define twice=double-of-17-again
  (same 34))


(claim twice-Vec
  (Pi ((E U)
       (l Nat))
    (-> (Vec E l)
      (Vec E (twice l)))))


(claim double-Vec
  (Pi ((E U)
       (l Nat))
    (-> (Vec E l)
      (Vec E (double l)))))

(claim base-double-Vec
  (Pi ((E U))
    (-> (Vec E zero)
      (Vec E (double zero)))))
(define base-double-Vec
  (lambda (E)
    (lambda (es)
      vecnil)))


(claim mot-double-Vec
  (-> U Nat
    U))
(define mot-double-Vec
  (lambda (E k)
    (-> (Vec E k)
      (Vec E (double k)))))


(claim step-double-Vec
  (Pi ((E U)
       (l-1 Nat))
    (-> (-> (Vec E l-1)
          (Vec E (double l-1)))
      (-> (Vec E (add1 l-1))
        (Vec E (double (add1 l-1)))))))
(define step-double-Vec
  (lambda (E l-1)
    (lambda (double-Vec_l-1)
      (lambda (es)
        (vec:: (head es)
          (vec:: (head es)
            (double-Vec_l-1
              (tail es))))))))

(define double-Vec
  (lambda (E l)
    (ind-Nat l
      (mot-double-Vec E)
      (base-double-Vec E)
      (step-double-Vec E))))


;; Solve Easy Problems First
;; =========================
;; If two functions produce equal results, then use the easier
;; one when defining a dependent function, and then use
;; "replace" to give it the desired type.


(define twice-Vec
  (lambda (E l)
    (lambda (es)
      (replace
        (symm (twice=double l))
        (lambda (k)
          (Vec E k))
        (double-Vec E l es)))))


;; The Law of symm
;; ===============
;; If e is an (= X from to), then (symm e) is an (= X to from).

;; The Commandment of symm
;; =======================
;; If x is an X, then (symm (same x)) is the same (= X x x)
;; as (same x).


;; Chap 10
;; =======

;; The Law of Sigma
;; ================
;; The expression (Sigma ((x A)) D) is a type when A is a type,
;; and D is a type if x is an A.

;; NOTE: It is sometimes referred as "D is a family of types over A".

;; The Commandment of cons
;; =======================
;; If p is a (Sigma ((x A)) D), then p is the same as
;; (cons (car p) (cdr p)).


;; NOTE: (Pair A D) is a short way of writing (Sigma ((x A)) D)
;;       where x is not used in D.

;; NOTE: Types built with ->, Pi, and = can be read as statements, and
;;       expressions of those types are PROOFS. Similarly, types built
;;       with Pair and Sigma can be read as statements.

;; Use a Specific Type for Correctness
;; ===================================
;; Specific types can rule out foolish definitions.
;; NOTE: I wouldn't call them foolish, they are just not correct enough.


(claim replicate
  (Pi ((E U)
       (l Nat))
    (-> E
      (Vec E l))))


(claim mot-replicate
  (-> U Nat
    U))
(define mot-replicate
  (lambda (E k)
    (Vec E k)))


(claim step-replicate
  (Pi ((E U)
       (e E)
       (l-1 Nat))
    (-> (mot-replicate E l-1)
      (mot-replicate E (add1 l-1)))))
(define step-replicate
  (lambda (E e l-1)
    (lambda (step-replicate_l-1)
      (vec:: e step-replicate_l-1))))


(define replicate
  (lambda (E l)
    (lambda (e)
      (ind-Nat l
        (mot-replicate E)
        vecnil
        (step-replicate E e)))))


(claim list->vec
  (Pi ((E U)
       (es (List E)))
    (Vec E (length E es))))


;; The Law of ind-List
;; ===================
;; If target is a (List E), mot is an (-> (List E) U),
;; base is a (mot nil), and step is a
;;   (Pi ((e E)
;;        (es (List E)))
;;     (-> (mot es )
;;       (mot (:: e es ))))
;; then (ind-List target mot base step) is a (mot target).


;; The First Commandment of ind-List
;; =================================
;; The ind-List-expression (ind-List nil mot base step)
;; is the same (mot nil) as base.


;; The Second Commandment of ind-List
;; ==================================
;; The ind-List-expression
;; (ind-List (:: e es)
;;   mot
;;   base
;;   step)
;; is the same (mot (:: e es)) as
;;   (step e es
;;     (ind-List es
;;       mot
;;       base
;;       step)).


(claim mot-list->vec
  (Pi ((E U))
    (-> (List E)
      U)))
(define mot-list->vec
  (lambda (E)
    (lambda (es)
      (Vec E (length E es)))))


(claim step-list->vec
  (Pi ((E U)
       (e E)
       (es (List E)))
    (-> (mot-list->vec E es)
      (mot-list->vec E (:: e es)))))
(define step-list->vec
  (lambda (E e es)
    (lambda (list->vec_es)
      (vec:: e list->vec_es))))


(define list->vec
  (lambda (E es)
    (ind-List es
      (mot-list->vec E)
      vecnil
      (step-list->vec E))))


;; Chap 11
;; =======

(claim vec-append
  (Pi ((E U)
       (l Nat)
       (j Nat))
    (-> (Vec E l) (Vec E j)
      (Vec E (+ l j)))))


;; The Law of ind-Vec
;; ==================
;; If n is a Nat, target is a (Vec E n), mot is a
;; (Pi ((k Nat))
;;   (-> (Vec E k )
;;     U)),
;; base is a (mot zero vecnil), and step is a
;; (Pi ((k Nat)
;;      (h E)
;;      (t (Vec E k)))
;;   (-> (mot k t)
;;     (mot (add1 k) (vec:: h t))))
;; then
;; (ind-Vec n target
;;   mot
;;   base
;;   step)
;; is a (mot n target).


;; The First Commandment of ind-Vec
;; ================================
;; The ind-Vec-expression
;; (ind-Vec zero vecnil
;;   mot
;;   base
;;   step)
;; is the same (mot zero vecnil) as base.


;; The Second Commandment of ind-Vec
;; =================================
;; The ind-Vec-expression
;; (ind-Vec (add1 n) (vec:: e es)
;;   mot
;;   base
;;   step)
;; is the same (mot (add1 n) (vec:: e es)) as
;; (step n e es
;;   (ind-Vec n es
;;     mot
;;     base
;;     step)).


(claim mot-vec-append
  (Pi ((E U)
       (j Nat)
       (k Nat))
    (-> (Vec E k)
      U)))
(define mot-vec-append
  (lambda (E j k)
    (lambda (es)
      (Vec E (+ k j)))))


(claim step-vec-append
  (Pi ((E U)
       (j Nat)
       (k Nat)
       (e E)
       (es (Vec E k)))
    (-> (mot-vec-append E j
          k es)
      (mot-vec-append E j
        (add1 k) (vec:: e es)))))
(define step-vec-append
  (lambda (E j l-1 e es)
    (lambda (vec-append_es)
      (vec:: e vec-append_es))))


(define vec-append
  (lambda (E l j)
    (lambda (es end)
      (ind-Vec l es
        (mot-vec-append E j)
        end
        (step-vec-append E j)))))


;; NOTE: Sometimes, using a more specific type is called
;; an intrinsic proof. Similarly, using a separate proof
;; is called extrinsic.


(claim mot-vec->list
  (Pi ((E U)
       (l Nat))
    (-> (Vec E l)
      U)))
(define mot-vec->list
  (lambda (E l)
    (lambda (es)
      (List E))))

(claim step-vec->list
  (Pi ((E U)
       (l-1 Nat)
       (e E)
       (es (Vec E l-1)))
    (-> (mot-vec->list E
          l-1 es)
      (mot-vec->list E
        (add1 l-1) (vec:: e es)))))
(define step-vec->list
  (lambda (E l-1 e es)
    (lambda (vec->list_es)
      (:: e vec->list_es))))


(claim vec->list
  (Pi ((E U)
       (l Nat))
    (-> (Vec E l)
      (List E))))
(define vec->list
  (lambda (E l)
    (lambda (es)
      (ind-Vec l es
        (mot-vec->list E)
        nil
        (step-vec->list E)))))


(claim list->vec->list=
  (Pi ((E U)
       (es (List E)))
    (= (List E)
      es
      (vec->list E
        (length E es)
        (list->vec E es)))))

(claim mot-list->vec->list=
  (Pi ((E U))
    (-> (List E)
      U)))
(define mot-list->vec->list=
  (lambda (E es)
    (= (List E)
      es
      (vec->list E
        (length E es)
        (list->vec E es)))))


(claim step-list->vec->list=
  (Pi ((E U)
       (e E)
       (es (List E)))
    (-> (mot-list->vec->list= E
          es)
      (mot-list->vec->list= E
          (:: e es)))))

;; Statement for "Two equal lists of treats"

(claim Treat-Statement
  U)
(define Treat-Statement
  (Pi ((some-treats (List Atom))
       (more-treats (List Atom)))
    (-> (= (List Atom)
          some-treats
          more-treats)
      (= (List Atom)
        (:: 'plattar some-treats)
        (:: 'plattar more-treats)))))


(claim ::-plattar
  (-> (List Atom)
    (List Atom)))
(define ::-plattar
  (lambda (tasty-treats)
    (:: 'plattar tasty-treats)))


(claim treat-proof
  Treat-Statement)
(define treat-proof
  (lambda (some-treats more-treats)
    (lambda (treats=)
      (cong treats= ::-plattar))))


(claim length-treats=
  (Pi ((some-treats (List Atom))
       (more-treats (List Atom)))
    (-> (= (List Atom)
          some-treats
          more-treats)
      (= Nat
        (length Atom some-treats)
        (length Atom more-treats)))))
(define length-treats=
  (lambda (some-treats more-treats)
    (lambda (treats=)
      (cong treats= (length Atom)))))


;; Coming back to the list->vec->list=


(claim ::-fun
  (Pi ((E U))
    (-> E (List E)
      (List E))))
(define ::-fun
  (lambda (E)
    (lambda (e es)
      (:: e es))))


(define step-list->vec->list=
  (lambda (E e es)
    (lambda (list->vec->list=_es)
      (cong list->vec->list=_es
        (::-fun E e)))))


(define list->vec->list=
  (lambda (E es)
    (ind-List es
      (mot-list->vec->list= E)
      (same nil)
      (step-list->vec->list= E))))


;; Chap 12
;; =======

(claim Even
  (-> Nat
    U))
(define Even
  (lambda (n)
    (Sigma ((half Nat))
      (= Nat n (double half)))))


(claim zero-is-even
  (Even 0))
(define zero-is-even
  (cons 0
    (same 0)))


(claim +two-even
  (Pi ((n Nat))
    (-> (Even n)
      (Even (+ 2 n)))))


;; Carefully Choose Definitions
;; ============================
;; Carefully chosen definitions can greatly simpilfy
;; later proofs.


(define +two-even
  (lambda (n e_n)
    (cons (add1 (car e_n))
      (cong (cdr e_n) (+ 2)))))


(claim two-is-even
  (Even 2))
(define two-is-even
  (+two-even 0 zero-is-even))


(claim Odd
  (-> Nat
    U))
(define Odd
  (lambda (n)
    (Sigma ((haf Nat))
      (= Nat n (add1 (double haf))))))


(claim one-is-odd
  (Odd 1))
(define one-is-odd
  (cons 0
    (same 1)))


(claim add1-even->odd
  (Pi ((n Nat))
    (-> (Even n)
      (Odd (add1 n)))))
(define add1-even->odd
  (lambda (n e_n)
    (cons (car e_n)
      (cong (cdr e_n) (+ 1)))))


(claim add1-odd->even
  (Pi ((n Nat))
    (-> (Odd n)
      (Even (add1 n)))))
(define add1-odd->even
  (lambda (n o_n)
    (cons (add1 (car o_n))
      (cong (cdr o_n) (+ 1)))))


(claim repeat
  (-> (-> Nat
        Nat)
      Nat
    Nat))
(define repeat
  (lambda (f n)
    (iter-Nat n
      (f 1)
      (lambda (iter_fn-1)
        (f iter_fn-1)))))


(claim ackermann
  (-> Nat Nat
    Nat))
(define ackermann
  (lambda (n)
    (iter-Nat n
      (+ 1)
      (lambda (ackermann_n-1)
        (repeat ackermann_n-1)))))

;; Chap 13
;; =======
;; NOTE: Ahh, it's getting too familiar (Haskell).

;; The Law of Either
;; =================
;; (Either L R) is a type if L is a type and R is a type.


;; The Law of left
;; ===============
;; (left lt) is an (Either L R) if lt is an L.


;; The Law of right
;; ================
;; (right rt) is an (Either L R) is rt is an R.


;; The Law of ind-Either
;; =====================
;; If target is an (Either L R), mot is an
;;   (-> (Either L R)
;;     U),
;; base-left is a
;;   (Pi ((x L))
;;     (mot (left x))),
;; and base-right is a
;;   (Pi ((y R))
;;     (mot (right y)))
;; then
;;   (ind-Either target
;;     mot
;;     base-left
;;     base-right)
;; is a (mot target).


;; The First Commandment of ind-Either
;; ===================================
;; (ind-Either (left x)
;;   mot
;;   base-left
;;   base-right)
;; is the same (mot (left x)) as (base-left x).


;; The Second Commandment of ind-Either
;; ====================================
;; (ind-Either (right y)
;;   mot
;;   base-left
;;   base-right)
;; is the same (mot (right y)) as (base-right y).


(claim even-or-odd
  (Pi ((n Nat))
    (Either (Even n) (Odd n))))


(claim mot-even-or-odd
  (-> Nat
    U))
(define mot-even-or-odd
  (lambda (k)
    (Either (Even k) (Odd k))))


(claim step-even-or-odd
  (Pi ((n-1 Nat))
    (-> (mot-even-or-odd n-1)
      (mot-even-or-odd (add1 n-1)))))
(define step-even-or-odd
  (lambda (n-1)
    (lambda (e-or-o_n-1)
      (ind-Either e-or-o_n-1
        (lambda (e-or-o_n-1)
          (mot-even-or-odd
            (add1 n-1)))
        (lambda (e_n-1)
          (right
            (add1-even->odd
              n-1 e_n-1)))
        (lambda (o_n-1)
          (left
            (add1-odd->even
              n-1 o_n-1)))))))


(define even-or-odd
  (lambda (n)
    (ind-Nat n
      mot-even-or-odd
      (left zero-is-even)
      step-even-or-odd)))


;; Chap 14
;; =======

;; The Law of Trivial
;; ==================
;; Trivial is a type.


;; The Law of sole
;; ===============
;; sole is a Trivial.


;; The Commandment of sole
;; =======================
;; If e is a Trivial, then e is the same as sole.

;; NOTE: They are basically Unit () type like in haskell.

(claim Maybe
  (-> U
    U))
(define Maybe
  (lambda (X)
    (Either X Trivial)))


(claim nothing
  (Pi ((E U))
    (Maybe E)))
(define nothing
  (lambda (E)
    (right sole)))


(claim just
  (Pi ((E U))
    (-> E
      (Maybe E))))
(define just
  (lambda (E e)
    (left e)))


(claim maybe-head
  (Pi ((E U))
    (-> (List E)
      (Maybe E))))
(define maybe-head
  (lambda (E es)
    (rec-List es
      (nothing E)
      (lambda (hd tl head_tl)
        (just E hd)))))


(claim maybe-tail
  (Pi ((E U))
    (-> (List E)
      (Maybe (List E)))))
(define maybe-tail
  (lambda (E es)
    (rec-List es
      (nothing (List E))
      (lambda (hd tl tail_tl)
        (just (List E) tl)))))


(claim list-ref
  (Pi ((E U))
    (-> Nat (List E)
      (Maybe E))))


(claim step-list-ref
  (Pi ((E U))
    (-> Nat
        (-> (List E)
          (Maybe E))
      (-> (List E)
        (Maybe E)))))
(define step-list-ref
  (lambda (E)
    (lambda (n-1 list-ref_n-1)
      (lambda (es)
        (ind-Either (maybe-tail E es)
          (lambda (maybe_tl)
            (Maybe E))
          (lambda (tl)
            (list-ref_n-1 tl))
          (lambda (empty)
            (nothing E)))))))


(define list-ref
  (lambda (E n)
    (rec-Nat n
      (maybe-head E)
      (step-list-ref E))))


;; The Law of Absurd
;; =================
;; Absurd is a type.


(claim similarly-absurd
  (-> Absurd
    Absurd))
(define similarly-absurd
  (lambda (x)
    x))


;; The Commandment of Absurdities
;; ==============================
;; Every expression of type Absurd is neutral, and all of them
;; are the same.


;; The Law of ind-Absurd
;; =====================
;; The expression
;;   (ind-Absurd target
;;     mot)
;; is a mot if target is an Absurd and mot is a U.


(claim Fin
  (-> Nat
    U))
(define Fin
  (lambda (n)
    (iter-Nat n
      Absurd
      Maybe)))


(claim fzero
  (Pi ((n Nat))
    (Fin (add1 n))))
(define fzero
  (lambda (n)
    (nothing (Fin n))))


(claim fadd1
  (Pi ((n Nat))
    (-> (Fin n)
      (Fin (add1 n)))))
(define fadd1
  (lambda (n)
    (lambda (i-1)
      (just (Fin n) i-1))))


(claim vec-ref
  (Pi ((E U)
       (l Nat))
    (-> (Fin l) (Vec E l)
      E)))


(claim base-vec-ref
  (Pi ((E U))
    (-> (Fin zero) (Vec E zero)
      E)))
(define base-vec-ref
  (lambda (E)
    (lambda (no-value-ever es)
      (ind-Absurd no-value-ever
        E))))


(claim step-vec-ref
  (Pi ((E U)
       (l-1 Nat))
    (-> (-> (Fin l-1)
            (Vec E l-1)
          E)
      (-> (Fin (add1 l-1))
          (Vec E (add1 l-1))
        E))))
(define step-vec-ref
  (lambda (E l-1)
    (lambda (vec-ref_l-1)
      (lambda (i es)
        (ind-Either i
          (lambda (i)
            E)
          (lambda (i-1)
            (vec-ref_l-1
              i-1 (tail es)))
          (lambda (triv)
            (head es)))))))


(define vec-ref
  (lambda (E l)
    (ind-Nat l
      (lambda (k)
        (-> (Fin k) (Vec E k)
          E))
      (base-vec-ref E)
      (step-vec-ref E))))


;; Turner's Teaser
