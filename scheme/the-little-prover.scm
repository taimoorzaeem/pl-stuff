;; THESE ARE THE INTIAL DEFINITIONS
;; ================================

(define s.car car)
(define s.cdr cdr)
(define s.+ +)
(define s.< <)
(define (num x) (if (number? x) x 0))
(define (if/nil Q A E)
  (if (equal? Q 'nil) (E) (A)))

(define (atom x) (if (pair? x) 'nil 't))
(define (car x) (if (pair? x) (s.car x) '()))
(define (cdr x) (if (pair? x) (s.cdr x) '()))
(define (equal x y) (if (equal? x y) 't 'nil))
(define (natp x)
  (if (integer? x) (if (s.< x 0) 'nil 't) 'nil))
(define (+ x y) (s.+ (num x) (num y)))
(define (< x y)
  (if (s.< (num x) (num y)) 't 'nil))

(define-syntax iff
  (syntax-rules ()
    ((_ Q A E)
     (if/nil Q (lambda () A) (lambda () E)))))

(define-syntax defun
  (syntax-rules ()
    ((_ name (arg ...) body)
     (define (name arg ...) body))))

(define-syntax dethm
  (syntax-rules ()
    ((_ name (arg ...) body)
     (define (name arg ...) body))))

(defun size (x)
  (iff (atom x)
    '0
    (+ '1 (+ (size (car x)) (size (cdr x))))))

;; From now on, we only use "iff" instead of "if". 
;; This is done because mit-scheme doesn't allow
;; redefining "if" reserved word at the top level.
;; ===============================

;; The Axioms of Cons (initial)

(dethm atom/cons (x y) (equal (atom (cons x y)) 'nil))
(dethm car/cons  (x y) (equal (car (cons  x y)) x))
(dethm cdr/cons  (x y) (equal (cdr (cons  x y)) y))

;; The Axioms of Equal (initial)

(dethm equal-same (x) (equal (equal x x) 't))
(dethm equal-swap (x y) (equal (equal x y) (equal y x)))

;; The Axioms of If (initial)

(dethm if-true (x y) (equal (iff 't x y) x))
(dethm if-false (x y) (equal (iff 'nil x y) y))
(dethm if-same (x y) (equal (iff x y y) y))

;; The Axioms of Equal (final)

(dethm equal-if (x y) (iff (equal x y) (equal x y) 't))

;; Example theorem used for rewriting examples
(dethm jabberwocky (x)
  (iff (brillig x)
    (iff (slithy x)
    (equal (mimsy x) 'borogove)
    (equal (mome x) 'rath))
  (iff (uffish x)
    (equal (frumious x) 'bandersnatch)
    (equal (frabjous x) 'beamish))))

;; The Axioms of Cons (final)

(dethm cons/car+cdr (x) (iff (atom x) 't (equal (cons (car x) (cdr x)) x)))

;; The Axioms of If (final)

(dethm if-nest-A (x y z) (iff x (equal (iff x y z) y) 't))
(dethm if-nest-E (x y z) (iff x 't (equal (iff x y z) z)))

;; Example:
;; We can use the if-next axioms to simplify the following expression:

;; (cons 'statement
;;   (cons
;;     (if (equal a 'question)
;;       (cons n '(answer))
;;       (cons n '(else)))
;;     (if (equal a 'question)
;;       (cons n '(other answer))
;;       (cons n '(other else)))))

;; into:

;; (cons 'statement
;;   (if (equal a 'question)
;;     (cons (cons n '(answer))
;;       (cons n '(other answer)))
;;     (cons (cons n '(else))
;;       (cons n '(other else)))))


;; Chap 03

(defun pair (x y)
  (cons x (cons y '())))

(defun first-of (x)
  (car x))

(defun second-of (x)
  (car (cdr x)))

;; Proof:
;; A proof is a sequence of rewriting steps
;; that ENDS IN 't. If we can rewrite a claim,
;; step by step, to 't, then that claim is a
;; theorem.

(dethm first-of-pair (a b)
  (equal (first-of (pair a b)) a))

(dethm second-of-pair (a b)
  (equal (second-of (pair a b)) b))

(defun in-pair? (xs)
  (iff (equal (first-of xs) '?)
       't
       (equal (second-of xs) '?)))

(dethm in-first-of-pair (b)
  (equal (in-pair? (pair '? b)) 't))

(dethm in-second-of-pair (a)
  (equal (in-pair? (pair a '?)) 't))

;; Chap 04

;; The Axioms of Size

(dethm natp/size (x)
  (equal (natp (size x)) 't))

(dethm size/car (x)
  (iff (atom x) 't (equal (< (size (car x)) (size x)) 't)))

(dethm size/cdr (x)
  (iff (atom x) 't (equal (< (size (cdr x)) (size x)) 't)))


(defun sub (x y)
  (iff (atom y)
      (iff (equal y '?)
          x
          y)
      (cons (sub x (car y))
        (sub x (cdr y)))))


;; Chap 05

(defun memb? (xs)
  (iff (atom xs)
      'nil
      (iff (equal (car xs) '?)
          't
          (memb? (cdr xs)))))


(defun remb (xs)
  (iff (atom xs)
      '()
      (iff (equal (car xs) '?)
          (remb (cdr xs))
          (cons (car xs)
            (remb (cdr xs))))))


(dethm memb?/remb0 ()
  (equal (memb? (remb '())) 'nil))

;; Insight: Rewrite from the Inside Out
;; ====================================
;; Rewrite an expression from the “inside” out, starting inside
;; if answers, if elses, and function arguments. Simplify the
;; arguments of a function application as much as possible,
;; then use the Law of Defun to replace the application with
;; the function’s body. Rewrite if questions as necessary to
;; use theorems that require premises. Proceed to outer
;; expressions when inner expressions cannot be simpliﬁed.


;; If Lifting
;; ==========
;; (original-context         (original-context
;;   (original-focus ------>   (if Q
;;     (if Q A E)))              (original-focus E)
;;                               (original-focus E))

(dethm memb?/remb1 (x1)
  (equal (memb? (remb (cons x1 '())))
         'nil))

(dethm memb?/remb2 (x1 x2)
  (equal (memb? (remb (cons x2 (cons x1 '()))))
         'nil))


;; Chap 06
;; =======

;; PROOF BY LIST INDUCTION

;; (if (atom x) C (if C_cdr C 't))

;; Prove memb?/remb on any list using an inductive proof
;; This means that we should prove a BASE CASE and an INDUCTIVE CASE

(dethm memb?/remb (xs)
  (equal (memb? (remb (xs))) 'nill))


;; Chap 07
;; =======

(defun ctx? (x)
  (iff (atom x)
       (equal x '?)
       (iff (ctx? (car x))
            't
            (ctx? (cdr x)))))


(dethm ctx?/sub (x y)
  (iff (ctx? x)
       (iff (ctx? y)
            (equal (ctx? (sub x y)) 't)
            't)
       't))


;; PROOF BY STAR INDUCTION
;; =======================
;;
;; (if (atom x) C (if C_car (if C_cdr C 't) 't))
;; =============================================
;;    where C_car is C with x replaced by (car x) and C_cdr is C with
;;;   x replaced by (cdr x)

;; Create helper proofs

(dethm ctx?/t (x)
  (iff (ctx? x)
       (equal (ctx? x) 't)
       't))


;; Chap 08

(defun member? (x ys)
  (iff (atom x)
       'nil
       (iff (equal (car ys) x)
            't
            (member? x (cdr ys)))))


(defun set? (xs)
  (iff (atom xs)
       't
       (iff (member? (car xs) (cdr xs))
            'nil
            (set? (cdr xs)))))


(defun add-atoms (x ys)
  (iff (atom x)
       (iff (member? x ys)
            ys
            (cons x ys))
       (add-atoms (car x)
         (add-atoms (cdr x) ys))))

;; The functions that contain multiple recursive calls should also have
;; as many measures (conjunction of measure per each recursive call)
;;
;; Totality claim for add-atoms is:
;; (if (natp (size x))
;;      (if (atom x)
;;         't
;;          (if (< (size (car x)) (size x))    ;; conjunction using if
;;              (< (size (cdr x)) (size x))
;;              'nil))
;;      'nil)

(defun atoms (x)
  (add-atoms x '()))


;; Chap 09
;; =======
;; This chapter is all about DEFUN INDUCTION.
;; I still quite not understand this induction because it is slightly
;; harder than list or star induction. Revisit this chapter later for
;; more in-depth understanding.

(dethm set?/atoms (a)
  (equal (set? (atoms a)) 't))


(dethm set?/add-atoms (a)
  (equal (set? (add-atoms a '())) 't))

;; A more general claim then the previous one
(dethm set?/add-atoms (a bs)
  (iff (set? bs)
       (equal (set? (add-atoms a bs)) 't)
       't))

;; Helper theorems
(dethm set?/t (xs)
  (iff (set? xs)
       (equal (set? xs) 't)
       't))

(dethm set?/nil (xs)
  (iff (set? xs)
       't
       (equal (set? xs) 'nil)))


;; Chap 10
;; =======

(defun rotate (x)
  (cons (car (car x))
    (cons (cdr (car x)) (cdr x))))


(dethm rotate/cons (x y z)
  (equal (rotate (cons (cons x y) z))
         (cons x (cons y z))))


(defun align (x)
  (iff (atom x)
       x
       (iff (atom (car x))
            (cons (car x) (align (cdr x)))
            (align (rotate x)))))


(defun wt (x)
  (iff (atom x)
       '1
       (+ (+ (wt (car x)) (wt (car x)))
          (wt (cdr x)))))


;; The Axioms of + and <

(dethm identity-+ (x)
  (iff (natp x)
       (equal (+ '0 x) x)
       't))


(dethm commute-+ (x y)
  (equal (+ x y) (+ y x)))


(dethm associate-+ (x y z)
  (equal (+ x (+ y z)) (+ (+ x y) z)))


(dethm positive-+ (x y)
  (iff (< '0 x)
       (iff (< '0 y)
            (equal (< '0 (+ x y)) 't)
            't)
       't))


(dethm natp/+ (x y)
  (iff (natp x)
       (iff (natp y)
            (equal (natp (+ x y)) 't)
            't)
       't))


(dethm common-addends-< (x y z)
  (equal (< (+ x z) (+ y z)) (< x y)))


(dethm positive/wt (x)
  (equal (< '0 (wt x)) 't))


(dethm align/align (x)
  (equal (align (align x)) (align x)))
