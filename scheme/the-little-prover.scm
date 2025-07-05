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
(dethm atom/car  (x y) (equal (atom (car  x y)) x))
(dethm atom/cdr  (x y) (equal (atom (cdr  x y)) y))

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
