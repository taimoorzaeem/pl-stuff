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
