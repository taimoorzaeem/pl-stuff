#lang racket

;; Y Combinator
;; ============

;; Here is a factorial function implemented as lambda expression
;; which doesn't call itself explicitly.

((lambda (n)
    ((lambda (fact) (fact fact n))
     (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
  5)

;; evaluates to 120

;; Here is the fibonacci function with the same pattern

((lambda (n)
    ((lambda (fibo) (fibo fibo n))
     (lambda (fib k)
        (cond ((= k 0) 1)
              ((= k 1) 1)
              (else (+ (fib fib (- k 2)) (fib fib (- k 1))))))))
  5)

;; evaluate to 8

;; We can generalize this pattern and create a function, which allows
;; recursion in pure lambda calculus

;; Standard Y combinator definition

(define Y
  (lambda (f)
    ((lambda (x) (f (x x)))
     (lambda (x) (f (x x))))))

;; NOTE: This will only work in lazy evaluated (call-by-name) languages.
;;       So, this won't work here. For racket which is eagerly evaluated,
;;       we need to change it slightly.

;; Solution:
;;   Add an extra lambda. This will prevent immediate evaluation.
;;   Essentially it makes it lazy by building a thunk.

(define Z
  (lambda (f)
    ((lambda (x) (f (lambda (y) ((x x) y))))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

;; Here is how you define the recursive function using Z.
;; Note that it won't work with Y because it will not terminate.

(define fact
  (Z (lambda (f)
       (lambda (n)
         (if (<= n 1)
             1
             (* n (f (- n 1))))))))

;; Mutual Recursion
;; ================
;; This explains how mutation recursion is implemented under 
;; the hood using only lambdas. This is really cool.


;; This is for even numbers
(define (is-even? x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
      (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
      (if (= n 0) false (ev? ev? od? (- n 1))))))

;; This is for odd numbers
(define (is-odd? x)
  ((lambda (even? odd?) (odd? even? odd? x))
   (lambda (ev? od? n)
      (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
      (if (= n 0) false (ev? ev? od? (- n 1))))))
