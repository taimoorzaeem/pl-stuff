;; Stuff to load when working with the book, The Reasoned Schemer
;; For loading:
;;  - enter racket repl
;;  > (enter! "minikanren.rkt")
#lang racket
(require minikanren)

;; suggested as per the footnote in frame 6 of Chapter 1, #s is written "succeed"
(define succeed (== #t #t)) ;; #s is written succeed

(define fail (== #f #t)) ;; and #u is written fail

(define caro
  (lambda (p a)
    (fresh (d)
      (== (cons a d) p))))

(define cdro
  (lambda (p d)
    (fresh (a)
      (== (cons a d) p))))

(define conso
  (lambda (a d p)
    (== (cons a d) p)))

(define nullo
  (lambda (x)
    (== '() x)))

(define pairo
  (lambda (p)
    (fresh (a d)
      (conso a d p))))

(define listo
  (lambda (l)
    (conde
      ((nullo l) succeed)
      ((pairo l)
        (fresh (d)
          (cdro l d)
          (listo d)))
      (else fail))))

(define lolo
  (lambda (l)
    (conde
      ((nullo l) succeed)
      ((fresh (a)
        (caro l a)
        (listo a))
      (fresh (d)
        (cdro l d)
        (lolo d)))
      (else fail))))

;; TODO: Understand how this definiton is really working under the hood
;; (define twinso
;;   (lambda (s)
;;     (fresh (x y)
;;      (conso x y s)
;;      (conso x '() y))))

(define twinso
  (lambda (s)
    (fresh (x)
      (conso x x s))))

(define loto
  (lambda (l)
    (conde
      ((nullo l) succeed)
      ((fresh (a)
        (caro l a)
        (twinso a))
       (fresh (d)
        (cdro l d)
        (loto d)))
      (else fail))))


(define listofo
  (lambda (predo l)
    (conde
      ((nullo l) succeed)
      ((fresh (a)
        (caro l a)
        (predo a))
       (fresh (d)
        (cdro l d)
        (listofo predo d)))
      (else fail))))


(define eq-caro
  (lambda (l x)
    (caro l x)))


(define membero
  (lambda (x l)
    (conde
      ((nullo l) fail)
      ((eq-caro l x) succeed)
      (else
        (fresh (d)
          (cdro l d)
          (membero x d))))))

(define identity
  (lambda (l)
    (run* (y)
      (membero y l))))

(define pmembero
  (lambda (x l)
    (conde
      ((eq-caro l x)
        (fresh (a d)
          (cdro l (cons a d))))
      ((eq-caro l x) (cdro l '()))
      (else
        (fresh (d)
          (cdro l d)
          (pmembero x d))))))


(define memberrevo
  (lambda (x l)
    (conde
      ((nullo l) fail)
      (succeed
        (fresh (d)
          (cdro l d)
          (memberrevo x d)))
      (else (eq-caro l x)))))


(define reverse-list
  (lambda (l)
    (run* (x) (memberrevo x l))))
