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


(define memo
  (lambda (x l out)
    (conde
      ((eq-caro l x) (== l out))
      (else
        (fresh (d)
          (cdro l d)
          (memo x d out))))))


(define rembero
  (lambda (x l out)
    (conde
      ((nullo l) (== '() out))
      ((eq-caro l x) (cdro l out))
      (else
        (fresh (a d res)
          (conso a d l)
          (rembero x d res)
          (conso a res out))))))


(define surpriseo
  (lambda (s)
    (rembero s '(a b c) '(a b c))))


(define appendo
  (lambda (l s out)
    (conde
      ((nullo l) (== s out))
      (else
        (fresh (a d res)
          (conso a d l)
          (appendo d s res)
          (conso a res out))))))


(define swappendo
  (lambda (l s out)
    (conde
      (succeed
        (fresh (a d res)
          (conso a d l)
          (conso a res out)
          (swappendo d s res)))
      (else (nullo l) (== s out)))))


(define unwrapo
  (lambda (x out)
    (conde
      (succeed (== x out))
      (else 
        (fresh (a)
          (caro x a)
          (unwrapo a out))))))


(define flatteno
  (lambda (s out)
    (conde
      ((nullo s) (== '() out))
      ((pairo s)
        (fresh (a d res-a res-d)
          (conso a d s)
          (flatteno a res-a)
          (flatteno d res-d)
          (appendo res-a res-d out)))
      (else (conso s '() out)))))


(define flattenrevo
  (lambda (s out)
    (conde
      (succeed (conso s '() out))
      ((nullo s) (== '() out))
      (else
        (fresh (a d res-a res-d)
          (conso a d s)
          (flatteno a res-a)
          (flatteno d res-d)
          (appendo res-a res-d out))))))


;; Chap 06

(define anyo
  (lambda (g)
    (conde
      (g succeed)
      (else (anyo g)))))

(define nevero (anyo fail))

(define alwayso (anyo succeed))


(define salo
  (lambda (g)
    (conde
      (succeed succeed)
      (else g))))

;; Chap 07

(define bit-xoro
  (lambda (x y r)
    (conde
      ((== 0 x) (== 0 y) (== 0 r))
      ((== 1 x) (== 0 y) (== 1 r))
      ((== 0 x) (== 1 y) (== 1 r))
      ((== 1 x) (== 1 y) (== 0 r))
      (else fail))))


(define bit-ando
  (lambda (x y r)
    (conde
      ((== 0 x) (== 0 y) (== 0 r))
      ((== 1 x) (== 0 y) (== 0 r))
      ((== 0 x) (== 1 y) (== 0 r))
      ((== 1 x) (== 1 y) (== 1 r))
      (else fail))))


(define build-num
  (lambda (n)
    (cond
      ((zero? n) '())
      ((and (not (zero? n)) (even? n))
            (cons 0 (build-num (/ n 2))))
      ((odd? n)
        (cons 1
              (build-num (/ (- n 1) 2)))))))


(define poso
  (lambda (n)
    (fresh (a d)
      (== (cons a d) n))))


(define >1o
  (lambda (n)
    (fresh (a ad dd)
      (== (cons a (cons ad dd)) n))))


(define width
  (lambda (n)
    (cond
      ((null? n) 0)
      ((pair? n) (+ (width (cdr n)) 1))
      (else 1))))


;; Skip Chap 08
;; ============
;; The chapter is skipped since it builds on chap 07 and there
;; is nothing new as far as I am concerned
