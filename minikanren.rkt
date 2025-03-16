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
