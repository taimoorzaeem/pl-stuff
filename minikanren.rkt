;; Took it from: https://stackoverflow.com/a/65392420/16623126

#lang racket
;; install with: raco pkg install minikanren
(require minikanren)

(define succeed (== #t #t)) ;; #s is written as succeed
(define fail (== #f #t)) ;; #u is written as fail


;; check by running
(run* (q) succeed) ;; evaluates to '(_.0)
