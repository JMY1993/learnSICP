#lang racket
;2019-04-14
(define (sum-squares-top2 x y z)
  (define (bigger a b)
    (if (<= a b) b a))
  (define (square x)
    (* x x))
  (define (sum-of-squares x y)
    (+ (square x)
       (square y)))
  (define second-biggest
    (bigger x y))
  (define biggest
    (bigger second-biggest z))
  (define sum-of-3
    (+ x y z))
  (sum-of-squares biggest second-biggest))

(sum-squares-top2 1 2 3)

;2019-09-20
(define (square x) (* x x))
(define (popsmallest x y z) (cond ((< x (/ (+ x y z) 3)) x)
                                  ((< y (/ (+ x y z) 3)) y)
                                  (else z)))
(define (larger2-square-sum x y z) (-
                                    (+ (square x)
                                       (square y)
                                       (square z))
                                    (square (popsmallest x y z))))
(larger2-square-sum 9 10 11)