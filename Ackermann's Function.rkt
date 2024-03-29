#lang racket
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))

;;2n
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)

;;2^n
(g 1)
(g 2)
(g 3)
(g 4)
(g 5)

;;2^2^2连续二次幂
(h 1)
(h 2)
(h 3)
(h 4)

(A 2 1)