#lang racket
;2019-04-15
(define (cube-root x precision)
(define (cube-iter guess)
  (define (improve guess)
    (/ (+ (/ x (* guess guess)) (* 2 guess))
       3))
  (define good-enough?
    (<= (/
         (abs (-
               (improve guess)
               guess))
         guess)
        precision))
  
  (if good-enough? guess (cube-iter
                          (improve guess))))
  (cube-iter 1))
(cube-root 27 0.001)