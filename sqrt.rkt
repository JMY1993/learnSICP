#lang racket
;2019-04-14
(define (sqrt x precision)
(define (sqrt-iter guess)
  (define (improve guess)
    (/ (+ guess
          (/ x guess)) 2))
  (define good-enough?
    (<= (/
         (abs (-
               (improve guess)
               guess))
         guess)
        precision))
  
  (if good-enough? guess (sqrt-iter
                          (improve guess))))
  (sqrt-iter 1))
(sqrt 9 0.001)

;2019-09-20
(define (sqrt-iter guess x satisfaction)
  (if (goodenough? (- guess (/ x guess)) satisfaction)
      guess
      (sqrt-iter (/ (+ (/ x guess) guess) 2) x satisfaction)))
(define (goodenough? diff satisfaction) (<= (abs diff) satisfaction))
(sqrt-iter 7 9 0.001)