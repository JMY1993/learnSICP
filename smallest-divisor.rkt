#lang sicp
(define (smallest-divisor n)
  (find-divisor-from 2 n))
(define (find-divisor-from a n)
  (cond ((> (* a a) n) n)
        ((divides? a n) a)
        (else (find-divisor-from (+ a 1) n))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
(prime? 10)
        
