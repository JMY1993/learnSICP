#lang racket
(require "summation.rkt")
(provide integral)

(define integral
  (lambda (f a b dx)
    (define base (+ a (/ dx 2)))
    (define add-dx
      (lambda (base)
        (+ base dx)))
    (* (sum f base add-dx b)
       dx)))