#lang racket
; 2019-04-16
(define (factorial x)
  (if (= x 1)
      1
      (* x (factorial (- x 1)))))

(factorial 4)