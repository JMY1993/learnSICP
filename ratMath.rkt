#lang racket
(require "basicMath.rkt")

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;;

(define (make-rat n d)
  (let ((g (gcd n d))
        (v (cond ((= 0 (* n d))
                  (if (or (< n 0) (< d 0))
                      -1
                      1))
                 ((> 0 (* n d)) -1)
                 (else 1))))
    (cons (* v (abs (/ n g))) (abs (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

;;

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))