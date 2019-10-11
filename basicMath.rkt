#lang racket
;; math cal procedures
(define (square x)
    (* x x))

(define cube
  (lambda (x)
    (* x x x)))

(define (gcd x y)
  (if (= y 0)
      x
      (gcd y (remainder x y))))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y)
                              (- (square y) x))
                            newton-transform
                            1.0))
(define cube-root
  (lambda (x)
    (fixed-point-of-transform (lambda (y)
                                (- (cube y) x))
                              newton-transform
                              1.0)))

(define (fib n)
  (define (iter a b n)
    (if (= n 0)
        a
        (iter b (+ a b) (- n 1))))
  (iter 0 1 n))

;; helper procedures
(define (average a b)
  (/ (+ a b) 2))
; prime
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
; transforms
;average=dump
(define average-dump
  (lambda (f)
    (lambda (x)
      (average x (f x)))))
; fixed point
(define tolerance 0.00001)
(define fixed-point
  (lambda (f first-guess)
    (define close-enough?
      (lambda (v1 v2)
        (< (abs (- v1 v2))
           tolerance)))
    (define try
      (lambda (guess)
        (let ((next (f guess)))
          (if (close-enough? guess next)
              next
              (try next)))))
    (try first-guess)))
; summation
(define sum
  (lambda (term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b)))))
; derivative
(define (deriv g)
  (let ((dx 0.00001))
    (lambda (x)
      (/ (- (g (+ x dx)) (g x)) dx))))
; newton's transform
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
; newton's method
(define newton-method
  (lambda (g guess)
    (fixed-point (newton-transform g) guess)))
; fixed-point-of-transform
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
;;
(provide square)
(provide cube)
(provide gcd)
(provide sqrt)
(provide cube-root)
(provide fib)
;;
(provide prime?)
(provide average)
(provide average-dump)
(provide fixed-point)
(provide newton-transform)
(provide fixed-point-of-transform)
(provide sum)