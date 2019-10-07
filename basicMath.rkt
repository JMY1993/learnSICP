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

;; helper procudures
(define (average a b)
  (/ (+ a b) 2))

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
;;
(provide average)
(provide average-dump)
(provide fixed-point)
(provide newton-transform)
(provide fixed-point-of-transform)
(provide sum)