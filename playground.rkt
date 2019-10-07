#lang racket
(define sum
  (lambda (term a next b)
    (if (greater? a b)
        (make-pair 0 0)
        (eat (term a)
           (sum term (next a) next b)))))
(define (make-pair a b)
  (lambda (select)
    (if (= select 1)
        a
        b)))
(define (car pair)
  (pair 1))
(define (cdr pair)
  (pair 2))

(define (mult-pair pair n)
  (make-pair (* (car pair) n) (* (cdr pair) n)))

(define (pair-next pair step)
  (make-pair (+ (car pair) step) (+ (cdr pair) step)))
(define (greater? a b)
  (> (car a) (car b)))
(define (eat a b)
  (make-pair (+ (car a) (car b)) (+ (cdr a) (cdr b))))

(define test (sum (lambda (pair) (mult-pair pair 5)) (make-pair 1 2) (lambda (pair) (pair-next pair 2)) (make-pair 5 6)))