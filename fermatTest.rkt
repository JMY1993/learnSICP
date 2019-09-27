#lang sicp
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         ;;这里没有把(base^(exp/2)^2 化归为(base^2)^(exp/2)，因为如果化归以后，就需要再次引入一个fast-exp函数
         (remainder (square (expmod base
                                    (/ exp 2)
                                    m))
                    m))
        (else
         (remainder (* base
                       (expmod base
                               (- exp 1)
                               m))
                    m))))


(define (square x) (* x x))