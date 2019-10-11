#lang sicp
(define list-ref
  (lambda (l n)
    (if (= n 0)
        (car l)
        (list-ref (cdr l) (- n 1)))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (lth items)
  (define (lth-iter length items)
    (if (null? items)
        length
        (lth-iter (+ 1 length) (cdr items))))
  (lth-iter 0 items))

(define (unshift l item)
  (cons item l))

(define (append lp ln)
  (if (null? lp)
      ln
      (unshift (append (cdr lp) ln) (car lp))))