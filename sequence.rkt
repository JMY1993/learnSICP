#lang racket
(require "basicMath.rkt")
(define nil '())

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

(define shift car)

(define (pop l)
  (if (null? (cdr l))
      (car l)
      (pop (cdr l))))

(define (unshift l item)
  (cons item l))

(define (recursive-append lp ln)
  (if (null? lp)
      ln
      (unshift (recursive-append (cdr lp) ln) (car lp))))

(define (push l item)
  (recursive-append l (cons item '())))

(define (reverse l)
  (define (iter l reversed)
    (if (null? l)
        reversed
        (iter (cdr l) (unshift reversed (car l)))))
  (iter l '()))

(define (recursive-reverse l)
  (if (null? l)
      '()
      (push (recursive-reverse (cdr l)) (car l))))
(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l))
            (map f (cdr l)))))

(define (for-each proc items)
  (if (null? items)
      "done"
      (begin
      (proc (car items))
      (for-each proc (cdr items)))))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
;; Exercise 2.27
(define (deep-reverse items)
  (reverse (map items reverse)))

(define (recursive-tree-reverse tree)
  (define (leaf? tree)
    (not (pair? tree)))
  (cond ((null? tree) '())
        ((leaf? tree) tree)
        (else (reverse (map tree recursive-tree-reverse)))))

(define (tree-reverse tree)
  (define (iter tree reversed)
    (if (null? tree)
        reversed
        (iter (cdr tree)
              (cons (if (pair? (car tree))
                        (tree-reverse (car tree))
                        (car tree))
                    reversed))))
  (iter tree '()))
(define test-tree (list (list 1 2 (list 3 4 5) 6) (list 7 (list (list 8 9 (list 10 11)) 12 13))))

;;Exercise 2.28
(define (fringe l)
  (if (null? l)
      '()
      (if (pair? (car l))
          (append (fringe (car l)) (fringe (cdr l)))
          (cons (car l) (fringe (cdr l))))))

(define (map-tree proc tree)
  (if (null? tree)
      '()
      (if (pair? (car tree))
          (cons (map-tree proc (car tree))
                (map-tree proc (cdr tree)))
          (cons (proc (car tree))
                (map-tree proc (cdr tree))))))
;;Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (λ (item)
                       (cons (car s) item))
                     rest)))))

(define (sum-odd-squares tree)
  (define (leaf? tree)
    (not (pair? tree)))
  (cond ((null? tree) 0)
        ((leaf? tree)
         (if (odd? tree)
             (square tree)
             0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
