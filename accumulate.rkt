#lang racket
(require "basicMath.rkt")

(define nil '())

(define test-tree (list (list 1 2) (list 3 (list 4 5) (list 6 (list 7 8 (list 9 10)) 11 12)) 13 14 (list 15 16)))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (map p seq)
  (accumulate (λ (x y)
                (cons (p x) y))
              nil
              seq))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (λ (x y) (+ 1 y)) 0 seq))

(define (filter pred seq)
  (accumulate (λ (x y)
                (if (pred x)
                    (cons x y)
                    y)) nil seq))


;;Exercise 2.34
(define (horner-eval x coef-seq)
  (accumulate (λ (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coef-seq))

;;Exercise 2.35
(define (count-leaves t)
  (accumulate +
              0
              (map (λ (tree)
                     (cond ((null? tree) 0)
                           ((not (pair? tree)) 1)
                           (else (count-leaves tree))))
                   t)))

;;下面这个函数可以起到count-leaves的作用，但是把nil也算作leaf。我自己的算法是不计算nil的
(define (count-leave tree)
    (accumulate +
                0
                (map (λ (sub-tree)
                         (if (pair? sub-tree)           ; 如果这个节点有分支
                             (count-leaves sub-tree)    ; 那么这个节点调用 count-leaves 的结果就是这个节点的树叶数量
                             1))                        ; 遇上一个叶子节点就返回 1
                     tree)))
;;Exercise 2.36
;;自己实现的版本
(define (accumulate-n-self op init seqs)
  (define (pile seqs)
    (define (iter piles seqs)
      (if (null? (car seqs))
          piles
          (iter (append piles (list (map car seqs)))
                (map cdr seqs))))
    (iter nil seqs))
  (map (λ (seq)
         (accumulate op init seq))
       (pile seqs)))
;;按照题目版本
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
;;如果根据题目中递归过程设计pile算法：
(define (pile seqs)
  (if (null? (car seqs))
      nil
      (cons (map car seqs)
            (pile (map cdr seqs)))))

;;Exercise 2.37
(define test-m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define (transpose m)
    (accumulate-n cons '() m))

(define (map+ p . w)
  (map (λ (seq)
         (eval (append (list p) seq))) ;这里用了eval，实现了和scheme原生一样的map效果
       (pile w)))

(define (dot-product v w)
  (accumulate + 0 (map+ * v w)))

(define (matrix-*-vector m v)
  (map (λ (row)
         (dot-product row v))
       m))

(define (matrix-*-matrix m n)
  (let ((trans-n (transpose n)))
    (map (λ (row-m)
           (matrix-*-vector trans-n row-m))
         m)))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (λ (item)
                       (cons (car s) item))
                     rest)))))

;;利用accumulate 生成interval
(define (enumerate-interval low high step)
  ;其实这里的move-step已经生成了interval，accumulate只是把它们接在一起。
  ;这个方法很蠢，因为本身accumulate就是on2复杂度。
  (define (move-step head max step)
    (if (> head max)
        nil
        (cons head (move-step (+ head step) max step))))
  (accumulate cons
              nil
              (move-step low high step)))
;;自己实现的prime-pairs函数
(define (prime-pairs n)
  (filter (λ (seq)
            (prime? (accumulate +
                                0
                                seq)))
          (filter (λ (seq)
                    (= 2 (length seq)))
                  (subsets (enumerate-interval 1 n 1)))))
;教材的标准答案
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (λ (n)
                          (map (λ (m)
                                 (list n m))
                               (enumerate-interval 1 (- n 1) 1)))
                          (enumerate-interval 1 n 1)))))

;;permutations 自己实现
;只能计算链表中所有元素都是不同的，如果链表中存在相同元素，就会报错
(define (permutations-my s)
  (define (absence seq s)
  (filter (λ (atom) (not (= atom s)))
          seq))
  (cond ((null? s) (list nil))
        (else (accumulate append
                          nil
                          (map (λ (n)
                                 (map (λ (seq)
                                        (cons n seq))
                                      (permutations-my (absence s n))))
                               s)))))
;;教材的标准答案
(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (λ (x)
                 (map (λ (p)
                        (cons x p))
                      (permutations (remove x s))))
               s))) ;这里的算法和我的没有任何差别，除了把accumulate->append进一步抽象成了flatmap
(define (remove item sequence)
  (filter (λ (x) (not (= x item)))
          sequence)) ;和我的absence是一个意思
