#lang sicp

;; Chapter 2
;; Building Abstractions with Data

;; 2.2
;; Hierarchical Data and the Closure Property

(#%provide list-ref-)
(define (list-ref- items n)
  (if (= n 0)
    (car items)
    (list-ref- (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

;; | (list-ref- squares 3)
;; 16

(#%provide length-)
(define (length- items)
  (if (null? items)
    0
    (+ 1 (length- (cdr items)))))

(#%provide odds)
(define odds (list 1 3 5 7))

;; (length- odds)
;; 4

(define (length-iter items)
  (define (iter a cnt)
    (if (null? a)
      cnt
      (iter (cdr a) (+ 1 cnt))))
  (iter items 0))

#| (append squares odds) |#
#| (append odds squares) |#

(define (append- list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append- (cdr list1) list2))))

#| 2.17 |#

(#%provide last-pair-)
(define (last-pair- l)
  (if (= (length l) 1)
    (car l)
    (last-pair- (cdr l))))

#| 2.18 |#

(#%provide reverse-)
(define (reverse- l)
  (define (iter res xs)
    (if (null? xs)
      res
      (iter (cons (car xs) res) (cdr xs))))
  (iter nil l))

#| 2.19 |#

(#%provide us-coins)
(define us-coins (list 50 25 10 5 1))

(#%provide uk-coins)
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(#%provide cc)
(define (cc amount coin-values)
  (cond
    ((= amount 0) 1)
    ((or (< amount 0) (no-more? coin-values)) 0)
    (else
      (+
        (cc
          amount
          (except-first-denomination coin-values))
        (cc
          (- amount (first-denomination coin-values))
          coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

#| 2.20 |#

(#%provide same-parity)
(define (same-parity num . nums)
  (define (filter pred xs)
    (cond
      ((null? xs))
      ((pred (car xs)) (cons (car xs) (filter pred (cdr xs))))
      (else (filter pred (cdr xs)))))
  (if (even? num)
    (cons num (filter even? nums))
    (cons num (filter odd? nums))))

(#%provide scale-list)
(define (scale-list items factor)
  (if (null? items)
    nil
    (cons
      (* (car items) factor)
      (scale-list (cdr items) factor))))

(#%provide list-)
(define list- list)

(#%provide map-)
(define (map- proc items)
  (if (null? items)
    nil
    (cons
      (proc (car items))
      (map- proc (cdr items)))))

(#%provide scale-list-new)
(define (scale-list-new items factor)
  (map-
    (lambda (x) (* x factor))
    items))

#| 2.21 |#

(#%provide square-list)
(define (square-list items)
  (define (square x) (* x x ))
  (if (null? items)
    nil
    (cons
      (square (car items))
      (square-list (cdr items)))))

(#%provide square-list-)
(define (square-list- items)
  (map- (lambda (x) (* x x)) items))

#| 2.22 |#

(define (square x) (* x x))

(#%provide square-list-iter)
(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter
        (cdr things)
        (cons (square (car things)) answer))))
  (iter items nil))

(#%provide square-list-iter-right)
(define (square-list-iter-right items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter
        (cdr things)
        (cons answer (square (car things))))))
  (iter items nil))

#| 2.23 |#

(#%provide for-each-)
(define (for-each- proc items)
  (cond
    ((null? items) (newline))
    (else
      (proc (car items))
      (for-each- proc (cdr items)))))

(define (count-leaves x)
  (cond
    ((null? x) 0)
    ((not (pair? x)) 1)
    (else
      (+
        (count-leaves (car x))
        (count-leaves (cdr x))))))

;; (define x (cons (list 1 2) (list 3 4)))

;; (length x)
;; 3

;; (count-leaves x)
;; 4

#| 2.24 |#

;; (list 1 (list 2 (list 3 4)))
;; (1 (2 (3 4)))

#| 2.25 |#

#| (car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9)))))) |#
#| (car (car (list (list 7)))) |#
#| (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr |#
#|   (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))))))))))) |#

#| 2.26 |#

#| (define x (list 1 2 3)) |#
#| (define y (list 4 5 6)) |#

;; (append x y)
;; (1 2 3 4 5 6)

;; (cons x y)

;; ((1 2 3) 4 5 6)

;; (list x y)

;; ((1 2 3) (4 5 6))

#| (#%provide x) |#
#| (define x (list (list 1 2) (list 3 4))) |#

#| 2.27 |#

(#%provide deep-reverse)
(define (deep-reverse l)
  (define (iter res xs)
    (if (null? xs)
      res
      (let
        ((x (if (pair? (car xs)) (deep-reverse (car xs)) (car xs))))
        (iter (cons x res) (cdr xs)))))
  (iter nil l))

#| 2.28 |#

(#%provide fringe)
(define (fringe x)
  (cond
    ((null? x) nil)
    ((not (pair? x)) (list x))
    ((append (fringe (car x)) (fringe (cdr x))))))

#| (fringe x) |#
#| (fringe (list x x)) |#

#| 2.29 |#

(#%provide make-mobile)
(define (make-mobile left right)
  (list left right))

(#%provide make-branch)
(define (make-branch len structure)
  (list len structure))

(#%provide left-branch)
(define (left-branch x)
  (car x))

(#%provide right-branch)
(define (right-branch x)
  (car (cdr x)))

(#%provide branch-length)
(define (branch-length x)
  (car x))

(#%provide branch-structure)
(define (branch-structure x)
  (car (cdr x)))

(#%provide total-weight)
(define (total-weight m)
  (let
    ((l (branch-structure (left-branch m)))
     (r (branch-structure (right-branch m))))
    (+
      (if (not (pair? l)) l (total-weight l))
      (if (not (pair? r)) r (total-weight r)))))

(#%provide balanced?)
(define (balanced? m)
  (let
    ((l (branch-structure (left-branch m)))
     (r (branch-structure (right-branch m)))
     (llen (branch-length (left-branch m)))
     (rlen (branch-length  (right-branch m))))
    (let
      ((lweight (if (not (pair? l)) l (total-weight l)))
       (rweight (if (not (pair? r)) r (total-weight r))))
      (and
        (or (not (pair? l)) (balanced? l))
        (or (not (pair? r)) (balanced? r))
        (= (* lweight llen) (* rweight rlen))))))

(#%provide scale-tree)
(define (scale-tree tree factor)
  (cond
    ((null? tree) nil)
    ((not (pair? tree)) (* tree factor))
    (else
      (cons
        (scale-tree (car tree) factor)
        (scale-tree (cdr tree) factor)))))

(#%provide scale-tree-)
(define (scale-tree- tree factor)
  (map
    (lambda (sub-tree)
      (if (pair? sub-tree)
        (scale-tree sub-tree factor)
        (* sub-tree factor)))
    tree))

#| 2.30 |#

(#%provide square-tree-direct)
(define (square-tree-direct tree)
  (cond
    ((null? tree) nil)
    ((not (pair? tree)) (square tree))
    (else
      (cons
        (square-tree-direct (car tree))
        (square-tree-direct (cdr tree))))))

(#%provide square-tree)
(define (square-tree tree)
  (map
    (lambda (sub-tree)
      (if (pair? sub-tree)
        (square-tree sub-tree)
        (square sub-tree)))
    tree))

#| 2.31 |#

(#%provide tree-map)
(define (tree-map proc tree)
  (cond
    ((null? tree) nil)
    ((not (pair? tree)) (proc tree))
    (else
      (cons
        (tree-map proc (car tree))
        (tree-map proc (cdr tree))))))

(#%provide square-tree-)
(define (square-tree- tree)
  (tree-map square tree))

#| 2.32 |#

(#%provide subsets)
(define (subsets s)
  (if (null? s)
    (list nil)
    (let
      ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(#%provide sum-odd-squares-)
(define (sum-odd-squares- tree)
  (cond
    ((null? tree) 0)
    ((not (pair? tree))
     (if (odd? tree) (square tree) 0))
    (else
      (+
        (sum-odd-squares (car tree))
        (sum-odd-squares (cdr tree))))))

(define (fib n)
  (define (fib-iter a b p q i)
    (cond
      ((= i 0) b)
      ((even? i)
       (fib-iter
         a
         b
         (+ (square p) (square q))
         (+ (* 2 p q) (square q))
         (/ i 2)))
      (else
        (fib-iter
          (+ (* b q) (* a q) (* a p))
          (+ (* b p) (* a q))
          p
          q
          (- i 1)))))
  (fib-iter 1 0 0 1 n))

(#%provide even-fibs-)
(define (even-fibs- n)
  (define (next k)
    (if (> k n)
      nil
      (let ((f (fib k)))
        (if (even? f)
          (cons f (next (+ k 1)))
          (next (+ k 1))))))
  (next 0))

(#%provide filter-)
(define (filter- predicate sequence)
  (cond
    ((null? sequence) nil)
    ((predicate (car sequence))
     (cons
       (car sequence)
       (filter- predicate (cdr sequence))))
    (else (filter- predicate (cdr sequence)))))

(#%provide accumulate)
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op
      (car sequence)
      (accumulate op initial (cdr sequence)))))

(#%provide enumerate-interval)
(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(#%provide enumerate-tree)
(define (enumerate-tree tree)
  (cond
    ((null? tree) nil)
    ((not (pair? tree)) (list tree))
    (else
      (append
        (enumerate-tree (car tree))
        (enumerate-tree (cdr tree))))))

(#%provide sum-odd-squares)
(define (sum-odd-squares tree)
  (accumulate + 0
    (map square
      (filter- odd?
        (enumerate-tree tree)))))

(#%provide even-fibs)
(define (even-fibs n)
  (accumulate cons nil
    (filter- even?
      (map fib
        (enumerate-interval 0 n)))))

(#%provide list-fib-squares)
(define (list-fib-squares n)
  (accumulate cons nil
    (map square
      (map fib
        (enumerate-interval 0 n)))))

(#%provide product-of-squares-of-odd-elements)
(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1
    (map square
      (filter- odd?
        sequence))))

#| (define (salary-of-highest-paid-programmer records) |#
#|   (accumulate max 0 |#
#|     (map salary |#
#|       (filter- programmer? |#
#|         records)))) |#

#| 2.33 |#

(#%provide map--)
(define (map-- p sequence)
  (accumulate
    (lambda (x y) (cons (p x) y))
    nil
    sequence))

(#%provide append--)
(define (append-- seq1 seq2)
  (accumulate cons seq2 seq1))

(#%provide length--)
(define (length-- sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

#| 2.34 |#

(#%provide horner-eval)
(define (horner-eval x coefficient-sequence)
  (accumulate
    (lambda (this-coeff higher-terms)
      (+ (* higher-terms x) this-coeff))
    0
    coefficient-sequence))

#| 2.35 |#

(#%provide count-leaves-)
(define (count-leaves- t)
  (accumulate
    (lambda (x y) (+ y 1))
    0
    (enumerate-tree t)))

#| 2.36 |#

(#%provide accumulate-n)
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons
      (accumulate op init (map car seqs))
      (accumulate-n op init (map cdr seqs)))))

