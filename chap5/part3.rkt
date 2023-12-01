#lang sicp
(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; Chapter 5
;; Computing with Register Machines

;; 5.3
;; Storage Allocation and Garbage Collection

;; Memory as Vectors

#| 5.20 |#

#| (define x (cons 1 2)) |#
#| (define y (list x x)) |#

#|          |  0 |  1 |  2 |  3 |  4 | |#
#| ---------|----|----|----|----|----| |#
#| the-cars | n1 | p0 | p0 | |#
#| the-cdrs | n2 | e0 | p1 | |#

#| x = p0 |#
#| y = p2 |#
#| free = 3 |#

#| 5.21 |#

(define (count-leaves tree)
  (cond
    ((null? tree) 0)
    ((not (pair? tree)) 1)
    (else
      (+
        (count-leaves (car tree))
        (count-leaves (cdr tree))))))

(define count-leaves-controller
  '(controller
       (assign continue (label count-leaves-done))
       (assign tree (op read))
     count-leaves
       (test (op null?) (reg tree))
       (branch (label if-null))
       (assign t (op pair?) (reg tree))
       (test (op not) (reg t))
       (branch (label if-leaf))
       (save continue)
       (save tree)
       (assign continue (label after-car-tree))
       (assign tree (op car) (reg tree))
       (goto (label count-leaves))
     after-car-tree
       (restore tree)
       (save tree)
       (save val)
       (assign tree (op cdr) (reg tree))
       (assign continue (label after-cdr-tree))
       (goto (label count-leaves))
     after-cdr-tree
       (assign t (reg val))
       (restore val)
       (restore tree)
       (restore continue)
       (assign val (op +) (reg t) (reg val))
       (goto (reg continue))
     if-null
       (assign val (const 0))
       (goto (reg continue))
     if-leaf
       (assign val (const 1))
       (goto (reg continue))
     count-leaves-done))

(define (count-leaves- tree)
  (define (count-iter tree n)
    (cond
      ((null? tree) n)
      ((not (pair? tree)) (+ n 1))
      (else
        (count-iter
          (cdr tree)
          (count-iter (car tree) n)))))
  (count-iter tree 0))

(define count-leaves-iter-controller
  '(controller
       (assign continue (label count-leaves-done))
       (assign tree (op read))
       (assign n (const 0))
     count-iter
       (test (op null?) (reg tree))
       (branch (label if-null))
       (assign t (op pair?) (reg tree))
       (test (op not) (reg t))
       (branch (label if-leaf))
       (save tree)
       (save continue)
       (assign tree (car tree))
       (assign continue (label after-car-tree))
       (goto (label count-iter))
     after-car-tree
       (restore continue)
       (restore tree)
       (assign tree (cdr tree))
       (goto (label count-iter))
     if-null
       (goto (reg continue))
     if-leaf
       (assign n (op +) (reg n) (const 1))
       (goto (reg continue))
     count-leaves-done))

#| 5.22 |#

(define (append x y)
  (if (null? x)
    y
    (cons (car x) (append (cdr x) y))))

(define append-controller
  '(controller
      (assign x (op read))
      (assign y (op read))
      (assign continue (label append-done))
    append
      (test (op null?) (reg x))
      (branch (label if-null))
      (save x)
      (save continue)
      (assign x (op cdr) (reg x))
      (assign continue (label after-append))
      (goto (label append))
    after-append
      (restore continue)
      (restore x)
      (assign x (op car) (reg x))
      (assign val (op cons) (reg x) (reg val))
      (goto (reg continue))
    if-null
      (assign val (reg y))
      (goto (reg continue))
    append-done))

(#%provide append!)
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define append!-controller
  '(controller
      (assign x (op read))
      (assign y (op read))
      (assign head (reg x))
      (assign rest (op cdr) (reg x))
    last-pair
      (test (op null?) (reg rest))
      (branch (label found-last-pair))
      (assign head (reg rest))
      (assign rest (op cdr) (reg rest))
      (goto (label last-pair))
    found-last-pair
      (perform (op set-cdr!) (reg head) (reg y))))

;; Maintaining the Illusion of Infinite Memory

(define gc-controller
  '(begin-garbage-collection
      (assign free (const 0))
      (assign scan (const 0))
      (assign old (reg root))
      (assign relocate-continue (label reassign-root))
      (goto (label relocate-old-result-in-new))
    reassign-root
      (assign root (reg new))
      (goto (label gc-loop))
    gc-loop
      (test (op =) (reg scan) (reg free))
      (branch (label gc-flip))
      (assign old (op vector-ref) (reg new-cars) (reg scan))
      (assign relocate-continue (label update-car))
      (goto (label relocate-old-result-in-new))
    update-car
      (perform (op vector-set!) (reg new-cars) (reg scan) (reg new))
      (assign old (op vector-ref) (reg new-cdrs) (reg scan))
      (assign relocate-continue (label update-cdr))
      (goto (label relocate-old-result-in-new))
    update-cdr
      (perform (op vector-set!) (reg new-cdrs) (reg scan) (reg new))
      (assign scan (op +) (reg scan) (const 1))
      (goto (label gc-loop))
    relocate-old-result-in-new
      (test (op pointer-to-pair?) (reg old))
      (branch (label pair))
      (assign new (reg old))
      (goto (reg relocate-continue))
    pair
      (assign oldcr (op vector-ref) (reg the-cars) (reg old))
      (test (op broken-heart?) (reg oldcr))
      (branch (label already-moved))
      (assign new (reg free))
      (assign free (op +) (reg free) (const 1))
      (perform (op vector-set!) (reg new-cars) (reg new) (reg olcr))
      (assign oldcr (op vector-ref) (reg the-cdrs) (reg old))
      (perform (op vector-set!) (reg new-cdrs) (reg new) (reg olcr))
      (perform (op vector-set!) (reg the-cars) (reg old) (const broken-heart))
      (perform (op vector-set!) (reg the-cdrs) (reg old) (reg new))
      (goto (reg relocate-continue))
    already-moved
      (assign new (op vector-ref) (reg the-cdrs) (reg old))
      (goto (reg relocate-continue))
    gc-flip
      (assign temp (reg the-cdrs))
      (assign the-cdrs (reg new-cdrs))
      (assign new-cdrs (reg temp))
      (assign temp (reg the-cars))
      (assign the-cars (reg new-cars))
      (assign new-cars (reg temp))))
