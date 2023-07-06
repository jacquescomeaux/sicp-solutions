#lang sicp
(#%require graphics/graphics)
(open-graphics)
(define vp (open-viewport "Picture Language" 500 500))

(#%provide clear)
(define (clear) ((clear-viewport vp)))

(define (vector-to-posn v)
  (make-posn (xcor-vect v) (- 500 (ycor-vect v))))

(#%provide line)
(define (line a b)
  ((draw-line vp)
    (vector-to-posn a)
    (vector-to-posn b)))

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

(#%provide filter)
(define (filter predicate sequence)
  (cond
    ((null? sequence) nil)
    ((predicate (car sequence))
     (cons
       (car sequence)
       (filter predicate (cdr sequence))))
    (else (filter predicate (cdr sequence)))))

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
      (filter odd?
        (enumerate-tree tree)))))

(#%provide even-fibs)
(define (even-fibs n)
  (accumulate cons nil
    (filter even?
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
      (filter odd?
        sequence))))

#| (define (salary-of-highest-paid-programmer records) |#
#|   (accumulate max 0 |#
#|     (map salary |#
#|       (filter programmer? |#
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

#| 2.37 |#

(#%provide dot-product)
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(#%provide matrix-*-vector)
(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(#%provide transpose)
(define (transpose mat)
  (accumulate-n cons nil mat))

(#%provide matrix-*-matrix)
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector m v)) cols)))

#| 2.38 |#

(#%provide fold-left)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter
        (op result (car rest))
        (cdr rest))))
  (iter initial sequence))

(#%provide fold-right)
(define fold-right accumulate)

#| (fold-right / 1 (list 1 2 3)) |#
#| (fold-left / 1 (list 1 2 3)) |#
#| (fold-right list nil (list 1 2 3)) |#
#| (fold-left list nil (list 1 2 3)) |#

#| 3/2 |#
#| 1/6 |#
#| (list 1 (list 2 (list 3 nil))) |#
#| (list (list (list nil 1) 2) 3) |#

  #| A |#
#| 1   \ |#
  #|     A |#
  #|   A   \ |#
  #| 2   \   n |#
  #|       A |#
  #|     A   \ |#
  #|   3   A   n |#
  #|     n   n |#

          #| A |#
        #| /   A |#
      #| A   3   n |#
    #| /   A |#
  #| A   2   n |#
#| n   A |#
  #| 1   n |#

;; op should be associative for fold-left and fold-right to
;; give the same result


#| 2.39 |#

(#%provide reverse-right)
(define (reverse-right sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(#%provide reverse-left)
(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(#%provide flatmap)
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next n)
    (if (= n 2) 3 (+ n 2)))
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(#%provide prime?)
(define (prime? n)
  (= n (smallest-divisor n)))

(#%provide prime-sum-pairs)
(define (prime-sum-pairs n)
  (map make-pair-sum
    (filter prime-sum?
      (flatmap
        (lambda (i)
          (map
            (lambda (j) (list i j))
            (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n)))))

(define (remove item sequence)
  (filter
    (lambda (x) (not (= x item)))
    sequence))

(#%provide permutations)
(define (permutations s)
  (if (null? s)
    (list nil)
    (flatmap
      (lambda (x)
        (map
          (lambda (p) (cons x p))
          (permutations (remove x s))))
      s)))

#| 2.40 |#

(#%provide unique-pairs)
(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map
        (lambda (j) (list i j))
        (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(#%provide prime-sum-pairs-)
(define (prime-sum-pairs- n)
  (map make-pair-sum
    (filter prime-sum?
      (unique-pairs n))))

#| 2.41 |#

(#%provide unique-triple-sum)
(define (unique-triple-sum n)
  (filter
    (lambda (xyz) (= (accumulate + 0 xyz) n))
    (let ((s (enumerate-interval 1 n)))
      (flatmap
        (lambda (x)
          (let ((s- (remove x s)))
            (flatmap
              (lambda (y)
                (map
                  (lambda (z) (list x y z))
                  (remove y s-)))
              s-)))
        s))))

#| 2.42 |#

(define (make-pos r c) (list r c))

(define (row pos) (car pos))

(define (col pos) (cadr pos))

(define empty-board nil)

(define (adjoin-position r c board)
  (cons (make-pos r c) board))

(define (find-by-col k board)
  (car
    (filter
      (lambda (pos) (= (col pos) k))
      board)))

(define (all-but k board)
  (filter
    (lambda (pos) (not (= (col pos) k)))
    board))

(define (safe? k board)
  (let
    ((new-pos (find-by-col k board)))
    (accumulate (lambda (x y) (and x y)) true
      (map
        (lambda (q)
          (not
            (or
              (= (row q) (row new-pos))
              (= (+ (row q) (col q)) (+ (row new-pos) (col new-pos)))
              (= (+ (row q) (col new-pos)) (+ (row new-pos) (col q))))))
        (cdr board)))))

(#%provide queens)
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map
              (lambda (new-row)
                (adjoin-position new-row k rest-of-queens))
              (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

#| 2.43 |#

(#%provide slow-queens)
(define (slow-queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (new-row)
            (map
              (lambda (rest-of-queens)
                (adjoin-position new-row k rest-of-queens))
              (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(define (beside- painter1 painter2)
  (let
    ((split-point (make-vect 0.5 0)))
    (let
      ((paint-left
         (transform-painter
           painter1
           (make-vect 0 0)
           split-point
           (make-vect 0 1)))
       (paint-right
         (transform-painter
           painter2
           split-point
           (make-vect 1 0)
           (make-vect 0.5 1))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below-- painter1 painter2)
  (rotate270
    (beside-
      (rotate90 painter2)
      (rotate90 painter1))))

(#%provide right-split)
(define (right-split painter n)
  (if (= n 0)
    painter
    (let
      ((smaller (right-split painter (- n 1))))
      (beside- painter (below-- smaller smaller)))))

(#%provide corner-split)
(define (corner-split painter n)
  (if (= n 0)
    painter
    (let
      ((up (up-split painter (- n 1)))
       (right (right-split painter (- n 1))))
      (let
        ((top-left (beside- up up))
         (bottom-right (below-- right right))
         (corner (corner-split painter (- n 1))))
        (beside-
          (below-- painter top-left)
          (below-- bottom-right corner))))))

(define (flip-vert- painter)
  (transform-painter
    painter
    (make-vect 0 1)
    (make-vect 1 1)
    (make-vect 0 0)))

(#%provide square-limit)
(define (square-limit painter n)
  (let
    ((quarter (corner-split painter n)))
    (let
      ((half (beside- (flip-horiz quarter) quarter)))
      (below-- (flip-vert- half) half))))

#| 2.44 |#

(#%provide up-split)
(define (up-split painter n)
  (if (= n 0)
    painter
    (let
      ((smaller (up-split painter (- n 1))))
      (below-- painter (beside- smaller smaller)))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let
      ((top (beside (tl painter) (tr painter)) )
       (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let
    ((combine4
       (square-of-four 
         identity flip-vert-
         identity flip-vert-)))
    (combine4 painter)))

(define flipped-pairs- (square-of-four identity flip-vert- identity flip-vert-))

(#%provide square-limit-)
(define (square-limit- painter n)
  (let
    ((combine4
       (square-of-four
         flip-horiz identity
         rotate180 flip-vert-)))
    (combine4 (corner-split painter n))))

#| 2.45 |#

(#%provide split)
(define (split a b)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let
        ((smaller ((split a b) painter (- n 1))))
        (a painter (b smaller smaller))))))

(#%provide right-split-)
(define right-split- (split beside- below--))

(#%provide up-split-)
(define up-split- (split below-- beside-))

(#%provide frame-coord-map)
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect
        (scale-vect
          (xcor-vect v)
          (edge1-frame frame))
        (scale-vect
          (ycor-vect v)
          (edge2-frame frame))))))

#| 2.46 |#

(#%provide make-vect)
(define (make-vect x y)
  (list x y))

(#%provide xcor-vect)
(define (xcor-vect v)
  (car v))

(#%provide ycor-vect)
(define (ycor-vect v)
  (cadr v))

(#%provide add-vect)
(define (add-vect v u)
  (map + v u))

(#%provide sub-vect)
(define (sub-vect v u)
  (map - v u))

(#%provide scale-vect)
(define (scale-vect s v)
  (map (lambda (x) (* x s)) v))

#| 2.47 |#

(#%provide make-frame)
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(#%provide origin-frame)
(define (origin-frame f) (car f))

(#%provide edge1-frame)
(define (edge1-frame f) (cadr f))

(#%provide edge2-frame)
(define (edge2-frame f) (caddr f))

(#%provide make-frame-)
(define (make-frame- origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(#%provide origin-frame-)
(define (origin-frame- f) (car f))

(#%provide edge1-frame-)
(define (edge1-frame- f) (cadr f))

(#%provide edge2-frame-)
(define (edge2-frame- f) (cddr f))

(#%provide default-frame)
(define default-frame
  (make-frame
    (make-vect 20 20)
    (make-vect 460 0)
    (make-vect 0 460)))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each-
      (lambda (segment)
        (line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

#| 2.48 |#

(#%provide make-segment)
(define (make-segment start end)
  (cons start end))

(#%provide start-segment)
(define (start-segment s) (car s))

(#%provide end-segment)
(define (end-segment s) (cdr s))

#| 2.49 |#

(#%provide outline)
(define (outline frame)
  (let
    ((segments
       (let
         ((a (make-vect 0 0))
          (b (make-vect 0 1))
          (c (make-vect 1 1))
          (d (make-vect 1 0)))
         (list
           (make-segment a b)
           (make-segment b c)
           (make-segment c d)
           (make-segment d a)))))
    ((segments->painter segments) frame)))

(#%provide cross)
(define (cross frame)
  (let
    ((segments
       (let
         ((a (make-vect 0 0))
          (b (make-vect 0 1))
          (c (make-vect 1 1))
          (d (make-vect 1 0)))
         (list
           (make-segment a c)
           (make-segment b d)))))
    ((segments->painter segments) frame)))

(#%provide diamond)
(define (diamond frame)
  (let
    ((segments
       (let
         ((ab (make-vect 0 0.5))
          (bc (make-vect 0.5 1))
          (cd (make-vect 1 0.5))
          (da (make-vect 0.5 0)))
         (list
           (make-segment ab bc)
           (make-segment bc cd)
           (make-segment cd da)
           (make-segment da ab)))))
    ((segments->painter segments) frame)))

(#%provide wave)
(define wave
  (segments->painter
   (list
    (make-segment (make-vect 0.20 0.00) (make-vect 0.35 0.50))
    (make-segment (make-vect 0.35 0.50) (make-vect 0.30 0.60))
    (make-segment (make-vect 0.30 0.60) (make-vect 0.15 0.45))
    (make-segment (make-vect 0.15 0.45) (make-vect 0.00 0.60))
    (make-segment (make-vect 0.00 0.80) (make-vect 0.15 0.65))
    (make-segment (make-vect 0.15 0.65) (make-vect 0.30 0.70))
    (make-segment (make-vect 0.30 0.70) (make-vect 0.40 0.70))
    (make-segment (make-vect 0.40 0.70) (make-vect 0.35 0.85))
    (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1.00))
    (make-segment (make-vect 0.60 1.00) (make-vect 0.65 0.85))
    (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.70))
    (make-segment (make-vect 0.60 0.70) (make-vect 0.75 0.70))
    (make-segment (make-vect 0.75 0.70) (make-vect 1.00 0.40))
    (make-segment (make-vect 1.00 0.20) (make-vect 0.60 0.48))
    (make-segment (make-vect 0.60 0.48) (make-vect 0.80 0.00))
    (make-segment (make-vect 0.40 0.00) (make-vect 0.50 0.30))
    (make-segment (make-vect 0.50 0.30) (make-vect 0.60 0.00)))))

#| 2.50 |#

(#%provide transform-painter)
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let
      ((m (frame-coord-map frame)))
      (let
        ((new-origin (m origin)))
        (painter
          (make-frame
            new-origin
            (sub-vect (m corner1) new-origin)
            (sub-vect (m corner2) new-origin)))))))

(#%provide flip-vert)
(define (flip-vert painter)
  (transform-painter
    painter
    (make-vect 0 1)
    (make-vect 1 1)
    (make-vect 0 0)))

(#%provide shrink-to-upper-right)
(define (shrink-to-upper-right painter)
  (transform-painter
    painter
    (make-vect 0.5 0.5)
    (make-vect 1.0 0.5)
    (make-vect 0.5 1.0)))

(#%provide rotate90)
(define (rotate90 painter)
  (transform-painter
    painter
    (make-vect 1 0)
    (make-vect 1 1)
    (make-vect 0 0)))

(#%provide squash-inwards)
(define (squash-inwards painter)
  (transform-painter
    painter
    (make-vect 0 0)
    (make-vect 0.65 0.35)
    (make-vect 0.35 0.65)))

#| 2.50 |#

(#%provide beside)
(define (beside painter1 painter2)
  (let
    ((split-point (make-vect 0.5 0)))
    (let
      ((paint-left
         (transform-painter
           painter1
           (make-vect 0 0)
           split-point
           (make-vect 0 1)))
       (paint-right
         (transform-painter
           painter2
           split-point
           (make-vect 1 0)
           (make-vect 0.5 1))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(#%provide flip-horiz)
(define (flip-horiz painter)
  (transform-painter
    painter
    (make-vect 1 0)
    (make-vect 0 0)
    (make-vect 1 1)))

(#%provide rotate180)
(define (rotate180 painter)
  (transform-painter
    painter
    (make-vect 1 1)
    (make-vect 0 1)
    (make-vect 1 0)))

(#%provide rotate270)
(define (rotate270 painter)
  (transform-painter
    painter
    (make-vect 0 1)
    (make-vect 0 0)
    (make-vect 1 1)))

#| 2.51 |#

(#%provide below)
(define (below painter1 painter2)
  (let
    ((split-point (make-vect 0 0.5)))
    (let
      ((paint-bottom
         (transform-painter
           painter1
           (make-vect 0 0)
           (make-vect 1 0)
           split-point))
       (paint-top
         (transform-painter
           painter2
           split-point
           (make-vect 1 0.5)
           (make-vect 0 1))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(#%provide below-)
(define (below- painter1 painter2)
  (rotate270
    (beside
      (rotate90 painter2)
      (rotate90 painter1))))

#| 2.52 |#

(#%provide wave+)
(define wave+
  (segments->painter
   (list
    (make-segment (make-vect 0.20 0.00) (make-vect 0.35 0.50))
    (make-segment (make-vect 0.35 0.50) (make-vect 0.30 0.60))
    (make-segment (make-vect 0.30 0.60) (make-vect 0.15 0.45))
    (make-segment (make-vect 0.15 0.45) (make-vect 0.00 0.60))
    (make-segment (make-vect 0.00 0.80) (make-vect 0.15 0.65))
    (make-segment (make-vect 0.15 0.65) (make-vect 0.30 0.70))
    (make-segment (make-vect 0.30 0.70) (make-vect 0.40 0.70))
    (make-segment (make-vect 0.40 0.70) (make-vect 0.35 0.85))
    (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1.00))
    (make-segment (make-vect 0.60 1.00) (make-vect 0.65 0.85))
    (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.70))
    (make-segment (make-vect 0.60 0.70) (make-vect 0.75 0.70))
    (make-segment (make-vect 0.75 0.70) (make-vect 1.00 0.40))
    (make-segment (make-vect 1.00 0.20) (make-vect 0.60 0.48))
    (make-segment (make-vect 0.60 0.48) (make-vect 0.80 0.00))
    (make-segment (make-vect 0.40 0.00) (make-vect 0.50 0.30))
    (make-segment (make-vect 0.45 0.95) (make-vect 0.45 0.85)) ; new
    (make-segment (make-vect 0.55 0.95) (make-vect 0.55 0.85)) ; new
    (make-segment (make-vect 0.40 0.80) (make-vect 0.60 0.80)) ; new
    (make-segment (make-vect 0.50 0.30) (make-vect 0.60 0.00)))))

(#%provide corner-split+)
(define (corner-split+ painter n)
  (if (= n 0)
    painter
    (let
      ((up (up-split painter (- n 1)))
       (right (right-split painter (- n 1))))
      (let
        ((top-left up)
         (bottom-right right)
         (corner (corner-split+ painter (- n 1))))
        (beside
          (below painter top-left)
          (below bottom-right corner))))))

(#%provide square-limit+-)
(define (square-limit+- painter n)
  (let
    ((quarter (corner-split+ painter n)))
    (let
      ((half (beside- (flip-horiz quarter) quarter)))
      (below-- (flip-vert- half) half))))

(#%provide square-limit+)
(define (square-limit+ painter n)
  (let
    ((combine4
       (square-of-four
         flip-vert rotate180
         identity flip-horiz)))
    (combine4 (corner-split painter n))))
