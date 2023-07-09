#lang sicp
(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; Chapter 2
;; Building Abstractions with Data

;; 2.3
;; Symbolic Data

(#%provide memq)
(define (memq item x)
  (cond
    ((null? x) false)
    ((eq? item (car x)) x)
    (else (memq item (cdr x)))))

#| 2.53 |#

;; (list 'a 'b 'c)
;; (a b c)

;; (list (list 'george))
;; ((george))

;; (cdr '((x1 x2) (y1 y2)))
;; ((y1 y2))

;; (cadr '((x1 x2) (y1 y2)))
;; (y1 y2)

;; (pair? (car '(a short list)))
;; #f

;; (memq 'red '((red shoes) (blue socks)))
;; #f

;; (memq 'red '(red shoes blue socks))
;; (red shoes blue socks)

#| 2.54 |#

(#%provide equal?)
(define (equal? a b)
  (cond
    ((and (not (pair? a)) (not (pair? b)))
     (eq? a b))
    ((and (pair? a) (pair? b))
     (and
       (equal? (car a) (car b))
       (equal? (cdr a) (cdr b))))
    (else false)))

#| 2.55 |#

;; (car ''abracadabra)
;; (car '(quote abracadabra))
;; quote

(#%provide deriv)
(define (deriv exp var)
  (cond
    ((number? exp) 0)
    ((variable? exp)
     (if (same-variable? exp var) 1 0))
    ((sum? exp)
     (make-sum
       (deriv (addend exp) var)
       (deriv (augend exp) var)))
    ((product? exp)
     (make-sum
       (make-product
         (multiplier exp)
         (deriv (multiplicand exp) var))
       (make-product
         (deriv (multiplier exp) var)
         (multiplicand exp))))
    ((exponentiation? exp)
     (make-product
       (make-product
         (exponent exp)
         (make-exponentiation
           (base exp)
           (- (exponent exp) 1)))
       (deriv (base exp) var)))
    (else (error "bro what"))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum- a1 a2)
  (cond
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2))
     (+ a1 a2))
    (else (list '+ a1 a2))))

(define (make-product- m1 m2)
  (cond
    ((or (=number? m1 0) (=number? m2 0))
     0)
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ((and (number? m1) (number? m2))
     (* m1 m2))
    (else (list '* m1 m2))))

(define (sum?- x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend- s) (cadr s))

(define (augend- s) (caddr s))

(define (product?- x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier- p)
  (cadr p))

(define (multiplicand- p) (caddr p))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

#| 2.56 |#

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base s) (cadr s))

(define (exponent s) (caddr s))

(define (make-exponentiation u n)
  (cond
    ((not (number? n)) (error "exponent must be a number"))
    ((=number? n 0) 1)
    ((=number? n 1) u)
    (else (list '** u n))))

#| 2.57 |#

(define (augend-- s)
  (cond
    ((null? (cdddr s)) (caddr s))
    (else (cons '+ (cddr s)))))

(define (multiplicand-- p)
  (cond
    ((null? (cdddr p)) (caddr p))
    (else (cons '* (cddr p)))))

#| 2.58 |#

(define (make-sum a1 a2)
  (cond
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2))
     (+ a1 a2))
    (else
      (let
        ((a1- (if (pair? a1) a1 (list a1)))
         (a2- (if (pair? a2) a2 (list a2))))
        (append a1- (cons '+ a2-))))))

(define (make-product m1 m2)
  (cond
    ((or (=number? m1 0) (=number? m2 0))
     0)
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ((and (number? m1) (number? m2))
     (* m1 m2))
    (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (memq '+ x)))

(define (addend s)
  (let ((front (take-until '+ s)))
    (if (product? front) front (car front))))

(define (augend s)
  (let ((end (take-after '+ s)))
    (if
      (or (sum? end) (product? end)) 
      end
      (car end))))

(define (product? x)
  (and
    (pair? x)
    (not (memq '+ x))
    (memq '* x)))

(define (multiplier p)
  (car (take-until '* p)))

(define (multiplicand p)
  (let ((end (take-after '* p)))
    (if (product? end) end (car end))))

(define (take-after item x)
  (cond
    ((null? x) x)
    ((eq? item (car x)) (cdr x))
    (else (take-after item (cdr x)))))

(define (take-until item x)
  (define (iter seen rest)
    (cond
      ((null? rest) rest)
      ((eq? item (car rest)) (reverse seen))
      (else (iter (cons (car rest) seen) (cdr rest)))))
  (iter '() x))

(#%provide element-of-set?-1)
(define (element-of-set?-1 x set)
  (cond
    ((null? set) false)
    ((equal? x (car set)) true)
    (else (element-of-set?-1 x (cdr set)))))

(#%provide adjoin-set-1)
(define (adjoin-set-1 x set)
  (if (element-of-set?-1 x set)
    set
    (cons x set)))

(#%provide intersection-set-1)
(define (intersection-set-1 set1 set2)
  (cond
    ((or (null? set1) (null? set2)) '())
    ((element-of-set?-1 (car set1) set2)
     (cons
       (car set1)
       (intersection-set-1 (cdr set1) set2)))
    (else (intersection-set-1 (cdr set1) set2))))

#| 2.59 |#

(#%provide union-set-1)
(define (union-set-1 set1 set2)
  (cond
    ((null? set1) set2)
    ((element-of-set?-1 (car set1) set2)
     (union-set-1 (cdr set1) set2))
    (else (cons (car set1) (union-set-1 (cdr set1) set2)))))

#| 2.60 |#

(#%provide element-of-set?-2)
(define (element-of-set?-2 x set)
  (cond
    ((null? set) false)
    ((equal? x (car set)) true)
    (else (element-of-set?-2 x (cdr set)))))

(#%provide adjoin-set-2)
(define (adjoin-set-2 x set)=
  (cons x set))

(#%provide intersection-set-2)
(define (intersection-set-2 set1 set2)
  (cond
    ((or (null? set1) (null? set2)) '())
    ((element-of-set?-2 (car set1) set2)
     (cons
       (car set1)
       (intersection-set-2 (cdr set1) set2)))
    (else (intersection-set-2 (cdr set1) set2))))

(#%provide union-set-2)
(define (union-set-2 set1 set2)
  (cond
    ((null? set1) set2)
    (else (append set1 set2))))

(#%provide element-of-set?-3)
(define (element-of-set?-3 x set)
  (cond
    ((null? set) false)
    ((= x (car set)) true)
    ((< x (car set)) false)
    (else (element-of-set?-3 x (cdr set)))))

(#%provide intersection-set-3)
(define (intersection-set-3 set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond
        ((= x1 x2) (cons x1 (intersection-set-3 (cdr set1) (cdr set2))))
        ((< x1 x2) (intersection-set-3 (cdr set1) set2))
        (else (intersection-set-3 set1 (cdr set2)))))))

#| 2.61 |#

(#%provide adjoin-set-3)
(define (adjoin-set-3 x set)
  (cond
    ((null? set) (list x))
    ((= x (car set)) set)
    ((< x (car set)) (cons x set))
    (else (cons (car set) (adjoin-set-3 x (cdr set))))))

#| 2.62 |#

(#%provide union-set-3)
(define (union-set-3 set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    (else
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond
          ((= x1 x2) (cons x1 (union-set-3 (cdr set1) (cdr set2))))
          ((< x1 x2) (cons x1 (union-set-3 (cdr set1) set2)))
          (else (cons x2 (union-set-3 set1 (cdr set2)))))))))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(#%provide element-of-set?-4)
(define (element-of-set?-4 x set)
  (cond
    ((null? set) false)
    ((= x (entry set)) true)
    ((< x (entry set)) (element-of-set?-4 x (left-branch set)))
    (else (element-of-set?-4 x (right-branch set)))))

(#%provide adjoin-set-4)
(define (adjoin-set-4 x set)
  (cond
    ((null? set) (make-tree x '() '()))
    ((= x (entry set)) set)
    ((< x (entry set))
     (make-tree (entry set) (adjoin-set-4 x (left-branch set)) (right-branch set)))
    (else
     (make-tree (entry set) (left-branch set) (adjoin-set-4 x (right-branch set))))))

#| 2.63 |#

(#%provide tree->list-1)
(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append
      (tree->list-1 (left-branch tree))
      (cons
        (entry tree)
        (tree->list-1 (right-branch tree))))))

(#%provide tree->list-2)
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list
        (left-branch tree)
        (cons
          (entry tree)
          (copy-to-list
            (right-branch tree)
            result-list)))))
  (copy-to-list tree '()))

;; tree->list-1 and tree-list-2 produce the same result
;; tree->list-1 has order of growth n^2
;; tree->list-2 has order of growth n

#| 2.64 |#

(#%provide list->tree)
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let
        ((left-result
           (partial-tree elts left-size))
         (right-size (- n (+ left-size 1))))
        (let
          ((left-tree (car left-result))
           (non-left-elts (cdr left-result)))
          (let
            ((this-entry (car non-left-elts))
             (right-result
               (partial-tree (cdr non-left-elts) right-size)))
            (let
              ((right-tree (car right-result))
               (remaining-elts (cdr right-result)))
              (cons
                (make-tree this-entry left-tree right-tree)
                remaining-elts))))))))

;; partial-tree calls itself recursively with size n/2 twice: first to make the
;; left subtree, and then again to make the right subtree. The list of elements
;; for the second call are those that remain after the first call and taking out
;; one for the current entry

;; partial-tree has linear order of growth

#| 2.65 |#

(#%provide intersection-set-4)
(define (intersection-set-4 set1 set2)
  (list->tree
    (intersection-set-3
      (tree->list-2 set1)
      (tree->list-2 set2))))

(#%provide union-set-4)
(define (union-set-4 set1 set2)
  (list->tree
    (union-set-3
      (tree->list-2 set1)
      (tree->list-2 set2))))

(define (lookup-1 given-key set-of-records)
  (cond
    ((null? set-of-records) false)
    ((equal? given-key (key (car set-of-records)))
     (car set-of-records))
    (else (lookup-1 given-key (cdr set-of-records)))))

#| 2.66 |#

(define (make-rec k v) (cons k v))

(define (key rec) (car rec))

(define (value rec) (cdr rec))

(define (lookup-2 k records)
  (cond
    ((null? records) false)
    ((= k (key (entry records))) (entry records))
    ((< k (key (entry records))) (lookup-2 k (left-branch records)))
    (else (lookup-2 k (right-branch records)))))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list
    left
    right
    (append (symbols left) (symbols right))
    (+ (weight left) (weight right))))

(define (left-branch- tree) (car tree))

(define (right-branch- tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(#%provide decode)
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let
        ((next-branch
           (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons
            (symbol-leaf next-branch)
            (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond
    ((= bit 0) (left-branch- branch))
    ((= bit 1) (right-branch- branch))
    (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set- x set)
  (cond
    ((null? set) (list x))
    ((< (weight x) (weight (car set))) (cons x set))
    (else
      (cons
        (car set)
        (adjoin-set- x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set-
        (make-leaf
          (car pair)
          (cadr pair))
        (make-leaf-set (cdr pairs))))))

#| 2.67 |#

(#%provide sample-tree)
(define sample-tree
  (make-code-tree
    (make-leaf 'A 4)
    (make-code-tree
      (make-leaf 'B 2)
      (make-code-tree
        (make-leaf 'D 1)
        (make-leaf 'C 1)))))

(#%provide sample-message)
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

#| A 0 |#
#| D 1 1 0 |#
#| A 0 |#
#| B 1 0 |#
#| B 1 0 |#
#| C 1 1 1 |#
#| A 0 |#

#| 2.68 |#

(define (element-of-set?- x set)
  (cond
    ((null? set) false)
    ((eq? x (car set)) true)
    (else (element-of-set?- x (cdr set)))))

(#%provide encode)
(define (encode message tree)
  (if (null? message)
    '()
    (append
      (encode-symbol (car message) tree)
      (encode (cdr message) tree))))

(define (encode-symbol sym tree)
  (define (search node path)
    (cond
      ((leaf? node) (reverse path))
      ((element-of-set?- sym (symbols (left-branch- node)))
       (search (left-branch- node) (cons 0 path)))
      (else (search (right-branch- node) (cons 1 path)))))
  (if (element-of-set?- sym (symbols tree))
    (search tree '())
    (error "symbol not in tree")))

#| 2.69 |#

(#%provide generate-huffman-tree)
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (cond
    ((null? leaves) (error "empty leaf set"))
    ((null? (cdr leaves)) (car leaves))
    (else
      (successive-merge
        (adjoin-set-
          (make-code-tree (car leaves) (cadr leaves))
          (cddr leaves))))))

#| 2.70 |#

(#%provide rock-tree)
(define rock-tree
  (generate-huffman-tree
    '((A 2)
      (BOOM 1)
      (GET 2)
      (JOB 2)
      (NA 16)
      (SHA 3)
      (YIP 9)
      (WAH 1))))

(#%provide lyrics)
(define lyrics
  '(GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM))

;; (equal? lyrics (decode (encode lyrics rock-tree) rock-tree))
;; #t

;; (length lyrics)
;; 36

;; (length (encode lyrics rock-tree))
;; 84

;; for an eight-symbol fixed-length code, each symbol would require
;; 3 bits, for a total of 3 * 36 = 108 bits

#| 2.71 |#

;; An alphabet of n symbols with relative frequencies
;; 1, 2, 4, ..., 2^n-1

;; With n = 5:
;;     o
;;   /   \
;; A       o 
;;       /   \
;;     B       o
;;           /   \
;;         C       o
;;               /   \
;;             D       E

;; In such a tree, the most frequent symbol requires 1 bit
;; and the least frequent symbol requires n-1 bits

#| 2.72 |#

;; Order of growth in number of steps to encode a symbol
;; using a tree like the one above:

;; Most frequent symbol: Theta(n)
;; Least frequent symbol: Theta(n^2)
