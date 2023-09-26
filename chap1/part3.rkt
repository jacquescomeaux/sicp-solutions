#lang sicp

;; Chapter 1
;; Building Abstractions with Procedures

;; 1.3
;; Formulating Abstractions with Higher-Order Procedures

#| 1.29 |#

(#%provide simpson)
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (single-term k)
    (f (+ a (* k h))))
  (define (simpson-term k)
    (+
      (single-term (- k 1))
      (* 4.0 (single-term k))
      (single-term (+ k 1))))
  (define (simpson-next k) (+ k 2))
  (* (/ h 3.0) (sum simpson-term 1 simpson-next n)))

#| 1.30 |#

(#%provide sum-iter)
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))

#| 1.31 |#

(#%provide product)
(define (product term a next b)
  (if (> a b)
    1
    (*
      (term a)
      (product term (next a) next b))))

(#%provide factorial)
(define (factorial n)
  (product id 1 inc n))

(#%provide pi-prod)
(define (pi-prod n)
  (define (pi-term x) (/ (* (- x 1) (+ x 1)) (square x)))
  (define (pi-next x) (+ x 2))
  (* 4.0 (product pi-term 3 pi-next n)))

(#%provide product-iter)
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))

#| 1.32 |#

(#%provide accumulate)
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner
      (term a)
      (accumulate combiner null-value term (next a) next b))))

(#%provide prod-acc)
(define (prod-acc term a next b)
  (accumulate * 1 term a next b))

(#%provide sum-acc)
(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(#%provide acc-iter)
(define (acc-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

#| 1.33 |#

(#%provide filtered-accumulate)
(define (filtered-accumulate combiner null-value pred term a next b)
  (if (> a b)
    null-value
    (if (pred a)
      (combiner
        (term a)
        (filtered-accumulate combiner null-value pred term (next a) next b))
      (filtered-accumulate combiner null-value pred term (next a) next b))))

(#%provide sum-prime-square)
(define (sum-prime-square a b)
  (filtered-accumulate + 0 prime? square a inc b))

(#%provide prod-coprime)
(define (prod-coprime n)
  (define (pred i) (= (gcd- i n) 1))
  (filtered-accumulate * 1 pred id 1 inc n))

(#%provide pi-sum-lam)
(define (pi-sum-lam a b)
  (sum
    (lambda (x) (/ 1.0 (* x (+ x 2))))
    a
    (lambda (x) (+ x 4))
    b))

(#%provide integral-lam)
(define (integral-lam f a b dx)
  (*
    (sum
      f
      (+ a (/ dx 2.0))
      (lambda (x) (+ x dx))
      b)
    dx))

(#%provide f-help)
(define (f-help x y)
  (define (f-helper a b)
    (+
      (* x (square a))
      (* y b)
      (* a b)))
  (f-helper
    (+ 1 (* x y))
    (- 1 y)))

(#%provide f-lam)
(define (f-lam x y)
  ((lambda (a b)
     (+
       (* x (square a))
       (* y b)
       (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(#%provide f-let)
(define (f-let x y)
  (let
    ((a (+ 1 (* x y)))
     (b (- 1 y)))
    (+
     (* x (square a))
     (* y b)
     (* a b))))

(#%provide f-def)
(define (f-def x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+
    (* x (square a))
    (* y b)
    (* a b)))

#| 1.34 |#

;; (define (f g) (g 2))

;; (f square) 4

;; (f (lambda (z) (* z (+ z 1)))) 6

;; (f f) (f 2) (2 2) error

(#%provide search)
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond
          ((positive? test-value)
           (search f neg-point midpoint))
          ((negative? test-value)
           (search f midpoint pos-point))
          (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(#%provide half-interval-method)
(define (half-interval-method f a b)
  (let
    ((a-value (f a))
     (b-value (f b)))
    (cond
      ((and (negative? a-value) (positive? b-value))
       (search f a b))
      ((and (negative? b-value) (positive? a-value))
       (search f b a))
      (else
        (error "Values are not of opposite sign" a b)))))

(define tolerance 0.0001)

(#%provide fixed-point)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(#%provide sqrt-fix)
(define (sqrt-fix x)
  (fixed-point
    (lambda (y) (average y (/ x y)))
    1.0))

#| 1.35 |#

(#%provide golden)
(define (golden)
  (fixed-point
    (lambda (x) (+ 1.0 (/ 1.0 x)))
    1.0))

#| 1.36 |#

(#%provide x-to-the-x)
(define (x-to-the-x)
  (fixed-point
    (lambda (x) (/ (log 1000.0) (log x)))
    2.0))

#| 1.37 |#

(#%provide cont-frac)
(define (cont-frac n d k)
  (define (iter res i)
    (if (= i 0)
      res
      (iter (/ (n i) (+ (d i) res)) (- i 1))))
  (iter 0 k))

;; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
;; 0.6180555555555556

;; Accurate to 4 places after 11 iterations

(#%provide cont-frac-rec)
(define (cont-frac-rec n d k)
  (define (rec i)
    (if (> i k)
      0
      (/ (n i) (+ (d i) (rec (+ i 1))))))
  (rec 1))

#| 1.38 |#

(#%provide e-approx)
(define (e-approx k)
  (define (n i) 1.0)
  (define (d i)
    (if (divides? 3 (+ i 1))
      (* 2 (/ (+ i 1) 3))
      1.0))
  (+ (cont-frac n d k) 2))

#| 1.39 |#

(#%provide tan-cf)
(define (tan-cf x k)
  (define (rec prod sum)
    (let ((stop (+ 1 (* 2 (- k 1)))))
      (if (> sum stop)
        0
        (/ prod (- sum (rec (* prod x) (+ sum 2)))))))
  (rec x 1.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)

(#%provide sqrt-avg-damp)
(define (sqrt-avg-damp x)
  (fixed-point
    (average-damp (lambda (y) (/ x y)))
    1.0))

(#%provide cbrt-avg-damp)
(define (cbrt-avg-damp x)
  (fixed-point
    (average-damp (lambda (y) (/ x (square y))))
    1.0))

(define (deriv g)
  (lambda (x)
    (/
      (- (g (+ x dx)) (g x))
      dx)))

(define dx 0.00001)

((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(#%provide newtons-method)
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(#%provide sqrt-newt)
(define (sqrt-newt x)
  (newtons-method
    (lambda (y) (- (square y) x))
    1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(#%provide sqrt-ad-trans)
(define (sqrt-ad-trans x)
  (fixed-point-of-transform
    (lambda (y) (/ x y))
    average-damp
    1.0))

(#%provide sqrt-newt-trans)
(define (sqrt-newt-trans x)
  (fixed-point-of-transform
    (lambda (y) (- (square y) x))
    newton-transform
    1.0))

#| 1.40 |#

(#%provide cubic)
(define (cubic a b c)
  (lambda (x)
    (+
      (cube x)
      (* a (square x))
      (* b x)
      c)))

;; (newtons-method (cubic 0 0 -8.0) 4.0)

#| 1.41 |#

(#%provide twice)
(define (twice f)
  (lambda (x) (f (f x))))

;; (twice inc 1)
;; 3

;; (((twice (twice twice)) inc) 5)
;; 21

#| 1.42 |#

(#%provide compose-)
(define (compose- f g)
  (lambda (x) (f (g x))))

;; ((compose- square inc) 6)
;; 49

#| 1.43 |#

(#%provide repeated)
(define (repeated f n)
  (if (= n 0)
    (lambda (x) x)
    (compose- (repeated f (- n 1)) f)))

;; ((repeated square 2) 5)
;; 625

#| 1.44 |#

(#%provide smooth)
(define (smooth f)
  (lambda (x)
    (/
      (+
        (f (- x dx))
        (f x)
        (f (+ x dx)))
      3.0)))

(#%provide n-smooth)
(define (n-smooth f n)
  (repeated smooth n))

#| 1.45 |#

(#%provide flog2)
(define (flog2 n) (floor (/ (log n) (log 2))))

(#%provide nth-root)
(define (nth-root n x)
  (fixed-point
    ((repeated average-damp (flog2 n))
     (lambda (y) (/ x (fast-expt-iter y (- n 1)))))
    1.0))

#| 1.46 |#

(#%provide iterative-improve)
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (define (iter x)
      (if (good-enough? x)
        x
        (iter (improve x))))
    (iter guess)))

(#%provide sqrt-it-imp)
(define (sqrt-it-imp x)
  ((iterative-improve
     (lambda (guess) (< (abs (- (square guess) x)) 0.001))
     (lambda (guess) (average guess (/ x guess))))
   1.0))

(#%provide fixed-point-it-imp)
(define (fixed-point-it-imp f first-guess)
  ((iterative-improve
     (lambda (guess) (< (abs (- guess (f guess))) tolerance))
     f)
   first-guess))
