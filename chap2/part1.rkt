#lang sicp

;; Chapter 2
;; Building Abstractions with Data

;; 2.1
;; Introduction to Data Abstraction

(define (make-rat-1 n d) (cons n d))

(#%provide numer)
(define (numer x) (car x))

(#%provide denom)
(define (denom x) (cdr x))

(#%provide print-rat)
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(#%provide add-rat)
(define (add-rat x y)
  (make-rat
    (+
      (* (numer x) (denom y))
      (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(#%provide sub-rat)
(define (sub-rat x y)
  (make-rat
    (-
      (* (numer x) (denom y))
      (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(#%provide mul-rat)
(define (mul-rat x y)
  (make-rat
    (* (numer x) (numer y))
    (* (denom x) (denom y))))

(#%provide div-rat)
(define (div-rat x y)
  (make-rat
    (* (numer x) (denom y))
    (* (denom x) (numer y))))

(#%provide equal-rat?)
(define (equal-rat? x y)
  (=
    (* (numer x) (denom y))
    (* (numer y) (denom x))))

(define (make-rat-2 n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

#| 2.1 |#

(#%provide make-rat)
(define (make-rat n d)
  (let ((g (gcd n d)))
    (let
      ((n-norm (/ n g))
       (d-norm (/ d g)))
      (if (< d 0)
        (cons (* -1 n-norm) (* -1 d-norm))
        (cons n-norm d-norm)))))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

(print-rat one-half)
(print-rat (add-rat one-half one-third))
(print-rat (add-rat one-third one-third))

#| 2.2 |#

(#%provide make-segment)
(define (make-segment p q) (cons p q))

(#%provide start-segment)
(define (start-segment pq) (car pq))

(#%provide end-segment)
(define (end-segment pq) (cdr pq))

(#%provide make-point)
(define (make-point x y) (cons x y))

(#%provide x-point)
(define (x-point p) (car p))

(#%provide y-point)
(define (y-point p) (cdr p))

(#%provide print-point)
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (average a b)
  (/ (+ a b) 2))

(#%provide midpoint-segment)
(define (midpoint-segment pq)
  (let
    ((p (start-segment pq))
     (q (end-segment pq)))
    (make-point
      (average (x-point p) (x-point q))
      (average (y-point p) (y-point q)))))

#| 2.3 |#

(#%provide make-rect-1)
(define (make-rect-1 p width height) (cons p (cons width height)))
(define (rect-corner-1 rect) (car rect))
(define (rect-width-1 rect) (car (cdr rect)))
(define (rect-height-1 rect) (cdr (cdr rect)))

(#%provide make-rect-2)
(define (make-rect-2 p width height)
  (cons
    p
    (make-point
      (+ (x-point p) width)
      (+ (y-point p) height))))

(define (rect-corner-2 rect) (car rect))
(define (rect-width-2 rect)
  (-
    (x-point (cdr rect))
    (x-point (car rect))))
(define (rect-height-2 rect)
  (-
    (y-point (cdr rect))
    (y-point (car rect))))

(#%provide perimeter)
(define (perimeter rect)
  (* 2 (+ (rect-width-2 rect) (rect-height-2 rect))))

(#%provide area)
(define (area rect)
  (* (rect-width-2 rect) (rect-height-2 rect)))

(define (cons- x y)
  (define (dispatch m)
    (cond
      ((= m 0) x)
      ((= m 1) y)
      (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car- z) (z 0))
(define (cdr- z) (z 1))

#| 2.4 |#

(define (cons-- x y)
  (lambda (m) (m x y)))

(define (car-- z)
  (z (lambda (p q) p)))

(define (cdr-- z)
  (z (lambda (p q) q)))

#| (car-- (cons-- x y)) |#
#| (car-- (lambda (m) (m x y))) |#
#| ((lambda (m) (m x y)) (lambda (p q) p)) |#
#| ((lambda (p q) p) x y) |#
#| x |#

#| (cdr-- (cons-- x y)) |#
#| (cdr-- (lambda (m) (m x y))) |#
#| ((lambda (m) (m x y)) (lambda (p q) q)) |#
#| ((lambda (p q) q) x y) |#
#| y |#

#| 2.5 |#

(#%provide cons-3)
(define (cons-3 x y)
  (* (expt 2 x) (expt 3 y)))

(#%provide car-3)
(define (car-3 z)
  (define (iter z res)
    (if (= (remainder z 2) 0)
      (iter (/ z 2) (+ res 1))
      res))
  (iter z 0))

(#%provide cdr-3)
(define (cdr-3 z)
  (define (iter z res)
    (if (= (remainder z 3) 0)
      (iter (/ z 3) (+ res 1))
      res))
  (iter z 0))

#| 2.6 |#

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add n m)
  (lambda (f) (lambda (x) (n f ((m f) x)))))

(define (add-interval x y)
  (make-interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let
    ((p1 (* (lower-bound x) (lower-bound y)))
     (p2 (* (lower-bound x) (upper-bound y)))
     (p3 (* (upper-bound x) (lower-bound y)))
     (p4 (* (upper-bound x) (lower-bound y))))
    (make-interval
      (min p1 p2 p3 p4)
      (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
    x
    (make-interval
      (/ 1.0 (upper-bound y))
      (/ 1.0 (lower-bound y)))))

#| 2.7 |#

(#%provide make-interval)
(define (make-interval a b)
  (cons a b))

(#%provide upper-bound)
(define (upper-bound x)
  (cdr x))

(#%provide lower-bound)
(define (lower-bound x)
  (car x))

#| 2.8 |#

(define (sub-interval x y)
  (let
    ((p1 (- (lower-bound x) (lower-bound y)))
     (p2 (- (lower-bound x) (upper-bound y)))
     (p3 (- (upper-bound x) (lower-bound y)))
     (p4 (- (upper-bound x) (lower-bound y))))
    (make-interval
      (min p1 p2 p3 p4)
      (max p1 p2 p3 p4))))

(#%provide make-center-width)
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(#%provide center)
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(#%provide width)
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

#| 2.12 |#

(#%provide make-center-percent)
(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

(#%provide percent)
(define (percent i)
  (/ (width i) (center i)))
