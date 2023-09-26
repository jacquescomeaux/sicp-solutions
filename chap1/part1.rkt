#lang sicp

;; Chapter 1
;; Building Abstractions with Procedures

;; 1.1
;; The Elements of Programming

#| 1.1 |#

#| 10 |#
#| 10 |#

#| (+ 5 3 4) |#
#| 12 |#

#| (- 9 1) |#
#| 8 |#

#| (/ 6 2) |#
#| 3 |#

#| (+ (* 2 4) (- 4 6)) |#
#| 6 |#

#| (define a 3) |#
#| () |#

#| (define b (+ a 1)) |#
#| () |#

#| (+ a b (* a b)) |#
#| 19 |#

#| (= a b) |#
#| #f |#

#| (if |#
#|   (and (> b a) (< b (* a b))) |#
#|   b |#
#|   a) |#
#| 4 |#

#| (cond |#
#|   ((= a 4) 6) |#
#|   ((= b 4) (+ 6 7 a)) |#
#|   (else 25)) |#
#| 16 |#

#| (+ 2 (if (> b a) b a)) |#
#| 6 |#

#| (* |#
#|   (cond |#
#|     ((> a b) a) |#
#|     ((< a b) b) |#
#|     (else -1)) |#
#|   (+ a 1)) |#
#| 16 |#

#| 1.2 |#

(/
  (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
  (* 3 (- 6 2) (- 2 7)))


(#%provide square)
(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))

#| 1.3 |#

(#%provide sos-two-larger)
(define (sos-two-larger a b c)
  (if (> a b)
    (sum-of-squares a (if (> b c) b c))
    (sum-of-squares b (if (> a c) a c))))

#| 1.4 |#

(#%provide a-plus-abs-b)
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

#| 1.5 |#

(#%provide p)
(#%provide test)
(define (p) (p))
(define (test x y)
  (if (= x 0)
    0
    y))

#| Applicative order: this will loop forever |#
#| Normal order: this will terminate after one call to test |#

#| (test 0 (p)) |#


(define (average x y)
  (/ (+ x y) 2))

(#%provide sqrt-)
(define (sqrt- x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

#| 1.6 |#

(define (new-if pred then-clause else-clause)
  (cond
    (pred then-clause)
    (else else-clause)))

(#%provide sqrt-new)
(define (sqrt-new x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (new-if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

#| 1.7 |#

(#%provide sqrt-delt)
(define (sqrt-delt x)
  (define (good-enough? last-guess guess)
    (< (/ (abs (- last-guess guess)) x) 0.000000000001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter last-guess guess)
    (if (good-enough? last-guess guess)
      guess
      (sqrt-iter guess (improve guess))))
  (sqrt-iter 1.0 (improve 1.0)))

(square (sqrt-delt 479800023432748679))
(square (sqrt-delt 0.00000024353))

#| 1.8 |#

(#%provide cube)
(define (cube x) (* x x x))

(#%provide cbrt)
(define (cbrt x)
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))
  (define (improve y)
    (/ (+ (/ x (square y)) (* 2 y)) 3))
  (define (cbrt-iter guess)
    (if (good-enough? guess)
      guess
      (cbrt-iter (improve guess))))
  (cbrt-iter 1.0))
