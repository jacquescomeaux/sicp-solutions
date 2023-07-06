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

#| 1.9 |#

;; recursive process:
#| (define (+ a b) |#
#|   (if (= a 0) |#
#|     b |#
#|     (inc (+ (dec a) b)))) |#

;; iterative process:
#| (define (+ a b) |#
#|   (if (= a 0) |#
#|     b |#
#|     (+ (dec a) (inc b)))) |#

#| 1.10 |#

(#%provide A)
(define (A x y)
  (cond
    ((= y 0) 0)
    ((= x 0) (* 2 y))
    ((= y 1) 2)
    (else
      (A
        (- x 1)
        (A x (- y 1))))))

#| (A 1 10) |#
#| (A 0 (A 1 9)) |#
#| (A 0 (A 0 (A 1 8))) |#
#| (A 0 (A 0 (A 0 (A 1 7)))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 1 6))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 1 (A 1 5)))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3)))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1)))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 2)))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 4))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 8))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16)))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 16)))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 32))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 32))))) |#
#| (A 0 (A 0 (A 0 (A 0 64)))) |#
#| (A 0 (A 0 (A 0 (* 2 64)))) |#
#| (A 0 (A 0 (A 0 128))) |#
#| (A 0 (A 0 (* 2 128))) |#
#| (A 0 (A 0 256)) |#
#| (A 0 (* 2 256)) |#
#| (A 0 512) |#
#| (* 2 512) |#
#| 1024 |#

#| (A 2 4) |#
#| (A 1 (A 2 3)) |#
#| (A 1 (A 1 (A 2 2))) |#
#| (A 1 (A 1 (A 1 (A 2 1)))) |#
#| (A 1 (A 1 (A 1 2))) |#
#| (A 1 (A 1 (A 0 (A 1 1)))) |#
#| (A 1 (A 1 (A 0 2))) |#
#| (A 1 (A 1 (* 2 2))) |#
#| (A 1 (A 1 4)) |#
#| (A 1 (A 0 (A 1 3))) |#
#| (A 1 (A 0 (A 0 (A 1 2)))) |#
#| (A 1 (A 0 (A 0 (A 0 (A 1 1))))) |#
#| (A 1 (A 0 (A 0 (A 0 2)))) |#
#| (A 1 (A 0 (A 0 (* 2 2)))) |#
#| (A 1 (A 0 (A 0 4))) |#
#| (A 1 (A 0 (* 2 4))) |#
#| (A 1 (A 0 8)) |#
#| (A 1 (* 2 8)) |#
#| (A 1 16) |#
#| (A 0 (A 1 15)) |#
#| (A 0 (A 0 (A 1 14))) |#
#| (A 0 (A 0 (A 0 (A 1 13)))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 1 12))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11)))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 9)))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 8))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 7)))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 6))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 (A 1 5)))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3)))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2))))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1)))))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2))))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 2)))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 4))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 8))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16)))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 16)))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 64)))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 64)))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 128))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 128))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 256)))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 256)))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 512))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 512))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024)))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 1024)))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 2048))))) |#
#| (A 0 (A 0 (A 0 (A 0 (* 2 2048))))) |#
#| (A 0 (A 0 (A 0 (A 0 4096)))) |#
#| (A 0 (A 0 (A 0 (* 2 4096)))) |#
#| (A 0 (A 0 (A 0 8182))) |#
#| (A 0 (A 0 (* 2 8182))) |#
#| (A 0 (A 0 16364)) |#
#| (A 0 (* 2 16364)) |#
#| (A 0 32768) |#
#| (* 2 32768) |#
#| 65536 |#


#| (A 3 3) |#
#| (A 2 (A 3 2)) |#
#| (A 2 (A 2 (A 3 1))) |#
#| (A 2 (A 2 2)) |#
#| (A 2 (A 1 (A 2 1))) |#
#| (A 2 (A 1 2)) |#
#| (A 2 (A 0 (A 1 1))) |#
#| (A 2 (A 0 2)) |#
#| (A 2 (* 2 2)) |#
#| (A 2 4) |#
#| (A 1 (A 2 3)) |#
#| (A 1 (A 1 (A 2 2))) |#
#| (A 1 (A 1 (A 1 (A 2 1)))) |#
#| (A 1 (A 1 (A 1 2))) |#
#| (A 1 (A 1 (A 0 (A 1 1)))) |#
#| (A 1 (A 1 (A 0 2))) |#
#| (A 1 (A 1 (* 2 2))) |#
#| (A 1 (A 1 4)) |#
#| (A 1 (A 0 (A 1 3))) |#
#| (A 1 (A 0 (A 0 (A 1 2)))) |#
#| (A 1 (A 0 (A 0 (A 0 (A 1 1))))) |#
#| (A 1 (A 0 (A 0 (A 0 2)))) |#
#| (A 1 (A 0 (A 0 (* 2 2)))) |#
#| (A 1 (A 0 (A 0 4))) |#
#| (A 1 (A 0 (* 2 4))) |#
#| (A 1 (A 0 8)) |#
#| (A 1 (* 2 8)) |#
#| (A 1 16) |#
#| (A 0 (A 1 15)) |#
#| (A 0 (A 0 (A 1 14))) |#
#| (A 0 (A 0 (A 0 (A 1 13)))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 1 12))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11)))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 9)))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 8))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 7)))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 6))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 (A 1 5)))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3)))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2))))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1)))))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2))))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 2)))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 4))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 8))))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16)))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 16)))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32))))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 64)))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 64)))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 128))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 128))))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 256)))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 256)))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 512))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 512))))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024)))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 1024)))))) |#
#| (A 0 (A 0 (A 0 (A 0 (A 0 2048))))) |#
#| (A 0 (A 0 (A 0 (A 0 (* 2 2048))))) |#
#| (A 0 (A 0 (A 0 (A 0 4096)))) |#
#| (A 0 (A 0 (A 0 (* 2 4096)))) |#
#| (A 0 (A 0 (A 0 8182))) |#
#| (A 0 (A 0 (* 2 8182))) |#
#| (A 0 (A 0 16364)) |#
#| (A 0 (* 2 16364)) |#
#| (A 0 32768) |#
#| (* 2 32768) |#
#| 65536 |#

;; f(n) = 2n
(define (f n) (A 0 n))

;; g(n) = 2^n
(define (g n) (A 1 n))

;; h(n) = 2^(2^... n times ...(2^2)...)
(define (h n) (A 2 n))

;; k(n) = 5n^2
(define (k n) (* 5 n n))

(#%provide count-change)
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond
    ((= amount 0) 1)
    ((or (< amount 0) (= kinds-of-coins 0)) 0)
    (else
      (+
        (cc amount (- kinds-of-coins 1))
        (cc
          (- amount (first-denomination kinds-of-coins))
          kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond
    ((= kinds-of-coins 1) 1)
    ((= kinds-of-coins 2) 5)
    ((= kinds-of-coins 3) 10)
    ((= kinds-of-coins 4) 25)
    ((= kinds-of-coins 5) 50)))

#| 1.11 |#

(#%provide f-rec)
(define (f-rec n)
  (cond
    ((< n 3) n)
    (else
      (+
        (f-rec (- n 1))
        (* 2 (f-rec (- n 2)))
        (* 3 (f-rec (- n 3)))))))

(#%provide f-iter)
(define (f-iter n)
  (f-iter- 2 1 0 n))

(define (f-iter- a b c i)
  (if (= i 0)
    c
    (f-iter-
      (+ a (* 2 b) (* 3 c))
      a
      b
      (- i 1))))

#| 1.12 |#

(#%provide pascal)
(define (pascal row elem)
  (cond
    ((> elem row) 0)
    ((= elem 0) 1)
    (else
      (+
        (pascal (- row 1) (- elem 1))
        (pascal (- row 1) elem)))))

#| 1.13 |#

;; phi = (1 + sqrt(5)) / 2
;; psi = (1 - sqrt(5)) / 2

;; note that:
;; phi^2 = phi + 1, and
;; psi^2 = psi + 1

;; Proof by induction that Fib(n) = (phi^n - psi^n) / sqrt(5)

;; Base cases:
;; Fib(0) = 0 = (phi^0 - psi^0) / sqrt(5)
;; Fib(1) = 1 = = (((1 + sqrt(5)) - (1 - sqrt(5))) / 2) / sqrt(5)
;;        = (phi^1 - psi^1) / sqrt(5)

;; Inductive case:

;; Suppose
;; Fib(n-2) = (phi^(n-2) - psi^(n-2)) / sqrt(5), and
;; Fib(n-1) = (phi^(n-1) - psi^(n-1)) / sqrt(5), and

;; Then
;; Fib(n) = Fib(n-1) + Fib(n - 2)
;;        = (phi^(n-1) - psi^(n-1) + phi^(n-2) - psi^(n-2)) / sqrt(5)
;;        = (phi^(n-1) + phi^(n-2) - psi(^n-1) - psi^(n-2)) / sqrt(5)
;;        = ((phi + 1)phi^(n-2) - (psi + 1)psi^(n-2)) / sqrt(5)
;;        = ((phi^2)phi^(n-2) - (psi^2)psi^(n-2)) / sqrt(5)
;;        = (phi^n - psi^n) / sqrt(5)

;; For all n, psi^n / sqrt(5) < 1/2. So Fib(n) is the closest integer to phi^n / sqrt(5)


#| 1.14 |#

#| The process generated by (count-change 11): |#

#| - cc 11 5 |#
#| | - cc -39 5 |#
#| | - cc 11 4 |#
#|   | - cc -14 4 |#
#|   | - cc 11 3 |#
#|     | - cc 11 2 |#
#|     | | - cc 11 1 |#
#|     | | | - cc 11 0 |#
#|     | | | - cc 10 1 |#
#|     | |   | - cc 10 0 |#
#|     | |   | - cc 9 1 |#
#|     | |     | - cc 9 0 |#
#|     | |     | - cc 8 1 |#
#|     | |       | - cc 8 0 |#
#|     | |       | - cc 7 1 |#
#|     | |         | - cc 7 0 |#
#|     | |         | - cc 6 1 |#
#|     | |           | - cc 6 0 |#
#|     | |           | - cc 5 1 |#
#|     | |             | - cc 5 0 |#
#|     | |             | - cc 4 1 |#
#|     | |               | - cc 4 0 |#
#|     | |               | - cc 3 1 |#
#|     | |                 | - cc 3 0 |#
#|     | |                 | - cc 2 1 |#
#|     | |                   | - cc 2 0 |#
#|     | |                   | - cc 1 1 |#
#|     | |                     | - cc 1 0 |#
#|     | |                     | - cc 0 1 |#
#|     | | - cc 6 2 |#
#|     |   | - cc 6 1 |#
#|     |   | | - cc 6 0 |#
#|     |   | | - cc 5 1 |#
#|     |   |   | - cc 5 0 |#
#|     |   |   | - cc 4 1 |#
#|     |   |     | - cc 4 0 |#
#|     |   |     | - cc 3 1 |#
#|     |   |       | - cc 3 0 |#
#|     |   |       | - cc 2 1 |#
#|     |   |         | - cc 2 0 |#
#|     |   |           - cc 1 1 |#
#|     |   |           | - cc 1 0 |#
#|     |   |           | - cc 0 1 |#
#|     |   | - cc 1 2 |#
#|     |     | - cc 1 1 |#
#|     |     | | - cc 1 0 |#
#|     |     | | - cc 0 1 |#
#|     |     | - cc -4 2 |#
#|     | - cc 1 3 |#
#|       | - cc 1 2 |#
#|       | | - cc 1 1 |#
#|       | | | - cc 1 0 |#
#|       | | | - cc 0 1 |#
#|       | | - cc -4 2 |#
#|       | - cc -9 3 |#

#| count-change is Theta(n) in space (max depth of tree) |#
#| count-change is Theta(e^n) in time (number of nodes in tree) |#

#| 1.15 |#

(#%provide sine)
(define (sine ang)
  (define (p x) (- (* 3 x) (* 4 (cube x))))
  (if (not (> (abs ang) 0.1))
    ang
    (p (sine (/ ang 3.0)))))

#| sine 12.15 |#
#| p (sine 4.05) |#
#| p (p (sine 1.35)) |#
#| p (p (p (sine 0.45))) |#
#| p (p (p (p (sine 0.15)))) |#
#| p (p (p (p (p (sine 0.05))))) |#
#| p (p (p (p (p 0.05)))) |#

#| p is called 5 times |#

#| The process generated by sine is recursive |#
#| Order of growth in space: Theta(log_3(n)) |#
#| Order of growth in time: Theta(log_3(n)) |#

(#%provide expt-rec)
(define (expt-rec b n)
  (if (= n 0)
    1
    (* b (expt-rec b (- n 1)))))

(#%provide expt-iter)
(define (expt-iter b n)
  (expt-iter- b n 1))

(define (expt-iter- b i prod)
  (if (= i 0)
    prod
    (expt-iter-
      b
      (- i 1)
      (* b prod))))

(define (even-? k) (= (remainder k 2) 0))

(#%provide fast-expt-rec)
(define (fast-expt-rec b n)
  (cond
    ((= n 0) 1)
    ((even-? n) (square (fast-expt-rec b (/ n 2))))
    (else (* b (fast-expt-rec b (- n 1))))))

#| 1.16 |#

(#%provide fast-expt-iter)
(define (fast-expt-iter b n)
  (fast-expt-iter- 1 b n))

(define (fast-expt-iter- a b n)
  (cond
    ((= n 0) a)
    ((even-? n) (fast-expt-iter- a (square b) (/ n 2)))
    (else (fast-expt-iter- (* a b) b (- n 1)))))

(#%provide mult)
(define (mult a b)
  (if (= b 0)
    0
    (+ a (mult a (- b 1)))))

#| 1.17 |#

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(#%provide fast-times-rec)
(define (fast-times-rec a b)
  (cond
    ((= b 0) 0)
    ((even-? b) (double (fast-times-rec a (halve b))))
    (else (+ a (fast-times-rec a (- b 1))))))

#| 1.18 |#

(#%provide fast-times-iter)
(define (fast-times-iter a b)
  (fast-times-iter- 0 a b))

(define (fast-times-iter- res a b)
  (cond
    ((= b 0) res)
    ((even-? b) (fast-times-iter- res (double a) (halve b)))
    (else (fast-times-iter- (+ res a) a (- b 1)))))

#| 1.19 |#

;; T
;; a <- a + b
;; b <- a

;; Tpq
;; a <- bq + aq + ap
;; b <- bp + aq

;; Tpq^2
;; a <- (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;; b <- (bp + aq)p + (bq + aq + ap)q

;; a <- b(2pq + qq) + a(2pq + qq) + a(pp + qq)
;; b <- b(pp + qq) + a(2qp + qq)

;; p' = pp + qq
;; q' = 2pq + qq

(#%provide fib)
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q i)
  (cond
    ((= i 0) b)
    ((even-? i)
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

(#%provide fib-slow)
(define (fib-slow n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fib (- n 1)) (fib (- n 2))))))

#| 1.20 |#

(#%provide gcd-)
(define (gcd- a b)
  (if (= b 0)
    a
    (gcd- b (remainder a b))))

;; Process generated by normal order evaluation

#| (gcd- 206 40) |#
#| (if (= 40 0) 206 (gcd- 40 (rem 206 40))) |#
#| (gcd- 40 (rem 206 40)) |#
#| (if (= (rem 206 40) 0) 40 (gcd- b (rem 40 (rem 206 40)))) ; one call |#
#| (if (= 6 0) 40 (gcd- (rem 206 40) (rem 40 (rem 206 40)))) |#
#| (gcd- (rem 206 40) (rem 40 (rem 206 40))) |#
#| (if (= (rem 40 (rem 206 40)) 0) ... (gcd- (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40))))) ; one call |#
#| (if (= (rem 40 6) 0) ... (gcd- (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40))))) ; one call |#
#| (if (= 4 0) ... (gcd- (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40))))) |#
#| (gcd- (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40)))) |#
#| (if (= (rem (rem 206 40) (rem 40 (rem 206 40))) 0) ... (gcd- (rem (rem 206 40) (rem 40 (rem 206 40))) ...)) ; one call |#
#| (if (= (rem 6 (rem 40 (rem 206 40))) 0) ... (gcd- (rem (rem 206 40) (rem 40 (rem 206 40))) ...)) ; one call |#
#| (if (= (rem 6 (rem 40 6)) 0) ... (gcd- (rem (rem 206 40) (rem 40 (rem 206 40))) ...)) ; one call |#
#| (if (= (rem 6 4) 0) ... (gcd- (rem (rem 206 40) (rem 40 (rem 206 40))) ...)) ; one call |#
#| (if (= 2 0) ... (gcd- (rem (rem 206 40) (rem 40 (rem 206 40))) ...)) |#
#| (gcd- (rem (rem 206 40) (rem 40 (rem 206 40))) (rem (rem 40 (rem 206 40)) ...)) |#
#| (if (= (rem (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40)))) 0) (rem (rem 206 40) ...) ...) ; one call |#
#| (if (= (rem (rem 40 6) (rem (rem 206 40) (rem 40 (rem 206 40)))) 0) (rem (rem 206 40) ...) ...) ; one call |#
#| (if (= (rem 4 (rem (rem 206 40) (rem 40 (rem 206 40)))) 0) (rem (rem 206 40) (rem 40 (rem 206 40))) ...) ; one call |#
#| (if (= (rem 4 (rem 6 (rem 40 (rem 206 40)))) 0) (rem (rem 206 40) (rem 40 (rem 206 40))) ...) ; one call |#
#| (if (= (rem 4 (rem 6 (rem 40 6))) 0) (rem (rem 206 40) (rem 40 (rem 206 40))) ...) ; one call |#
#| (if (= (rem 4 (rem 6 4)) 0) (rem (rem 206 40) (rem 40 (rem 206 40))) ...) ; one call |#
#| (if (= (rem 4 2) 0) (rem (rem 206 40) (rem 40 (rem 206 40))) ...) ; one call |#
#| (if (= 0 0) (rem (rem 206 40) (rem 40 (rem 206 40))) ...) |#
#| (rem (rem 206 40) (rem 40 (rem 206 40))) ; one call |#
#| (rem 6 (rem 40 (rem 206 40))) ; one call |#
#| (rem 6 (rem 40 6)) ; one call |#
#| (rem 6 4) ; one call |#
#| 2 |#

;; 18 calls to remainder are performed

;; Process generated by applicative order evaluation

#| (gcd- 206 40) |#
#| (if (= 40 0) 206 (gcd- 40 (remainder 206 40))) ; one call |#
#| (gcd- 40 6) |#
#| (gcd- 40 6) |#
#| (if (= 6 0) 40 (gcd- 6 (remainder 40 6))) |#
#| (gcd- 6 (remainder 40 6)) ; one call |#
#| (gcd- 6 4) |#
#| (if (= 4 0) 6 (gcd- 4 (remainder 6 4))) |#
#| (gcd- 4 (remainder 6 4)) ; one call |#
#| (gcd- 4 2) |#
#| (if (= 2 0) 4 (gcd- 2 (remainder 4 2))) |#
#| (gcd- 2 (remainder 4 2)) ; one call |#
#| (gcd- 2 0) |#
#| (if (= 0 0) 2 (gcd- 0 (remainder 2 0))) |#
#| 2 |#

;; 4 calls to remainder are performed

(#%provide smallest-divisor)
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(#%provide prime?)
(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base expo m)
  (cond
    ((= expo 0) 1)
    ((even-? expo)
     (remainder (square (expmod base (/ expo 2) m)) m))
    (else
      (remainder
        (* base (expmod base (- expo 1) m))
        m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond
    ((= times 0) true)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else false)))

#| 1.21 |#

#| (smallest-divisor 199) |#
#| (smallest-divisor 1999) |#
#| (smallest-divisor 19999) |#

(#%provide timed-prime-test)
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
(display " *** ")
  (display elapsed-time))

#| 1.22 |#

(#%provide search-for-primes)
(define (search-for-primes a b)
  (cond
    ((> a b) (newline))
    (else
      (timed-prime-test a)
      (search-for-primes (+ a 1) b))))

;; Smallest primes over 1000:
;; 1009 3us
;; 1013 3
;; 1019 3

;; Smallest primes over 10000:
;; 10007 9us
;; 10019 8
;; 10037 9

;; Smallest primes over 100000:
;; 100003 22us
;; 100019 21
;; 100043 20

;; Smallest primes over 1000000:
;; 1000003 65us
;; 1000033 65
;; 1000037 65

;; 9 / 3 = 3
;; 21 / 9 = 2.33
;; 65 / 21 = 3.095

;; sqrt(10) = 3.16

#| 1.23 |#

(define (next n)
  (if (= n 2) 3 (+ n 2)))

;; Smallest primes over 1000:
;; 1009 2us
;; 1013 2
;; 1019 2

;; Smallest primes over 10000:
;; 10007 5us
;; 10019 4
;; 10037 4

;; Smallest primes over 100000:
;; 100003 11us
;; 100019 12
;; 100043 11

;; Smallest primes over 1000000:
;; 1000003 34us
;; 1000033 34
;; 1000037 34

;; 3 / 2 = 1.4
;; 9 / 4 = 2.25
;; 21 / 11 = 1.9
;; 65 / 34 = 1.9

#| 1.24 |#

;; Using fast-prime?

;; Smallest primes over 1000:
;; 1009 .28ms
;; 1013 .29
;; 1019 .31

;; Smallest primes over 10000:
;; 10007 .37ms
;; 10019 .36
;; 10037 .40

;; Smallest primes over 100000:
;; 100003 .42ms
;; 100019 .44
;; 100043 .46

;; Smallest primes over 1000000:
;; 1000003 .50ms
;; 1000033 .49
;; 1000037 .51

;; Scaling up by a factor of ten adds a constant amount of time

#| 1.25 |#

;; This version of expmod requires too much space to
;; represent the intermediate result when using very
;; large exponents

(define (expmod-bad base expo m)
  (remainder (fast-expt-iter base expo) m))

#| 1.26 |#

;; This version of expmod makes two recursive calls of
;; size n / 2, making it Theta(n) in time rather than
;; Theta(log(n))

(define (expmod-slow base expo m)
  (cond
    ((= expo 0) 1)
    ((even-? expo)
     (remainder
       (*
         (remainder (expmod-slow base (/ expo 2) m)
         (remainder (expmod-slow base (/ expo 2) m))))
       m))
    (else
      (remainder
        (*
          base
          (expmod-slow base (- expo 1) m))
        m))))

#| 1.27 |#

(#%provide fermat-test-exhaustive)
(define (fermat-test-exhaustive n)
  (define (iter i res)
    (define (try-it a) (= (expmod a n n) a))
    (if (= i n)
      res
      (iter (+ i 1) (and res (try-it i)))))
  (iter 1 true))

#| (fermat-test-exhaustive 561) |#
#| (fermat-test-exhaustive 1105) |#
#| (fermat-test-exhaustive 1729) |#
#| (fermat-test-exhaustive 2465) |#
#| (fermat-test-exhaustive 2821) |#
#| (fermat-test-exhaustive 6601) |#

#| 1.28 |#

(define (expmod-sig base expo m)
  (define (square-sig x)
    (define squared (remainder (square x) m))
    (if
      (and
        (= squared 1)
        (not (= x 1))
        (not (= x expo)))
      0
      squared))
  (cond
    ((= expo 0) 1)
    ((even-? expo)
     (square-sig (expmod base (/ expo 2) m)))
    (else
      (remainder
        (* base (expmod base (- expo 1) m))
        m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(#%provide mr-fast-prime?)
(define (mr-fast-prime? n times)
  (cond
    ((= times 0) true)
    ((miller-rabin-test n) (mr-fast-prime? n (- times 1)))
    (else false)))

(#%provide sum-integers-)
(define (sum-integers- a b)
  (if (> a b)
    0
    (+ a (sum-integers- (+ a 1) b))))

(#%provide sum-cubes-)
(define (sum-cubes- a b)
  (if (> a b)
    0
    (+ (cube a) (sum-cubes- (+ a 1) b))))

(#%provide pi-sum-)
(define (pi-sum- a b)
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum- (+ a 4) b))))

(#%provide sum)
(define (sum term a next b)
  (if (> a b)
    0
    (+
      (term a)
      (sum term (next a) next b))))

(define (inc n) (+ n 1))

(#%provide sum-cubes)
(define (sum-cubes a b) (sum cube a inc b))

(define (id x) x)

(#%provide sum-integers)
(define (sum-integers a b) (sum id a inc b))

(#%provide pi-sum)
(define (pi-sum a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (sum pi-term a pi-next b))

(#%provide integral)
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

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
