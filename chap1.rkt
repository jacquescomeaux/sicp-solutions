#lang sicp

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

(#%provide times)
(define (times a b)
  (if (= b 0)
    0
    (+ a (times a (- b 1)))))

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
