#lang sicp

;; Chapter 1
;; Building Abstractions with Procedures

;; 1.2
;; Procedures and the Processes They Generate

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
