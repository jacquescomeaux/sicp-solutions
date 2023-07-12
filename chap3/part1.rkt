#lang sicp
(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; Chapter 3
;; Modularity, Objects, and State

;; 3.1
;; Assignment and Local State

(define balance 100)

(#%provide withdraw)
(define (withdraw amount)
  (if (>= balance amount)
    (begin
      (set! balance (- balance amount))
      balance)
    "Insufficient funds"))

(#%provide new-withdraw)
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))))

(#%provide make-withdraw)
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds")))

(#%provide make-account)
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

#| 3.1 |#

(#%provide make-accumulator)
(define (make-accumulator sum)
  (lambda (x)
    (begin
      (set! sum (+ sum x))
      sum)))

#| 3.2 |#

(#%provide make-monitored)
(define (make-monitored f)
  (let ((call-count 0))
    (lambda (arg)
      (cond
        ((and (symbol? arg) (eq? arg 'how-many-calls?))
         call-count)
        ((and (symbol? arg) (eq? arg 'reset-count))
         (set! call-count 0))
        (else
          (set! call-count (+ call-count 1))
          (f arg))))))

#| 3.3 |#

(#%provide make-account-pass)
(define (make-account-pass balance secret-pass)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (if (eq? pass secret-pass)
      (cond
        ((eq? m 'withdraw) withdraw)
        ((eq? m 'deposit) deposit)
        (else (error "Unknown request -- MAKE-ACCOUNT" m)))
      (error "Incorrect password")))
    dispatch)

#| 3.4 |#

(define (call-the-cops)
  (display "WEEOOOWEEOOOWEEOOOOWEEOO")
  (newline)
  (display "STEP OUT OF THE VEHICLE")
  (newline))

(#%provide make-account-pass-cops)
(define (make-account-pass-cops balance secret-pass)
  (define fail-amount 0)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (if (eq? pass secret-pass)
      (begin
        (set! fail-amount 0)
        (cond
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
      (begin
        (set! fail-amount (+ fail-amount 1))
        (if (> fail-amount 7) (call-the-cops))
        (error "Incorrect password"))))
  dispatch)

#| (define random-init 4) |#

#| (define rand |#
#|   (let ((x random-init)) |#
#|     (lambda () |#
#|       (set! x (rand-update x)) |#
#|       x))) |#

(#%provide estimate-pi)
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (random 10000) (random 10000)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      ((= trials-remaining 0)
       (/ trials-passed trials))
      ((experiment)
       (iter (- trials-remaining 1) (+ trials-passed 1)))
      (else
       (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

#| 3.5 |#

(define (square x) (* x x))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(#%provide estimate-pi-integral)
(define (estimate-pi-integral trials)
  (*
    4
    (estimate-integral
      (lambda (x y) (<= (+ (square x) (square y)) 1.0))
      0 1.0
      0 1.0
      trials)))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (*
    (- x2 x1)
    (- y2 y1)
    (monte-carlo
      trials
      (lambda ()
        (P
          (random-in-range x1 x2)
          (random-in-range y1 y2))))))

#| 3.6 |#

(define random-init 4)

(define (rand-update x)
  (modulo (+ (* 75 x) 74) 65537))

(#%provide rand-new)
(define rand-new
  (let ((x random-init))
    (lambda (sym)
      (cond
        ((eq? sym 'generate)
         (set! x (rand-update x))
         x)
        ((eq? sym 'reset)
         (lambda (new-value)
           (set! x new-value)))
        (else error "unknown symbol")))))

(#%provide factorial)
(define (factorial n)
  (let
    ((product 1)
     (counter 1))
    (define (iter)
      (if (> counter n)
        product
        (begin
          (set! product (* counter product))
          (set! counter (+ counter 1))
          (iter))))
    (iter)))

(#%provide factorial-bad)
(define (factorial-bad n)
  (let
    ((product 1)
     (counter 1))
    (define (iter)
      (if (> counter n)
        product
        (begin
          (set! counter (+ counter 1))
          (set! product (* counter product))
          (iter))))
    (iter)))

#| 3.7 |#

(#%provide make-account-pass-)
(define (make-account-pass- balance secret-pass)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass)
    (if (eq? pass secret-pass)
      (lambda (m)
        (cond
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
      (error "Incorrect password")))
    dispatch)

(#%provide make-joint)
(define (make-joint acc old-pass new-pass)
  (define inner-account (acc old-pass))
  (define (dispatch pass)
    (lambda (m)
      (if (eq? pass new-pass)
        (inner-account m)
        (error "Incorrect password"))))
    dispatch)

#| 3.8 |#

(#%provide f)
(define f
  (let ((init false))
    (lambda (x)
      (if init
        0
        (begin (set! init true) x)))))
