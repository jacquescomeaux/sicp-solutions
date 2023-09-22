#lang sicp
(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; Chapter 3
;; Modularity, Objects, and State

;; 3.2
;; The Environment Model of Evaluation

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

#| 3.9 |#

(#%provide factorial)
(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

#| global |#
#| ------------------- |#
#| | factorial: -> A | |#
#| ------------------- |#

#| A |#
#| -> |# 
#| parameters: n |#
#| body: |#
#| (if (= n 1) |#
#|   1 |#
#|   (* n (factorial (- n 1)))) |#
#| -> global |#

#| E1          E2 |# 
#| --------    -------- |#
#| | n: 6 |    | n: 5 | |#
#| --------    -------- |#
#| -> global   -> global |#

#| E3          E4 |# 
#| --------    -------- |#
#| | n: 4 |    | n: 3 | |#
#| --------    -------- |#
#| -> global   -> global |#

#| E5          E6 |# 
#| --------    -------- |#
#| | n: 2 |    | n: 1 | |#
#| --------    -------- |#
#| -> global   -> global |#

#| In each of E1 - E6: |#

#| (if (= n 1) |#
#|   1 |#
#|   (* n (factorial (- n 1)))) |#

(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter
      (* counter product)
      (+ counter 1)
      max-count)))

(#%provide factorial-)
(define (factorial- n)
  (fact-iter 1 1 n))

#| global |#
#| -------------------- |#
#| | fact-iter: -> A  | |#
#| | factorial-: -> B | |#
#| -------------------- |#

#| A |#
#| -> |# 
#| parameters: product counter max-count |#
#| body: |#
#| (if (> counter max-count) |#
#|   product |#
#|   (fact-iter |#
#|     (* counter product) |#
#|     (+ counter 1) |#
#|     max-count)) |#
#| -> global |#

#| B |#
#| -> |# 
#| parameters: n |#
#| body: |#
#| (fact-iter 1 1 n) |#
#| -> global |#

#| E1 |#
#| -------- |#
#| | n: 6 | |#
#| -------- |#
#| -> global |#

#| In E1: |#

#| (fact-iter 1 1 n) |#

#| E2                 E3 |#
#| ----------------   ---------------- |#
#| | product: 1   |   | product: 1   | |#
#| | counter: 1   |   | counter: 2   | |#
#| | max-count: 6 |   | max-count: 6 | |#
#| ----------------   ---------------- |#
#| -> global          -> global |#

#| E4                 E5 |#
#| ----------------   ---------------- |#
#| | product: 2   |   | product: 6   | |#
#| | counter: 3   |   | counter: 4   | |#
#| | max-count: 6 |   | max-count: 6 | |#
#| ----------------   ---------------- |#
#| -> global          -> global |#

#| E6                 E7 |#
#| ----------------   ---------------- |#
#| | product: 24  |   | product: 120 | |#
#| | counter: 5   |   | counter: 6   | |#
#| | max-count: 6 |   | max-count: 6 | |#
#| ----------------   ---------------- |#
#| -> global          -> global |#

#| E8 |#
#| ---------------- |#
#| | product: 720 | |#
#| | counter: 7   | |#
#| | max-count: 6 | |#
#| ---------------- |#
#| -> global |#

#| In each of E2 - E8: |#

#| (if (> counter max-count) |#
#|   product |#
#|   (fact-iter |#
#|     (* counter product) |#
#|     (+ counter 1) |#
#|     max-count)) |#

(#%provide make-withdraw)
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds")))

#| 3.10 |#

(#%provide make-withdraw-)
(define (make-withdraw- initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))))

#| global |#
#| ------------------------- |#
#| | make-withdraw-: -> A  | |#
#| | W1: -> B              | |#
#| | W2: -> C              | |#
#| ------------------------- |#

#| (define W1 (make-withdraw- 100)) |#
#| (W1 50) |#
#| (define W2 (make-withdraw- 100)) |#

#| A |#
#| -> |# 
#| parameters: initial-amount |#
#| body: |#
#| (let ((balance initial-amount)) |#
#|   (lambda (amount) |#
#|     (if (>= balance amount) |#
#|       (begin |#
#|         (set! balance (- balance amount)) |#
#|         balance) |#
#|       "Insufficient funds"))) |#
#| -> global |#

#| E1 |#
#| ----------------------- |#
#| | initial-amount: 100 | |#
#| ----------------------- |#
#| -> global |#

#| ((lambda (balance) |#
#|   (lambda (amount) |#
#|     (if (>= balance amount) |#
#|       (begin |#
#|         (set! balance (- balance amount)) |#
#|         balance) |#
#|       "Insufficient funds"))) initial-amount) |#

#| E2 |#
#| ---------------- |#
#| | balance: 50  | |#
#| ---------------- |#
#| -> E1 |#

#| (lambda (amount) |#
#|   (if (>= balance amount) |#
#|     (begin |#
#|       (set! balance (- balance amount)) |#
#|       balance) |#
#|     "Insufficient funds")) |#

#| B |#
#| -> |# 
#| parameters: amount |#
#| body: |#
#| (if (>= balance amount) |#
#|   (begin |#
#|     (set! balance (- balance amount)) |#
#|     balance) |#
#|   "Insufficient funds") |#
#| -> E2 |#

#| E3 |#
#| ---------------- |#
#| | amount: 50   | |#
#| ---------------- |#
#| -> E2 |#

#| (if (>= balance amount) |#
#|   (begin |#
#|     (set! balance (- balance amount)) |#
#|     balance) |#
#|   "Insufficient funds") |#

#| (begin |#
#|   (set! balance 50) |#
#|   balance) |#

#| E4 |#
#| ----------------------- |#
#| | initial-amount: 100 | |#
#| ----------------------- |#
#| -> global |#

#| ((lambda (balance) |#
#|   (lambda (amount) |#
#|     (if (>= balance amount) |#
#|       (begin |#
#|         (set! balance (- balance amount)) |#
#|         balance) |#
#|       "Insufficient funds"))) initial-amount) |#

#| E5 |#
#| ---------------- |#
#| | balance: 100  | |#
#| ---------------- |#
#| -> E4 |#

#| (lambda (amount) |#
#|   (if (>= balance amount) |#
#|     (begin |#
#|       (set! balance (- balance amount)) |#
#|       balance) |#
#|     "Insufficient funds")) |#

#| C |#
#| -> |# 
#| parameters: amount |#
#| body: |#
#| (if (>= balance amount) |#
#|   (begin |#
#|     (set! balance (- balance amount)) |#
#|     balance) |#
#|   "Insufficient funds") |#
#| -> E5 |#

#| 3.11 |#

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
      (else
        (error
          "Unknown request -- MAKE-ACCOUNT"
          m))))
  dispatch)

#| global |#
#| ---------------------- |#
#| | make-account: -> A | |#
#| | acc:          -> D | |#
#| | acc2:         -> G | |#
#| ---------------------- |#

#| (define acc (make-account 50)) |#
#| ((acc 'deposit) 40) |#
#| ((acc 'withdraw) 60) |#
#| (define acc2 (make-account 100)) |#

#| A |#
#| -> |#
#| parameters: balance |#
#| body: |#
#| (define (withdraw amount) |#
#|   (if (>= balance amount) |#
#|     (begin |#
#|       (set! balance (- balance amount)) |#
#|       balance) |#
#|     "Insufficient funds")) |#
#| (define (deposit amount) |#
#|   (set! balance (+ balance amount)) |#
#|   balance) |#
#| (define (dispatch m) |#
#|   (cond |#
#|     ((eq? m 'withdraw) withdraw) |#
#|     ((eq? m 'deposit) deposit) |#
#|     (else |#
#|       (error |#
#|         "Unknown request -- MAKE-ACCOUNT" |#
#|         m)))) |#
#| dispatch |#
#| -> global |#

#| E1 |#
#| ------------------ |#
#| | balance: 30    | |#
#| | withdraw: -> B | |#
#| | deposit: -> C  | |#
#| | dispatch: -> D | |#
#| ------------------ |#
#| -> global |#

#| B |#
#| -> |#
#| parameters: amount |#
#| body: |#
#| (if (>= balance amount) |#
#|   (begin |#
#|     (set! balance (- balance amount)) |#
#|     balance) |#
#|   "Insufficient funds") |#
#| -> E1 |#

#| C |#
#| -> |#
#| parameters: amount |#
#| body: |#
#| (set! balance (+ balance amount)) |#
#| balance |#
#| -> E1 |#

#| D |#
#| -> |#
#| parameters: m |#
#| body: |#
#| (cond |#
#|   ((eq? m 'withdraw) withdraw) |#
#|   ((eq? m 'deposit) deposit) |#
#|   (else |#
#|     (error |#
#|       "Unknown request -- MAKE-ACCOUNT" |#
#|       m))) |#
#| -> E1 |#

#| E2 |#
#| --------------- |#
#| | m: 'deposit | |#
#| --------------- |#
#| -> E1 |#

#| (cond |#
#|   ((eq? m 'withdraw) withdraw) |#
#|   ((eq? m 'deposit) deposit) |#
#|   (else |#
#|     (error |#
#|       "Unknown request -- MAKE-ACCOUNT" |#
#|       m))) |#

#| E3 |#
#| --------------- |#
#| | amount: 40  | |#
#| --------------- |#
#| -> E1 |#

#| (set! balance (+ balance amount)) |#
#| balance |#

#| (set! balance (+ 50 40)) |#
#| balance |#

#| (set! balance 90) |#
#| balance |#

#| E4 |#
#| ---------------- |#
#| | m: 'withdraw | |#
#| ---------------- |#
#| -> E1 |#

#| (cond |#
#|   ((eq? m 'withdraw) withdraw) |#
#|   ((eq? m 'deposit) deposit) |#
#|   (else |#
#|     (error |#
#|       "Unknown request -- MAKE-ACCOUNT" |#
#|       m))) |#

#| E5 |#
#| --------------- |#
#| | amount: 60  | |#
#| --------------- |#
#| -> E1 |#

#| (if (>= balance amount) |#
#|   (begin |#
#|     (set! balance (- balance amount)) |#
#|     balance) |#
#|   "Insufficient funds") |#

#| (if (>= 90 60) |#
#|   (begin |#
#|     (set! balance (- 90 60)) |#
#|     balance) |#
#|   "Insufficient funds") |#

#| (begin |#
#|   (set! balance 30) |#
#|   balance) |#

#| E6 |#
#| ------------------ |#
#| | balance: 100   | |#
#| | withdraw: -> E | |#
#| | deposit: -> F  | |#
#| | dispatch: -> G | |#
#| ------------------ |#
#| -> global |#

#| E |#
#| -> |#
#| parameters: amount |#
#| body: |#
#| (if (>= balance amount) |#
#|   (begin |#
#|     (set! balance (- balance amount)) |#
#|     balance) |#
#|   "Insufficient funds") |#
#| -> E6 |#

#| F |#
#| -> |#
#| parameters: amount |#
#| body: |#
#| (set! balance (+ balance amount)) |#
#| balance |#
#| -> E6 |#

#| G |#
#| -> |#
#| parameters: m |#
#| body: |#
#| (cond |#
#|   ((eq? m 'withdraw) withdraw) |#
#|   ((eq? m 'deposit) deposit) |#
#|   (else |#
#|     (error |#
#|       "Unknown request -- MAKE-ACCOUNT" |#
#|       m))) |#
#| -> E6 |#
