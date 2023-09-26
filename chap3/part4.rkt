#lang sicp
(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(#%require (only racket/base thread thread-wait))
(#%require (only ffi/unsafe/atomic start-atomic end-atomic))
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; Chapter 3
;; Modularity, Objects, and State

;; 3.4
;; Concurrency: Time Is of the Essence

(#%provide parallel-execute)
(define (parallel-execute . procs)
  (map
    thread-wait
    (map
      (lambda (proc) (thread proc))
      procs)))

(#%provide clear!)
(define (clear! cell)
  (set-car! cell false))

(#%provide test-and-set!)
(define (test-and-set! cell)
  (start-atomic)
  (define result
    (if (car cell)
      true
      (begin
        (set-car! cell true)
        false)))
  (end-atomic)
  result)

(#%provide make-mutex)
(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond
        ((eq? m 'acquire)
         (if (test-and-set! cell)
           (the-mutex 'acquire)))
        ((eq? m 'release) (clear! cell))))
    the-mutex))

(#%provide make-serializer)
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

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
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond
        ((eq? m 'withdraw) (protected withdraw))
        ((eq? m 'deposit) (protected deposit))
        ((eq? m 'balance) balance)
        (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

#| 3.39 |#

#| (define x 10) |#

#| (define s (make-serializer)) |#

#| (parallel-execute |#
#|   (lambda () (set! x ((s (lambda () (* x x )))))) |#
#|   (lambda () (s (set! x (+ x 1))))) |#

#| A (set! x ...) |#
#| B (* x x) |#
#| C (set! x (+ x 1)) |#

#| C B A 10 11 121 |#
#| B C A 10 11 100 |#
#| B A C 10 |#

#| 3.40 |#

#| (define x 10) |#

#| (parallel-execute |#
#|   (lambda () (set! x (* x x))) |#
#|   (lambda () (set! x (* x x x)))) |#

#| 100 |#
#| 1000 |#
#| 10000 |#
#| 100000 |#
#| 1000000 |#

#| (define x 10) |#

#| (define s (make-serializer)) |#

#| (parallel-execute |#
#|   (s (lambda () (set! x (* x x)))) |#
#|   (s (lambda () (set! x (* x x x))))) |#

#| 1000000 |#

#| 3.41 |#

#| (define (make-account balance) |#
#|   (define (withdraw amount) |#
#|     (if (>= balance amount) |#
#|       (begin |#
#|         (set! balance (- balance amount)) |#
#|         balance) |#
#|       "Insufficient funds")) |#
#|   (define (deposit amount) |#
#|     (set! balance (+ balance amount)) |#
#|     balance) |#
#|   (let ((protected (make-serializer))) |#
#|     (define (dispatch m) |#
#|       (cond |#
#|         ((eq? m 'withdraw) (protected withdraw)) |#
#|         ((eq? m 'deposit) (protected deposit)) |#
#|         ((eq? m 'balance) |#
#|          ((protected (lambda () balance)))) |#
#|         (else (error "Unknown request -- MAKE-ACCOUNT" m)))) |#
#|     dispatch)) |#

;; balance is read-only so it doesn't need to be serialized

(#%provide exchange)
(define (exchange account1 account2)
  (let
    ((difference
     (-
       (account1 'balance)
       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'desposit) difference)))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond
        ((eq? m 'withdraw) withdraw)
        ((eq? m 'deposit) deposit)
        ((eq? m 'balance) balance)
        ((eq? m 'serializer) balance-serializer)
        (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

(define (deposit account amount)
  (let
    ((s (account 'serializer))
     (d (account 'deposit)))
    ((s d) amount)))

(#%provide serialized-exchange)
(define (serialized-exchange account1 account2)
  (let
    ((serializer1 (account1 'serializer))
     (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

#| 3.44 |#

(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

;; unlike the exchange problem,
;; there is no temporary value stored here

#| 3.45 |#

(define (make-account-and-serializer-bad balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond
        ((eq? m 'withdraw) (balance-serializer withdraw))
        ((eq? m 'deposit) (balance-serializer deposit))
        ((eq? m 'balance) balance)
        ((eq? m 'serializer) balance-serializer)
        (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

;; with this version, the calls to deposit and withdraw
;; inside of exchange cannot not proceed, because the
;; internal serializers for account1 and account2 are
;; already locked out by the calls to serializer1 and
;; serializer2 in serialized-exchange
;;
;; in a word, deadlock

#| 3.47 |#

(#%provide make-semaphore)
(define (make-semaphore n)
  (let
    ((mutex (make-mutex))
     (count 0))
    (define (make-permit)
      (let ((valid true))
        (lambda ()
          (if valid (set! count (- count 1)))
          (set! valid false))))
    (define (acquire)
      (if (= count n)
        (acquire)
        (begin
          (set! count (+ count 1))
          (make-permit))))
    (define (dispatch m)
      (cond
        ((eq? m 'acquire)
         (mutex 'acquire)
         (let ((permit (acquire)))
           (mutex 'release)
           permit))
        ((eq? m 'count) count)
        (else (error "Unknown request -- MAKE-SEMAPHORE" m))))
    dispatch))

(#%provide make-account-num)
(define (make-account-num id balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond
        ((eq? m 'withdraw) (protected withdraw))
        ((eq? m 'deposit) (protected deposit))
        ((eq? m 'balance) balance)
        ((eq? m 'id) id)
        (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))


#| 3.48 |#

(#%provide serialized-exchange-num)
(define (serialized-exchange-num account1 account2)
  (let
    ((serializer1 (account1 'serializer))
     (id1 (account1 'id))
     (serializer2 (account2 'serializer))
     (id2 (account2 'id)))
    (let
      ((smaller (if (< id1 id2) serializer1 serializer2))
       (larger (if (> id1 id2) serializer1 serializer2)))
      (if (= id1 id2) (error "Same account id -- SERIALIZED-EXCHANGE-NUM"))
      ((larger (smaller exchange))
         account1
         account2))))
