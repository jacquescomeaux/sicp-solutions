#lang sicp
(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; Chapter 3
;; Modularity, Objects, and State

;; 3.3
;; Modeling with Mutable Data

#| (define (cons x y) |#
#|   (let ((new (get-new-pair))) |#
#|     (set-car! new x) |#
#|     (set-cdr! new y)) |#
#|   new) |#

#| 3.12 |#

(#%provide append)
(define (append x y)
  (if (null? x)
    y
    (cons (car x) (append (cdr x) y))))

(#%provide append!)
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

#| (define x (list 'a 'b)) |#
#| (define y (list 'c 'd)) |#
#| (define z (append x y)) |#
#| (cdr x) |#
#| (b) |#
#| (define w (append! x y)) |#
#| (cdr x) |#
#| (b c d) |#

#| 3.13 |#

(#%provide make-cycle)
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(#%provide z)
(define z (make-cycle (list 'a 'b 'c)))

#| 3.14 |#

(#%provide reverse-in-place)
(define (reverse-in-place x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (loop temp x))))
  (loop x '()))

#| 3.16 |#

(#%provide count-pairs-bad)
(define (count-pairs-bad x)
  (if (not (pair? x))
    0
    (+
      (count-pairs-bad (car x))
      (count-pairs-bad (cdr x))
      1)))

(#%provide returns-3)
(define returns-3 (list 'a 'b 'c))

(#%provide returns-4)
(define returns-4
  (let ((x (list 'b)))
    (cons 'a (cons x x))))

(#%provide returns-7)
(define returns-7
  (let ((x (list 'b)))
    (let ((y (cons x x)))
      (cons y y))))

(#%provide never-returns)
(define never-returns
  (let ((3-loop (list 'a 'b 'c)))
    (make-cycle 3-loop)))

#| 3.17 |#

(#%provide count-pairs)
(define (count-pairs x)
  (define seen '())
  (define (occurs? x xs)
    (if (null? xs)
      false
      (or
        (eq? x (car xs))
        (occurs? x (cdr xs)))))
  (define (seen? x) (occurs? x seen))
  (define (mark-seen! x) (set! seen (cons x seen)))
  (define (count-pairs- x)
    (if
      (or (not (pair? x)) (seen? x))
      0
      (begin
        (mark-seen! x)
        (let
          ((car-pairs (count-pairs- (car x)))
           (cdr-pairs (count-pairs- (cdr x))))
          (+ 1 car-pairs cdr-pairs)))))
  (count-pairs- x))

#| 3.18 |#

(#%provide contains-cycle?)
(define (contains-cycle? x)
  (define seen '())
  (define (seen? x)
    (define (occurs? x xs)
      (if (null? xs)
        false
        (or
          (eq? x (car xs))
          (occurs? x (cdr xs)))))
    (occurs? x seen))
  (define (mark-seen! x)
    (set! seen (cons x seen)))
  (define (loop x)
    (cond
      ((null? x) false)
      ((seen? x) true)
      (else
        (mark-seen! x)
        (loop (cdr x)))))
  (loop x))

#| 3.19 |#

(#%provide contains-cycle?-)
(define (contains-cycle?- x)
  (define (seen? x n)
    (define (loop y i)
      (cond
        ((null? y) false)
        ((> i n) false)
        ((eq? y x) true)
        (else (loop (cdr y) (+ i 1)))))
    (loop (cdr x) 0))
  (define (loop x len)
    (cond
      ((null? x) false)
      ((seen? x len) true)
      (else (loop (cdr x) (+ len 1)))))
  (loop x 0))

(#%provide cons-)
(define (cons- x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond
      ((eq? m 'car) x)
      ((eq? m 'cdr) y)
      ((eq? m 'set-car!) set-x!)
      ((eq? m 'set-cdr!) set-y!)
      (else (error "Undefined operation -- CONS" m))))
  dispatch)

(#%provide car-)
(define (car- z) (z 'car))

(#%provide cdr-)
(define (cdr- z) (z 'cdr))

(#%provide set-car!-)
(define (set-car!- z new-value) ((z 'set-car!) new-value) z)

(#%provide set-cdr!-)
(define (set-cdr!- z new-value) ((z 'set-cdr!) new-value) z)

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(#%provide make-queue)
(define (make-queue) (cons '() '()))

(#%provide empty-queue?)
(define (empty-queue? queue) (null? (front-ptr queue)))

(#%provide front-queue)
(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

(#%provide insert-queue!)
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond
      ((empty-queue? queue)
       (set-front-ptr! queue new-pair)
       (set-rear-ptr! queue new-pair)
       queue)
      (else
        (set-cdr! (rear-ptr queue) new-pair)
        (set-rear-ptr! queue new-pair)
        queue))))

(#%provide delete-queue!)
(define (delete-queue! queue)
  (cond
    ((empty-queue? queue)
     (error "DELETE! called with an empty queue" queue))
    (else
      (set-front-ptr! queue (cdr (front-ptr queue)))
      queue)))

#| 3.21 |#

(#%provide print-queue)
(define (print-queue queue)
  (front-ptr queue))

#| 3.22 |#

(#%provide make-queue-)
(define (make-queue-)
  (let
    ((front-ptr '())
     (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
        (error "FRONT called with an empty queue")
        (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond
          ((empty-queue?)
           (set! front-ptr new-pair)
           (set! rear-ptr new-pair)
           front-ptr)
          (else
            (set-cdr! rear-ptr new-pair)
            (set! rear-ptr new-pair)
            front-ptr))))
    (define (delete-queue!)
      (cond
        ((empty-queue?)
         (error "DELETE! called with an empty queue"))
        (else
          (set! front-ptr (cdr front-ptr))
          front-ptr)))
    (define (dispatch m)
      (cond
        ((eq? m 'empty-queue?) (empty-queue?))
        ((eq? m 'front-queue) (front-queue))
        ((eq? m 'insert-queue!) insert-queue!)
        ((eq? m 'delete-queue!) delete-queue!)
        (error "Unknown request -- MAKE-QUEUE" m)))
  dispatch))

(#%provide lookup)
(define (lookup key table)
  (let
    ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      false)))

(#%provide assoc)
(define (assoc key records)
  (cond
    ((null? records) false)
    ((equal? key (caar records))
     (car records))
    (else (assoc key (cdr records)))))

(#%provide insert!)
(define (insert! key value table)
  (let
    ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr!
        table
        (cons (cons key value) (cdr table)))))
  'ok)

(#%provide make-table)
(define (make-table)
  (list '*table*))

(#%provide lookup-2)
(define (lookup-2 key-1 key-2 table)
  (let
    ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let
        ((record (assoc key-2 (cdr subtable))))
        (if record
          (cdr record)
          false))
      false)))

(#%provide insert!-2)
(define (insert!-2 key-1 key-2 value table)
  (let
    ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let
        ((record (assoc key-2 (cdr subtable))))
        (if record
          (set-cdr! record value)
          (set-cdr!
            subtable
            (cons (cons key-2 value) (cdr subtable)))))
      (set-cdr!
        table
        (cons
          (list key-1 (cons key-2 value))
          (cdr table)))))
  'ok)

(#%provide make-table-object)
(define (make-table-object)
  (let
    ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let
        ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let
            ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              false))
          false)))
    (define (insert! key-1 key-2 value)
      (let
        ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let
            ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr!
                subtable
                (cons
                  (cons key-2 value)
                  (cdr subtable)))))
          (set-cdr!
            local-table
            (cons
              (list key-1 (cons key-2 value))
              (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table-object))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

#| 3.24 |#

(#%provide make-table-object-)
(define (make-table-object- same-key?)
  (define (assoc key records)
    (cond
      ((null? records) false)
      ((same-key? key (caar records))
       (car records))
      (else (assoc key (cdr records)))))
  (let
    ((local-table (list '*table*)))
    (define (lookup key)
      (let
        ((record (assoc key (cdr local-table))))
        (if record
          (cdr record)
          false)))
    (define (insert! key value)
      (let
        ((record (assoc key (cdr local-table))))
        (if record
          (set-cdr! record value)
          (set-cdr!
            local-table
            (cons
              (cons key value)
              (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        (else (error "Unknown operation -- TABLE" m))))
    dispatch))

#| 3.25 |#

(#%provide lookup-many)
(define (lookup-many keys table)
  (if (null? keys)
    (cdr table)
    (let
      ((subtable (assoc (car keys) (cdr table))))
      (if subtable
        (lookup-many (cdr keys) subtable)
        false))))

(#%provide insert-many!)
(define (insert-many! keys value table)
  (if (null? keys)
    (set-cdr! table value)
    (let
      ((subtable (assoc (car keys) (cdr table))))
      (if subtable
        (insert-many! (cdr keys) value subtable)
        (let
          ((subtable (list (car keys))))
          (insert-many! (cdr keys) value subtable)
          (set-cdr! table (cons subtable (cdr table))))))))

#| 3.27 |#

(#%provide fib)
(define (fib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else
      (+
        (fib (- n 1))
        (fib (- n 2))))))

(#%provide memoize)
(define (memoize f)
  (let
    ((table (make-table)))
    (lambda (x)
      (let
        ((previously-computed-result (lookup x table)))
        (or
          previously-computed-result
          (let
            ((result (f x)))
            (insert! x result table)
            result))))))

(#%provide memo-fib)
(define memo-fib
  (memoize
    (lambda (n)
      (cond
        ((= n 0) 0)
        ((= n 1) 1)
        (else
          (+
            (memo-fib (- n 1))
            (memo-fib (- n 2))))))))

(#%provide slow-memo-fib)
(define slow-memo-fib (memoize fib))

;; slow-memo-fib only provides speed up on the second invocation
;; the recursive calls don't take advantage of the table

#| global |#
#| -------------------- |#
#| | lookup: ...      | |#
#| | insert!: ...     | |#
#| | make-table: -> A | |#
#| | memoize: -> B    | |#
#| | memo-fib: -> D   | |#
#| -------------------- |#

#| A |#
#| -> |#
#| parameters: |#
#| body: |#
#| (list '*table*) |#
#| -> global |#

#| B |#
#| -> |#
#| parameters: f |#
#| body: |#
#| (let |#
#|   ((table (make-table))) |#
#|   (lambda (x) |#
#|     (let |#
#|       ((previously-computed-result (lookup x table))) |#
#|       (or |#
#|         previously-computed-result |#
#|         (let |#
#|           ((result (f x))) |#
#|           (insert! x result table) |#
#|           result))))) |#
#| -> global |#

#| E1 |#
#| ----------- |#
#| | f: -> C | |#
#| ----------- |#
#| -> global |#

#| C |#
#| -> |#
#| parameters: n |#
#| body: |#
#| (cond |#
#|   ((= n 0) 0) |#
#|   ((= n 1) 1) |#
#|   (else |#
#|     (+ |#
#|       (memo-fib (- n 1)) |#
#|       (memo-fib (- n 2))))) |#
#| -> global |#

#| E2 |#
#| --------------- |#
#| | table: -> T | |#
#| --------------- |#
#| -> E1 |#

#| T |#
#| -> |#
#| '(*table* (0 . 0) (1 . 1) (2 . 1) (3 . 2)) |#

#| D |#
#| -> |#
#| parameters: x |#
#| body: |#
#| (let |#
#|   ((previously-computed-result (lookup x table))) |#
#|   (or |#
#|     previously-computed-result |#
#|     (let |#
#|       ((result (f x))) |#
#|       (insert! x result table) |#
#|       result))) |#
#| -> E2 |#

#| E3 |#
#| -------- |#
#| | x: 3 | |#
#| -------- |#
#| -> E2 |#

#| E |#
#| -> |#
#| parameters: previously-computed-result |#
#| body: |#
#| (or |#
#|   previously-computed-result |#
#|   (let |#
#|     ((result (f x))) |#
#|     (insert! x result table) |#
#|     result)) |#
#| -> E3 |#

#| E4 |#
#| ------------------------------------- |#
#| | previously-computed-result: false | |#
#| ------------------------------------- |#
#| -> E3 |#

#| F |#
#| -> |#
#| parameters: result |#
#| body: |#
#| (insert! x result table) |#
#| result |#
#| -> E4 |#

#| E5 |#
#| -------- |#
#| | n: 3 | |#
#| -------- |#
#| -> global |#

#| E6 |#
#| -------- |#
#| | x: 2 | |#
#| -------- |#
#| -> E2 |#

#| G |#
#| -> |#
#| parameters: previously-computed-result |#
#| body: |#
#| (or |#
#|   previously-computed-result |#
#|   (let |#
#|     ((result (f x))) |#
#|     (insert! x result table) |#
#|     result)) |#
#| -> E6 |#

#| E7 |#
#| ------------------------------------- |#
#| | previously-computed-result: false | |#
#| ------------------------------------- |#
#| -> E6 |#

#| H |#
#| -> |#
#| parameters: result |#
#| body: |#
#| (insert! x result table) |#
#| result |#
#| -> E7 |#

#| E8 |#
#| -------- |#
#| | n: 2 | |#
#| -------- |#
#| -> global |#

#| E9 |#
#| -------- |#
#| | x: 1 | |#
#| -------- |#
#| -> E2 |#

#| I |#
#| -> |#
#| parameters: previously-computed-result |#
#| body: |#
#| (or |#
#|   previously-computed-result |#
#|   (let |#
#|     ((result (f x))) |#
#|     (insert! x result table) |#
#|     result)) |#
#| -> E2 |#

#| E10 |#
#| ------------------------------------- |#
#| | previously-computed-result: false | |#
#| ------------------------------------- |#
#| -> E9 |#

#| J |#
#| -> |#
#| parameters: result |#
#| body: |#
#| (insert! x result table) |#
#| result |#
#| -> E10 |#

#| E11 |#
#| -------- |#
#| | n: 1 | |#
#| -------- |#
#| -> global |#

#| E12 |#
#| ------------- |#
#| | result: 1 | |#
#| ------------- |#
#| -> E10 |#

#| E13 |#
#| -------- |#
#| | x: 0 | |#
#| -------- |#
#| -> E2 |#

#| K |#
#| -> |#
#| parameters: previously-computed-result |#
#| body: |#
#| (or |#
#|   previously-computed-result |#
#|   (let |#
#|     ((result (f x))) |#
#|     (insert! x result table) |#
#|     result)) |#
#| -> E2 |#

#| E14 |#
#| ------------------------------------- |#
#| | previously-computed-result: false | |#
#| ------------------------------------- |#
#| -> E13 |#

#| L |#
#| -> |#
#| parameters: result |#
#| body: |#
#| (insert! x result table) |#
#| result |#
#| -> E14 |#

#| E15 |#
#| -------- |#
#| | n: 0 | |#
#| -------- |#
#| -> global |#

#| E16 |#
#| ------------- |#
#| | result: 0 | |#
#| ------------- |#
#| -> E14 |#

#| E17 |#
#| ------------- |#
#| | result: 1 | |#
#| ------------- |#
#| -> E7 |#

#| E18 |#
#| -------- |#
#| | x: 1 | |#
#| -------- |#
#| -> E2 |#

#| M |#
#| -> |#
#| parameters: previously-computed-result |#
#| body: |#
#| (or |#
#|   previously-computed-result |#
#|   (let ((result (f x))) |#
#|     (insert! x result table) |#
#|     result)) |#
#| -> E18 |#

#| E19 |#
#| --------------------------------- |#
#| | previously-computed-result: 1 | |#
#| --------------------------------- |#
#| -> E18 |#

#| E20 |#
#| ------------- |#
#| | result: 2 | |#
#| ------------- |#
#| -> E4 |#







;; constraints
