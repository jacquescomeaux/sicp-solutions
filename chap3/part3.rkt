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

(#%provide make-time-segment)
(define (make-time-segment time queue)
  (cons time queue))

(#%provide segment-time)
(define (segment-time s) (car s))

(#%provide segment-queue)
(define (segment-queue s) (cdr s))

(#%provide make-agenda)
(define (make-agenda) (list 0))

(#%provide current-time)
(define (current-time agenda) (car agenda))

(#%provide set-current-time!)
(define (set-current-time! agenda time)
  (set-car! agenda time))

(#%provide segments)
(define (segments agenda) (cdr agenda))

(#%provide set-segments!)
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(#%provide first-segment)
(define (first-segment agenda)
  (car (segments agenda)))

(#%provide rest-segments)
(define (rest-segments agenda)
  (cdr (segments agenda)))

(#%provide empty-agenda?)
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(#%provide add-to-agenda!)
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or
      (null? segments)
      (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      (insert-queue!
        (segment-queue (car segments))
        action)
      (let
        ((rest (cdr segments)))
        (if (belongs-before? rest)
          (set-cdr!
            segments
            (cons
              (make-new-time-segment time action)
              (cdr segments)))
          (add-to-segments! rest)))))
  (let
    ((segments (segments agenda)))
    (if
      (belongs-before? segments)
      (set-segments!
        agenda
        (cons
          (make-new-time-segment time action)
          segments))
      (add-to-segments! segments))))

(#%provide remove-first-agenda-item!)
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(#%provide first-agenda-item)
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty -- FIRST-AGENDA-ITEM")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(#%provide propagate)
(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))

(define (after-delay delay action)
  (add-to-agenda!
    (+ delay (current-time the-agenda))
    action
    the-agenda))

(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin
      ((car procedures))
      (call-each (cdr procedures)))))

(#%provide make-wire)
(define (make-wire)
  (let
    ((signal-value 0)
     (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
        (begin
          (set! signal-value new-value)
          (call-each action-procedures))
        'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond
        ((eq? m 'get-signal) signal-value)
        ((eq? m 'set-signal!) set-my-signal!)
        ((eq? m 'add-action!) accept-action-procedure!)
        (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(#%provide get-signal)
(define (get-signal wire)
  (wire 'get-signal))

(#%provide set-signal!)
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(#%provide add-action!)
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (logical-not s)
  (cond
    ((= s 0) 1)
    (else 0)))

(#%provide inverter)
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay
        inverter-delay
        (lambda ()
          (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-and a b)
  (cond
    ((= a 0) 1)
    (else b)))

(#%provide and-gate)
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let
      ((new-value
        (logical-and
          (get-signal a1)
          (get-signal a2))))
      (after-delay
        and-gate-delay
        (lambda ()
          (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

#| 3.28 |#

(define (logical-or a b)
  (cond
    ((= a 0) b)
    (else 1)))

(#%provide or-gate)
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let
      ((new-value
        (logical-or
          (get-signal a1)
          (get-signal a2))))
      (after-delay
        or-gate-delay
        (lambda ()
          (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(#%provide half-adder)
(define (half-adder a b s c)
  (let
    ((d (make-wire))
     (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(#%provide full-adder)
(define (full-adder a b c-in sum c-out)
  (let
    ((s (make-wire))
     (c1 (make-wire))
     (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

#| 3.29 |#

(#%provide or-gate-)
(define (or-gate- a1 a2 output)
  (let
    ((na1 (make-wire))
     (na2 (make-wire))
     (nout (make-wire)))
    (inverter a1 na1)
    (inverter a2 na2)
    (and-gate na1 na2 nout)
    (inverter nout na2)
    'ok))

(#%provide probe-wire)
(define (probe-wire name wire)
  (add-action!
    wire
    (lambda ()
      (newline)
      (display name)
      (display " ")
      (display (current-time the-agenda))
      (display "  New-value = ")
      (display (get-signal wire)))))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(#%provide has-value?)
(define (has-value? connector)
  (connector 'has-value?))

(#%provide get-value)
(define (get-value connector)
  (connector 'value))

(#%provide set-value!)
(define (set-value! connector new-value informant)
  ((connector 'set-value) new-value informant))

(#%provide forget-value!)
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(#%provide connect)
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond
      ((null? items) 'done)
      ((eq? (car items) exception)
       (loop (cdr items)))
      (else
        (procedure (car items))
        (loop (cdr items)))))
  (loop list))

(#%provide make-connector)
(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond
        ((not (has-value? me))
         (set! value newval)
         (set! informant setter)
         (for-each-except
           setter
           inform-about-value
           constraints))
        ((not (= value newval))
         (error "Contradiction" (list value newval)))
        (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
        (begin
          (set! informant false)
          (for-each-except
            retractor
            inform-about-no-value
            constraints))
        'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set!
          constraints
          (cons new-constraint constraints)))
      (if (has-value? me)
        (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond
        ((eq? request 'has-value?)
         (if informant true false))
        ((eq? request 'value) value)
        ((eq? request 'set-value) set-my-value)
        ((eq? request 'forget) forget-my-value)
        ((eq? request 'connect) connect)
        (else
          (error
            "Unknown operation -- CONNECTOR"
            request))))
    me))


(#%provide adder)
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond
      ((and (has-value? a1) (has-value? a2))
       (set-value!
         sum
         (+ (get-value a1) (get-value a2))
         me))
      ((and (has-value? a1) (has-value? sum))
       (set-value!
         a2
         (- (get-value sum) (get-value a1))
         me))
      ((and (has-value? a2) (has-value? sum))
       (set-value!
         a1
         (- (get-value sum) (get-value a2))
         me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value)
       (process-new-value))
      ((eq? request 'I-lost-my-value)
       (process-forget-value))
      (else
        (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(#%provide multiplier)
(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond
      ((or
         (and (has-value? m1) (= (get-value m1) 0))
         (and (has-value? m2) (= (get-value m2) 0)))
       (set-value! product 0 me))
      ((and (has-value? m1) (has-value? m2))
       (set-value!
         product
         (* (get-value m1) (get-value m2))
         me))
      ((and (has-value? m1) (has-value? product))
       (set-value!
         m2
         (/ (get-value product) (get-value m1))
         me))
      ((and (has-value? m2) (has-value? product))
       (set-value!
         m1
         (/ (get-value product) (get-value m2))
         me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value)
       (process-new-value))
      ((eq? request 'I-lost-my-value)
       (process-forget-value))
      (else
        (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(#%provide constant)
(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(#%provide probe)
(define (probe name connector)
  (define (print-probe value)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value)
    (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value)
       (process-new-value))
      ((eq? request 'I-lost-my-value)
       (process-forget-value))
      (else
        (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(#%provide celsius-fahrenheit-converter)
(define (celsius-fahrenheit-converter c f)
  (let
    ((u (make-connector))
     (v (make-connector))
     (w (make-connector))
     (x (make-connector))
     (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

#| 3.33 |#

(#%provide averager)
(define (averager a b c)
  (let
    ((sum (make-connector))
     (n (make-connector)))
    (adder a b sum)
    (multiplier n c sum)
    (constant 2 n)
    'ok))

#| 3.34 |#

(#%provide squarer-bad)
(define (squarer-bad a b)
  (multiplier a a b))

;; a can't be determined from b

#| 3.35 |#

(#%provide squarer)
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
      (if (< (get-value b) 0)
        (error
          "square less than 0 -- SQUARER"
          (get-value b))
        (set-value! a (sqrt (get-value b)) me))
      (if (has-value? a)
        (set-value! b (* (get-value a) (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value)
       (process-new-value))
      ((eq? request 'I-lost-my-value)
       (process-forget-value))
      (else
        (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

#| 3.37 |#

(#%provide c+)
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(#%provide c-)
(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(#%provide c*)
(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(#%provide c/)
(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(#%provide cv)
(define (cv v)
  (let ((z (make-connector)))
    (constant v z)
    z))

(#%provide celsius-fahrenheit-converter-exp)
(define (celsius-fahrenheit-converter-exp x)
  (c+
    (c* (c/ (cv 9) (cv 5)) x)
    (cv 32)))
