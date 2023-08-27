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
