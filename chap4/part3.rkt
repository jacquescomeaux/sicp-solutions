#lang sicp
(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; Chapter 4
;; Metalinguistic Abstraction

;; 4.3
;; Variations on a Scheme -- Nondeterministic Computing

;; Amb and Search

#| (define (prime-sum-pair list1 list2) |#
#|   (let |#
#|     ((a (an-element-of list1)) |#
#|      (b (an-element-of list2))) |#
#|     (require (prime? (+ a b))) |#
#|     (list a b))) |#

#| (define (require p) |#
#|   (if (not p) (amb))) |#

#| (define (an-element-of items) |#
#|   (require (not (null? items))) |#
#|   (amb (car items) (an-element-of (cdr items)))) |#

#| (define (an-integer-starting-from n) |#
#|   (amb n (an-integer-starting-from (+ n 1)))) |#

#| 4.35 |#

#| (define (an-integer-between low high) |#
#|   (require (<= low high)) |#
#|   (amb low (an-integer-between (+ low 1) high)) |#

#| (define (a-pythagorean-triple-between low high) |#
#|   (let ((i (an-integer-between low high))) |#
#|     (let ((j (an-integer-between i high))) |#
#|       (let ((k (an-integer-between j high))) |#
#|         (require (= (+ (* i i) (* j j)) (* k k))) |#
#|         (list i j k))))) |#

#| 4.36 |#

#| (define (a-pythagorean-triple-bad) |#
#|   (let ((i (an-integer-starting-from 1))) |#
#|     (let ((j (an-integer-starting-from i))) |#
#|       (let ((k (an-integer-starting-from j))) |#
#|         (require (= (+ (* i i) (* j j)) (* k k))) |#
#|         (list i j k))))) |#

;; there is not a valid k for every value of i j
;; the procedure would get stuck trying new values
;; of k forever

#| (define (a-pythagorean-triple) |#
#|   (let ((i (an-integer-starting-from 1))) |#
#|     (let ((j (an-integer-starting-from i))) |#
#|       (let ((k (an-integer-between j (+ i j)))) |#
#|         (require (= (+ (* i i) (* j j)) (* k k))) |#
#|         (list i j k))))) |#

#| 4.37 |#

#| (define (a-pythagorean-triple-between low high) |#
#|   (let |#
#|     ((i (an-integer-between low high)) |#
#|      (hsq (* high high))) |#
#|     (let ((j (an-integer-between i high))) |#
#|       (let ((ksq (+ (* i i) (* j j)))) |#
#|         (require (>= hsq ksq)) |#
#|         (let ((k (sqrt ksq))) |#
#|           (require (integer? k)) |#
#|           (list i j k)))))) |#

;; this version explores fewer possibilities

;; Examples of Nondeterministic Programs

(define (distinct? items)
  (cond
    ((null? items) true)
    ((null? (cdr items) true))
    ((member (car items) (cdr items)) false)
    (else (distinct? (cdr items)))))

#| (define (multiple-dwelling) |#
#|   (let |#
#|     ((baker (amb 1 2 3 4 5)) |#
#|      (cooper (amb 1 2 3 4 5)) |#
#|      (fletcher (amb 1 2 3 4 5)) |#
#|      (miller (amb 1 2 3 4 5)) |#
#|      (smith (amb 1 2 3 4 5))) |#
#|     (require |#
#|       (distinct? (list baker cooper fletcher miller smith))) |#
#|     (require (not (= baker 5))) |#
#|     (require (not (= cooper 1))) |#
#|     (require (not (= fletcher 5))) |#
#|     (require (not (= fletcher 1))) |#
#|     (require (< miller cooper)) |#
#|     (require (not (= (abs (- smith fletcher)) 1))) |#
#|     (require (not (= (abs (- fletcher cooper)) 1))) |#
#|     (list |#
#|       (list 'baker baker) |#
#|       (list 'cooper cooper) |#
#|       (list 'fletcher fletcher) |#
#|       (list 'miller miller) |#
#|       (list 'smith smith)))) |#

#| 4.38 |#

#| (define (multiple-dwelling-mod) |#
#|   (let |#
#|     ((baker (amb 1 2 3 4 5)) |#
#|      (cooper (amb 1 2 3 4 5)) |#
#|      (fletcher (amb 1 2 3 4 5)) |#
#|      (miller (amb 1 2 3 4 5)) |#
#|      (smith (amb 1 2 3 4 5))) |#
#|     (require |#
#|       (distinct? (list baker cooper fletcher miller smith))) |#
#|     (require (not (= baker 5))) |#
#|     (require (not (= cooper 1))) |#
#|     (require (not (= fletcher 5))) |#
#|     (require (not (= fletcher 1))) |#
#|     (require (< miller cooper)) |#
#|     (require (not (= (abs (- fletcher cooper)) 1))) |#
#|     (list |#
#|       (list 'baker baker) |#
#|       (list 'cooper cooper) |#
#|       (list 'fletcher fletcher) |#
#|       (list 'miller miller) |#
#|       (list 'smith smith)))) |#

#| 4.39 |#

#| (define (multiple-dwelling-reorder) |#
#|   (let |#
#|     ((baker (amb 1 2 3 4 5)) |#
#|      (cooper (amb 1 2 3 4 5)) |#
#|      (fletcher (amb 1 2 3 4 5)) |#
#|      (miller (amb 1 2 3 4 5)) |#
#|      (smith (amb 1 2 3 4 5))) |#
#|     (require (< miller cooper)) |#
#|     (require (not (= (abs (- smith fletcher)) 1))) |#
#|     (require (not (= (abs (- fletcher cooper)) 1))) |#
#|     (require |#
#|       (distinct? (list baker cooper fletcher miller smith))) |#
#|     (require (not (= baker 5))) |#
#|     (require (not (= cooper 1))) |#
#|     (require (not (= fletcher 5))) |#
#|     (require (not (= fletcher 1))) |#
#|     (list |#
#|       (list 'baker baker) |#
#|       (list 'cooper cooper) |#
#|       (list 'fletcher fletcher) |#
#|       (list 'miller miller) |#
#|       (list 'smith smith)))) |#

#| 4.40 |#

#| (define (multiple-dwelling-quick) |#
#|   (let |#
#|     ((baker (amb 1 2 3 4 5))) |#
#|     (require (not (= baker 5))) |#
#|     (let ((cooper (amb 1 2 3 4 5))) |#
#|       (require (not (= cooper 1))) |#
#|       (require (not (= cooper baker))) |#
#|       (let ((fletcher (amb 1 2 3 4 5))) |#
#|         (require (not (= (abs (- fletcher cooper)) 1))) |#
#|         (require (not (= fletcher 1))) |#
#|         (require (not (= fletcher 5))) |#
#|         (require (not (= fletcher baker))) |#
#|         (require (not (= fletcher cooper))) |#
#|         (let ((miller (amb 1 2 3 4 5))) |#
#|           (require (< miller cooper)) |#
#|           (require (not (= miller baker))) |#
#|           (require (not (= miller cooper))) |#
#|           (require (not (= miller fletcher))) |#
#|           (let ((smith (amb 1 2 3 4 5))) |# 
#|             (require (not (= (abs (- smith fletcher)) 1))) |#
#|             (require (not (= smith baker))) |#
#|             (require (not (= smith cooper))) |#
#|             (require (not (= smith fletcher))) |#
#|             (require (not (= smith miller))) |#
#|             (list |#
#|               (list 'baker baker) |#
#|               (list 'cooper cooper) |#
#|               (list 'fletcher fletcher) |#
#|               (list 'miller miller) |#
#|               (list 'smith smith)))))))) |#

#| 4.41 |#

(define (with-next value prev proc)
  (if (null? value)
    (prev)
    (proc (car value) (cdr value))))

(define (require test this next)
  (lambda (x value)
    (if (not (test x))
      (this value)
      (next x value))))

(#%provide multiple-dwelling-scheme)
(define (multiple-dwelling-scheme) 
  (define (fail) (error "No solution"))
  (define (bakerfunc bakers)
    (with-next bakers fail
    (require (lambda (b) (not (= b 5))) bakerfunc
    (lambda (baker bakers)
      (define (cooperfunc coopers)
        (with-next coopers (lambda () bakerfunc bakers)
        (require (lambda (c) (not (= c 1))) cooperfunc
        (require (lambda (c) (not (= c baker))) cooperfunc
        (lambda (cooper coopers)
          (define (fletcherfunc fletchers)
            (with-next fletchers (lambda () (cooperfunc coopers))
            (require (lambda (f) (not (= (abs (- f cooper)) 1))) fletcherfunc
            (require (lambda (f) (not (= f 1))) fletcherfunc
            (require (lambda (f) (not (= f 5))) fletcherfunc
            (require (lambda (f) (not (= f baker))) fletcherfunc
            (require (lambda (f) (not (= f cooper))) fletcherfunc
            (lambda (fletcher fletchers)
              (define (millerfunc millers)
                (with-next millers (lambda () (fletcherfunc fletchers))
                (require (lambda (m) (< m cooper)) millerfunc
                (require (lambda (m) (not (= m baker))) millerfunc
                (require (lambda (m) (not (= m cooper))) millerfunc
                (require (lambda (m) (not (= m fletcher))) millerfunc
                (lambda (miller millers)
                  (define (smithfunc smiths)
                    (with-next smiths (lambda () (millerfunc millers))
                    (require (lambda (s) (not (= (abs (- s fletcher)) 1))) smithfunc
                    (require (lambda (s) (not (= s baker))) smithfunc
                    (require (lambda (s) (not (= s cooper))) smithfunc
                    (require (lambda (s) (not (= s fletcher))) smithfunc
                    (require (lambda (s) (not (= s miller))) smithfunc
                    (lambda (smith smiths)
                      (list
                        (list 'baker baker bakers)
                        (list 'cooper cooper coopers)
                        (list 'fletcher fletcher fletchers)
                        (list 'miller miller millers)
                        (list 'smith smith smiths))))))))))
                  (smithfunc (list 1 2 3 4 5)))))))))
              (millerfunc (list 1 2 3 4 5))))))))))
          (fletcherfunc (list 1 2 3 4 5)))))))
      (cooperfunc (list 1 2 3 4 5))))))
  (bakerfunc (list 1 2 3 4 5)))

#| 4.42 |#
#| 4.43 |#
#| 4.44 |#

#| 4.45 |#
#| 4.46 |#
#| 4.47 |#
#| 4.48 |#
#| 4.49 |#

;; Implementing the Amb Evaluator

#| 4.50 |#
#| 4.51 |#
#| 4.52 |#
#| 4.53 |#
#| 4.54 |#
