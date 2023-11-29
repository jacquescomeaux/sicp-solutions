#lang sicp
(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; Chapter 5
;; Computing with Register Machines

;; 5.1
;; Designing Register Machines

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

#| 5.1 |#

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter
        (* counter product)
        (+ counter 1))))
  (iter 1 1))

;; A Language for Describing Register Machines

(define gcd-data-paths
  '(data-paths
     (registers
       ((name a)
        (buttons ((name a<-b) (source (register b)))))
       ((name b)
        (buttons ((name b<-t) (source (register t)))))
       ((name t)
        (buttons ((name t<-r) (source (operation rem))))))
     (operations
       ((name rem)
        (inpus (register a) (register b)))
       ((name =)
        (inputs (register b) (constant 0))))))

(define gcd-controller
  '(controller
    test-b
      (test =)
      (branch (label gcd-done))
      (t<-r)
      (a<-b)
      (b<-t)
      (goto (label test-b))
    gcd-done))

(define gcd-succinct
  '(controller
    test-b
      (test (op =) (reg b) (const 0))
      (branch (label gcd-done))
      (assign t (op rem) (reg a) (reg b))
      (assign a (reg b))
      (assign b (reg t))
      (goto (label test-b))
    gcd-done))

#| 5.2 |#

(define factorial-controller
  '(controller
      (assign product (const 1))
      (assign counter (const 1))
    test-counter
      (test (op >) (reg counter) (reg n))
      (branch (label fact-done))
      (assign product (op *) (reg counter) (reg product))
      (assign counter (op +) (reg counter) (const 1))
      (goto (label test-counter))
    fact-done))

(define gcd-new
  '(controller
    gcd-loop
      (assign a (op read))
      (assign b (op read))
    test-b
      (test (op =) (reg b) (const 0))
      (branch (label gcd-done))
      (assign t (op rem) (reg a) (reg b))
      (assign a (reg b))
      (assign b (reg t))
      (goto (label test-b))
    gcd-done
      (perform (op print) (reg a))
      (goto (label gcd-loop))))

;; Abstraction in Machine Design

(define (remainder n d)
  (if (< n d)
    n
    (remainder (- n d) d)))

(define gcd-prim
  '(controller
    test-b
      (test (op =) (reg b) (const 0))
      (branch (label gcd-done))
      (assign t (reg a))
    rem-loop
      (test (op <) (reg t) (reg b))
      (branch (label rem-done))
      (assign t (op -) (reg t) (reg b))
      (goto (label rem-loop))
    rem-done
      (assign a (reg b))
      (assign b (reg t))
      (goto (label test-b))
    gcd-done))

#| 5.3 |#

(define (sqrt x)
  (define (square x) (* x x))
  (define (average a b) (/ (+ a b) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define sqrt-controller-1
  '(controller
      (assign guess (const 1.0))
    test-guess
      (test (op good-enough?) (reg guess))
      (branch (label sqrt-done))
      (assign guess (op improve) (reg guess))
      (goto (label test-guess))
    sqrt-done))

(define sqrt-controller-2
  '(controller
      (assign guess (const 1.0))
    test-guess
      (test (op good-enough?) (reg guess))
      (branch (label sqrt-done))
      (assign t (op /) (reg x) (reg guess))
      (assign guess (op average) (reg guess) (reg t))
      (goto (label test-guess))
    sqrt-done))

(define sqrt-controller-3
  '(controller
      (assign guess (const 1.0))
    test-guess
      (test (op good-enough?) (reg guess))
      (branch (label sqrt-done))
      (assign t (op /) (reg x) (reg guess))
      (assign t (op +) (reg t) (reg guess))
      (assign guess (op /) (reg t) (const 2))
      (goto (label test-guess))
    sqrt-done))

(define sqrt-controller-4
  '(controller
      (assign guess (const 1.0))
    test-guess
      (assign t (op *) (reg guess) (reg guess))
      (assign t (op -) (reg t) x)
      (assign t (op abs) (reg t))
      (test (op <) (reg t) (const 0.001))
      (branch (label sqrt-done))
      (assign t (op /) (reg x) (reg guess))
      (assign t (op +) (reg t) (reg guess))
      (assign guess (op /) (reg t) (const 2))
      (goto (label test-guess))
    sqrt-done))

;; Subroutines

(define gcd-twice
  '(controller
    gcd-1
      (test (op =) (reg b) (const 0))
      (branch (label after-gcd-1))
      (assign t (op rem) (reg a) (reg b))
      (assign a (reg b))
      (assign b (reg t))
      (goto (label gcd-1))
    after-gcd-1
      ; ...
    gcd-2
      (test (op =) (reg b) (const 0))
      (branch (label after-gcd-2))
      (assign t (op rem) (reg a) (reg b))
      (assign a (reg b))
      (assign b (reg t))
      (goto (label gcd-2))
    after-gcd-2))

(define gcd-sub
  '(controller
    gcd
      (test (op =) (reg b) (const 0))
      (branch (label gcd-done))
      (assign t (op rem) (reg a) (reg b))
      (assign a (reg b))
      (assign b (reg t))
      (goto (label gcd))
    gcd-done
      (test (op =) (reg continue) (const 0))
      (branch (label after-gcd-1))
      (goto (label after-gcd-2))
      ; ...
      (assign continue (const 0))
      (goto (label gcd))
    after-gcd-1
      ; ...
      (assign continue (const 1))
      (goto (label gcd)
    after-gcd-2)))

(define gcd-sub-label
  '(controller
    gcd
      (test (op =) (reg b) (const 0))
      (branch (label gcd-done))
      (assign t (op rem) (reg a) (reg b))
      (assign a (reg b))
      (assign b (reg t))
      (goto (label gcd))
    gcd-done
      (goto (reg continue))
      ; ...
      (assign continue (label after-gcd-1))
      (goto (label gcd))
    after-gcd-1
      ; ...
      (assign continue (label after-gcd-2))
      (goto (label gcd)
    after-gcd-2)))

;; Using a Stack to Implement Recursion

(define (factorial-rec n)
  (if (= n 1)
    1
    (* (factorial (- n 1)) n)))

(define fact-controller
  '(controller
      (assign (continue (label fact-done)))
    fact-loop
      (test (op =) (reg n) (const 1))
      (branch (label base-case))
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-fact))
      (goto (label fact-loop))
    after-fact
      (restore n)
      (restore continue)
      (assign val (op *) (reg n) (reg val))
      (goto (reg continue))
    base-case
      (assign val (const 1))
      (goto (reg continue))
    fact-done))

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(define fib-controller
  '(controller
      (assign continue (label fib-done))
    fib-loop
      (test (op <) (reg n) (const 2))
      (branch (label immediate answer))
      (save continue)
      (assign continue (label afterfib-n-1))
      (save n)
      (assign n (op -) (reg n) (const 1))
      (goto (label fib-loop))
    afterfib-n-1
      (restore n)
      (restore continue)
      (assign n (op -) (reg n) (const 2))
      (save continue)
      (assign continue (label afterfib-n-2))
      (save val)
      (goto (label fib-loop))
    afterfib-n-2
      (assign n (reg val))
      (restore val)
      (restore continue)
      (assign val (op +) (reg val) (reg n))
      (goto (reg continue))
    immediate-answer
      (assign val (reg n))
      (goto (reg continue))
    fib-done))

#| 5.4 |#

(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

(define expt-controller
  '(controller
      (assign (continue (label expt-done)))
    expt-loop
      (test (op =) (reg n) (const 0))
      (branch (label base-case))
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-expt))
      (goto (label expt-loop))
    after-expt
      (restore n)
      (restore continue)
      (assign val (op *) (reg b) (reg val))
      (goto (reg continue))
    base-case
      (assign val (const 1))
      (goto (reg continue))
    expt-done))

(define (expt- b n)
  (define (expt-iter counter product)
    (if (= counter 0)
      product
      (expt-iter (- counter 1) (* b product))))
  (expt-iter n 1))

(define expt-iter-controller
  '(controller
      (assign counter (reg n))
      (assign product (const 1))
    test-counter
      (test (op =) (reg counter) (const 0))
      (branch (label expt-done))
      (assign counter (op -) (reg counter) (const 1))
      (assign product (op *) (reg b) (reg product))
      (goto (label test-counter))
    expt-done))

#| 5.6 |#

(define fib-controller-trimmed
  '(controller
      (assign continue (label fib-done))
    fib-loop
      (test (op <) (reg n) (const 2))
      (branch (label immediate answer))
      (save continue)
      (assign continue (label afterfib-n-1))
      (save n)
      (assign n (op -) (reg n) (const 1))
      (goto (label fib-loop))
    afterfib-n-1
      (restore n)
      ;; (restore continue)
      (assign n (op -) (reg n) (const 2))
      ;; (save continue)
      (assign continue (label afterfib-n-2))
      (save val)
      (goto (label fib-loop))
    afterfib-n-2
      (assign n (reg val))
      (restore val)
      (restore continue)
      (assign val (op +) (reg val) (reg n))
      (goto (reg continue))
    immediate-answer
      (assign val (reg n))
      (goto (reg continue))
    fib-done))

;; Instruction Summary

#| (assign [register-name] (reg [register-name])) |#
#| (assign [register-name] (const [constant-value])) |#
#| (assign [register-name] (op [operation-name]) [input-1] ... [input-n]) |#
#| (perform (op [operation-name]) [input-1] ... [input-n]) |#
#| (test (op [operation-name]) [input-1] ... [input-n]) |#
#| (branch (label [label-name])) |#
#| (goto (label [label-name])) |#

#| (assign [register-name] (label [label-name])) |#
#| (goto (reg [register-name])) |#

#| (save [register-name]) |#
#| (restore [register-name]) |#
