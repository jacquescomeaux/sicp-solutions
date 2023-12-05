#lang sicp
(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; Chapter 5
;; Computing with Register Machines

;; 5.5
;; Compilation

;; Structure of the Compiler

(#%provide show-compile)
(define (show-compile exp)
  (for-each
    (lambda (inst)
      (display inst)
      (newline))
    (statements (compile exp 'val 'next))))

(#%provide compile)
(define (compile exp target linkage)
  (cond
    ((self-evaluating? exp)
     (compile-self-evaluating exp target linkage))
    ((quoted? exp) (compile-quoted exp target linkage))
    ((variable? exp)
     (compile-variable exp target linkage))
    ((assignment? exp)
     (compile-assignment exp target linkage))
    ((definition? exp)
     (compile-definition exp target linkage))
    ((if? exp) (compile-if exp target linkage))
    ((lambda? exp)
     (compile-lambda exp target linkage))
    ((begin? exp)
     (compile-sequence (begin-actions exp) target linkage))
    ((cond? exp)
     (compile (cond->if exp) target linkage))
    ((and (pair? exp) (eq? (car exp) '=))
     (compile-bin-op '= exp target linkage))
    ((and (pair? exp) (eq? (car exp) '*))
     (compile-bin-op '* exp target linkage))
    ((and (pair? exp) (eq? (car exp) '-))
     (compile-bin-op '- exp target linkage))
    ((and (pair? exp) (eq? (car exp) '+))
     (compile-bin-op '+ exp target linkage))
    ((application? exp)
     (compile-application exp target linkage))
    (else
      (error "Unknown expression type: COMPILE" exp))))

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

#| 5.31 |#

;; a. preserve env around evaluation of operator
;; b. preserve env around evaluation of each operand (except last)
;; c. preserve argl around evaluation of each operand
;; d. preserve proc around entire operand sequence

;; (f 'x 'y)
;; don't need a, b, c, or d

;; ((f) 'x 'y)
;; don't need b, c, or d

;; (f (g 'x) y)
;; don't need a

;; (f (g 'x) 'y)
;; don't need a or b

;; Compiling Expressions

(define (compile-linkage linkage)
  (cond
    ((eq? linkage 'return)
     (make-instruction-sequence
       '(continue)
       '()
       '((goto (reg continue)))))
    ((eq? linkage 'next)
     (empty-instruction-sequence))
    (else
      (make-instruction-sequence
        '()
        '()
        `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving
    '(continue)
    instruction-sequence
    (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
    linkage
    (make-instruction-sequence
      '()
      (list target)
      `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage
    linkage
    (make-instruction-sequence
      '()
      (list target)
      `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage)
  (end-with-linkage
    linkage
    (make-instruction-sequence
      '(env)
      (list target)
      `((assign
         ,target
         (op lookup-variable-value)
         (const ,exp)
         (reg env))))))

(define (compile-assignment exp target linkage)
  (let
    ((var (assignment-variable exp))
     (get-value-code
       (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage
      linkage
      (preserving
        '(env)
        get-value-code
        (make-instruction-sequence
          '(env val)
          (list target)
          `((perform
             (op set-variable-value!)
             (const ,var)
             (reg val)
             (reg env))
            (assign ,target (const ok))))))))

(define (compile-definition exp target linkage)
  (let
    ((var (definition-variable exp))
     (get-value-code
       (compile (definition-value exp) 'val 'next)))
    (end-with-linkage
      linkage
      (preserving
        '(env)
        get-value-code
        (make-instruction-sequence
          '(env val)
          (list target)
          `((perform
             (op define-variable!)
             (const ,var)
             (reg val)
             (reg env))
            (assign ,target (const ok))))))))

(define (compile-if exp target linkage)
  (let
    ((t-branch (make-label 'true-branch))
     (f-branch (make-label 'false-branch))
     (after-if (make-label 'after-if)))
    (let
      ((consequent-linkage
        (if (eq? linkage 'next) after-if linkage)))
      (let
        ((p-code (compile (if-predicate exp) 'val 'next))
         (c-code
          (compile (if-consequent exp) target consequent-linkage))
         (a-code
          (compile (if-alternative exp) target linkage)))
        (preserving
          '(env continue)
          p-code
          (append-instruction-sequences
            (make-instruction-sequence
              '(val)
              '()
              `((test (op false?) (reg val))
                (branch (label ,f-branch))))
            (parallel-instruction-sequences
              (append-instruction-sequences t-branch c-code)
              (append-instruction-sequences f-branch a-code))
            after-if))))))

(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
    (compile (first-exp seq) target linkage)
    (preserving
      '(env continue)
      (compile (first-exp seq) target 'next)
      (compile-sequence (rest-exps seq) target linkage))))

(define (compile-lambda exp target linkage)
  (let
    ((proc-entry (make-label 'entry))
     (after-lambda (make-label 'after-lambda)))
    (let
      ((lambda-linkage
        (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
        (tack-on-instruction-sequence
          (end-with-linkage
            lambda-linkage
            (make-instruction-sequence
              '(env)
              (list target)
              `((assign
                 ,target
                 (op make-compiled-procedure)
                 (label ,proc-entry)
                 (reg env)))))
          (compile-lambda-body exp proc-entry))
        after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
      (make-instruction-sequence
        '(env proc argl)
        '(env)
        `(,proc-entry
          (assign env (op compiled-procedure-env) (reg proc))
          (assign
           env
           (op extend-environment)
           (const ,formals)
           (reg argl)
           (reg env))))
      (compile-sequence (lambda-body exp) 'val 'return))))

(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
    (string-append
      (symbol->string name)
      (number->string (new-label-number)))))

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))

(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))

(define (compiled-procedure-entry c-proc) (cadr c-proc))

(define (compiled-procedure-env c-proc) (caddr c-proc))

;; Compiling Combinations

(define (compile-application exp target linkage)
  (let
    ((proc-code (compile (operator exp) 'proc 'next))
     (operand-codes
       (map
         (lambda (operand) (compile operand 'val 'next))
         (operands exp))))
    (preserving
      '(env continue)
      proc-code
      (preserving
        '(proc continue)
        (construct-arglist operand-codes)
        (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
      (make-instruction-sequence
        '()
        '(argl)
        '((assign argl (const ()))))
      (let
        ((code-to-get-last-arg
          (append-instruction-sequences
           (car operand-codes)
           (make-instruction-sequence
             '(val)
             '(argl)
             '((assign argl (op list) (reg val)))))))
        (if (null? (cdr operand-codes))
          code-to-get-last-arg
          (preserving
            '(env)
            code-to-get-last-arg
            (code-to-get-rest-args
              (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let
    ((code-for-next-arg
      (preserving
        '(argl)
        (car operand-codes)
        (make-instruction-sequence
          '(val argl)
          '(argl)
          '((assign argl (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
      code-for-next-arg
      (preserving
        '(env)
        code-for-next-arg
        (code-to-get-rest-args
          (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
  (let
    ((primitive-branch (make-label 'primitive-branch))
     (compiled-branch (make-label 'compiled-branch))
     (compound-branch (make-label 'compound-branch))
     (after-call (make-label 'after-call)))
    (let
      ((compiled-linkage
        (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
        (make-instruction-sequence
          '(proc)
          '()
          `((test (op primitive-procedure?) (reg proc))
            (branch (label ,primitive-branch))
            (test (op compound-procedure?) (reg proc))
            (branch (label ,compound-branch))))
        (parallel-instruction-sequences
          (parallel-instruction-sequences
            (append-instruction-sequences
              compiled-branch
              (compile-proc-appl target compiled-linkage))
            (append-instruction-sequences
              compound-branch
              (compound-proc-appl target compiled-linkage)))
          (append-instruction-sequences
            primitive-branch
            (end-with-linkage
              linkage
              (make-instruction-sequence
                '(proc argl)
                (list target)
                `((assign
                   ,target
                   (op apply-primitive-procedure)
                   (reg proc)
                   (reg argl)))))))
        after-call))))

(define (compile-proc-appl target linkage)
  (cond
    ((and
       (eq? target 'val)
       (not (eq? linkage 'return)))
     (make-instruction-sequence
       '(proc)
       all-regs
      `((assign continue (label ,linkage))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val)))))
    ((and
       (not (eq? target 'val))
       (not (eq? linkage 'return)))
     (let ((proc-return (make-label 'proc-return)))
       (make-instruction-sequence
         '(proc)
         all-regs
         `((assign continue (label ,proc-return))
           (assign val (op compiled-procedure-entry) (reg proc))
           (goto (reg val))
           ,proc-return
           (assign ,target (reg val))
           (goto (label ,linkage))))))
    ((and
       (eq? target 'val)
       (eq? linkage 'return))
     (make-instruction-sequence
       '(proc continue)
       all-regs
       '((assign val (op compiled-procedure-entry) (reg proc))
         (goto (reg val)))))
    (else
      #| (and (not (eq? target 'val)) (eq? linkage 'return)) |#
     (error "return linkage, target not val: COMPILE" target))))

(define all-regs '(env proc val argl continue))

;; Combining Instruction Sequences

(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(#%provide statements)
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
      (list-union
        (registers-needed seq1)
        (list-difference
          (registers-needed seq2)
          (registers-modified seq1)))
      (list-union
        (registers-modified seq1)
        (registers-modified seq2))
      (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
      (empty-instruction-sequence)
      (append-2-sequences
        (car seqs)
        (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond
    ((null? s1) s2)
    ((memq (car s1) s2) (list-union (cdr s1) s2))
    (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond
    ((null? s1) '())
    ((memq (car s1) s2) (list-difference (cdr s1) s2))
    (else (cons (car s1) (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
    (append-instruction-sequences seq1 seq2)
    (let ((first-reg (car regs)))
      (if
        (and
          (needs-register? seq2 first-reg)
          (modifies-register? seq1 first-reg))
        (preserving
          (cdr regs)
          (make-instruction-sequence
            (list-union
              (list first-reg)
              (registers-needed seq1))
            (list-difference
              (registers-modified seq1)
              (list first-reg))
            (append
              `((save ,first-reg))
              (statements seq1)
              `((restore ,first-reg))))
          seq2)
        (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
    (registers-needed seq)
    (registers-modified seq)
    (append
      (statements seq)
      (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
    (list-union
      (registers-needed seq1)
      (registers-needed seq2))
    (list-union
      (registers-modified seq1)
      (registers-modified seq2))
    (append
      (statements seq1)
      (statements seq2))))

;; An Example of Compiled Code

(#%provide fact-def)
(define fact-def
  '(define (factorial n)
     (if (= n 1)
       1
       (* (factorial (- n 1)) n))))

#| 5.33 |#

(#%provide fact-alt-def)
(define fact-alt-def
  '(define (factorial-alt n)
     (if (= n 1)
       1
       (* n (factorial-alt (- n 1))))))

;; The two factorial definitions produce
;; compiled code with the same number of instructions

;; while factorial must save and restore argl
;; around the recursive call,
;; factorial-alt must save and restore env
;; around the recursive call

#| 5.34 |#

(#%provide fact-iter-def)
(define fact-iter-def
  '(define (factorial-iter n)
     (define (iter product counter)
       (if (> counter n)
         product
         (iter
           (* counter product)
           (+ counter 1))))
     (iter 1 1)))

;; This version uses constant space
;; continue is restored before the recursive call
;; there is no work to do after the recursive call

#| 5.35 |#

(#%provide decompiled)
(define decompiled
  '(define (f x)
     (+ x (g (+ x 2)))))

#| 5.36 |#

;; The compiler evaluates arguments right-to-left.
;; left-to-right requires appending to the end of
;; the argument list rather than cons-ing on to
;; the front

#| 5.37 |#

(define (preserving-bad regs seq1 seq2)
  (if (null? regs)
    (append-instruction-sequences seq1 seq2)
    (let ((first-reg (car regs)))
      (preserving-bad
        (cdr regs)
        (make-instruction-sequence
          (list-union
            (list first-reg)
            (registers-needed seq1))
          (list-difference
            (registers-modified seq1)
            (list first-reg))
          (append
            `((save ,first-reg))
            (statements seq1)
            `((restore ,first-reg))))
        seq2))))

#| 5.38 |#

(define (spread-arguments op1 op2 next-code)
  (preserving
    '(env)
    (compile op1 'arg1 'next)
    (preserving
      '(arg1)
      (compile op2 'arg2 'next)
      next-code)))

(define (compile-bin-op prim-op exp target linkage)
  (let ((ops (operands exp)))
    (if (= (length ops) 2)
      (end-with-linkage
        linkage
        (spread-arguments
          (car ops)
          (cadr ops)
          (make-instruction-sequence
            '(arg1 arg2)
            (list target)
            `((assign ,target (op ,prim-op) (reg arg1) (reg arg2))))))
      (error "Wrong number of operands -- COMPILE-BIN-OP" (length ops)))))

;; Lexical Addressing

;; Interfacing Compiled Code to the Evaluator

#| 5.45 |#

(#%provide total-pushes-compiled-factorial)
(define (total-pushes-compiled-factorial n)
  (+ (* 2 n) 3))

(#%provide max-depth-compiled-factorial)
(define (max-depth-compiled-factorial n)
  (max 3 (* 2 (- n 1))))

#| total-pushes: |#
#| compiled:interpreted = 2:32 = 1:16 |#
#| compiled:special-purpose = 2:2 = 1:1|#

#| max-depth: |#
#| compiled:interpreted = 2:5 = 1:2.5 |#
#| compiled:special-purpose = 2:2 = 1:1 |#

;; compilation of factorial gives
;; 16x speedup over interpretation
;; and uses 60% less space

;; compilation with open-coded primitives matches
;; hand-written performance

#| 5.46 |#

;; fib

#| max-depth: |#
#| compiled:interpreted = 2:5 |#

#| total-pushes |#
#| compiled:    (14  21  35  56  91) |#
#| interpreted: (72 128 240 408 688) |#

;; still exponential

#| 5.47 |#

#| (compound-branch (make-label 'compound-branch)) |#

#|     (parallel-instruction-sequences |#
#|       (append-instruction-sequences |#
#|         compiled-branch |#
#|         (compile-proc-appl target compiled-linkage)) |#
#|       (append-instruction-sequences |#
#|         compound-branch |#
#|         (compound-proc-appl target compiled-linkage))) |#

(define (compound-proc-appl target linkage)
  (cond
    ((and
       (eq? target 'val)
       (not (eq? linkage 'return)))
     (make-instruction-sequence
       '(proc)
       all-regs
      `((assign continue (label ,linkage))
        (save continue)
        (goto (reg compapp)))))
    ((and
       (not (eq? target 'val))
       (not (eq? linkage 'return)))
     (let ((proc-return (make-label 'proc-return)))
       (make-instruction-sequence
         '(proc)
         all-regs
         `((assign continue (label ,proc-return))
           (save continue)
           (goto (reg compapp))
           ,proc-return
           (assign ,target (reg val))
           (goto (label ,linkage))))))
    ((and
       (eq? target 'val)
       (eq? linkage 'return))
     (make-instruction-sequence
       '(proc continue)
       all-regs
       '((save continue)
         (goto (reg compapp)))))
    (else
     (error "return linkage, target not val: COMPILE" target))))

#| 5.48 |#

(define (compile-and-run expression)
  (let
    ((instructions
      (assemble
        (statements
          (compile expression 'val 'next))
        eceval))
     (old-pc (get-register-contents eceval 'pc)))
    (set-register-contents! eceval 'pc instructions)
    (eceval 'execute)
    (set-register-contents! eceval 'pc old-pc)
    (get-register-contents eceval 'val)))

#| 5.49 |#

(define read-compile-execute-print-loop
  '(read-compile-execute-print-loop
      (perform (op initialize-stack))
      (perform (op prompt-for-input) (const ";;; EC-Compile input:"))
      (assign exp (op read))
      (assign env (op get-global-environment))
      (assign continue (label print-result))
      (assign val (op compile) (reg exp) (const val) (const return))
      (assign val (op statements) (reg val))
      (assign val (op assemble-eccompile) (reg val))
      (goto (reg val))
    print-result
      (perform (op print-stack-statistics))
      (perform (op announce-output) (const ";;; EC-Compile value:"))
      (perform (op user-print) (reg val))
      (goto (label read-compile-execute-print-loop))))

(define (flatten xss)
  (if (null? xss)
    '()
    (append
      (car xss)
      (flatten (cdr xss)))))

#| 5.51 |#

;; https://github.com/jacquescomeaux/pdp11-lisp

;; explicit-control evaluator

(define eval-dispatch
  '(eval-dispatch
      (test (op self-evaluating?) (reg exp))
      (branch (label ev-self-eval))
      (test (op variable?) (reg exp))
      (branch (label ev-variable))
      (test (op quoted?) (reg exp))
      (branch (label ev-quoted))
      (test (op assignment?) (reg exp))
      (branch (label ev-assignment))
      (test (op definition?) (reg exp))
      (branch (label ev-definition))
      (test (op if?) (reg exp))
      (branch (label ev-if))
      (test (op lambda?) (reg exp))
      (branch (label ev-lambda))
      (test (op begin?) (reg exp))
      (branch (label ev-begin))
      (test (op cond?) (reg exp))
      (branch (label ev-cond))
      (test (op let?) (reg exp))
      (branch (label ev-let))
      (test (op application?) (reg exp))
      (branch (label ev-application))
      (goto (label unknown-expression-type))))

(define ev-self-eval
  '(ev-self-eval
      (assign val (reg exp))
      (goto (reg continue))))

(define ev-variable
  '(ev-variable
      (assign val (op lookup-variable-value) (reg exp) (reg env))
      (goto (reg continue))))

(define ev-quoted
  '(ev-quoted
      (assign val (op text-of-quotation) (reg exp))
      (goto (reg continue))))

(define ev-lambda
  '(ev-lambda
      (assign unev (op lambda-parameters) (reg exp))
      (assign exp (op lambda-body) (reg exp))
      (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
      (goto (reg continue))))

(define (empty-arglist) '())

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops)
  (null? (cdr ops)))

(define ev-application
  '(ev-application
      (save continue)
      (save env)
      (assign unev (op operands) (reg exp))
      (save unev)
      (assign exp (op operator) (reg exp))
      (assign continue (label ev-appl-did-operator))
      (goto (label eval-dispatch))
    ev-appl-did-operator
      (restore unev)
      (restore env)
      (assign argl (op empty-arglist))
      (assign proc (reg val))
      (test (op no-operands?) (reg unev))
      (branch (label apply-dispatch))
      (save proc)
    ev-appl-operand-loop
      (save argl)
      (assign exp (op first-operand) (reg unev))
      (test (op last-operand?) (reg unev))
      (branch (label ev-appl-last-arg))
      (save env)
      (save unev)
      (assign continue (label ev-appl-accumulate-arg))
      (goto (label eval-dispatch))
    ev-appl-accumulate-arg
      (restore unev)
      (restore env)
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (assign unev (op rest-operands) (reg unev))
      (goto (label ev-appl-operand-loop))
    ev-appl-last-arg
      (assign continue (label ev-appl-accum-last-arg))
      (goto (label eval-dispatch))
    ev-appl-accum-last-arg
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (restore proc)
      (goto (label apply-dispatch))))

(define apply-dispatch
  '(apply-dispatch
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-apply))
      (test (op compound-procedure?) (reg proc))
      (branch (label compound-apply))
      (test (op compiled-procedure?) (reg proc))
      (branch (label compiled-apply))
      (perform (op user-print) (reg proc))
      (goto (label unknown-procedure-type))
    compiled-apply
      (restore continue)
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
    primitive-apply
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      (restore continue)
      (goto (reg continue))
    compound-apply
      (assign unev (op procedure-parameters) (reg proc))
      (assign env (op procedure-environment) (reg proc))
      (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
      (assign unev (op procedure-body) (reg proc))
      (goto (label ev-sequence))))

(define ev-begin
  '(ev-begin
      (assign unev (op begin-actions) (reg exp))
      (save continue)
      (goto (label ev-sequence))))

(define ev-sequence
  '(ev-sequence
      (assign exp (op first-exp) (reg unev))
      (test (op last-exp?) (reg unev))
      (branch (label ev-sequence-last-exp))
      (save unev)
      (save env)
      (assign continue (label ev-sequence-continue))
      (goto (label eval-dispatch))
    ev-sequence-continue
      (restore env)
      (restore unev)
      (assign unev (op rest-exps) (reg unev))
      (goto (label ev-sequence))
    ev-sequence-last-exp
      (restore continue)
      (goto (label eval-dispatch))))

(define (no-more-exps? seq) (null? seq))

(define ev-sequence-bad
  '(ev-sequence
      (test (op no-more-exps?) (reg unev))
      (branch (label ev-sequence-end))
      (assign exp (op first-exp) (reg unev))
      (save unev)
      (save env)
      (assign continue (label ev-sequence-continue))
      (goto (label eval-dispatch))
    ev-sequence-continue
      (restore env)
      (restore unev)
      (assign unev (op rest-exps) (reg unev))
      (goto (label ev-sequence))
    ev-sequence-end
      (restore continue)
      (goto (reg continue))))

(define ev-if
  '(ev-if
      (save exp)
      (save env)
      (save continue)
      (assign continue (label ev-if-decide))
      (assign exp (op if-predicate) (reg exp))
      (goto (label eval-dispatch))
    ev-if-decide
      (restore continue)
      (restore env)
      (restore exp)
      (test (op true?) (reg val))
      (branch (label ev-if-consequent))
    ev-if-alternative
      (assign exp (op if-alternative) (reg exp))
      (goto (label eval-dispatch))
    ev-if-consequent
      (assign exp (op if-consequent) (reg exp))
      (goto (label eval-dispatch))))

(define ev-assignment
  '(ev-assignment
      (assign unev (op assignment-variable) (reg exp))
      (save unev)
      (assign exp (op assignment-value) (reg exp))
      (save env)
      (save continue)
      (assign continue (label ev-assignment-1))
      (goto (label eval-dispatch))
    ev-assignment-1
      (restore continue)
      (restore env)
      (restore unev)
      (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
      (assign val (const ok))
      (goto (reg continue))))

(define ev-definition
  '(ev-definition
      (assign unev (op definition-variable) (reg exp))
      (save unev)
      (assign exp (op definition-value) (reg exp))
      (save env)
      (save continue)
      (assign continue (label ev-definition-1))
      (goto (label eval-dispatch))
    ev-definition-1
      (restore continue)
      (restore env)
      (restore unev)
      (perform (op define-variable!) (reg unev) (reg val) (reg env))
      (assign val (const ok))
      (goto (reg continue))))

(define ev-cond
  '(ev-cond
      (assign exp (op cond->if) (reg exp))
      (goto (label eval-dispatch))))

(define ev-let
  '(ev-let
      (assign exp (op let->combination) (reg exp))
      (goto (label eval-dispatch))))

(define read-eval-print-loop
  '(  (assign compapp (label compound-apply))
      (branch (label external-entry))
    read-eval-print-loop
      (perform (op initialize-stack))
      (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
      (assign exp (op read))
      (assign env (op get-global-environment))
      (assign continue (label print-result))
      (goto (label eval-dispatch))
    print-result
      (perform (op print-stack-statistics))
      (perform (op announce-output) (const ";;; EC-Eval value:"))
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop))
    external-entry
      (perform (op initialize-stack))
      (assign env (op get-global-environment))
      (assign continue (label print-result))
      (goto (reg val))))

(define errors
  '(unknown-expression-type
      (assign val (const unknown-expression-type-error))
      (goto (label signal-error))
    unknown-procedure-type
      (restore continue)
      (assign val (const unknown-procedure-type-error))
      (goto (label signal-error))
    signal-error
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop))))

(define errors-2
  '(unknown-expression-type
      (assign val (const unknown-expression-type-error))
      (goto (label signal-error))
    unknown-procedure-type
      (restore continue)
      (assign val (const unknown-procedure-type-error))
      (goto (label signal-error))
    signal-error
      (perform (op user-print) (reg val))
      (goto (label read-compile-execute-print-loop))))

(#%provide eceval-controller)
(define eceval-controller
  (flatten
    (list
      read-eval-print-loop
      eval-dispatch
      ev-self-eval
      ev-variable
      ev-quoted
      ev-lambda
      ev-application
      apply-dispatch
      ev-begin
      ev-cond
      ev-let
      ev-sequence
      ev-if
      ev-assignment
      ev-definition
      errors)))

(define eccompile-controller
  (flatten
    (list
      read-compile-execute-print-loop
      eval-dispatch
      ev-self-eval
      ev-variable
      ev-quoted
      ev-lambda
      ev-application
      apply-dispatch
      ev-begin
      ev-cond
      ev-let
      ev-sequence
      ev-if
      ev-assignment
      ev-definition
      errors-2)))

;; syntax procedures

(define (self-evaluating? exp)
  (cond
    ((number? exp) true)
    ((string? exp) true)
    (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda
      (cdadr exp)
      (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond
    ((null? seq) seq)
    ((last-exp? seq) (first-exp seq))
    (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-predicate clause)
  (car clause))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let
      ((first (car clauses))
       (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error
            "ELSE clause isn't last -- COND->IF"
            clauses))
        (make-if
          (cond-predicate first)
          (sequence->exp (cond-actions first))
          (expand-clauses rest))))))

(define (let? exp) (tagged-list? exp 'let))

(define (binding-var binding)
  (car binding))

(define (binding-exp binding)
  (cadr binding))

(define (let-bindings exp)
  (cadr exp))

(define (let-body exp)
  (cddr exp))

(define (let->combination exp)
  (if (null? (let-bindings exp))
    (if (null? (cdr (let-body exp)))
      (car (let-body exp))
      (cons 'begin (let-body exp)))
    (cons
      (cons
        'lambda
        (cons
          (map binding-var (let-bindings exp))
          (let-body exp)))
      (map binding-exp (let-bindings exp)))))

;; run-time procedures

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond
        ((null? vars)
         (env-loop (enclosing-environment env)))
        ((eq? var (car vars))
         (car vals))
        (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan
          (frame-variables frame)
          (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond
        ((null? vars)
         (env-loop (enclosing-environment env)))
        ((eq? var (car vars))
         (set-car! vals val))
        (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan
          (frame-variables frame)
          (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond
        ((null? vars)
         (add-binding-to-frame! var val frame))
        ((eq? var (car vars))
         (set-car! vals val))
        (else (scan (cdr vars) (cdr vals)))))
    (scan
      (frame-variables frame)
      (frame-values frame))))

(define (get-value binding)
  (cdr binding))

(define (set-value! val)
  (lambda (binding)
    (set-cdr! binding val)))

(define (setup-environment)
  (let
    ((initial-env
      (extend-environment
        (primitive-procedure-names)
        (primitive-procedure-objects)
        the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map
    (lambda (proc) (list 'primitive (cadr proc)))
    primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply
    (primitive-implementation proc) args))

(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (user-print object)
  (cond
    ((compound-procedure? object)
     (display
       (list
         'compound-procedure
         (procedure-parameters object)
         (procedure-body object)
         '<procedure-env>)))
    ((compiled-procedure? object)
     (display '<compiled-procedure>))
    (else (display object))))

; register-machine simulator

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

(define (make-instruction text)
  (list text false '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-label inst)
  (cadr inst))

(define (instruction-execution-proc inst)
  (caddr inst))

(define (set-instruction-label! inst label)
  (set-cdr! inst (cons label (cddr inst))))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! (cdr inst) (list proc)))

(define (make-new-machine)
  (let
    ((pc (make-register 'pc))
     (flag (make-register 'flag))
     (stack (make-statck))
     (the-instruction-sequence '())
     (instructions-used '())
     (entry-regs '())
     (stack-regs '())
     (reg-assignments '())
     (inst-count 0)
     (trace? false))
    (let
      ((the-ops
        (list
          (list
            'initialize-stack
            (lambda () (stack 'initialize)))
          (list
            'print-stack-statistics
            (lambda () (stack 'print-statistics)))))
       (register-table
         (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply-defined register: " name)
          (set! register-table
            (cons
              (list name (make-register name))
              register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (error "Unknown register:" name))))
      (define (lookup-register- name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (begin
              (allocate-register name)
              (lookup-register name)))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
            'done
            (begin
              (if trace?
                (begin
                  (if (instruction-label (car insts))
                    (begin
                      (display (instruction-label (car insts)))
                      (newline)))
                  (display "  ")
                  (display (instruction-text (car insts)))
                  (newline)))
              ((instruction-execution-proc (car insts)))
              (set! inst-count (+ inst-count 1))
              (execute)))))
      (define (write-datapath-info insts)
        (set! instructions-used (list-insts insts))
        (set! entry-regs
          (dedup
            (map
              cadr
              (filter
                (lambda (x) (eq? (car x) 'reg))
                (map
                  goto-dest
                  (filter
                    (lambda (inst) (eq? (car inst) 'goto))
                    instructions-used))))))
        (set! stack-regs
          (dedup
            (map
              stack-inst-reg-name
              (filter
                (lambda (inst)
                  (or
                    (eq? (car inst) 'save)
                    (eq? (car inst) 'restore)))
                instructions-used))))
        (set! reg-assignments
          (map
            (lambda (r)
              (list
                r
                (map
                  assign-value-exp
                  (filter
                    (lambda (assign-inst)
                      (eq? (assign-reg-name assign-inst) r))
                    (filter
                      (lambda (inst) (eq? (car inst) 'assign))
                      instructions-used)))))
            (map car register-table)))
        'done)
      (define (dispatch message)
        (cond
          ((eq? message 'start)
           (set-contents! pc the-instruction-sequence)
           (execute))
          ((eq? message 'execute) (execute))
          ((eq? message 'install-instruction-sequence)
           (lambda (seq) (set! the-instruction-sequence seq)))
          ((eq? message 'allocate-register) allocate-register)
          ((eq? message 'get-register) lookup-register)
          ((eq? message 'get-register-alloc) lookup-register-)
          ((eq? message 'install-operations)
           (lambda (ops) (set! the-ops (append the-ops ops))))
          ((eq? message 'stack) stack)
          ((eq? message 'instructions-used) instructions-used)
          ((eq? message 'entry-regs) entry-regs)
          ((eq? message 'stack-regs) stack-regs)
          ((eq? message 'reg-assigns) reg-assignments)
          ((eq? message 'operations) the-ops)
          ((eq? message 'write-info) write-datapath-info)
          ((eq? message 'reset-inst-count)
           (display (list inst-count 'instructions 'executed))
           (newline)
           (set! inst-count 0))
          ((eq? message 'trace-on) (set! trace? true))
          ((eq? message 'trace-off) (set! trace? false))
          (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(#%provide start)
(define (start machine)
  (machine 'start))

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(#%provide make-machine)
(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    ((machine 'write-info) controller-text)
    machine))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
      (cdr val)
      (error "Undefined label -- ASSEMBLE" label-name))))

(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels
      (cdr text)
      (lambda (insts labels)
        (let ((next-inst (car text)))
          (if (symbol? next-inst)
            (if (assoc next-inst labels)
              (error "Duplicate label -- ASSEMBLE" next-inst)
              (begin
                (if (not (null? insts))
                  (set-instruction-label! (car insts) next-inst))
                (receive
                  insts
                  (cons
                    (make-label-entry next-inst insts)
                    labels))))
            (receive
              (cons
                (make-instruction next-inst)
                insts)
              labels)))))))

(define (update-insts! insts labels machine)
  (let
    ((pc (get-register machine 'pc))
     (flag (get-register machine 'flag))
     (stack (machine 'stack))
     (ops (machine 'operations)))
    (for-each
      (lambda (inst)
        (set-instruction-execution-proc!
          inst
          (make-execution-procedure
            (instruction-text inst)
            labels
            machine
            pc
            flag
            stack
            ops)))
      insts)))

(define (assemble controller-text machine)
  (extract-labels
    controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))

(define (assemble-eccompile controller-text)
  (extract-labels
    controller-text
    (lambda (insts labels)
      (update-insts! insts labels eccompile)
      insts)))

(define primitive-procedures
  (list
    (list 'car car)
    (list 'cdr cdr)
    (list 'cons cons)
    (list 'null? null?)
    (list 'number? number?)
    (list '+ +)
    (list '- -)
    (list '* *)
    (list '/ /)
    (list '> >)
    (list '< <)
    (list '= =)
    (list '<= <=)
    (list '>= >=)
    (list 'compile-and-run compile-and-run)))


(define (make-execution-procedure
          inst
          labels
          machine
          pc
          flag
          stack
          ops)
  (cond
    ((eq? (car inst) 'assign)
     (make-assign inst machine labels ops pc))
    ((eq? (car inst) 'test)
     (make-test inst machine labels ops flag pc))
    ((eq? (car inst) 'branch)
     (make-branch inst machine labels flag pc))
    ((eq? (car inst) 'goto)
     (make-goto inst machine labels pc))
    ((eq? (car inst) 'save)
     (make-save inst machine stack pc))
    ((eq? (car inst) 'restore)
     (make-restore inst machine stack pc))
    ((eq? (car inst) 'perform)
     (make-perform inst machine labels ops pc))
    (else
      (error "Unknown instruction type -- ASSEMBLE" inst))))

(define (make-assign inst machine labels operations pc)
  (let
    ((target
      (get-register-alloc machine (assign-reg-name inst)))
     (value-exp (assign-value-exp inst)))
    (let
      ((value-proc
        (if (operation-exp? value-exp)
          (make-operation-exp
            value-exp
            machine
            labels
            operations)
          (make-primitive-exp
            (car value-exp)
            machine
            labels))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
      (let
        ((condition-proc
          (make-operation-exp
            condition
            machine
            labels
            operations)))
        (lambda ()
          (set-contents! flag (condition-proc))
          (advance-pc pc)))
      (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
      (let ((insts (lookup-label labels (label-exp-label dest))))
        (lambda ()
          (if (get-contents flag)
            (set-contents! pc insts)
            (advance-pc pc))))
      (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond
      ((label-exp? dest)
       (let
         ((insts (lookup-label labels (label-exp-label dest))))
         (lambda () (set-contents! pc insts))))
      ((register-exp? dest)
       (let
         ((reg (get-register-alloc machine (register-exp-reg dest))))
         (lambda () (set-contents! pc (get-contents reg)))))
      (else (error "Bad GOTO instruction -- ASSEMBLE" inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let
    ((reg
      (get-register-alloc
        machine
        (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let
    ((reg
      (get-register-alloc
        machine
        (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
      (let
        ((action-proc
          (make-operation-exp
            action
            machine
            labels
            operations)))
        (lambda ()
          (action-proc)
          (advance-pc pc)))
      (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond
    ((constant-exp? exp)
     (let ((c (constant-exp-value exp)))
       (lambda () c)))
    ((label-exp? exp)
     (let ((insts (lookup-label labels (label-exp-label exp))))
       (lambda () insts)))
    ((register-exp? exp)
     (let ((r (get-register-alloc machine (register-exp-reg exp))))
       (lambda () (get-contents r))))
    (else
     (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))

(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let
    ((op (lookup-prim (operation-exp-op exp) operations))
     (aprocs
      (map
        (lambda (e)
          (make-primitive-exp e machine labels))
        (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
      (cadr val)
      (error "Unknown operation -- ASSEMBLE" symbol))))

(define (sort-insts insts)
  (define (number inst)
    (cond
      ((symbol? inst) 0)
      ((eq? (car inst) 'assign) 1)
      ((eq? (car inst) 'test) 2)
      ((eq? (car inst) 'branch) 3)
      ((eq? (car inst) 'goto) 4)
      ((eq? (car inst) 'save) 5)
      ((eq? (car inst) 'restore) 6)
      ((eq? (car inst) 'perform) 7)
      (else 8)))
  (define (compare inst1 inst2)
    (cond
      ((< (number inst1) (number inst2)) 'LT)
      ((= (number inst1) (number inst2)) 'EQ)
      (else 'GT)))
  (define (split xs)
    (cond
      ((null? xs) (list '() '()))
      ((null? (cdr xs)) (list xs '()))
      (else
       (let ((rest-parted (split (cddr xs))))
         (list
           (cons (car xs) (car rest-parted))
           (cons (cadr xs) (cadr rest-parted)))))))
  (define (merge insts1 insts2)
    (cond
      ((null? insts1) insts2)
      ((null? insts2) insts1)
      (else
        (let
          ((i1 (car insts1))
           (i2 (car insts2)))
          (cond
            ((eq? (compare i1 i2) 'LT)
             (cons i1 (merge (cdr insts1) insts2)))
            ((eq? (compare i1 i2) 'EQ)
             (cons i1 (cons i2 (merge (cdr insts1) (cdr insts2)))))
            (else
             (cons i2 (merge insts1 (cdr insts2)))))))))
  (if (or (null? insts) (null? (cdr insts)))
    insts
    (let ((parted (split insts)))
      (merge
        (sort-insts (car parted))
        (sort-insts (cadr parted))))))

(define (filter pred xs)
  (cond
    ((null? xs) '())
    ((pred (car xs))
     (cons (car xs) (filter pred (cdr xs))))
    (else
     (filter pred (cdr xs)))))

(define (nub xs)
  (cond
    ((null? xs) '())
    ((null? (cdr xs)) xs)
    ((equal? (car xs) (cadr xs))
     (nub (cons (car xs) (cddr xs))))
    (else
     (cons (car xs) (nub (cdr xs))))))

(define (list-insts insts)
  (nub
    (filter
      (lambda (x) (not (symbol? x)))
      (sort-insts insts))))

(define (dedup syms)
  (cond
    ((null? syms) '())
    ((memq (car syms) (cdr syms))
     (dedup (cdr syms)))
    (else (cons (car syms) (dedup (cdr syms))))))

(define (get-register-alloc machine reg-name)
  ((machine 'get-register-alloc) reg-name))

(define (make-statck)
  (let
    ((s '())
     (number-pushes 0)
     (max-depth 0)
     (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
        (error "Empty statck -- POP")
        (let ((top (car s)))
          (set! s (cdr s))
          (set! current-depth (- current-depth 1))
          top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (display
        (list
          'total-pushes '= number-pushes
          'maximum-depth '= max-depth))
      (newline))
    (define (dispatch message)
      (cond
        ((eq? message 'push) push)
        ((eq? message 'pop) (pop))
        ((eq? message 'initialize) (initialize))
        ((eq? message 'print-statistics)
         (print-statistics))
        (else
         (error "Unknown request -- STATCK" message))))
    dispatch))

(define (make-register name)
  (let
    ((contents '*unassigned*)
     (trace? false))
    (define (dispatch message)
      (cond
        ((eq? message 'get) contents)
        ((eq? message 'set)
         (lambda (value)
           (if trace?
             (begin
               (display
                 (list
                    name
                    'old-contents '= contents
                    'new-contents '= value))
               (newline)))
           (set! contents value)))
        ((eq? message 'trace-on) (set! trace? true))
        ((eq? message 'trace-off) (set! trace? false))
        (else
         (error "Unknown request -- REGISTER" message))))
    dispatch))

(#%provide trace-reg)
(define (trace-reg machine reg-name on?)
  (let ((reg (get-register machine reg-name)))
    (cond
      ((eq? on? 'on) (reg 'trace-on))
      ((eq? on? 'off) (reg 'trace-off))
      (else "Unknown request - TRACE-REG" on?))))

(define the-global-environment (setup-environment))

(define (get-global-environment) the-global-environment)

(define (start-eceval)
  (set! the-global-environment (setup-environment))
  (set-register-contents! eceval 'flag false)
  (start eceval))

(#%provide eceval-operations)
(define eceval-operations
  (list
    (list 'adjoin-arg adjoin-arg)
    (list 'announce-output announce-output)
    (list 'application? application?)
    (list 'apply-primitive-procedure apply-primitive-procedure)
    (list 'assignment? assignment?)
    (list 'assignment-value assignment-value)
    (list 'assignment-variable assignment-variable)
    (list 'begin? begin?)
    (list 'begin-actions begin-actions)
    (list 'compiled-procedure? compiled-procedure?)
    (list 'compiled-procedure-entry compiled-procedure-entry)
    (list 'compiled-procedure-env compiled-procedure-env)
    (list 'compound-procedure? compound-procedure?)
    (list 'cond? cond?)
    (list 'cond->if cond->if)
    (list 'define-variable! define-variable!)
    (list 'definition? definition?)
    (list 'definition-value definition-value)
    (list 'definition-variable definition-variable)
    (list 'empty-arglist empty-arglist)
    (list 'extend-environment extend-environment)
    (list 'false? false?)
    (list 'first-exp first-exp)
    (list 'first-operand first-operand)
    (list 'get-global-environment get-global-environment)
    (list 'if? if?)
    (list 'if-alternative if-alternative)
    (list 'if-consequent if-consequent)
    (list 'if-predicate if-predicate)
    (list 'lambda? lambda?)
    (list 'lambda-body lambda-body)
    (list 'lambda-parameters lambda-parameters)
    (list 'last-exp? last-exp?)
    (list 'last-operand? last-operand?)
    (list 'let? let?)
    (list 'let->combination let->combination)
    (list 'lookup-variable-value lookup-variable-value)
    (list 'make-compiled-procedure make-compiled-procedure)
    (list 'make-procedure make-procedure)
    (list 'no-more-exps? no-more-exps?)
    (list 'no-operands? no-operands?)
    (list 'operands operands)
    (list 'operator operator)
    (list 'primitive-procedure? primitive-procedure?)
    (list 'procedure-body procedure-body)
    (list 'procedure-environment procedure-environment)
    (list 'procedure-parameters procedure-parameters)
    (list 'prompt-for-input prompt-for-input)
    (list 'quoted? quoted?)
    (list 'read read)
    (list 'rest-exps rest-exps)
    (list 'rest-operands rest-operands)
    (list 'self-evaluating? self-evaluating?)
    (list 'set-variable-value! set-variable-value!)
    (list 'text-of-quotation text-of-quotation)
    (list 'true? true?)
    (list 'user-print user-print)
    (list 'variable? variable?)
    (list 'list list)
    (list 'cons cons)
    (list 'compile compile)
    (list 'assemble-eccompile assemble-eccompile)
    (list 'statements statements)
    (list '= =)
    (list '* *)
    (list '- -)
    (list '+ +)))

(define eceval (make-machine eceval-operations eceval-controller))

(#%provide eccompile)
(define eccompile (make-machine eceval-operations eccompile-controller))

(#%provide compile-and-go)
(define (compile-and-go expression)
  (let
    ((instructions
      (assemble
        (statements
          (compile expression 'val 'return))
        eceval)))
    (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    #| (eceval 'trace-on) |#
    #| (trace-reg eceval 'proc 'on) |#
    (start eceval)))
