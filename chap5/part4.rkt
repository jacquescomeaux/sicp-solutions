#lang sicp
(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; Chapter 5
;; Computing with Register Machines

;; 5.4
;; The Explicit-Control Evaluator

;; The Core of the Explicit-Control Evaluator

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
      (goto (label unknown-procedure-type))
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

;; Sequence Evaluation and Tail Recursion

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

(#%provide count)
(define (count n)
  (newline)
  (display n)
  (count (+ n 1)))

;; Conditionals, Assignments, and Definitions

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

#| 5.23 |#

(define ev-cond
  '(ev-cond
      (assign exp (op cond->if) (reg exp))
      (goto (label eval-dispatch))))

(define ev-let
  '(ev-let
      (assign exp (op let->combination) (reg exp))
      (goto (label eval-dispatch))))

;; Running the Evaluator

(define read-eval-print-loop
  '(read-eval-print-loop
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
      (goto (label read-eval-print-loop))))

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

(define (flatten xss)
  (if (null? xss)
    '()
    (append
      (car xss)
      (flatten (cdr xss)))))

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

#| 5.26 |#

(define (factorial-iter n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product) (+ counter 1))))
  (iter 1 1))

; maximum stack depth for any n is 10

(#%provide total-pushes-factorial-iter)
(define (total-pushes-factorial-iter n)
  (+ (* 35 n) 29))

#| 5.27 |#

(define (factorial n)
  (if (= n 1) 1 (* (factorial (- n 1)) n)))

(#%provide max-depth-factorial-rec)
(define (max-depth-factorial-rec n)
  (+ (* 5 n) 3))

(#%provide total-pushes-factorial-rec)
(define (total-pushes-factorial-rec n)
  (- (* 32 n) 16))

          #| |   Max depth   |   Num pushes |#
#| ----------|---------------|----------------- |#
#| fact-rec  | (+ (* 5 n) 3) | (- (* 32 n) 16) |#
#| ----------|---------------|----------------- |#
#| fact-iter |            10 | (+ (* 35 n) 29) |#

#| 5.28 |#

;; without tail-recursive ev-sequence:

          #| |   Max depth    |   Num pushes |#
#| ----------|----------------|----------------- |#
#| fact-rec  |  (+ (* 8 n) 3) | (- (* 34 n) 16) |#
#| ----------|----------------|----------------- |#
#| fact-iter | (+ (* 3 n) 14) | (+ (* 37 n) 33) |#

#| 5.29 |#

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(#%provide max-depth-fib)
(define (max-depth-fib n)
  (+ (* 5 n) 3))

(#%provide total-pushes-fib)
(define (total-pushes-fib n)
  (if (< n 2)
    16
    (+
      40
      (total-pushes-fib (- n 1))
      (total-pushes-fib (- n 2)))))

(#%provide total-pushes-fib-)
(define (total-pushes-fib- n)
  (- (* 56 (fib (+ n 1))) 40))

; syntax and run-time procedures

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

(define (true? x)
  (not (eq? x false)))

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
    (list '>= >=)))

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
  (if (compound-procedure? object)
    (display
      (list
        'compound-procedure
        (procedure-parameters object)
        (procedure-body object)
        '<procedure-env>))
    (display object)))

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
          (make-op-primitive-exp e machine labels))
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

(define (make-op-primitive-exp exp machine labels)
  (cond
    ((constant-exp? exp)
     (let ((c (constant-exp-value exp)))
       (lambda () c)))
    ((register-exp? exp)
     (let ((r (get-register-alloc machine (register-exp-reg exp))))
       (lambda () (get-contents r))))
    ((label-exp? exp)
     (error "Label in operator expression" exp))
    (else
     (error "Unknown expression type -- ASSEMBLE" exp))))

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

(define (trace-reg machine reg-name on?)
  (let ((reg (get-register machine reg-name)))
    (cond
      ((eq? on? 'on) (reg 'trace-on))
      ((eq? on? 'off) (reg 'trace-off))
      (else "Unknown request - TRACE-REG" on?))))

(define the-global-environment (setup-environment))

(define (get-global-environment) the-global-environment)

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
    (list 'compound-procedure? compound-procedure?)
    (list 'cond? cond?)
    (list 'cond->if cond->if)
    (list 'define-variable! define-variable!)
    (list 'definition? definition?)
    (list 'definition-value definition-value)
    (list 'definition-variable definition-variable)
    (list 'empty-arglist empty-arglist)
    (list 'extend-environment extend-environment)
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
    (list 'variable? variable?)))
