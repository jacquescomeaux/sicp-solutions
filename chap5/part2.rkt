#lang sicp
(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; Chapter 5
;; Computing with Register Machines

;; 5.2
;; A Register-Machine Simulator

#| 5.7 |#

(#%provide expt-controller)
(define expt-controller
  '(controller
      (assign continue (label expt-done))
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

(#%provide expt-iter-controller)
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

(#%provide test-expt)
(define (test-expt)
  (let
    ((expt-machine
      (make-machine
        #| '(n b val continue) |#
        (list (list '- -) (list '* *) (list '= =))
        expt-controller)))
    (set-register-contents! expt-machine 'b 5)
    (set-register-contents! expt-machine 'n 4)
    #| (expt-machine 'trace-on) |#
    (trace-reg expt-machine 'val 'on)
    (start expt-machine)
    (expt-machine 'reset-inst-count)
    (get-register-contents expt-machine 'val)))

(#%provide test-expt-iter)
(define (test-expt-iter)
  (let
    ((expt-machine
      (make-machine
        #| '(n b product counter) |#
        (list (list '- -) (list '* *) (list '= =))
        expt-iter-controller)))
    (set-register-contents! expt-machine 'b 10)
    (set-register-contents! expt-machine 'n 4)
    (expt-machine 'trace-on)
    (start expt-machine)
    (expt-machine 'reset-inst-count)
    (get-register-contents expt-machine 'product)))

;; The Machine Model

(#%provide make-register-)
(define (make-register- name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond
        ((eq? message 'get) contents)
        ((eq? message 'set)
         (lambda (value) (set! contents value)))
        (else
         (error "Unknown request -- REGISTER" message))))
    dispatch))

(#%provide get-contents)
(define (get-contents register)
  (register 'get))

(#%provide set-contents!)
(define (set-contents! register value)
  ((register 'set) value))

(#%provide make-stack)
(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
        (error "Empty stack -- POP")
        (let ((top (car s)))
          (set! s (cdr s))
          top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond
        ((eq? message 'push) push)
        ((eq? message 'pop) (pop))
        ((eq? message 'initialize) (initialize))
        (else (error "Unknown request -- STACK" message))))
    dispatch))

(#%provide pop)
(define (pop stack)
  (stack 'pop))

(#%provide push)
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

(#%provide make-new-machine)
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

(#%provide get-register-contents)
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(#%provide set-register-contents!)
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(#%provide make-machine)
(define (make-machine ops controller-text)
#| (define (make-machine register-names ops controller-text) |#
  (let ((machine (make-new-machine)))
    #| (for-each |#
    #|   (lambda (register-name) |#
    #|     ((machine 'allocate-register) register-name)) |#
    #|   register-names) |#
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    ((machine 'write-info) controller-text)
    machine))

;; The Assembler

(define (make-label-entry label-name insts)
  (cons label-name insts))

(#%provide lookup-label)
(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
      (cdr val)
      (error "Undefined label -- ASSEMBLE" label-name))))

(#%provide extract-labels)
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

#| 5.8 |#

(#%provide ambiguous-labels)
(define ambiguous-labels
  '(start
      (goto (label here))
    here
      (assign a (const 3))
      (goto (label there))
    here
      (assign a (const 4))
      (goto (label there))
    there))

#| (if (assoc next-inst labels) |#
#|   (error "Duplicate label -- ASSEMBLE" next-inst) |#
#|   (receive |#
#|     insts |#
#|     (cons |#
#|       (make-label-entry next-inst insts) |#
#|       labels))) |#

;; Generating Execution Procedures for Instructions

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

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

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

#| 5.9 |#

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

#| 5.11 |#

(define (make-save- inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register-alloc machine reg-name)))
      (lambda ()
        (push stack (list reg-name (get-contents reg)))
        (advance-pc pc)))))

(define (make-restore- inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register-alloc machine reg-name)))
      (lambda ()
        (let ((name-val (pop stack)))
          (if (eq? (car name-val) reg-name)
            (begin
              (set-contents! reg (cadr name-val))
              (advance-pc pc))
            (error "Restore intro wrong register" inst)))))))

#| 5.12 |#

(#%provide sort-insts)
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

(define (flatten xss)
  (if (null? xss)
    '()
    (append
      (car xss)
      (flatten (cdr xss)))))

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

(#%provide list-insts)
(define (list-insts insts)
  (nub
    (filter
      (lambda (x) (not (symbol? x)))
      (sort-insts insts))))

(#%provide dedup)
(define (dedup syms)
  (cond
    ((null? syms) '())
    ((memq (car syms) (cdr syms))
     (dedup (cdr syms)))
    (else (cons (car syms) (dedup (cdr syms))))))

#| 5.13 |#

(define (inst-regs inst)
  (cond
    ((symbol? inst) '())
    ((eq? (car inst) 'assign)
     (cons
       (assign-reg-name inst)
       (let ((value-exp (assign-value-exp inst)))
         (if (operation-exp? value-exp)
           (operation-exp-regs value-exp)
           (prim-exp-regs (car value-exp))))))
    ((eq? (car inst) 'test)
     (operation-exp-regs (test-condition inst)))
    ((and
       (eq? (car inst) 'goto)
       (register-exp? (goto-dest inst)))
     (list (register-exp-reg (goto-dest inst))))
    ((eq? (car inst) 'save)
     (list (stack-inst-reg-name inst)))
    ((eq? (car inst) 'restore)
     (list (stack-inst-reg-name inst)))
    ((eq? (car inst) 'perform)
     (operation-exp-regs (perform-action inst)))
    (else '())))

(define (operation-exp-regs exp)
  (flatten
    (map
      prim-exp-regs
      (operation-exp-operands exp))))

(define (prim-exp-regs exp)
  (if (register-exp? exp)
    (list (register-exp-reg exp))
    '()))

(#%provide find-regs)
(define (find-regs controller-text)
  (dedup
    (flatten
      (map inst-regs controller-text))))

#| (define (lookup-register- name) |#
#|   (let ((val (assoc name register-table))) |#
#|     (if val |#
#|       (cadr val) |#
#|       (begin |#
#|         (allocate-register name) |#
#|         (lookup-register name))))) |#

(define (get-register-alloc machine reg-name)
  ((machine 'get-register-alloc) reg-name))

;; Monitoring Machine Performance

#| 5.14 |#

#| (list |#
#|   (list |#
#|     'initialize-stack |#
#|     (lambda () (stack 'initialize))) |#
#|   (list |#
#|     'print-stack-statistics |#
#|     (lambda () (stack 'print-statistics)))) |#

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

(#%provide fact-controller)
(define fact-controller
  '(controller
    fact-loop
      (perform (op initialize-stack))
      (assign n (op read))
      (assign continue (label fact-done))
    fact
      (test (op =) (reg n) (const 1))
      (branch (label base-case))
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-fact))
      (goto (label fact))
    after-fact
      (restore n)
      (restore continue)
      (assign val (op *) (reg n) (reg val))
      (goto (reg continue))
    base-case
      (assign val (const 1))
      (goto (reg continue))
    fact-done
      (perform (op print) (reg val))
      (perform (op print-stack-statistics))
      (goto (label fact-loop))))

(#%provide run-fact)
(define (run-fact)
  (let
    ((fact-machine
      (make-machine
        (list
          (list '- -)
          (list '* *)
          (list '= =)
          (list 'read read)
          (list 'print (lambda (x) (display x) (newline))))
        fact-controller)))
    (fact-machine 'trace-on)
    (start fact-machine)))

(#%provide fact-stack-pushes)
(define (fact-stack-pushes n)
  (* (- n 1) 2))

#| 5.15 |#

#| (inst-count 0) |#

#|      ((eq? message 'reset-inst-count) |#
#|       (display (list inst-count 'instructions 'executed)) |#
#|       (newline) |#
#|       (set! inst-count 0)) |#

#| 5.16 |#

#| (trace? false)) |#

#|          (if trace? |#
#|            (begin |#
#|              (display (instruction-text (car insts))) |#
#|              (newline))) |#

#|      ((eq? message 'trace-on) (set! trace? true)) |#
#|      ((eq? message 'trace-off) (set! trace? false)) |#

#| 5.17 |#

#| (define (make-instruction text) |#
#|   (list text false '())) |#

#| (define (instruction-text inst) |#
#|   (car inst)) |#

#| (define (instruction-label inst) |#
#|   (cadr inst)) |#

#| (define (instruction-execution-proc inst) |#
#|   (caddr inst)) |#

#| (define (set-instruction-label! inst label) |#
#|   (set-cdr! inst (cons label (cddr inst)))) |#

#| (define (set-instruction-execution-proc! inst proc) |#
#|   (set-cdr! (cdr inst) (list proc))) |#

#| (if (instruction-label (car insts)) |#
#|   (begin |#
#|     (display (instruction-label (car insts))) |#
#|     (newline))) |#

#| (begin |#
  #| (if (not (null? insts)) |#
    #| (set-instruction-label! (car insts) next-inst)) |#

#| 5.18 |#

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

(#%provide basic-ops)
(define basic-ops
  (list
    (list 'null? null?)
    (list 'pair? pair?)
    (list 'car car)
    (list 'cdr cdr)
    (list 'cdr cdr)
    (list 'cons cons)
    (list 'set-cdr! set-cdr!)
    (list '+ +)
    (list 'not not)
    (list 'read read)))

(#%provide count-leaves-controller)
(define count-leaves-controller
  '(controller
       (assign continue (label count-leaves-done))
       (assign tree (op read))
     count-leaves
       (test (op null?) (reg tree))
       (branch (label if-null))
       (assign t (op pair?) (reg tree))
       (test (op not) (reg t))
       (branch (label if-leaf))
       (save continue)
       (save tree)
       (assign continue (label after-car-tree))
       (assign tree (op car) (reg tree))
       (goto (label count-leaves))
     after-car-tree
       (restore tree)
       (save tree)
       (save val)
       (assign tree (op cdr) (reg tree))
       (assign continue (label after-cdr-tree))
       (goto (label count-leaves))
     after-cdr-tree
       (assign t (reg val))
       (restore val)
       (restore tree)
       (restore continue)
       (assign val (op +) (reg t) (reg val))
       (goto (reg continue))
     if-null
       (assign val (const 0))
       (goto (reg continue))
     if-leaf
       (assign val (const 1))
       (goto (reg continue))
     count-leaves-done))

(#%provide count-leaves-iter-controller)
(define count-leaves-iter-controller
  '(controller
       (assign continue (label count-leaves-done))
       (assign tree (op read))
       (assign n (const 0))
     count-iter
       (test (op null?) (reg tree))
       (branch (label if-null))
       (assign t (op pair?) (reg tree))
       (test (op not) (reg t))
       (branch (label if-leaf))
       (save tree)
       (save continue)
       (assign tree (op car) (reg tree))
       (assign continue (label after-car-tree))
       (goto (label count-iter))
     after-car-tree
       (restore continue)
       (restore tree)
       (assign tree (op cdr) (reg tree))
       (goto (label count-iter))
     if-null
       (goto (reg continue))
     if-leaf
       (assign n (op +) (reg n) (const 1))
       (goto (reg continue))
     count-leaves-done))

(#%provide append-controller)
(define append-controller
  '(controller
      (assign x (op read))
      (assign y (op read))
      (assign continue (label append-done))
    append
      (test (op null?) (reg x))
      (branch (label if-null))
      (save x)
      (save continue)
      (assign x (op cdr) (reg x))
      (assign continue (label after-append))
      (goto (label append))
    after-append
      (restore continue)
      (restore x)
      (assign x (op car) (reg x))
      (assign val (op cons) (reg x) (reg val))
      (goto (reg continue))
    if-null
      (assign val (reg y))
      (goto (reg continue))
    append-done))

(#%provide append!-controller)
(define append!-controller
  '(controller
      (assign x (op read))
      (assign y (op read))
      (assign head (reg x))
      (assign rest (op cdr) (reg x))
    last-pair
      (test (op null?) (reg rest))
      (branch (label found-last-pair))
      (assign head (reg rest))
      (assign rest (op cdr) (reg rest))
      (goto (label last-pair))
    found-last-pair
      (perform (op set-cdr!) (reg head) (reg y))))
