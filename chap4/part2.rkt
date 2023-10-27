#lang sicp
(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; Chapter 4
;; Metalinguistic Abstraction

;; 4.2
;; Variations on a Scheme -- Lazy Evaluation

;; Normal Order and Applicative Order

(#%provide eval)
(define (eval exp env)
  (cond
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (list-rep (text-of-quotation exp) env))
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-definition exp env))
    ((if? exp) (eval-if exp env))
    ((lambda? exp)
     (make-procedure
       (lambda-parameters exp)
       (lambda-body exp)
       env))
    ((begin? exp)
     (eval-sequence (begin-actions exp) env))
    ((cond? exp) (eval (cond->if exp) env))
    ((let? exp) (eval (let->combination exp) env))
    ((application? exp)
     (apply-
       (actual-value (operator exp) env)
       (operands exp)
       env))
    (else
      (error "Unknown expression type -- EVAL" exp))))

(define (apply- procedure arguments env)
  (cond
    ((primitive-procedure? procedure)
     (apply-primitive-procedure
       procedure
       (list-of-arg-values arguments env)))
    ((compound-procedure? procedure)
     (eval-sequence
       (procedure-body procedure)
       (extend-environment
         (procedure-parameters procedure)
         (list-of-delayed-args arguments env)
         (procedure-environment procedure))))
    (else
      (error "Unknown procedure type -- APPLY" procedure))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond
    ((last-exp? exps) (eval (first-exp exps) env))
    (else
      (eval (first-exp exps) env)
      (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value!
    (assignment-variable exp)
    (eval (assignment-value exp) env)
    env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

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

(#%provide definition-value)
(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda
      (cdadr exp)
      (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(#%provide lambda-body)
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

(#%provide cond-predicate)
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
          (if (cond-extended? first)
            (list (cond-recipient first) (cond-predicate first))
            (sequence->exp (cond-actions first)))
          (expand-clauses rest))))))

(#%provide cond-recipient)
(define (cond-recipient clause) (caddr clause))

(#%provide cond-extended?)
(define (cond-extended? clause)
  (tagged-list? (cdr clause) '=>))

(#%provide let?)
(define (let? exp) (tagged-list? exp 'let))

(#%provide binding-var)
(define (binding-var binding)
  (car binding))

(#%provide binding-exp)
(define (binding-exp binding)
  (cadr binding))

(#%provide let-bindings)
(define (let-bindings exp) (cadr exp))

(#%provide let-body)
(define (let-body exp) (cddr exp))

(#%provide let->combination)
(define (let->combination exp)
  (if (null? (let-bindings exp))
    (if (null? (cdr (let-body exp)))
      (car (let-body exp))
      (cons 'begin (let-body exp)))
    (list
      (cons 'lambda
        (cons
          (map binding-var (let-bindings exp))
          (let-body exp)))
      (map binding-exp (let-bindings exp)))))

(#%provide make-let)
(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(#%provide procedure-parameters)
(define (procedure-parameters p) (cadr p))

(#%provide procedure-body)
(define (procedure-body p) (caddr p))

(#%provide procedure-environment)
(define (procedure-environment p) (cadddr p))

(#%provide enclosing-environment)
(define (enclosing-environment env) (cdr env))

(#%provide first-frame)
(define (first-frame env) (car env))

(#%provide the-empty-environment)
(define the-empty-environment '())

(#%provide make-frame)
(define (make-frame variables values)
  (cons variables values))

(#%provide frame-variables)
(define (frame-variables frame) (car frame))

(#%provide frame-values)
(define (frame-values frame) (cdr frame))

(#%provide add-binding-to-frame!)
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(#%provide extend-environment)
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(#%provide lookup-variable-value)
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

(#%provide set-variable-value!)
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

(#%provide define-variable!)
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
    #| (list 'car car) |#
    #| (list 'cdr cdr) |#
    #| (list 'cons cons) |#
    #| (list 'list list) |#
    (list 'null? null?)
    (list '+ +)
    (list '- -)
    (list '* *)
    (list '/ /)
    (list '> >)
    (list '< <)
    (list '= =)
    (list '<= <=)
    (list '>= >=)
    (list 'newline newline)
    (list 'display display)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map
    (lambda (proc) (list 'primitive (cadr proc)))
    primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply
    (primitive-implementation proc) args))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(#%provide driver-loop)
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

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

(#%provide the-global-environment)
(define the-global-environment (setup-environment))
(define (unless-bad condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

#| 4.25 |#

(#%provide factorial-bad)
(define (factorial-bad n)
  (unless-bad (= n 1)
    (* n (factorial-bad (- n 1)))
    1))

#| 4.26 |#

(define (unless? exp) (tagged-list? exp 'unless))

(define (unless-predicate exp) (cadr exp))

(define (unless-usual exp) (caddr exp))

(define (unless-exceptional exp) (cadddr exp))

(#%provide unless->if)
(define (unless->if exp)
  (make-if
    (unless-predicate exp)
    (unless-exceptional exp)
    (unless-usual exp)))

;; An Interpreter with Lazy Evaluation

(#%provide actual-value)
(define (actual-value exp env)
  (force-it (eval exp env)))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
    '()
    (cons
      (actual-value (first-operand exps) env)
      (list-of-arg-values (rest-operands exps) env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
    '()
    (cons
      (delay-it (first-operand exps) env)
      (list-of-delayed-args
        (rest-operands exps)
        env))))

(define (force-it obj)
  #| (if (thunk? obj) |#
  #|   (actual-value |#
  #|     (thunk-exp obj) |#
  #|     (thunk-env obj)) |#
  #|   obj)) |#
  (cond
    ((thunk? obj)
     (let
       ((result
        (actual-value
          (thunk-exp obj)
          (thunk-env obj))))
       (set-car! obj 'evaluated-thunk)
       (set-car! (cdr obj) result)
       (set-cdr! (cdr obj) '())
       result))
    ((evaluated-thunk? obj)
     (thunk-value obj))
    (else obj)))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

#| 4.27 |#

#| (define count 0) |#

#| (define (id x) |#
#|   (set! count (+ count 1)) |#
#|   x) |#

#| (define w (id (id 10))) |#

#| count |#
;; 1

#| w |#
;; 10

#| count |#
;; 2

#| 4.28 |#

(define (max a b)
  (if (< a b) b a))

(define (plus-or-max x a b)
  (if (= x 0) + max) a b)

#| 4.29 |#

(#%provide repetitive)
(define repetitive
  '(define (list-of-30 val)
     (define (loop n)
       (if (= n 0)
         '()
         (cons val (loop (- n 1)))))
     (loop 30)))

(#%provide expensive)
(define expensive
  '(define (fib-slow n)
    (cond
      ((= n 0) 0)
      ((= n 1) 1)
      (else
        (+
          (fib-slow (- n 2))
          (fib-slow (- n 1)))))))

(#%provide memo-test)
(define memo-test '(list-of-30 (fib-slow 30)))

;; with memo:

#| (define count 0) |#

#| (define (id x) |#
#|   (set! count (+ count 1)) |#
#|   x) |#

#| (define (square x) (* x x)) |#

#| (square (id 10)) |#
;; 100

#| count |#
;; 1

;; without memo:

#| (define count 0) |#

#| (define (id x) |#
#|   (set! count (+ count 1)) |#
#|   x) |#

#| (define (square x) (* x x)) |#

#| (square (id 10)) |#
;; 100

#| count |#
;; 2

#| 4.30 |#

(#%provide for-each-def)
(define for-each-def
  '(define (for-each proc items)
     (if (null? items)
       'done
       (begin
         (proc (car items))
         (for-each proc (cdr items))))))

(#%provide for-each-example)
(define for-each-example
  '(for-each
     (lambda (x)
       (display x)
       (newline))
     (list 57 321 88)))

;; this works because the side-effecting procedures
;; are in operator position

(#%provide p1-def)
(define p1-def
  '(define (p1 x)
     (set! x (cons x '(2)))
     x))

(#%provide p2-def)
(define p2-def
  '(define (p2 x)
     (define (p e)
       e
       x)
     (p (set! x (cons x '(2))))))

;; (p1 1)
;; (1 2)

;; (p2 1)
;; 1

;; Streams as Lazy Lists

(#%provide list-defs)
(define list-defs
  '(begin
    (define (cons x y) (lambda (m) (m x y)))
    (define (car z) (z (lambda (p q) p)))
    (define (cdr z) (z (lambda (p q) q)))
    (define (list-ref items n)
      (if (= n 0)
       (car items)
       (list-ref (cdr items) (- n 1))))
    (define (map proc items)
      (if (null? items)
        '()
        (cons (proc (car items)) (map proc (cdr items)))))
    (define (scale-list items factor)
     (map (lambda (x) (* x factor)) items))
    (define (add-lists list1 list2)
      (cond
        ((null? list1) list2)
        ((null? list2) list1)
        (else
          (cons
            (+ (car list1) (car list2))
            (add-lists (cdr list1) (cdr list2))))))
    (define ones (cons 1 ones))
    (define integers (cons 1 (add-lists ones integers)))
    (define (integral integrand initial-value dt)
      (define int
        (cons
          initial-value
          (add-lists (scale-list integrand dt) int)))
      int)))

#| 4.33 |#

(#%provide list-test)
(define list-test '(car '(a b c)))

(define (list-rep exp env)
  (if (pair? exp)
    (make-procedure
      '(m)
      '((m x y))
      (extend-environment
        '(x y)
        (list
          (list-rep (car exp) env)
          (list-rep (cdr exp) env))
        env))
    exp))
