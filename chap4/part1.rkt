#lang sicp
(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; Chapter 4
;; Metalinguistic Abstraction

;; 4.1
;; The Metacircular Evaluator

;; The Core of the Evaluator

(#%provide eval)
(define (eval exp env)
  (cond
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
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
    ((and? exp) (eval-and (and-conjuncts exp) 'true env))
    ((or? exp) (eval-or (or-disjuncts exp) env))
    ((let? exp) (eval (let->combination exp) env))
    ((let*? exp) (eval (let*->nested-lets exp) env))
    ((application? exp)
     (apply-
       (eval (operator exp) env)
       (list-of-values (operands exp) env)))
    (else
      (error "Unknown expression type -- EVAL" exp))))

(define (apply- procedure arguments)
  (cond
    ((primitive-procedure? procedure)
     (apply-primitive-procedure procedure arguments))
    ((compound-procedure? procedure)
     (eval-sequence
       (procedure-body procedure)
       (extend-environment
         (procedure-parameters procedure)
         arguments
         (procedure-environment procedure))))
    (else
      (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons
      (eval (first-operand exps) env)
      (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
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

#| 4.1 |#

(define (list-of-values-left exps env)
  (if (no-operands? exps)
    '()
    (let ((first-value (eval (first-operand exps) env)))
      (let ((rest-values (list-of-values-left (rest-operands exps) env)))
        (cons first-value rest-values)))))

(define (list-of-values-right exps env)
  (if (no-operands? exps)
    '()
    (let ((rest-values (list-of-values-right (rest-operands exps) env)))
      (let ((first-value (eval (first-operand exps) env)))
        (cons first-value rest-values)))))

;; Representing Expressions

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

#| 4.2 |#

(#%provide application?-)
(define (application?- exp) (tagged-list? exp 'call))

(#%provide operator-)
(define (operator- exp) (cadr exp))

(#%provide operands-)
(define (operands- exp) (cddr exp))

#| 4.4 |#

(define (and? exp) (tagged-list? exp 'and))

(define (and-conjuncts exp) (cdr exp))

(define (eval-and conjuncts last-value env)
  (if (null? conjuncts)
    last-value
    (let
      ((value (eval (car conjuncts) env)))
      (if (true? value)
        (eval-and (cdr conjuncts) value env)
        'false))))

(define (or? exp) (tagged-list? exp 'or))

(define (or-disjuncts exp) (cdr exp))

(define (eval-or disjuncts env)
  (if (null? disjuncts)
    'false
    (let
      ((value (eval (car disjuncts) env)))
      (if (true? value)
        value
        (eval-or (cdr disjuncts) env)))))

#| 4.5 |#

(#%provide cond-recipient)
(define (cond-recipient clause) (caddr clause))

(#%provide cond-extended?)
(define (cond-extended? clause)
  (tagged-list? (cdr clause) '=>))

(#%provide expand-clauses-)
(define (expand-clauses- clauses)
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
          (expand-clauses- rest))))))

#| 4.6 |#

(#%provide let?)
(define (let? exp) (tagged-list? exp 'let))

(#%provide binding-var)
(define (binding-var binding)
  (car binding))

(#%provide binding-exp)
(define (binding-exp binding)
  (cadr binding))

(#%provide let-bindings)
(define (let-bindings exp)
  (if (named-let? exp)
    (caddr exp)
    (cadr exp)))

(#%provide let-body)
(define (let-body exp)
  (if (named-let? exp)
    (cdddr exp)
    (cddr exp)))

#| (#%provide let->combination) |#
#| (define (let->combination exp) |#
#|   (if (null? (let-bindings exp)) |#
#|     (if (null? (cdr (let-body exp))) |#
#|       (car (let-body exp)) |#
#|       (cons 'begin (let-body exp))) |#
#|     (list |#
#|       (cons 'lambda |#
#|         (cons |#
#|           (map binding-var (let-bindings exp)) |#
#|           (let-body exp))) |#
#|       (map binding-exp (let-bindings exp))))) |#

(#%provide let->combination)
(define (let->combination exp)
  (if (null? (let-bindings exp))
    (if (null? (cdr (let-body-extended exp)))
      (car (let-body-extended exp))
      (cons 'begin (let-body-extended exp)))
    (list
      (cons
        'lambda
        (cons
          (map binding-var (let-bindings exp))
          (let-body-extended exp)))
      (map binding-exp (let-bindings exp)))))

(define (let-body-extended exp)
  (if (named-let? exp)
    (let
      ((name-vars
       (cons
         (let-name exp)
         (map binding-var (let-bindings exp)))))
      (list
        (cons 'define (cons name-vars (let-body exp)))
        name-vars))
    (let-body exp)))

#| 4.7 |#

(#%provide let*?)
(define (let*? exp) (tagged-list? exp 'let*))

(#%provide make-let)
(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(#%provide let*->nested-lets)
(define (let*->nested-lets exp)
  (expand-lets
    (let-bindings exp)
    (let-body exp)))

(#%provide expand-lets)
(define (expand-lets bindings body)
  (if (null? bindings)
    (if (null? (cdr body)) (car body) (cons 'begin body))
    (make-let
      (list (car bindings))
      (list (expand-lets (cdr bindings) body)))))

#| 4.8 |#

(#%provide named-let?)
(define (named-let? exp)
  (and
    (tagged-list? exp 'let)
    (symbol? (cadr exp))))

(#%provide let-name)
(define (let-name exp)
  (cadr exp))

(#%provide let->combination-)
(define (let->combination- exp)
  (let ((variables (map binding-var (let-bindings exp))))
    (list
      (cons 'lambda
        (cons variables
          (if (named-let? exp)
            (let ((name-vars (cons (let-name exp) variables)))
              (list
                (cons 'define (cons name-vars (let-body exp)))
                name-vars))
            (let-body exp))))
      (map binding-exp (let-bindings exp)))))

#| 4.9 |#

(#%provide make-named-let)
(define (make-named-let name bindings body)
  (cons 'let (cons name (cons bindings body))))

(#%provide while?)
(define (while? exp) (tagged-list? exp 'do))

(#%provide while-condition)
(define (while-condition exp) (cadr exp))

(#%provide while-body)
(define (while-body exp) (cddr exp))

(#%provide while->named-let)
(define (while->named-let exp)
  (make-named-let
    'loop
    '()
    (list
      (make-if
        (while-condition exp)
        (cons 'begin (append (while-body exp) (list (list 'loop))))
        ''done))))

;; Evaluator Data Structures

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

#| 4.11 |#

(#%provide make-frame-)
(define (make-frame- variables values)
  (define (loop variables values)
    (if (null? variables)
      '()
      (cons
        (cons (car variables) (car values))
        (loop (cdr variables) (cdr values)))))
  (cons 'frame (loop variables values)))

(#%provide frame-variables-)
(define (frame-variables- frame) (map car (cdr frame)))

(#%provide frame-values-)
(define (frame-values- frame) (map cdr (cdr frame)))

(#%provide add-binding-to-frame!-)
(define (add-binding-to-frame!- var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

(#%provide extend-environment-)
(define (extend-environment- vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame- vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(#%provide lookup-variable-value-)
(define (lookup-variable-value- var env)
  (define (env-loop env)
    (define (scan bindings)
      (cond
        ((null? bindings)
         (env-loop (enclosing-environment env)))
        ((eq? var (car (car bindings)))
         (cdr (car bindings)))
        (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (scan (cdr (first-frame env)))))
  (env-loop env))

(#%provide set-variable-value!-)
(define (set-variable-value!- var val env)
  (define (env-loop env)
    (define (scan bindings)
      (cond
        ((null? bindings)
         (env-loop (enclosing-environment env)))
        ((eq? var (car (car bindings)))
         (set-car! bindings (cons (car (car bindings)) val)))
        (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (scan (cdr (first-frame env)))))
  (env-loop env))

(#%provide define-variable!-)
(define (define-variable!- var val env)
  (let ((frame (first-frame env)))
    (define (scan bindings)
      (cond
        ((null? bindings)
         (add-binding-to-frame!- var val frame))
        ((eq? var (car (car bindings)))
         (set-car! bindings (cons (car (car bindings)) val)))
        (else (scan (cdr bindings)))))
    (scan (cdr frame))))

#| 4.12 |#

(define (get-value binding)
  (cdr binding))

(define (set-value! val)
  (lambda (binding)
    (set-cdr! binding val)))

(define (traverse-environment-with proc var env)
  (define (env-loop env)
    (define (scan bindings)
      (cond
        ((null? bindings)
         (env-loop (enclosing-environment env)))
        ((eq? var (car (car bindings)))
         (proc (car bindings)))
        (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (scan (cdr (first-frame env)))))
  (env-loop env))

(#%provide lookup-variable-value--)
(define (lookup-variable-value-- var env)
  (traverse-environment-with get-value var env))

(#%provide set-variable-value!--)
(define (set-variable-value!-- var val env)
  (traverse-environment-with (set-value! val) var env))

;; Running the Evaluator as a Program

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

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(#%provide driver-loop)
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
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

#| 4.14 |#

;; This evaluator's representation of procedures is different
;; from that of the underlying Scheme. Defining map as a
;; primitive won't work because the underlying map expects
;; an underlying procedure object as the second argument

;; Data as Programs

#| 4.15 |#

;; Internal Definitions

#| 4.16 |#
#| 4.17 |#
#| 4.18 |#
#| 4.19 |#
#| 4.20 |#
#| 4.21 |#

;; Separating Syntactic Analysis from Execution

#| 4.22 |#
#| 4.23 |#
#| 4.24 |#
