#lang sicp
(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; Chapter 4
;; Metalinguistic Abstraction

;; 4.3
;; Variations on a Scheme -- Nondeterministic Computing

;; Amb and Search

(#%provide prime?-def)
(define prime?-def
  '(define (prime? n)
     (define (next n)
       (if (= n 2) 3 (+ n 2)))
     (define (square x) (* x x))
     (define (divides? a b)
       (= (remainder b a) 0))
     (define (find-divisor n test-divisor)
       (cond
         ((> (square test-divisor) n) n)
         ((divides? test-divisor n) test-divisor)
         (else (find-divisor n (next test-divisor)))))
     (define (smallest-divisor n)
       (find-divisor n 2))
     (= n (smallest-divisor n))))

(#%provide require-def)
(define require-def
  '(define (require p)
    (if (not p) (amb))))

(#%provide prime-sum-pair-def)
(define prime-sum-pair-def
  '(define (prime-sum-pair list1 list2)
     (let
       ((a (an-element-of list1))
        (b (an-element-of list2)))
       (require (prime? (+ a b)))
       (list a b))))

(#%provide an-element-of-def)
(define an-element-of-def
  '(define (an-element-of items)
    (require (not (null? items)))
    (amb (car items) (an-element-of (cdr items)))))

(#%provide an-integer-starting-from-def)
(define an-integer-starting-from-def
  '(define (an-integer-starting-from n)
    (amb n (an-integer-starting-from (+ n 1)))))


#| 4.35 |#

(#%provide an-integer-between-def)
(define an-integer-between-def
  '(define (an-integer-between low high)
     (require (<= low high))
     (amb low (an-integer-between (+ low 1) high))))

(#%provide a-pythagorean-triple-between-def)
(define a-pythagorean-triple-between-def
  '(define (a-pythagorean-triple-between low high)
    (let ((i (an-integer-between low high)))
      (let ((j (an-integer-between i high)))
        (let ((k (an-integer-between j high)))
          (require (= (+ (* i i) (* j j)) (* k k)))
          (list i j k))))))

#| 4.36 |#

(#%provide a-pythagorean-triple-bad-def)
(define a-pythagorean-triple-bad-def
  '(define (a-pythagorean-triple-bad)
    (let ((i (an-integer-starting-from 1)))
      (let ((j (an-integer-starting-from i)))
        (let ((k (an-integer-starting-from j)))
          (require (= (+ (* i i) (* j j)) (* k k)))
          (list i j k))))))

;; there is not a valid k for every value of i j
;; the procedure would get stuck trying new values
;; of k forever

(#%provide a-pythagorean-triple-def)
(define a-pythagorean-triple-def
  '(define (a-pythagorean-triple)
    (let ((k (an-integer-starting-from 1)))
      (let ((i (an-integer-between 1 k)))
        (let ((j (an-integer-between i k)))
          (require (= (+ (* i i) (* j j)) (* k k)))
          (list i j k))))))

#| 4.37 |#

(#%provide a-pythagorean-triple-between-fast-def)
(define a-pythagorean-triple-between-fast-def
  '(define (a-pythagorean-triple-between-fast low high)
    (let
      ((i (an-integer-between low high))
       (hsq (* high high)))
      (let ((j (an-integer-between i high)))
        (let ((ksq (+ (* i i) (* j j))))
          (require (>= hsq ksq))
          (let ((k (sqrt ksq)))
            (require (integer? k))
            (list i j k)))))))

;; this version explores fewer possibilities

;; Examples of Nondeterministic Programs

(#%provide distinct?-def)
(define distinct?-def
  '(define (distinct? items)
     (cond
       ((null? items) true)
       ((null? (cdr items)) true)
       ((member (car items) (cdr items)) false)
       (else (distinct? (cdr items))))))

(#%provide multiple-dwelling-def)
(define multiple-dwelling-def
  '(define (multiple-dwelling)
    (let
      ((baker (amb 1 2 3 4 5))
       (cooper (amb 1 2 3 4 5))
       (fletcher (amb 1 2 3 4 5))
       (miller (amb 1 2 3 4 5))
       (smith (amb 1 2 3 4 5)))
      (require
        (distinct? (list baker cooper fletcher miller smith)))
      (require (not (= baker 5)))
      (require (not (= cooper 1)))
      (require (not (= fletcher 5)))
      (require (not (= fletcher 1)))
      (require (> miller cooper))
      (require (not (= (abs (- smith fletcher)) 1)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (list
        (list 'baker baker)
        (list 'cooper cooper)
        (list 'fletcher fletcher)
        (list 'miller miller)
        (list 'smith smith)))))

#| 4.38 |#

(#%provide multiple-dwelling-mod-def)
(define multiple-dwelling-mod-def
  '(define (multiple-dwelling-mod)
    (let
      ((baker (amb 1 2 3 4 5))
       (cooper (amb 1 2 3 4 5))
       (fletcher (amb 1 2 3 4 5))
       (miller (amb 1 2 3 4 5))
       (smith (amb 1 2 3 4 5)))
      (require
        (distinct? (list baker cooper fletcher miller smith)))
      (require (not (= baker 5)))
      (require (not (= cooper 1)))
      (require (not (= fletcher 5)))
      (require (not (= fletcher 1)))
      (require (> miller cooper))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (list
        (list 'baker baker)
        (list 'cooper cooper)
        (list 'fletcher fletcher)
        (list 'miller miller)
        (list 'smith smith)))))

#| 4.39 |#

(#%provide multiple-dwelling-reorder-def)
(define multiple-dwelling-reorder-def
  '(define (multiple-dwelling-reorder)
    (let
      ((baker (amb 1 2 3 4 5))
       (cooper (amb 1 2 3 4 5))
       (fletcher (amb 1 2 3 4 5))
       (miller (amb 1 2 3 4 5))
       (smith (amb 1 2 3 4 5)))
      (require (> miller cooper))
      (require (not (= (abs (- smith fletcher)) 1)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (require
        (distinct? (list baker cooper fletcher miller smith)))
      (require (not (= baker 5)))
      (require (not (= cooper 1)))
      (require (not (= fletcher 5)))
      (require (not (= fletcher 1)))
      (list
        (list 'baker baker)
        (list 'cooper cooper)
        (list 'fletcher fletcher)
        (list 'miller miller)
        (list 'smith smith)))))

#| 4.40 |#

(#%provide multiple-dwelling-quick-def)
(define multiple-dwelling-quick-def
  '(define (multiple-dwelling-quick)
    (let
      ((baker (amb 1 2 3 4 5)))
      (require (not (= baker 5)))
      (let ((cooper (amb 1 2 3 4 5)))
        (require (not (= cooper 1)))
        (require (not (= cooper baker)))
        (let ((fletcher (amb 1 2 3 4 5)))
          (require (not (= (abs (- fletcher cooper)) 1)))
          (require (not (= fletcher 1)))
          (require (not (= fletcher 5)))
          (require (not (= fletcher baker)))
          (require (not (= fletcher cooper)))
          (let ((miller (amb 1 2 3 4 5)))
            (require (> miller cooper))
            (require (not (= miller baker)))
            (require (not (= miller cooper)))
            (require (not (= miller fletcher)))
            (let ((smith (amb 1 2 3 4 5)))
              (require (not (= (abs (- smith fletcher)) 1)))
              (require (not (= smith baker)))
              (require (not (= smith cooper)))
              (require (not (= smith fletcher)))
              (require (not (= smith miller)))
              (list
                (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))))))

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
        (with-next coopers (lambda () (bakerfunc bakers))
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
                (require (lambda (m) (> m cooper)) millerfunc
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

(#%provide liars-def)
(define liars-def
  '(define (liars)
    (define (one-lie x y)
      (amb
        (begin (require x) (require (not y)))
        (begin (require (not x)) (require y))))
    (let
      ((betty (amb 1 2 3 4 5))
       (ethel (amb 1 2 3 4 5))
       (joan (amb 1 2 3 4 5))
       (kitty (amb 1 2 3 4 5))
       (mary (amb 1 2 3 4 5)))
      (require (distinct? (list betty ethel joan kitty mary)))
      (one-lie (= kitty 2) (= betty 3))
      (one-lie (= ethel 1) (= joan 2))
      (one-lie (= joan 3) (= ethel 5))
      (one-lie (= kitty 2) (= mary 4))
      (one-lie (= mary 4) (= betty 1))
      (list
        (list 'betty betty)
        (list 'ethel ethel)
        (list 'joan joan)
        (list 'kitty kitty)
        (list 'mary mary)))))

#| 4.43 |#

(#%provide map-def)
(define map-def
  '(define (map f xs)
     (if (null? xs)
       '()
       (cons (f (car xs)) (map f (cdr xs))))))

(#%provide yachts-def)
(define yachts-def
  '(define (yachts)
    (define (daughters) (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa))
    (define (yachts-) (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa))
    (define (name man) (car man))
    (define (daughter man) (cadr man))
    (define (father men girl)
      (cond
        ((null? men) false)
        ((eq? (daughter (car men)) girl) (car men))
        (else (father (cdr men) girl))))
    (define (yacht man) (caddr man))
    (let
      ((mr-moore (list 'mr-moore (daughters) (yachts-)))
       (colonel-downing (list 'colonel-downing (daughters) (yachts-)))
       (mr-hall (list 'mr-hall (daughters) (yachts-)))
       (sir-barnacle-hood (list 'sir-barnacle-hood (daughters) (yachts-)))
       (dr-parker (list 'dr-parker (daughters) (yachts-))))
      (let
        ((men
         (list
           mr-moore
           colonel-downing
           mr-hall
           sir-barnacle-hood
           dr-parker)))
        (require (eq? (daughter mr-moore) 'mary-ann))
        (require (eq? (yacht sir-barnacle-hood) 'gabrielle))
        (require (eq? (yacht mr-moore) 'lorna))
        (require (eq? (yacht mr-hall) 'rosalind))
        (require (eq? (yacht colonel-downing) 'melissa))
        (require (eq? (daughter sir-barnacle-hood) 'melissa))
        (require (distinct? (map daughter men)))
        (require (distinct? (map yacht men)))
        (require
          (eq? (yacht (father men 'gabrielle)) (daughter dr-parker)))
        (name (father men 'lorna))))))

#| 4.44 |#

(#%provide show-row-def)
(define show-row-def
  '(define (show-row col board-size)
    (if (= board-size 0)
      (newline)
      (begin
        (if (= col 1)
          (display "  Q")
          (display "  _"))
        (show-row (- col 1) (- board-size 1))))))

(#%provide show-board-def)
(define show-board-def
  '(define (show-board board)
    (let ((board-size (length board)))
      (define (show-rows rows)
        (if (null? rows)
          (newline)
          (begin
            (show-row (cadr (car rows)) board-size)
            (show-rows (cdr rows)))))
      (newline)
      (show-rows board))))

(#%provide queens-amb-def)
(define queens-amb-def
  '(define (queens-amb board-size)
    (define empty-board '())
    (define (adjoin-position pos board) (cons pos board))
    (define (make-pos r c) (list r c))
    (define (row pos) (car pos))
    (define (col pos) (cadr pos))
    (define (zig pos) (+ (row pos) (col pos)))
    (define (zag pos) (- (row pos) (col pos)))
    (define (some-col) (an-integer-between 1 board-size))
    (define (pick-free proj pos board)
      (require (not (memq (proj pos) (map proj board)))))
    (define (queen-rows k)
      (if (= k 0)
        empty-board
        (let
          ((board (queen-rows (- k 1)))
           (new-pos (make-pos k (some-col))))
          (pick-free row new-pos board)
          (pick-free col new-pos board)
          (pick-free zig new-pos board)
          (pick-free zag new-pos board)
          (adjoin-position new-pos board))))
    (let ((result (reverse (queen-rows board-size))))
      (show-board result)
      result)))

(#%provide parse-def)
(define parse-def
  '(define (parse input)
    (define nouns '(nouns student professor cat class))
    (define verbs '(verb studies lectures eats sleeps))
    (define articles '(article the a))
    (define prepositions '(prep for to in by with))
    (define *unparsed* '())
    (define (parse-sentence)
      (list
        'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))
    (define (parse-noun-phrase)
      (define (maybe-extend noun-phrase)
        (amb
          noun-phrase
          (maybe-extend
            (list
              'noun-phrase
              noun-phrase
              (parse-prepositional-phrase)))))
      (maybe-extend (parse-simple-noun-phrase)))
    (define (parse-simple-noun-phrase)
      (list
        'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))
    (define (parse-prepositional-phrase)
      (list
        'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))
    (define (parse-verb-phrase)
      (define (maybe-extend verb-phrase)
        (amb
          verb-phrase
          (maybe-extend
            (list
              'verb-phrase
              verb-phrase
              (parse-prepositional-phrase)))))
      (maybe-extend (parse-word verbs)))
    (define (parse-word word-list)
      (require (not (null? *unparsed*)))
      (require (memq (car *unparsed*) (cdr word-list)))
      (let ((found-word (car *unparsed*)))
        (set! *unparsed* (cdr *unparsed*))
        (list (car word-list) found-word)))
    (set! *unparsed* input)
    (let ((sent (parse-sentence)))
      (require (null? *unparsed*))
      sent)))

#| 4.45 |#

#| '(the professor lectures to the student in the class with the cat) |#

#| '(to the (student in the (class with the cat))) |#
#| '(to the ((student in the class) with the cat)) |#
#| '((to the student) in the (class with the cat)) |#
#| '((to the (student in the class)) with the cat) |#
#| '(((to the student) in the class) with the cat) |#

#| (the professor lectures to (the student in (the class with the cat))) |#

#| there is a class with a cat |#
#| and in the class is a student |#
#| the professor lectures to said student |#

#| '(sentence |#
#|    (simple-noun-phrase (article the) (noun professor)) |#
#|    (verb-phrase |#
#|      (verb lectures) |#
#|      (prep-phrase |#
#|        (prep to) |#
#|        (noun-phrase |#
#|          (simple-noun-phrase (article the) (noun student)) |#
#|          (prep-phrase |#
#|            (prep in) |#
#|            (noun-phrase |#
#|              (simple-noun-phrase (article the) (noun class)) |#
#|              (prep-phrase |#
#|                (prep with) |#
#|                (simple-noun-phrase (article the) (noun cat))))))))) |#

#| (the professor lectures to ((the student in the class) with the cat)) |#

#| there is a student who |#
#| 1. is in a class |#
#| 2. has a cat |#
#| the professor lectures to that student |#

#| (sentence |#
#|   (simple-noun-phrase (article the) (noun professor)) |#
#|   (verb-phrase |#
#|     (verb lectures) |#
#|     (prep-phrase |#
#|       (prep to) |#
#|       (noun-phrase |#
#|         (noun-phrase |#
#|           (simple-noun-phrase (article the) (noun student)) |#
#|           (prep-phrase |#
#|             (prep in) |#
#|             (simple-noun-phrase (article the) (noun class)))) |#
#|         (prep-phrase |#
#|           (prep with) |#
#|           (simple-noun-phrase (article the) (noun cat))))))) |#

#| ((the professor lectures to (the student in the class)) with the cat) |#

#| there is a student in a class |#
#| the professor and the cat lecture to that student |#

#| (sentence |#
#|   (simple-noun-phrase the professor) |#
#|   (verb-phrase |#
#|     (verb-phrase |#
#|       (verb lectures) |#
#|       (prep-phrase |#
#|         (prep to) |#
#|         (noun-phrase |#
#|           (simple-noun-phrase (article the) (noun student)) |#
#|           (prep-phrase |#
#|             (prep in) |#
#|             (simple-noun-phrase (article the) (noun class)))))) |#
#|     (prep-phrase |#
#|       (prep with) |#
#|       (simple-noun (article the) (noun cat))))) |#

#| ((the professor lectures to the student) in the (class with the cat)) |#

#| there is a class with a cat |#
#| the professor lectures to the student |#
#| this occurs in said class |#

#| (sentence |#
#|   (simple-noun-phrase the professor) |#
#|   (verb-phrase |#
#|     (verb-phrase |#
#|       (verb lectures) |#
#|       (prep-phrase |#
#|         (prep to) |#
#|         (simple-noun-phrase (article the) (noun student)))) |#
#|     (prep-phrase |#
#|       (prep in) |#
#|       (noun-phrase |#
#|         (simple-noun-phrase (article the) (noun class)) |#
#|         (prep-phrase |#
#|           (prep with) |#
#|           (simple-noun-phrase (article the) (noun cat))))))) |#

#| (((the professor lectures to the student) in the class) with the cat) |#

#| the professor and the cat lecture to the student |#
#| this occurs in the class |#

#| (sentence |#
#|   (simple-noun-phrase the professor) |#
#|   (verb-phrase |#
#|     (verb-phrase |#
#|       (verb-phrase |#
#|         (verb lectures) |#
#|         (prep-phrase |#
#|           (prep to) |#
#|           (simple-noun-phrase (article the) (noun student)))) |#
#|       (prep-phrase |#
#|         (prep in) |#
#|         (simple-noun-phrase (article the) (noun class)))) |#
#|     (prep-phrase |#
#|       (prep with) |#
#|       (simple-noun-phrase (article the) (noun cat))))) |#

#| 4.49 |#

(#%provide parse-gen-def)
(define parse-gen-def
  '(define (parse-gen)
    (define nouns '(nouns student professor cat class))
    (define verbs '(verb studies lectures eats sleeps))
    (define articles '(article the a))
    (define prepositions '(prep for to in by with))
    (define (parse-sentence)
      (list
        'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))
    (define (parse-noun-phrase)
      (define (maybe-extend noun-phrase)
        (ramb
          noun-phrase
          (maybe-extend
            (list
              'noun-phrase
              noun-phrase
              (parse-prepositional-phrase)))))
      (maybe-extend (parse-simple-noun-phrase)))
    (define (parse-simple-noun-phrase)
      (list
        'simple-noun-phrase
        (parse-word-gen articles)
        (parse-word-gen nouns)))
    (define (parse-prepositional-phrase)
      (list
        'prep-phrase
        (parse-word-gen prepositions)
        (parse-noun-phrase)))
    (define (parse-verb-phrase)
      (define (maybe-extend verb-phrase)
        (ramb
          verb-phrase
          (maybe-extend
            (list
              'verb-phrase
              verb-phrase
              (parse-prepositional-phrase)))))
      (maybe-extend (parse-word-gen verbs)))
    (define (one-of words)
      (if (null? words)
        (amb)
        (ramb
          (car words)
          (one-of (cdr words)))))
    (define (parse-word-gen word-list)
      (let ((found-word (one-of (cdr word-list))))
        (list (car word-list) found-word)))
    (parse-sentence)))

;; Implementing the Amb Evaluator

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (self-evaluating? exp)
  (cond
    ((number? exp) true)
    ((string? exp) true)
    (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

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
          (sequence->exp (cond-actions first))
          (expand-clauses rest))))))

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
    (cons
      (cons 'lambda
        (cons
          (map binding-var (let-bindings exp))
          (let-body exp)))
      (map binding-exp (let-bindings exp)))))

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

(#%provide ambeval)
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(#%provide analyze)
(define (analyze exp)
  (cond
    ((self-evaluating? exp)
     (analyze-self-evaluating exp))
    ((quoted? exp) (analyze-quoted exp))
    ((variable? exp) (analyze-variable exp))
    ((assignment? exp) (analyze-assignment exp))
    ((permanent-assignment? exp) (analyze-permanent-assignment exp))
    ((definition? exp) (analyze-definition exp))
    ((if? exp) (analyze-if exp))
    ((if-fail? exp) (analyze-if-fail exp))
    ((lambda? exp) (analyze-lambda exp))
    ((begin? exp) (analyze-sequence (begin-actions exp)))
    ((cond? exp) (analyze (cond->if exp)))
    ((let? exp) (analyze (let->combination exp)))
    ((amb? exp) (analyze-amb exp))
    ((ramb? exp) (analyze-ramb exp))
    ((require? exp) (analyze-require exp))
    ((application? exp) (analyze-application exp))
    (else
      (error "Unknown expression type -- ANALYZE" exp))))

(define (amb? exp) (tagged-list? exp 'amb))

(define (amb-choices exp) (cdr exp))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail) (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail) (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-lambda exp)
  (let
    ((vars (lambda-parameters exp))
     (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

(define (analyze-assignment exp)
  (let
    ((var (assignment-variable exp))
     (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc
        env
        (lambda (val fail2)
          (let ((old-value (lookup-variable-value var env)))
            (set-variable-value! var val env)
            (succeed
              'ok
              (lambda ()
                (set-variable-value! var old-value env)
                (fail2)))))
        fail))))

(define (analyze-definition exp)
  (let
    ((var (definition-variable exp))
     (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc
        env
        (lambda (val fail2)
          (define-variable! var val env)
          (succeed 'ok fail2))
        fail))))

(define (analyze-if exp)
  (let
    ((pproc (analyze (if-predicate exp)))
     (cproc (analyze (if-consequent exp)))
     (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc
        env
        (lambda (pred-value fail2)
          (if (true? pred-value)
            (cproc env succeed fail2)
            (aproc env succeed fail2)))
        fail))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env succeed fail)
      (proc1
        env
        (lambda (p1-value fail2)
          (proc2 env succeed fail2))
        fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop
        (sequentially first-proc (car rest-procs))
        (cdr rest-procs))))
  (let
    ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let
    ((fproc (analyze (operator exp)))
     (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc
        env
        (lambda (proc fail2)
          (get-args
            aprocs
            env
            (lambda (args fail3)
              (execute-application proc args succeed fail3))
            fail2))
        fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
    (succeed '() fail)
    ((car aprocs)
     env
     (lambda (arg fail2)
       (get-args
         (cdr aprocs)
         env
         (lambda (args fail3)
           (succeed (cons arg args) fail3))
         fail2))
     fail)))

(define (execute-application proc args succeed fail)
  (cond
    ((primitive-procedure? proc)
     (succeed (apply-primitive-procedure proc args) fail))
    ((compound-procedure? proc)
     ((procedure-body proc)
      (extend-environment
        (procedure-parameters proc)
        args
        (procedure-environment proc))
      succeed
      fail))
    (else
      (error
        "Unknown procedure type -- EXECUTE-APPLICATION"
        proc))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          ((car choices)
           env
           succeed
           (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

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
    (list 'cadr cadr)
    (list 'caddr caddr)
    (list 'cons cons)
    (list 'null? null?)
    (list 'list list)
    (list 'member member)
    (list 'memq memq)
    (list 'reverse reverse)
    (list 'length length)
    (list 'not not)
    (list 'eq? eq?)
    (list 'newline newline)
    (list 'display display)
    (list 'integer? integer?)
    (list 'remainder remainder)
    (list 'abs abs)
    (list 'sqrt sqrt)
    (list 'even? even?)
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

(#%provide the-global-environment)
(define the-global-environment (setup-environment))

(#%provide driver-loop)
(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
        (try-again)
        (begin
          (newline)
          (display ";;; Starting a new problem ")
          (ambeval
            input
            the-global-environment
            (lambda (val next-alternative)
              (announce-output output-prompt)
              (user-print val)
              (internal-loop next-alternative))
            (lambda ()
              (announce-output
                ";;; There are no more values of")
              (user-print input)
              (driver-loop)))))))
  (internal-loop
    (lambda ()
      (newline)
      (display ";;; There is no current problem")
      (driver-loop))))

(#%provide driver-loop-init)
(define (driver-loop-init init-exps)
  (cond
    ((null? init-exps) (driver-loop))
    (else
     (ambeval
       (car init-exps)
       the-global-environment
       (lambda (val next-alternative)
         (driver-loop-init (cdr init-exps)))
       (lambda ()
         (announce-output ";;; There are no values of")
         (display (car init-exps))
         (driver-loop-init (cdr init-exps)))))))

(#%provide amb-defs)
(define amb-defs
  (list
    prime?-def
    require-def
    prime-sum-pair-def
    an-element-of-def
    an-integer-starting-from-def
    an-integer-between-def
    a-pythagorean-triple-between-def
    a-pythagorean-triple-bad-def
    a-pythagorean-triple-def
    a-pythagorean-triple-between-fast-def
    distinct?-def
    multiple-dwelling-def
    multiple-dwelling-mod-def
    multiple-dwelling-reorder-def
    multiple-dwelling-quick-def
    liars-def
    map-def
    yachts-def
    show-row-def
    show-board-def
    queens-amb-def
    parse-def
    parse-gen-def
    ))

#| 4.50 |#

(define (ramb? exp) (tagged-list? exp 'ramb))

(define (ramb-choices exp) (cdr exp))

(#%provide remove)
(define (remove n xs)
  (cond
    ((null? xs) '())
    ((= n 0) (cdr xs))
    (else (cons (car xs) (remove (- n 1) (cdr xs))))))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          (let ((i (random (length choices))))
            ((list-ref choices i)
             env
             succeed
             (lambda () (try-next (remove i choices)))))))
      (try-next cprocs))))

#| 4.51 |#

(define (permanent-assignment? exp)
  (tagged-list? exp 'permanent-set!))

(define (analyze-permanent-assignment exp)
  (let
    ((var (assignment-variable exp))
     (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc
        env
        (lambda (val fail2)
          (set-variable-value! var val env)
          (succeed 'ok fail2))
        fail))))

(define permanent-set-example
  '(begin
     (define count 0)
     (let
       ((x (an-element-of '(a b c)))
        (y (an-element-of '(a b c))))
       (permanent-set! count (+ count 1))
       (require (not (eq? x y)))
      (list x y count))))

#| 4.52 |#

(define (if-fail? exp) (tagged-list? exp 'if-fail))

(define (if-fail-value exp) (cadr exp))

(define (if-fail-alternative exp) (caddr exp))

(define (analyze-if-fail exp)
  (let
    ((vproc (analyze (if-fail-value exp)))
     (aproc (analyze (if-fail-alternative exp))))
    (lambda (env succeed fail)
      (vproc
        env
        succeed
        (lambda ()
          (aproc env succeed fail))))))

(define if-fail-example-1
  '(if-fail
     (let ((x (an-element-of '(1 3 5))))
       (require (even? x))
       x)
     'all-odd))

(define if-fail-example-2
  '(if-fail
     (let ((x (an-element-of '(1 3 5 8))))
       (require (even? x))
       x)
     'all-odd))

#| 4.53 |#

(define all-solutions-example
  '(let ((pairs '()))
    (if-fail
      (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
        (permanent-set! pairs (cons p pairs))
        (amb))
      pairs)))

#| 4.54 |#

(define (require? exp) (tagged-list? exp 'require-))

(define (require-predicate exp) (cadr exp))

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc
        env
        (lambda (pred-value fail2)
          (if (not pred-value)
            (fail)
            (succeed 'ok fail2)))
        fail))))
