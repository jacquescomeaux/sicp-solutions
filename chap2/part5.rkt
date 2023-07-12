#lang sicp
(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; Chapter 2
;; Building Abstractions with Data

;; 2.5
;; Systems with Generic Operations

(define (get) (error "get not implemented"))
(define (put) (error "put not implemented"))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args)) 
        (error
          "No method for these types -- APPLY-GENERIC"
          (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  ;; interface
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
    (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
    (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
    (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
    (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
    (lambda (x y) (tag (= x y))))
  (put '=zero? '(scheme-number)
    (lambda (x) (tag (= x 0))))
  (put 'negate '(scheme-number)
    (lambda (x) (tag (- 0 x))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat
      (+
        (* (numer x) (denom y))
        (* (numer y) (denom x)))
      (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat
      (-
        (* (numer x) (denom y))
        (* (numer y) (denom x)))
      (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat
      (* (numer x) (numer y))
      (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat
      (* (numer x) (denom y))
      (* (denom x) (numer y))))
  (define (equ?-rat x y)
    (=
      (* (numer x) (denom y))
      (* (denom x) (numer y))))
  (define (=zero?-rat x)
    (= (numer x) 0))
  (define (negate-rat x)
    (make-rat
      (- 0 (numer x))
      (denom x)))
  ;; interface
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
    (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
    (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
    (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
    (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
    (lambda (x y) (tag (equ?-rat x y))))
  (put '=zero? '(rational)
    (lambda (x) (tag (=zero?-rat x))))
  (put 'negate '(rational)
    (lambda (x) (tag (negate-rat x))))
  (put 'make 'rational
    (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag
      (+ (real-part z1) (real-part z2))
      (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
      (- (real-part z1) (real-part z2))
      (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
      (* (magnitude z1) (magnitude z2))
      (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
      (/ (magnitude z1) (magnitude z2))
      ( (angle z1) (angle z2))))
  (define (equ?-complex z1 z2)
    (and
      (= (real-part z1) (real-part z2))
      (= (imag-part z1) (imag-part z2))))
  (define (=zero?-complex z)
    (and
      (= (real-part z) 0)
      (= (imag-part z) 0)))
  (define (negate-complex z)
    (make-from-real-imag
      (- 0 (real-part z))
      (- 0 (imag-part z))))
  ;; interface
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
    (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
    (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
    (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
    (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
    (lambda (z1 z2) (tag (equ?-complex z1 z2))))
  (put '=zero? '(complex)
    (lambda (z) (tag (=zero?-complex z))))
  (put 'negate '(complex)
    (lambda (z) (tag (negate-complex z))))
  (put 'make-from-real-imag 'complex
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
    (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

#| 2.77 |#

;; (define z (make-complex-from-real-imag 3 4))

;; (magnitude z)

;; (magnitude '(complex rectangular (3 . 4)))

;; (apply-generic 'magnitude '(complex rectangular (3 . 4)))

;; ((get 'magnitude 'complex) '(rectangular (3 . 4)))

;; (magnitude '(rectangular (3 . 4)))

;; (apply-generic 'magnitude '(rectangular (3 . 4)))

;; ((get 'magnitude 'rectangular) '(3 . 4))

;; ((lambda (z) (sqrt (+ (square (real-part z)) (square (imag-part z))))) '(3 . 4))

;; (sqrt (+ (square 3) (square 4)))

;; 5

#| 2.78 |#

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond
    ((pair? datum) (car datum))
    ((number? datum) 'scheme-number)
    (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (cond
    ((pair? datum) (cdr datum))
    ((number? datum) datum)
    (error "Bad tagged datum -- CONTENTS" datum)))

#| 2.79 |#

(define (equ? x y) (apply-generic 'equ? x y))

;; Add to scheme-number package:

  #| (put 'equ? '(scheme-number scheme-number) |#
  #|   (lambda (x y) (tag (= x y)))) |#

;; Add to rational package:

  #| (define (equ?-rat x y) |#
  #|   (= |#
  #|     (* (numer x) (denom y)) |#
  #|     (* (denom x) (numer y)))) |#


  #| (put 'equ? '(rational rational) |#
  #|   (lambda (x y) (tag (equ?-rat x y)))) |#

;; Add to complex package:

  #| (define (equ?-complex z1 z2) |#
  #|   (and |#
  #|     (= (real-part z1) (real-part z2)) |#
  #|     (= (imag-part z1) (imag-part z2)))) |#

  #| (put 'equ? '(complex complex) |#
  #|   (lambda (z1 z2) (tag (equ?-complex z1 z2)))) |#

#| 2.80 |#

(define (=zero? x) (apply-generic '=zero? x))

;; Add to scheme-number package:

  #| (put '=zero? '(scheme-number) |#
  #|   (lambda (x) (tag (= x 0)))) |#

;; Add to rational package:

  #| (define (=zero?-rat x) |#
  #|   (= (numer x) 0)) |#

  #| (put '=zero? '(rational) |#
  #|   (lambda (x) (tag (=zero?-rat x)))) |#

;; Add to complex package:

  #| (define (=zero?-complex z) |#
  #|   (and |#
  #|     (= (real-part z) 0) |#
  #|     (= (imag-part z) 0))) |#

  #| (put '=zero? '(complex) |#
  #|   (lambda (z) (tag (=zero?-complex z)))) |#

(define (put-coercion) (error "put-coercion not implemented"))
(define (get-coercion) (error "get-coercion not implemented"))

#| (define (scheme-number->complex n) |#
#|   (make-complex-from-real-imag (contents n) 0)) |#

#| (put-coercion 'scheme-number 'complex scheme-number->complex) |#

(define (apply-generic-- op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let
            ((type1 (car type-tags))
             (type2 (cadr type-tags))
             (a1 (car args))
             (a2 (cadr args)))
            (let
              ((t1->t2 (get-coercion type1 type2))
               (t2->t1 (get-coercion type2 type1)))
              (cond
                (t1->t2
                 (apply-generic-- op (t1->t2 a1) a2))
                (t2->t1
                 (apply-generic-- op a1 (t2->t1 a2)))
                (else
                 (error
                   "No method for these types"
                   (list op type-tags))))))
          (error
            "No method for these types"
            (list op type-tags)))))))


(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons (variable term-list)))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (negate-terms L)
    (adjoin-term
      (negate-term (first-term L))
      (negate-terms (rest-terms L))))
  (define (negate-term t)
    (make-term
      (order t)
      (negate (coeff t))))
  ;; operations on poly
  (define (negate-poly p)
    (make-poly
      (variable p)
      (negate-terms (term-list p))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly
        (variable p1)
        (add-terms
          (term-list p1)
          (term-list p2)))
      (error
        "Polys not in same var -- ADD-POLY"
        (list p1 p2))))
  (define (add-terms L1 L2)
    (cond
      ((empty-termlist? L1) L2)
      ((empty-termlist? L2) L1)
      (else
        (let
          ((t1 (first-term L1))
           (t2 (first-term L2)))
          (cond
            ((> (order t1) (order t2))
             (adjoin-term
               t1
               (add-terms (rest-terms L1) L2)))
            ((< (order t1) (order t2))
             (adjoin-term
               t2
               (add-terms L1 (rest-terms L2))))
            (else
              (adjoin-term
                (make-term
                  (order t1)
                  (add (coeff t1) (coeff t2)))
                (add-terms
                  (rest-terms L1)
                  (rest-terms L2)))))))))
  (define (sub-poly p1 p2)
    (add-poly p1 (negate p2)))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly
        (variable p1)
        (mul-terms
          (term-list p1)
          (term-list p2)))
      (error
        "Polys not in same var -- SUB-POLY"
        (list p1 p2))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms
        (mul-term-by-all-terms (first-term L1) L2)
        (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly
        (variable p1)
        (div-terms
          (term-list p1)
          (term-list p2)))
      (error
        "Polys not in same var -- DIV-POLY"
        (list p1 p2))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let
        ((t1 (first-term L1))
         (t2 (first-term L2)))
        (if (> (order t2) (order t1))
          (list (the-empty-termlist) L1)
          (let
            ((new-c (div (coeff t1) (coeff t2)))
             (new-o (- (order t1) (order t2))))
            (let
              ((rest-of-result
                (div-terms
                  (add-terms
                    L1
                    (negate-terms
                      (mul-terms (make-term new-o new-c) L2)))
                  L2)))
              (list
                (adjoin-term (make-term new-o new-c) (car rest-of-result))
                (cdr rest-of-result))))))))
  (define (=zero?-poly p) (empty-termlist? (term-list p)))
  ;; interface
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
    (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
    (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial)
    (lambda (p) (tag (=zero?-poly p))))
  (put 'make 'polynomial
    (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

#| 2.87 |#

;; Add to polynomial package:

  #| (define (=zero?-poly p) (empty-termlist? (term-list p))) |#

  #| (put '=zero? '(polynomial) |#
  #|   (lambda (p) (tag (=zero?-poly p)))) |#

#| 2.88 |#

(define (negate x) (apply-generic 'negate x))

  #| (define (negate-terms L) |#
  #|   (adjoin-term |#
  #|     (negate-term (first-term L)) |#
  #|     (negate-terms (rest-terms L)))) |#
  #| (define (negate-term t) |#
  #|   (make-term |#
  #|     (order t) |#
  #|     (negate (coeff t)))) |#

  #| (define (negate-poly p) |#
  #|   (make-poly |#
  #|     (variable p) |#
  #|     (negate-terms (term-list p)))) |#

  #| (define (sub-poly p1 p2) |#
  #|   (add-poly p1 (negate p2))) |#

#| 2.91 |#

#| (define (div-poly p1 p2) |#
#|   (if (same-variable? (variable p1) (variable p2)) |#
#|     (make-poly |#
#|       (variable p1) |#
#|       (div-terms |#
#|         (term-list p1) |#
#|         (term-list p2))) |#
#|     (error |#
#|       "Polys not in same var -- DIV-POLY" |#
#|       (list p1 p2)))) |#

#| (define (div-terms L1 L2) |#
#|   (if (empty-termlist? L1) |#
#|     (list (the-empty-termlist) (the-empty-termlist)) |#
#|     (let |#
#|       ((t1 (first-term L1)) |#
#|        (t2 (first-term L2))) |#
#|       (if (> (order t2) (order t1)) |#
#|         (list (the-empty-termlist) L1) |#
#|         (let |#
#|           ((new-c (div (coeff t1) (coeff t2))) |#
#|            (new-o (- (order t1) (order t2)))) |#
#|           (let |#
#|             ((result-1 (make-term new-o new-c)) |#
#|              (rest-of-result |#
#|               (div-terms |#
#|                 (add-terms L1 (negate-terms (mul-terms result-1 L2))) |#
#|                 L2))) |#
#|             (list |#
#|               (adjoin-term result-1 (car rest-of-result)) |#
#|               (cdr rest-of-result)))))))) |#

#| 2.93 |#

(define (install-rational-package-)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d) (cons n d))
  (define (add-rat x y)
    (make-rat
      (add
        (mul (numer x) (denom y))
        (mul (numer y) (denom x)))
      (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat
      (sub
        (mul (numer x) (denom y))
        (mul (numer y) (denom x)))
      (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat
      (mul (numer x) (numer y))
      (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat
      (mul (numer x) (denom y))
      (mul (denom x) (numer y))))
  (define (equ?-rat x y)
    (=
      (mul (numer x) (denom y))
      (mul (denom x) (numer y))))
  (define (=zero?-rat x)
    (=zero? (numer x)))
  (define (negate-rat x)
    (make-rat
      (negate (numer x))
      (denom x)))
  ;; interface
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
    (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
    (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
    (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
    (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
    (lambda (x y) (tag (equ?-rat x y))))
  (put '=zero? '(rational)
    (lambda (x) (tag (=zero?-rat x))))
  (put 'negate '(rational)
    (lambda (x) (tag (negate-rat x))))
  (put 'make 'rational
    (lambda (n d) (tag (make-rat n d))))
  'done)

#| (define (gcd-terms a b) |#
#|   (if (empty-termlist? b) |#
#|     a |#
#|     (gcd-terms b (remainder-terms a b)))) |#

#| (define (remainder-terms a b) |#
#|   (cadr (div-terms a b))) |#
