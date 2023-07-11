#lang sicp
(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; Chapter 2
;; Building Abstractions with Data

;; 2.4
;; Multiple Representations for Abstract Data

(define (square x) (* x x))

;; Tagged data

(define (attach-tag- type-tag contents)
  (cons type-tag contents))

(define (type-tag- datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents- datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum -- CONTENTS" datum)))

;; Complex number representations

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;; Ben's representation

(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt
    (+
      (square (real-part-rectangular z))
      (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan
    (imag-part-rectangular z)
    (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag
    'rectangular
    (cons (* r (cos a)) (* r (sin a)))))

;; Alyssa's representation

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag
    'polar
    (cons
      (sqrt (+ (square x) (square y)))
      (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

;; Generic selectors

(#%provide real-part-)
(define (real-part- z)
  (cond
    ((rectangular? z)
     (real-part-rectangular (contents z)))
    ((polar? z)
     (real-part-polar (contents z)))
    (else (error "Unknown type -- REAL-PART" z))))

(#%provide imag-part-)
(define (imag-part- z)
  (cond
    ((rectangular? z)
     (imag-part-rectangular (contents z)))
    ((polar? z)
     (imag-part-polar (contents z)))
    (else (error "Unknown type -- IMAG-PART" z))))

(#%provide magnitude-)
(define (magnitude- z)
  (cond
    ((rectangular? z)
     (magnitude-rectangular (contents z)))
    ((polar? z)
     (magnitude-polar (contents z)))
    (else (error "Unknown type -- MAGNITUDE" z))))

(#%provide angle-)
(define (angle- z)
  (cond
    ((rectangular? z)
     (angle-rectangular (contents z)))
    ((polar? z)
     (angle-polar (contents z)))
    (else (error "Unknown type -- ANGLE" z))))

;; Constructors

(#%provide make-from-real-imag-)
(define (make-from-real-imag- x y)
  (make-from-real-imag-rectangular x y))

(#%provide make-from-mag-ang-)
(define (make-from-mag-ang- r a)
  (make-from-mag-ang-polar r a))

;; Complex number arithmetic operations

(#%provide add-complex-)
(define (add-complex- z1 z2)
  (make-from-real-imag-
    (+ (real-part- z1) (real-part- z2))
    (+ (imag-part- z1) (imag-part- z2))))

(#%provide sub-complex)
(define (sub-complex z1 z2)
  (make-from-real-imag-
    (- (real-part- z1) (real-part- z2))
    (- (imag-part- z1) (imag-part- z2))))

(#%provide mul-complex-)
(define (mul-complex- z1 z2)
  (make-from-mag-ang-
    (* (magnitude- z1) (magnitude- z2))
    (+ (angle- z1) (angle- z2))))

(#%provide div-complex-)
(define (div-complex- z1 z2)
  (make-from-mag-ang-
    (/ (magnitude- z1) (magnitude- z2))
    (- (angle- z1) (angle- z2))))

(define (get) (error "get not implemented"))
(define (put) (error "put not implemented"))

;; Ben's rectangular package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt
      (+
        (square (real-part z))
        (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to rest of system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Alyssa's polar package
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons
      (sqrt (+ (square x) (square y)))
      (atan y x)))
  ;; interface to rest of system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(poar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args)) 
        (error
          "No method for these types -- APPLY-GENERIC"
          (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

#| 2.73 |#

(define (deriv exp var)
  (cond
    ((number? exp) 0)
    ((variable? exp) (if (same-variable? exp var) 1 0))
    (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2))
     (+ a1 a2))
    (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond
    ((or (=number? m1 0) (=number? m2 0))
     0)
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ((and (number? m1) (number? m2))
     (* m1 m2))
    (else (list '* m1 m2))))

(define (make-exponentiation u n)
  (cond
    ((not (number? n)) (error "exponent must be a number"))
    ((=number? n 0) 1)
    ((=number? n 1) u)
    (else (list '** u n))))

(define (install-sum-package)
  ;; internal procedures
  (define (deriv-sum operands var)
    (make-sum
      (deriv (car operands) var)
      (deriv (cadr operands) var)))
  ;; interface to rest of system
  (put 'deriv '(+) deriv-sum)
  'done)

(define (install-product-package)
  ;; internal procedures
  (define (deriv-product operands var)
    (make-sum
      (make-product
        (car operands)
        (deriv (cadr operands) var))
      (make-product
        (deriv (car operands) var)
        (cadr operands))))
  ;; interface to rest of system
  (put 'deriv '(*) deriv-product)
  'done)

(define (install-exponentiation-package)
  ;; internal procedures
  (define (deriv-expo operands var)
    (make-product
      (make-product
        (cadr exp)
        (make-exponentiation
          (car exp)
           (- (cadr exp) 1)))
       (deriv (car exp) var)))
  ;; interface to rest of system
  (put 'deriv '(^^) deriv-expo)
  'done)

(#%provide make-from-real-imag--)
(define (make-from-real-imag-- x y)
  (define (dispatch op)
    (cond
      ((eq? op 'real-part) x)
      ((eq? op 'imag-part) y)
      ((eq? op 'magnitude))
      ((eq? op 'magnitude)
       (sqrt (+ (square x) (square y))))
      ((eq? op 'angle) (atan y x))
      (else (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(#%provide apply-generic-)
(define (apply-generic- op arg) (arg op))

#| 2.75 |#

(#%provide make-from-mag-ang--)
(define (make-from-mag-ang-- r a)
  (define (dispatch op)
    (cond
      ((eq? op 'real-part) (* r (cos a)))
      ((eq? op 'imag-part) (* r (sin a)))
      ((eq? op 'magnitude) r)
      ((eq? op 'angle) a)
      (else (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

#| 2.76 |#

;; Message passing is ideal if new types must often be added.
;; Generic operations with explict dispatch is best if new
;; operations must often be added.

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
