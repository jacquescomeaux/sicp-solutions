#lang sicp
(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; Chapter 3
;; Modularity, Objects, and State

;; 3.5
;; Streams

;; Streams Are Delayed Lists

(#%provide stream-car)
(define (stream-car stream) (car stream))

(#%provide stream-cdr)
(define (stream-cdr stream) (force (cdr stream)))

(#%provide stream-ref)
(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(#%provide stream-map)
(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream
      (proc (stream-car s))
      (stream-map proc (stream-cdr s)))))

(#%provide stream-for-each)
(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin
      (proc (stream-car s))
      (stream-for-each proc (stream-cdr s)))))

(#%provide display-stream)
(define (display-stream s)
  (stream-for-each display-line s))

(#%provide display-line)
(define (display-line x)
  (newline)
  (display x))

(#%provide stream-enumerate-interval)
(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval
        (+ low 1)
        high))))

(#%provide stream-filter)
(define (stream-filter pred stream)
  (cond
    ((stream-null? stream) the-empty-stream)
    ((pred (stream-car stream))
     (cons-stream
       (stream-car stream)
       (stream-filter pred (stream-cdr stream))))
    (else (stream-filter pred (stream-cdr stream)))))

(#%provide prime?)
(define (prime? n)
  (define (square x) (* x x))
  (define (divides? a b) (= (remainder b a) 0))
  (define (smallest-divisor) (find-divisor 2))
  (define (find-divisor test-divisor)
    (define (next i) (if (= i 2) 3 (+ i 2)))
    (cond
      ((> (square test-divisor) n) n)
      ((divides? test-divisor n) test-divisor)
      (else (find-divisor (next test-divisor)))))
  (= n (smallest-divisor)))

#| (define (force delayed-object) |#
#|   (delayed-object)) |#

(#%provide memo-proc)
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
        (begin
          (set! result (proc))
          (set! already-run? true)
          result)
        result))))

#| (delay ...) = (memo-proc (lambda () ...)) |#

#| 3.50 |#

(#%provide stream-map-)
(define (stream-map- proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply
        stream-map-
        (cons proc (map stream-cdr argstreams))))))

#| 3.51 |#

(#%provide show)
(define (show x) (display-line x) x)

#| (define x (stream-map show (stream-enumerate-interval 0 10))) |#

#| 0 |#

#| (stream-ref x 5) |#

#| 1 |#
#| 2 |#
#| 3 |#
#| 4 |#
#| 55 |#

#| (stream-ref x 7) |#

#| 6 |#
#| 77 |#

#| 3.52 |#

#| (define sum 0) ;; sum = 0 |#
#| (define (accum x) (set! sum (+ x sum)) sum) ;; sum = 0 |#
#| (define seq (stream-map accum (stream-enumerate-interval 1 20))) ;; sum = 1 |#
#| (define y (stream-filter even? seq)) ;; sum = 6 |#
#| (define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq)) ;; sum = 10 |#
#| (stream-ref y 7) ;; sum = 136 |#
#| (display-stream z) ;; sum = 210 |#

;; Infinite Streams

(#%provide integers-starting-from)
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(#%provide integers)
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(#%provide no-sevens)
(define no-sevens
  (stream-filter
    (lambda (x)
      (not (divisible? x 7)))
      integers))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(#%provide fibs)
(define fibs (fibgen 0 1))

(#%provide sieve)
(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve
      (stream-filter
        (lambda (x) (not (divisible? x (stream-car stream))))
        (stream-cdr stream)))))

(#%provide primes)
(define primes (sieve (integers-starting-from 2)))

(#%provide ones)
(define ones (cons-stream 1 ones))

(#%provide add-streams)
(define (add-streams s1 s2)
  (stream-map- + s1 s2))

(#%provide integers-)
(define integers- (cons-stream 1 (add-streams ones integers-)))

(#%provide fibs-)
(define fibs-
  (cons-stream
    0
    (cons-stream
      1
      (add-streams
        (stream-cdr fibs-)
        fibs-))))

(#%provide scale-stream)
(define (scale-stream stream factor)
  (stream-map
    (lambda (x) (* x factor))
    stream))

(#%provide double)
(define double (cons-stream 1 (scale-stream double 2)))

(define (square x) (* x x))

(#%provide primes-)
(define primes-
  (cons-stream
    2
    (stream-filter prime? (integers-starting-from 3))))

(define (prime?- n)
  (define (iter ps)
    (cond
      ((> (square (stream-car ps)) n) true)
      ((divisible? n (stream-car ps)) false)
      (else (iter (stream-cdr ps)))))
  (iter primes-))

#| 3.53 |#

#| (define s (cons-stream 1 (add-streams s s))) |#
#| 1, 2, 4, 8, 16, ... |#

#| 3.54 |#

(#%provide mul-streams)
(define (mul-streams s1 s2)
  (stream-map- * s1 s2))

(#%provide factorials)
(define factorials (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))

#| 3.55 |#

(#%provide partial-sums)
(define (partial-sums stream)
  (cons-stream
    (stream-car stream)
    (stream-map-
      (lambda (x) (+ x (stream-car stream)))
      (partial-sums (stream-cdr stream)))))

#| 3.56 |#

(define (merge s1 s2)
  (cond
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    (else
      (let
        ((s1car (stream-car s1))
         (s2car (stream-car s2)))
        (cond
          ((< s1car s2car)
           (cons-stream s1car (merge (stream-cdr s1) s2)))
          ((> s1car s2car)
           (cons-stream s2car (merge s1 (stream-cdr s2))))
          (else
           (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))

(#%provide S)
(define S
  (cons-stream
    1
    (merge
      (scale-stream S 2)
      (merge
        (scale-stream S 3)
        (scale-stream S 5)))))

#| 3.58 |#

(#%provide expand)
(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

;; expansion of a rational number in the given base

#| 3.59 |#

(#%provide integrate-series)
(define (integrate-series stream)
  (stream-map- / stream integers))

(#%provide exp-series)
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(#%provide cosine-series)
(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(#%provide sine-series)
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

#| 3.60 |#

(#%provide mul-series)
(define (mul-series s1 s2)
  (cons-stream
    (* (stream-car s1) (stream-car s2))
    (add-streams
      (scale-stream (stream-cdr s2) (stream-car s1))
      (mul-series (stream-cdr s1) s2))))

#| 3.61 |#

(#%provide invert-unit-series)
(define (invert-unit-series series)
  (define result
    (cons-stream
      1
      (scale-stream
        (mul-series (stream-cdr series) result)
        -1)))
  result)

#| 3.62 |#

(#%provide div-series)
(define (div-series s1 s2)
  (cond
    ((= 0 (stream-car s2))
     (error "Zero constant term in denominator -- DIV_SERIES"))
    (else
      (let ((factor (/ 1 (stream-car s2))))
        (mul-series
          (scale-stream s1 factor)
          (invert-unit-series (scale-stream s2 factor)))))))

(#%provide tangent-series)
(define tangent-series
  (div-series sine-series cosine-series))

;; Exploiting the Stream Paradigm

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(#%provide sqrt-stream)
(define (sqrt-stream x)
  (define guesses
    (cons-stream
      1.0
      (stream-map
        (lambda (guess)
          (sqrt-improve guess x))
        guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream
    (/ 1.0 n)
    (stream-map - (pi-summands (+ n 2)))))

(#%provide pi-stream)
(define pi-stream
  (scale-stream
    (partial-sums (pi-summands 1))
    4))

(#%provide euler-transform)
(define (euler-transform s)
  (let
    ((s0 (stream-ref s 0))
     (s1 (stream-ref s 1))
     (s2 (stream-ref s 2)))
    (cons-stream
      (-
        s2
        (/
          (square (- s2 s1))
          (+ s0 (* -2 s1) s2)))
      (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream
    s
    (make-tableau transform (transform s))))

(#%provide accelerated-sequence)
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

#| 3.63 |#

(#%provide sqrt-stream-bad)
(define (sqrt-stream-bad x)
  (cons-stream
    1.0
    (stream-map
      (lambda (guess)
        (sqrt-improve guess x))
      (sqrt-stream-bad x))))

;; no memoization


#| 3.64 |#

(#%provide stream-limit)
(define (stream-limit stream tolerance)
  (let
    ((s0 (stream-ref stream 0))
     (s1 (stream-ref stream 1)))
    (if (< (abs (- s0 s1)) tolerance)
      s1
      (stream-limit (stream-cdr stream) tolerance))))

(#%provide sqrt-)
(define (sqrt- x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

#| 3.65 |#

(define (ln2-summands n)
  (cons-stream
    (/ 1.0 n)
    (stream-map - (ln2-summands (+ n 1)))))

(#%provide ln2-stream)
(define ln2-stream
  (partial-sums (ln2-summands 1)))

(#%provide ln2-faster)
(define ln2-faster
  (euler-transform ln2-stream))

(#%provide ln2-fastest)
(define ln2-fastest
  (accelerated-sequence euler-transform ln2-stream))

#| (stream-filter |#
#|   (lambda (pair) |#
#|     (prime? (+ (car pair) (cadr pair)))) |#
#|   int-pairs) |#

(#%provide interleave)
(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream
      (stream-car s1)
      (interleave s2 (stream-cdr s1)))))

(#%provide pairs)
(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map
        (lambda (x) (list (stream-car s) x))
        (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(#%provide stream-append)
(define (stream-append s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream
      (stream-car s1)
      (stream-append (stream-cdr s1) s2))))

#| 3.66 |#

#| (pairs integers integers) |#
;; second number in pair gets large more quickly

#| 3.67 |#

(#%provide pairs-all)
(define (pairs-all s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (interleave
        (stream-map
          (lambda (x) (list (stream-car s) x))
          (stream-cdr t))
        (stream-map
          (lambda (x) (list x (stream-car t)))
          (stream-cdr s)))
      (pairs-all (stream-cdr s) (stream-cdr t)))))

#| 3.68 |#

(#%provide pairs-bad)
(define (pairs-bad s t)
  (interleave
    (stream-map
      (lambda (x)
        (list (stream-car s) x))
      t)
    (pairs-bad (stream-cdr s) (stream-cdr t))))

;; interleave needs the car of its second argument. but
;; since pairs-bad does not use a cons, this will loop
;; infinitely without producing any values

#| 3.69 |#

(#%provide triples)
(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map
        (lambda (xy) (cons (stream-car s) xy))
        (interleave
          (stream-map
            (lambda (x) (list (stream-car t) x))
            (stream-cdr u))
          (pairs (stream-cdr t) (stream-cdr u))))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(#%provide pythagorean-triples)
(define pythagorean-triples
  (stream-filter
    (lambda (ijk)
      (let
        ((i (car ijk))
         (j (cadr ijk))
         (k (caddr ijk)))
        (=
          (+ (square i) (square j))
          (square k))))
    (triples integers integers integers)))

#| 3.70 |#

(#%provide merge-weighted)
(define (merge-weighted s1 s2 weight)
  (cond
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    (else
      (let
        ((s1car (stream-car s1))
         (s2car (stream-car s2)))
        (let
          ((s1weight (weight s1car))
           (s2weight (weight s2car)))
          (cond
            ((< s1weight s2weight)
             (cons-stream
               s1car
               (merge-weighted (stream-cdr s1) s2 weight)))
            ((> s1weight s2weight)
             (cons-stream
               s2car
               (merge-weighted s1 (stream-cdr s2) weight)))
            (else
             (cons-stream
               s1car
               (cons-stream
                 s2car
                 (merge-weighted (stream-cdr s1) (stream-cdr s2) weight))))))))))


(#%provide weighted-pairs)
(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))

(#%provide ijs-sum)
(define ijs-sum
  (weighted-pairs
    integers
    integers
    (lambda (xy) (apply + xy))))

(#%provide ijs-235)
(define ijs-235
  (let
    ((not235
      (stream-filter
        (lambda (x) (not (divisible? x 5)))
        (stream-filter
          (lambda (x) (not (divisible? x 3)))
          (stream-filter
            (lambda (x) (not (divisible? x 2)))
            integers)))))
    (weighted-pairs
      not235
      not235
      (lambda (xy)
        (let
          ((i (car xy))
           (j (cadr xy)))
          (+ (* 2 i) (* 3 j) (* 5 i j)))))))

#| 3.71 |#

(define (cube x) (* x x x))

(define sum-of-cubes
  (lambda (xy)
    (+ (cube (car xy)) (cube (cadr xy)))))

(define (same-2-weights stream weight)
  (let
    ((s0 (stream-ref stream 0))
     (s1 (stream-ref stream 1)))
    (if (= (weight s0) (weight s1))
      (cons-stream
        (weight s0)
        (same-2-weights (stream-cdr stream) weight))
      (same-2-weights (stream-cdr stream) weight))))

(#%provide ramanujan-numbers)
(define ramanujan-numbers
  (same-2-weights
    (weighted-pairs
      integers
      integers
      sum-of-cubes)
    sum-of-cubes))

#| 1729 |#
#| 4104 |#
#| 13832 |#
#| 20683 |#
#| 32832 |#
#| 39312 |#

#| 3.72 |#

(define (same-3-weights stream weight)
  (let
    ((s0 (stream-ref stream 0))
     (s1 (stream-ref stream 1))
     (s2 (stream-ref stream 2)))
    (if
      (and
        (= (weight s0) (weight s1))
        (= (weight s0) (weight s2)))
      (cons-stream
        (list s0 s1 s2)
        (same-3-weights (stream-cdr stream) weight))
      (same-3-weights (stream-cdr stream) weight))))

(define (display-sum-of-cubes xy)
  (let
    ((x (car xy))
     (y (cadr xy)))
    (display x)
    (display "^3 + ")
    (display y)
    (display "^3 = ")
    (display (sum-of-cubes xy))
    (newline)))

(#%provide taxicab-3)
(define (taxicab-3)
  (stream-for-each
    (lambda (xyz)
      (for-each display-sum-of-cubes xyz))
    (same-3-weights
      (weighted-pairs
        integers
        integers
        sum-of-cubes)
      sum-of-cubes)))

(#%provide integral)
(define (integral integrand initial-value dt)
  (define int
    (cons-stream
      initial-value
      (add-streams
        (scale-stream integrand dt)
        int)))
  int)

#| 3.73 |#

(#%provide RC)
(define (RC R C dt)
  (lambda (i v0)
    (add-streams
      (scale-stream i R)
      (integral (scale-stream i (/ 1 C)) v0 dt))))

#| 3.74 |#

(define (sign-change-detector a b)
  (cond
    ((and (< a 0) (>= b 0)) 1)
    ((and (>= a 0) (< b 0)) -1)
    (else 0)))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
    (sign-change-detector (stream-car input-stream last-value))
    (make-zero-crossings
      (stream-cdr input-stream)
      (stream-car input-stream))))

(define (zero-crossings sense-data)
  (make-zero-crossings sense-data 0))

(#%provide zero-crossings-)
(define (zero-crossings- sense-data)
  (stream-map-
    sign-change-detector
    sense-data
    (stream-cdr sense-data)))

#| 3.75 |#

(#%provide make-zero-crossings-)
(define (make-zero-crossings- input-stream last-value last-avpt)
  (let
    ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream
      (sign-change-detector avpt last-avpt)
      (make-zero-crossings-
        (stream-cdr input-stream)
        (stream-car input-stream)
        avpt))))

#| 3.76 |#

(#%provide smooth)
(define (smooth stream)
  (let
    ((s0 (stream-ref stream 0))
     (s1 (stream-ref stream 1)))
    (cons-stream
      (average s0 s1)
      (smooth (stream-cdr stream)))))

(#%provide zero-crossings--)
(define (zero-crossings-- sense-data)
  (let ((smoothed (smooth sense-data)))
    (stream-map-
      sign-change-detector
      smoothed
      (stream-cdr smoothed))))

;; Streams and Delayed Evaluation

(#%provide integral-)
(define (integral- delayed-integrand initial-value dt)
  (define int
    (cons-stream
      initial-value
      (let ((integrand (force delayed-integrand)))
        (add-streams
          (scale-stream integrand dt)
          int))))
  int)

(#%provide solve)
(define (solve f y0 dt)
  (define y (integral- (delay (stream-map f y)) y0 dt))
  y)

#| 3.77 |#

(#%provide integral--)
(define (integral-- delayed-integrand initial-value dt)
  (cons-stream
    initial-value
    (let ((integrand (force delayed-integrand)))
      (if (stream-null? integrand)
        the-empty-stream
        (integral
          (stream-cdr integrand)
          (+
            (* dt (stream-car integrand))
            initial-value))))))

#| 3.78 |#

(#%provide solve-2nd)
(define (solve-2nd a b dt y0 dy0)
  (define ddy
     #| (add-streams |#
     #|   (scale-stream dy a) |#
     #|   (scale-stream y b))) |#
    (add-streams
      (scale-stream (integral- (delay ddy) dy0 dt) a)
      (scale-stream (integral- (delay (integral- (delay ddy) dy0 dt)) y0 dt) b)))
  (define dy (integral- (delay ddy) dy0 dt))
  (define y (integral- (delay dy) y0 dt))
  y)

#| 3.79 |#

(#%provide solve-2nd-)
(define (solve-2nd- f dt y0 dy0)
  (define ddy
    #| (f dy y) |#
    (f
      (integral- (delay ddy) dy0 dt)
      (integral- (delay (integral- (delay ddy) dy0 dt)) y0 dt)))
  (define dy (integral- (delay ddy) dy0 dt))
  (define y (integral- (delay dy) y0 dt))
  y)

#| 3.80 |#

#| (#%provide RLC) |#
#| (define (RLC R L C dt) |#
#|   (lambda (vc0 il0) |#
#|     (define dil |#
#|       (add-streams |#
#|         (scale-stream vc (/ 1 L)) |#
#|         (scale-stream il (- (/ R L))))) |#
#|     (define dvc (scale-stream il (- (/ 1 C)))) |#
#|     (define vc (integral- (delay dvc) vc0)) |#
#|     (define il (integral- (delay dil) il0)) |#
#|     (cons vc il))) |#

;; Modularity of Functional Programs and Modularity of Objects

(define random-init 4)

(define (rand-update x)
  (modulo (+ (* 75 x) 74) 65537))

(#%provide rand)
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(#%provide random-numbers)
(define random-numbers
  (cons-stream
    random-init
    (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
    (f (stream-car s) (stream-car (stream-cdr s)))
    (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(#%provide cesaro-stream)
(define cesaro-stream
  (map-successive-pairs
    (lambda (r1 r2) (= (gcd r1 r2) 1))
    random-numbers))

(#%provide monte-carlo)
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      (/ passed (+ passed failed))
      (monte-carlo
        (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
    (next (+ passed 1) failed)
    (next passed (+ failed 1))))

(#%provide pi)
(define pi
  (stream-map
    (lambda (p) (sqrt (/ 6 p)))
    (monte-carlo cesaro-stream 1 0)))

#| 3.81 |#

(#%provide rand-news)
(define (rand-news requests)
  (define (next requests current)
    (cond
      ((eq? (stream-car requests) 'generate)
      (cons-stream
        current
        (next
          (stream-cdr requests)
          (rand-update current))))
      ((eq? (stream-car requests) 'reset)
       (cons-stream
         (stream-car (stream-cdr requests))
         (next
           (stream-cdr (stream-cdr requests))
           (rand-update (stream-car (stream-cdr requests))))))
      (else error "unknown symbol")))
  (next requests random-init))

#| 3.82 |#

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (random-points x1 x2 y1 y2)
  (cons-stream
    (list
      (random-in-range x1 x2)
      (random-in-range y1 y2))
    (random-points x1 x2 y1 y2)))

(define (estimate-integral P x1 x2 y1 y2)
  (monte-carlo
    (stream-map
      (lambda (xy) (apply P xy))
      (random-points x1 x2 y1 y2))
    0
    0))

(#%provide estimate-pi-integral)
(define estimate-pi-integral
  (scale-stream
    (estimate-integral
      (lambda (x y) (<= (+ (square x) (square y)) 1.0))
      0.0 1.0
      0.0 1.0)
    4.0))
