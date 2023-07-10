#lang sicp
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;Chapter 1.3
(define (square x) (* x x))
;; Exercise 1.29 - Simpson's rule integration
; skipped

;; Exercise 1.30 - iteratively recursive version of sum procedure
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(sum square 1 inc 10)
;; Exercise 1.31 - product
;iterative
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;linear
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))
(product square 1 inc 3)
(product-iter square 1 inc 3)

;; Exercise 1.32 - accumulate
;linear
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))
(accumulate + 0 square 1 inc 10)
(accumulate * 1 square 1 inc 3)

;iterative
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))
(accumulate-iter + 0 square 1 inc 10)
(accumulate-iter * 1 square 1 inc 3)

;; Exercise 1.33 - accumulate with filter
(define (accumulate-filter combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a) (accumulate-filter combiner null-value term (next a) next b filter))
          (combiner null-value (accumulate-filter combiner null-value term (next a) next b filter)))))
             
; a. sum of squares of prime numbers in interval a to b
(accumulate-filter + 0 square 1 inc 100 prime?)
; b. product of all positive integers less than n that are relatively prime to n ( GCD(i,n) = 1 )
(define (relatively-prime-100 i)
  (= 1 (gcd i 100)))
(define (identity x)
  x)
(accumulate-filter + 0 identity 1 inc 100 relatively-prime-100)

;; Exercise 1.34
(define (f g) (g 2))
; what happens for (f f)?
; (f f) -> (f 2) -> (2 2) -> error! 2 is not a procedure

(define (average x y)
  (/ (+ x y) 2))
(define (close-enough? x y)
  (< (abs (- x y)) 0.0001))
;half-interval method
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))
;fixed-point
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; Exercise 1.35.  Show that the golden ratio  (section 1.2.2) is a fixed point of the transformation x = 1 + 1/x, and use this fact to compute golden ratio by means of the fixed-point procedure.

(define golden-ratio (/ (+ (sqrt 5) 1) 2))
(define (f2 x)
  (+ 1 (/ 1 x)))
(close-enough? (f2 golden-ratio) golden-ratio) ;true

(define computed-golden-ratio (fixed-point f2 1)) ;golden ratio computed from fixed-point

;; Exercise 1.36. Modified fixed-point
(define (fixed-point-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)(newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(fixed-point-print (lambda (x) (/ (log 1000) (log x))) 2)
(fixed-point-print (lambda (x) (average x (/ (log 1000) (log x)))) 2);converges much faster
;i was confused because it was not obvious to me that a fixed point of our function (/ (log 1000) (log x)) should also be a fixed point of (average x (/ (log 1000) (log x)))

;but if x is a fixed point, then f(x) = x -> so avg of x and x will be (/ (+ x x) 2) -> x

;; Exercise 1.37
(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        0
        (/ (n i) (+ (d i) (iter (inc i))))))
  (iter 1))
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 200)

(define (find-k guess count)
  (if (close-enough? 0.618034 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) count))
      count
      (find-k (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) (inc count)) (inc count))))
(find-k 0 1); 9
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)

;; Exercise 1.38 Euler's expansion
(define (D-value i)
  (cond ((= (remainder i 3) 2) (* 2 (+ 1 (/ (- i 2) 3)))) ;chatgpt helped with this one
        (else 1))) 
(+ 2 (cont-frac (lambda (i) 1.0) D-value 100)) ; don't forget this fraction is e - 2, not e (so we add 2)

;; Exercise 1.39 tan continued fraction
(define (tan-cf x k)
  (define (iter i)
    (if (= i k)
        0
        (/ (expt x i) (- (- (* 2 i) 1) (iter (inc i))))))
  (iter 1))
(define quarter-pi 0.785398)
(tan-cf quarter-pi 100)
