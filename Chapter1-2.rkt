#lang sicp
;; Exercise 1.9
;Each of the following two procedures defines a method for adding two positive integers in terms of the procedures inc, which increments its argument by 1, and dec, which decrements its argument by 1.
(define (add1 a b)
  (if (= a 0)
      b
      (inc (add1 (dec a) b))))

(define (add2 a b)
  (if (= a 0)
      b
      (add2 (dec a) (inc b))))
; first is recurive because it has to call itself again before evaluating 'inc'.
; second is iterative because at each step we get closer. also we could stop at any time and pick it up again.

;; Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;(A 1 10)
;> 1024
;(A 2 4)
;> 65536
;(A 3 3)
;> 65536

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))
;Give concise mathematical definitions for the functions computed by the procedures f, g, and h for positive integer values of n. For example, (k n) computes 5n2.

; f: x is held constant at 0 so A is just 2n

; g and h: no idea tbh. going to skip this one to avoid stalling out.

;; Exercise 1.11
;recursion
(define (f1 n)
  (cond ((< n 3) n)
        (else (+ (f1 (- n 1))
                 (* 2 (f1 (- n 2)))
                 (* 3 (f1 (- n 3)))))))
;iterative
(define (f-2 n)
  (define (f-iter a b c count)
    (cond ((= count 0) a)
          (else (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  (if (< n 3) 
      n
      (f-iter 2 1 0 (- n 2))))

;; Exercise 1.12 - Pascal's Triangle
(define (pascals x y)
  (cond ((< y 2) 1)
        ((= x 0) 1)
        ((= x y) 1)
        (else (+ (pascals (- x 1) (- y 1)) (pascals x (- y 1))))))

;print-pascals below was written entirely by chatgpt
(define (print-pascals n)
  (define (print-row y)
    (define (print-number x)
      (if (<= x y)
          (begin
            (display (pascals x y))
            (display " ")
            (print-number (+ x 1)))))
    (print-number 0)
    (newline))
  (define (print-all-rows i)
    (if (< i n)
        (begin
          (print-row i)
          (print-all-rows (+ i 1)))))
  (print-all-rows 0))

(display "Exercise 1.12")(newline)(print-pascals 5)

;; Exercise 1.13 - 1.19
; skipped

;; Exercise 1.20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;(gcd 206 40)
;(gcd 40 (remainder 206 40))
;(remainder 206 40) -> 6
;(gcd 40 6)
;(gcd 6 (remainder 40 6))
;(remainder 40 6) -> 4
;(gcd 6 4)
;(gcd 4 (remainder 6 4))
;(remainder 6 4) -> 2
;(gcd 4 2)
;(gcd 2 (remainder 4 2))
;(remainder 4 2) -> 0
;(gcd 2 0) -> 2

;Answer: 4 remainder operations

;; Exercise 1.21 - Use the smallest-divisor procedure to find the smallest divisor of each of the following numbers: 199, 1999, 19999.
(define (square x)
  (* x x))
(define (next start)
  (if (= 1 (remainder start 2))
      (+ 2 start)
      (inc start)))
(define (find-divisor2 n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor2 n (next test-divisor)))))
(define (smallest-divisor n)
  (find-divisor2 n 2))

;old version
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(display "Exercise 1.21")(newline)
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;; Exercise 1.22

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;;Function to find 'n' consecutive primes starting from 'start'
;(define (search-for-primes start n)
;  ;helper function to check elapsed time and then check for the next prime
;  (define (found-prime)
;    (timed-prime-test start)
;    (search-for-primes (+ 2 start) (dec n)))
;  ;if start is even, start from start + 1
;  (if (= 0 (remainder start 2))
;      (search-for-primes (inc start) n)
;      ;check for end condition
;      (if (= n 0)
;          (display " --Finished")
;          ;this is not ideal. for prime numbers, we check them twice, the second time to check the elapsed-time.
;          (if (prime? start)
;              (found-prime)
;              ;if not prime, we check the next odd number
;              (search-for-primes (+ 2 start) n)))))


;3 primes larger than 10000000
;10000019 *** 112
;10000079 *** 111
;10000103 *** 113

;; Exercise 1.23 - Modify the smallest-divisor procedure to use (next test-divisor) instead of (+ test-divisor 1).


;new version is slightly faster. had to go up to 10,000,000 to see a difference. modern CPUs are very fast I guess.
;3 primes larger than 10000000
;10000019 *** 98
;10000079 *** 99
;10000103 *** 99

;; Exercise 1.24 - use fermat's test
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
;Function to find 'n' consecutive primes starting from 'start'
(define (search-for-primes start n)
  ;helper function to check elapsed time and then check for the next prime
  (define (found-prime)
    (timed-prime-test start)
    (search-for-primes (+ 2 start) (dec n)))
  ;if start is even, start from start + 1
  (if (= 0 (remainder start 2))
      (search-for-primes (inc start) n)
      ;check for end condition
      (if (= n 0)
          (display " --Finished")
          ;this is not ideal. for prime numbers, we check them twice, the second time to check the elapsed-time.
          (if (fermat-test start)
              (found-prime)
              ;if not prime, we check the next odd number
              (search-for-primes (+ 2 start) n)))))
(search-for-primes 10000000 3)
;10000019 *** 92
;10000079 *** 89
;10000103 *** 90
; a little faster

;; Exercise 1.26
; when you use square, the recursive term is only evaluated once instead of twice

;; Exercise 1.27
(newline)(fermat-test 561)
;returns true but 561 is not prime

;Write a procedure that takes an integer n and tests whether an is congruent to a modulo n for every a<n, and try your procedure on the given Carmichael numbers.
;todo

;note: i've messed up the previous functions by swapping stuff out so many times. would fix but i've spent too much time on these and the juice doesn't seem worth the squeeze. moving on

;; Exercise 1.28
;-skipped
