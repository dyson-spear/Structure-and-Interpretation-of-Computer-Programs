#lang sicp

;; Exercise 1.1.  Below is a sequence of expressions. What is the result printed by the interpreter in response to each expression?
(display "Exercise 1.1: ")(newline)
10
;10
(+ 5 3 4)
;12
(- 9 1)
;8
(/ 6 2)
;3
(+ (* 2 4) (- 4 6))
;6
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(+ 3 4 (* 3 4))
;19
(= a b)
;#f
(if (and (> b a) (< b (* a b)))
    b
    a)
;b -- 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;b = 4
;(+ 6 7 3)
;16

(+ 2 (if (> b a) b a))
;(> b a) -> #t
;(+ 2 b)
;6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;; Exercise 1.2

(display "Exercise 1.2: ")
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3 Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.
(display "Exercise 1.3: ")
(define (larger-squares x y z)
  (cond ((and (> x z) (> y z)) (sum-of-squares x y))
        ((and (> x y) (> z y)) (sum-of-squares x z))
        (else (sum-of-squares y z))))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (square x)
  (* x x))

(larger-squares 1 2 3)

;; Exercise 1.4
(display "Exercise 1.4: ")(newline)
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;if b > 0, use + operator, otherwise use -

(a-plus-abs-b 3 4)
;(+ 3 4) = 7
;and should get the same if we use -4 instead
(a-plus-abs-b 3 -4)

;; Exercise 1.5 Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))
;Then he evaluates the expression
;(test 0 (p))
;What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What behavior will he observe with an interpreter that uses normal-order evaluation? 
;answer:

;With applicative-order the parameters will both be evaluated resulting in an infinite loop because (p) evaluates to itself

;With normal-order the parameters are evaluated as needed, so (p) is never evaluated and we get 0

;; Exercise 1.6
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
;Delighted, Alyssa uses new-if to rewrite the square-root program:
(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter2 (improve guess x)
                     x)))

;What happens when Alyssa attempts to use this to compute square roots? Explain.
;Answer: it runs forever with 'guess' getting larger and larger. I think this is because of the "applicative-order" evaluation. We need the special form because we don't want to evaluate the 'else' case unless needed

;; Exercise 1.7
; for small numbers the difference between your guess squared and the actual value could be less than 0.001 while still being very far form the real square root.

; for large numbers there is some loss of precision that could cause you to skip over the actual solution

(define (sqrt-iter3 guess x prev-guess)
  (if (good-enough-v2 guess prev-guess)
      guess
      (sqrt-iter3 (improve guess x) x guess)))

;improved good-enough check
(define (good-enough-v2 guess prev-guess)
  (< (abs (/ (- guess prev-guess) guess)) 0.001))
(define (sqrt x)
  (sqrt-iter3 1 x 0))
(display "Exercise 1.7: sqrt of .0001, new vs old good-enough?. actual value = 0.01 ")(newline)
(display "new: ")(sqrt .0001)
(display "old: ")(sqrt-iter 1 .0001)

;; Exercise 1.8
(define (cube-root-iter guess x prev-guess)
  (if (good-enough-v2 guess prev-guess)
      guess
      (cube-root-iter (improve-cube-root guess x) x guess)))
(define (improve-cube-root guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
(define (cube-root x)
  (cube-root-iter 1 x 0))
(display "Exercise 1.8: ")(newline)
(display "cube root of 27: ")
(cube-root 27)
