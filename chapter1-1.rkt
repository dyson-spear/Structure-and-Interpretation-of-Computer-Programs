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



