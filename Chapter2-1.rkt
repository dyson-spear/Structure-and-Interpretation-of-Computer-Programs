#lang sicp

;original make-rat
(define (make-rat n d)
  (let ((g (gcd n d)))
(cons (/ n g) (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;rational number operations
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))


;;Exercise 2.1: Define a better version of make-rat that handles both positive and negative arguments.
(define (better-make-rat n d)
  ;if: numerator and denomenator are both positive or both negative - the rat should be positive
  (if (or
       (and (> n 0) (> d 0))
       (and (< n 0) (< d 0)))
      (make-rat (abs n) (abs d))
      ;else: only numerator should be negative if either (but not both) are negative
      (make-rat (* -1 (abs n)) (abs d))))

;Test if it works

(display "original:")
(print-rat (make-rat -2 -6))
(print-rat (make-rat -2 6))
(print-rat (make-rat 2 -6))
(newline)
(display "better:")
(print-rat (better-make-rat -2 -6))
(print-rat (better-make-rat -2 6))
(print-rat (better-make-rat 2 -6))

;; Exercise 2.2: Consider the problem of representing line segments in a plane...
; print point
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; constructors
(define (make-segment start end)
  (cons start end))
(define (make-point x y)
  (cons x y))

; selectors
(define (start-segment line-segment)
  (car line-segment))
(define (end-segment line-segment)
  (cdr line-segment))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))
(define (avg x y)
  (/ (+ x y) 2))
; get midpoint
(define (get-midpoint line-segment)
  (make-point (avg (x-point (start-segment line-segment)) (x-point (end-segment line-segment)))
              (avg (y-point (start-segment line-segment)) (y-point (end-segment line-segment)))))

; Check if it works
(define start (make-point 0 0))
(define end (make-point 2 4))
(define line-segment (make-segment start end))
(newline)
(display "Midpoint of")
(print-point start)
(display "and ")
(print-point end)
(newline)
(display "is: ")
(print-point (get-midpoint line-segment))

;; Exercise 2.3: Implement a representation for rectangles in a plane.
;constructor - takes two corner points
(define (make-rectangle bottom-left top-right)
  (define left-edge
    (make-segment bottom-left (make-point (car bottom-left) (+ (cdr bottom-left) (cdr top-right)))))
  (define bottom-edge
    (make-segment bottom-left (make-point (+ (car bottom-left) (car top-right)) (cdr bottom-left))))
  ;rectangle is represented as a pair of two line segments
  (cons left-edge bottom-edge))

;helper function to calculate segment length
(define (segment-length segment)
  (let ((p1 (car segment))(p2 (cdr segment)))
    (sqrt (+ (expt (- (car p1) (car p2)) 2) (expt (- (cdr p1)(cdr p2)) 2)))))
;calculate perimeter of rectangle
(define (perimeter rect)
  (let ((seg1 (car rect))(seg2 (cdr rect)))
    (* 2 (+ (segment-length seg1) (segment-length seg2)))))
;calculate area of rectangle
(define (area rect)
  (let ((seg1 (car rect)) (seg2 (cdr rect)))
    (* (segment-length seg1) (segment-length seg2))))

(define rect1 (make-rectangle start end))
(perimeter rect1)
(area rect1)

;; Exercise 2.4.  Here is an alternative procedural representation of pairs. For this representation, verify that (car (cons x y)) yields x for any objects x and y.

;(define (cons x y)
;  (lambda (m) (m x y)))
;
;(define (car z)
;  (z (lambda (p q) p)))

;do the substitution z -> lambda (m) (m x y)
; (car (cons x y)) -> ( ((lambda (m) (m x y)) (lambda (p q) p))
;-> ( (lambda (p q) p) x y) -> x
; neat!

;What is the corresponding definition of cdr?

;same idea but return q instead of p

;(define (cdr z)
;  (z (lambda (p q) q)))

;; Exercise 2.5. Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations if we represent the pair a and b as the integer that is the product (2^a)(3^b). Give the corresponding definitions of the procedures cons, car, and cdr.

;what...?

(define (alt-cons a b)
  (* (expt 2 a) (expt 3 b)))

;how can we get a? probably a math trick.
;can't just divide out 3^b because we don't know b...

;chatgpt says we can use the fact 2 and 3 are prime.
; think of the number as 2*2*2*2...(a times)...*3*3*3*3...(b times)
;divide the number repeatedly by 2 or 3 until there are no more 2's or 3's left to find a or b

(define (alt-car x)
  (define (iter i current)
    ;if there's at least one 2 in there keep going
    (if (= 0 (remainder current 2))
        (iter (inc i) (/ current 2))
        ;if there's no more 2's return i
        i))
  (iter 0 x))

;same idea for cdr, but divide by 3 and check for even remainder

(define (alt-cdr x)
  (define (iter i current)
    (if (= 0 (remainder current 3))
        (iter (inc i) (/ current 3))
        i))
  (iter 0 x))

;test
(define test-cons (alt-cons 5 11))
(alt-car test-cons);5
(alt-cdr test-cons);11

;; Exercise 2.6. Church numerals 
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;I don't fully understand but I'll sub in zero to add-1 to get one

;(lambda (f) (lambda (x) x))
;(n f) -> (lambda (x) x)
;so (add-1 zero) should be the 'one' function
(lambda (f) (lambda (x) (f ((lambda (z) z) x))))
;when we feed x into that identity lambda we just get x
(define one
  (lambda (f) (lambda (x) (f x))))

;church to int for testing
(define (church-to-int n)
  ((n (lambda (x) (+ x 1))) 0))
;testing zero and one
(church-to-int zero)
(church-to-int (add-1 zero))
(church-to-int one)

;define two using the same kind of substitution
;(add-1 one)->
(lambda (f) (lambda (x) (f ((one f) x))))
;(one f) is just (lambda (x) (f x)). and we are passing in x to that procedure, so we can simplify
(lambda (f) (lambda (x) (f (f x))))
;and that should be 'two'. so we can give it a name
(define two (lambda (f) (lambda (x) (f (f x)))))
(display "church two: ")(church-to-int two)

;define + without using repeated add-1
(define (church+ n1 n2)
  (lambda (f)
    (lambda (x)
     ((n2 f) ((n1 f) x)))))
;try it
(display "church 1+0: ")(church-to-int (church+ one zero))
(display "church 1+1: ")(church-to-int (church+ one one))
(display "church 2+2: ")(church-to-int (church+ two two))
(display "church 0+0: ")(church-to-int (church+ zero zero))
;lfg!