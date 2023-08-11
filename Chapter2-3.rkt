#lang sicp

;check if symbol is in list
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;;Exercise 2.54.  Two lists are said to be equal? if they contain equal elements arranged in the same order. Define equal? recursively
(define (equal?-v2 a b)
  (cond
    ;base case where a or b is null
    ((or (null? a) (null? b)) (and (null? a) (null? b)))
    ;base case where a is not a list
    ((or (not (pair? a)) (not (pair? b)))
                         (eq? a b))
              
    ;if a is a list, and b is a list, recursively call on car and cdr
    (else 
     (and (equal?-v2 (car a) (car b))
          (equal?-v2 (cdr a) (cdr b))))))
(equal?-v2 'a 'a)
(equal?-v2 'a 'b)
(equal?-v2 '(this is a list) '(this is a list))
(equal?-v2 '(this is a list) '(this (is a) list))

;;Exercise 2.55 -Eva Lu Ator types to the interpreter the expression
(car ''abracadabra)
;equivalent to
(car (quote (quote abracadabra)))


;symbolic differentiation
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond
    ;if either number is 0 -> 0
    ((or (=number? m1 0) (=number? m2 0)) 0)
    ;if one of them is 1 -> return the other
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ;both numbers-> multiply them
    ((and (number? m1) (number? m2)) (* m1 m2))
    ;at least one is a symbol so just symbolically multiply them
    (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))




;;Exercise 2.56.  Show how to extend the basic differentiator to handle more kinds of expressions. For instance, implement the differentiation rule d/dx u^n = n*u^n-1 *du/dx

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponent? exp)
         (make-product (exponent exp)
                       (make-exponent var (subtract (exponent exp) 1))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (exponent? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exp)
  (car (cdr exp)))
(define (exponent exp)
  (caddr exp))
(define (make-exponent base exponent)
  (cond ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))
(define (subtract a1 a2)
  (make-sum a1 (- a2)))
