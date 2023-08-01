#lang sicp
;; Exercise 2.17 - Define a procedure last-pair that returns the list that contains only the last element of a given (nonempty) list:
(define (last-pair x)
  (define (iter remaining prev)
    (if (null? remaining)
        prev
        (iter (cdr remaining) remaining)))
  (iter x x))

;; Exercise 2.18.  Define a procedure reverse that takes a list as argument and returns a list of the same elements in reverse order:
(define (reverse-list x)
  (define (iter remaining result)
    (if (null? remaining)
        result
        (iter (cdr remaining) (cons (car remaining) result))))
  (iter x nil))

;; Exercise 2.19. Define the procedures first-denomination, except-first-denomination, and no-more? in terms of primitive operations on list structures.
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? x)
  (if (null? x)
      #t
      #f))
(define (first-denomination x)
  (car x))
(define (except-first-denomination x)
  (cdr x))
;; Exercise 2.20 - write a procedure same-parity that takes one or more integers and returns a list of all the arguments that have the same even-odd parity as the first argument

(define (same-parity x . y)
  (define (collect-type z result test)
    (if (null? z)
        (reverse result)
        (if (test (car z))
            (collect-type (cdr z) (cons (car z) result) test)
            (collect-type (cdr z) result test))))
  (if (even? x)
      (collect-type y (list x) even?)
      (collect-type y (list x) odd?)))
(same-parity 2 3 4 5 6 7)
(same-parity 1 2 3 4 5 6 7)

;; Exercise 2.21 -implement two versions of square-list
(define (square x)
  (* x x))
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))
(define (square-list2 items)
  (map square items))
(square-list (list 2 3 4 5 6 7))
(square-list2 (list 2 3 4 5 6 7))

;; Exercise 2.22
; the list is reversed becase we are pre-pending the items onto the new list as we go.
; the second solution won't work either because it is pre-pending the solution list onto the current value. i think it would be a bunch of nested lists?

;; Exercise 2.23
;could this be done without begin?
(define (for-each2 proc items)
  (if (not (null? items))
      (begin
        (proc (car items))
        (for-each2 proc (cdr items)))))
(for-each2 (lambda (x) (newline) (display x))
          (list 57 321 88))

;; Exercise 2.24
(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(list 1 (list 2 (list 3 4)))
; /\
;1  /\
;  2  /\
;    3  4

;; Exercise 2.25 - pick 7 from the following
(define a (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr a)))))
;(car (cdaddr a)) you can use this shortcut for up to 4 cxr operations

(define b (list (list 7)))
(car (car b))

(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car(cdr(car (cdadar(cdr (car (cdr (car (cdr c))))))))) ;had to kind of brute force this one one step at a time

;; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
; (append x y)
; (1 2 3 4 5 6)
; 
; (cons x y)
; ((1 2 3) 4 5 6);kind of surprising/confusing
; 
; (list x y)
; ((1 2 3) (4 5 6))


;; Exercise 2.27
; (define (reverse-list x)
;   (define (iter remaining result)
;     (if (null? remaining)
;         result
;         (iter (cdr remaining) (cons (car remaining) result))))
;   (iter x nil))

(define (deep-reverse x)
  (cond
    ((null? x) nil)
    ((pair? x) (append (deep-reverse (cdr x)) (list (deep-reverse (car x)))))
    (else x)))

(define z (list (list 1 2) (list 3 4)))
(deep-reverse z)

;; Exercise 2.28. Write a procedure fringe that takes as argument a tree (represented as a list) and returns a list whose elements are all the leaves of the tree arranged in left-to-right order

(define (fringe x)
  (cond
    ((null? x) nil)
    ;note: a leaf is a pair where (car x) is the value and (cdr x) is nil
    ((pair? x) (append (fringe (car x)) (fringe (cdr x))))
    (else (list x))))
(fringe z)
(fringe (list z z))

;; Exercise 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
;skipped

;; Exercise 2.30


(define (square-tree tree)
  (cond ((null? tree) nil)
        ((pair? tree) (cons (square-tree (car tree)) (square-tree (cdr tree))))
        (else (square tree))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
(square-tree-map
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;; Exercise 2.31 Abstract your answer to exercise 2.30 to produce a procedure tree-map with the property that square-tree could be defined as

(define (tree-map proc items)
  (cond ((null? items) nil)
        ((pair? items) (cons (tree-map proc (car items)) (tree-map proc (cdr items))))
        (else (proc items))))
(define (square-tree2 tree) (tree-map square tree))

;; Exercise 2.32

