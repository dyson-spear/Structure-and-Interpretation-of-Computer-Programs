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
