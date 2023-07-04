#lang sicp
(define (print-stars n)
  (cond
    ((> n 1)
     (begin
       (display "* ")
       (print-stars (- n 1))))
    ((= n 1)
     (display "*"))))

(define (print-stripes n)
  (cond
    ((> n 0)
     (begin
       (display "-")
       (print-stripes (- n 1))))))

(define (print-flag)
  (let loop ((n 13))
    (cond
      ((> n 0)
       (begin
         (cond
           ((> n 9)
            (begin
              (print-stars 6)
              (display " ")
              (print-stripes 21)))
           ((> n 7)
            (begin
              (print-stars 6)
              (display " ")
              (print-stripes 21)))
           (else
            (print-stripes 33))))
         (newline)
         (loop (- n 1))))))

(print-flag)
