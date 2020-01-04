#lang racket

(require rackunit)
(require "types.rkt" "valid-problem.rkt")

; problem with no variables or no constraints
(check-equal? (valid-problem? (problem (hash) (hash))) #t)

; one variable, one constraint
(check-equal? 
 (valid-problem?
  (make-problem '((a . (1))) (list (cons '(a) (list (lambda (x) (equal? x 1)))))))
 #t)

#|

; one variable, mulitple constraints
(check-equal? 
 (valid-problem?
  (make-problem '((a . (1))) '(((a) . ((lambda (x) (equal? x 1))))
                               ((a) . ((lambda (x) (not (equal? x 2)))))
                               ((a) . ((lambda (x) (not (equal? x 3))))))))
 #t)

; multiple variables, one constraint
(check-equal? 
 (valid-problem?
  (make-problem '((a . (1 2)) (b . (2 3)))
                '(((a b) . ((lambda (x y) (equal? x y)))))))
 #t)

; multiple variables, multiple constraints
(check-equal? 
 (valid-problem?
  (make-problem '((a . (1 2)) (b . (2 3)))
                '(((a b) . ((lambda (x y) (equal? x y))))
                  ((b) . ((lambda (y) (not (equal? y 3)))))
                  ((a) . ((lambda (x) (not (equal? x 9))))))))
 #t)

|#
;;;