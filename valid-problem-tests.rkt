#lang racket

(require rackunit)
(require "types.rkt" "valid-problem.rkt")

; problem with no variables or no constraints
(check-equal? (valid-problem? (problem (hash) (hash))) #t)

; one variable, one constraint, one constraint per variable
(check-equal? 
 (valid-problem?
  (make-problem '((a . (1)))
                (list (cons '(a) (list (λ (x) (equal? x 1)))))))
 #t)

; one variable, mulitple constraints, one constraint per variable
(check-equal? 
 (valid-problem?
  (make-problem '((a . (1)))
                (list
                 (cons '(a) (list (λ (x) (equal? x 1))))
                 (cons '(a)  (list (λ (x) (not (equal? x 2)))))
                 (cons '(a)  (list (λ (x) (not (equal? x 3))))))))
 #t)

; multiple variables, one constraint, one constraint per variable
(check-equal? 
 (valid-problem?
  (make-problem '((a . (1 2)) (b . (2 3)))
                (list (cons '(a b) (list (λ (x y) (equal? x y)))))))
 #t)

; multiple variables, multiple constraints, one constraint per variable
(check-equal? 
 (valid-problem?
  (make-problem '((a . (1 2)) (b . (2 3)))
                (list (cons '(a b) (list (λ (x y) (equal? x y))))
                      (cons '(b) (list (λ (x) (not (equal? x 3)))))
                      (cons '(a) (list (λ (x) (not (equal? x 9))))))))
 #t)