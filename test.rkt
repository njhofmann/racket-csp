#lang racket

(require "types.rkt" "simple-solver.rkt")

(define prob (make-problem
              '((a . (1 2 3 4 5 6))
                (b . (3 4 5 6 7 8)))
              (list (cons '(a b) (list (λ (x y) (equal? (+ x y) 9))))
                    (cons '(a) (list (λ (x) (> x 2)))))))


(simple-solver prob)