#lang racket

(require rackunit)
(require "types.rkt" "valid-problem.rkt")

(define (check-true? test)
  (check-equal? test #t))

; problem with no variables or no constraints
(check-true? (valid-problem? (problem '() '())))