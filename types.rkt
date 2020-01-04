#lang racket

(provide (struct-out problem) problem make-problem)

; Defines a CSP from a group of variables, a mapping of variable names to
; domains (symbols to list of integers) and constraints, a mapping of
; variables to constraint functions (list of symbols to a function).
; variables : Hashtable of Symbol to List of Integers
; constraints : Hashtable of List of Symbols to Function
(struct problem (variables constraints))

(define (make-problem variables constraints)
  (problem (make-immutable-hash variables) (make-immutable-hash constraints)))

; Creates a variable with the given name whose domain is the range from start
; to end. Variable is a returned as a pair of the symbol and created range.
; name : Symbol
; start : Integer
; end : Integer
; Return : Pair
(define (variable-from-range name start end)
  (name . (range start (add1 end))))