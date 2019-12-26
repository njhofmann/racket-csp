#lang racket
(require racket/set)

; Returns if the given list of booleans is all true.
; bools : Listof Boolean
(define (all bools)
  (cond
    [(empty? bools) #t]
    [(equal? (first bools) #f) #f]
    [else (all (rest bools))]))

; Defines a variable for CSP problem, consisting of a unique identifier and a
; list of all valid values that domain may take.
; name : Symbol - unique ID
; domain : Listof Integer 
(struct variable (name domain))

; Defines a constraint for a CSP, consisting of a list of variables to be
; applied to the associated constraint.
; variables : Listof Symbol 
; constraint: function taking in some number of associated Integers, returning
;             a boolean
(struct constraint (variables function))

; Defines a CSP from a given set of variables and constraints.
; variables : Listof Variable
; constraints : Listof Constraints
(struct problem (variables constraints))


; Checks that the domain of the given variable only consists of integers.
; variable : Variable
(define (verify-variable variable)
  (all (map integer? (variable-domain variable))))

; abstract out
(define (verify-variables-helper variables)
  (cond
    [(empty? variables) #t]
    [(verify-variable (first variables))
     (verify-variables-helper (rest variables))]
    [else (raise-argument-error 'mismatching-variables-arity
                                ""
                                (first variables))]))

(define (verify-variables problem)
  (verify-variables-helper (problem-variables problem)))


; Checks that the number of variables assigned to the given constraint match
; the number of expected arguments by the associated function.
(define (verify-constraint constraint)
  (equal? (length (constraint-variables constraint))
                  (procedure-arity (constraint-function constraint))))


(define (verify-constraints-helper constraints)
  (cond
    [(empty? constraints) #t]
    [(verify-constraint (first constraints))
     (verify-constraints-helper (rest constraints))]
    [else (raise-argument-error 'mismatching-constraint-arity
                                ""
                                (first constraints))]))

(define (verify-constraints problem)
  (verify-constraints-helper (problem-constraints problem)))

; Returns if the two given lists have no overlapping elements.
; a : Listof Nothing
; b : Listof Nothing
(define (no-overlap a b)
  (equal? (set a) (set b)))

(define (filter-by-set filter-list set-by)
  (filter (lambda (x) (set-member? set-by x)) filter-list))

(define (set-disjoin a b)
  (let ([set-a (set->list a)]
        [set-b (set->list b)])
    (append (filter-by-set set-a b) (filter-by-set set-b a))))

(define (verify-overlap problem)
  (let ([variable-set (append (map variable-domain
                               (problem-variables problem)))]
        [constraint-set (append (map constraint-variables
                               (problem-constraints problem)))])
    (if (no-overlap variable-set constraint-set)
      #t
      (raise-argument-error 'missing-vars ""
                            (set-disjoin variable-set constraint-set)))))

; Verifies that the given problem is valid, i.e. that the variables match all
; the variables in given constraints, and that the number of variables assigned
; to each constraint matches the number of variables that function takes in, and
; that each variable only has integers in its domain.
; Returns the problem is no issue is detected, else raises a specific issue.
; problem : Problem
(define (verify-problem problem)
  (if (and (verify-variables problem)
           (verify-constraints problem)
           (verify-overlap problem))
      problem
      (raise-argument-error 'invalid-problem "" problem)))  ; should never occur

; Creates a varialble with the given name whose domain is the range from start
; to end.
; name : Symbol
; start : Integer
; end : Integer
(define (variable-from-range name start end)
  (variable name (range start end)))

(define foo (problem (list (variable 'a '(1 2 3)))
                     (list (constraint '(a) (lambda (x) #t)))))

(verify-problem foo)
