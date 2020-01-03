#lang racket
(provide valid-problem?)
(require racket/set)
(require racket/local)
(require "types.rkt")

; Returns if the given list of booleans is all true.
; bools : Listof Boolean
; Return : Boolean  
(define (all bools)
  (cond
    [(empty? bools) #t]
    [(equal? (first bools) #f) #f]
    [else (all (rest bools))]))

; Checks that the domains of the variables of the given problem consist only of
; integers.
; problem : Problem
; Return : Boolean
(define (variable-domains-integers? problem)
  (local (; Checks that the domain of the given variable only consists of
          ; integers.
          ; variable : Variable
          ; Return : Boolean
          (define (variable-domain-integers? variable)
            (all (map integer? (variable-domain variable))))

          (define (verify-variables-helper variables)
            (cond
              [(empty? variables) #t]
              [(variable-domain-integers? (first variables))
               (verify-variables-helper (rest variables))]
              [else (raise-argument-error 'mismatching-variables-arity
                                          ""
                                          (first variables))])))
    (verify-variables-helper (problem-variables problem))))


; Checks that for each constraint of the given problem, number of variables
; assigned to that constraint match the number of arguments expected by its
; associated function.
; problem : Problem
; Return : Boolean
(define (matching-constraints-arity? problem)
  (local (; Checks that the number of variables assigned to the given constraint
          ; match the number of expected arguments of its associated function.
          ; constraint : Constraint
          ; Return : Boolean
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
                                          (first constraints))])))
    (verify-constraints-helper (problem-constraints problem))))

; Checks that for the given problem, the variables assigned in the constraints
; of this problem do not mention a variable not assigned to the problem.
; Possible for a variable to not be mentioned in a constraint, but no vice
; versa.
; problem : Problem
; Return : Boolean
(define (variable-overlap? problem)
  (let* ([assigned-variables
          (list->set (map variable-name (problem-variables problem)))]
         [constraint-variables
          (apply append
                 (map constraint-variables (problem-constraints problem)))]
         [unassigned-constraint-vars
          (filter (lambda (x) (not (set-member? assigned-variables x)))
                  constraint-variables)])
    (if (empty? unassigned-constraint-vars)
        #t
        (raise-argument-error
         'unassigned-variables
         "constraints have variable(s) not assigned to problem"
         unassigned-constraint-vars))))

; Returns if the given list of items has no duplicate items, i.e. is it a set.
; items : List of T
; Return : boolean
(define (unique-list? items)
  (equal? (length items) (length (set->list (list->set items)))))

; Returns if the variables in the given problem are unique.
; problem : Problem
; Return : boolean
(define (unique-variables? problem)
  (unique-list? (problem-variables problem)))

; For the given problem, for each associated constraint, returns if variables
; are unique.
; problem : Problem
; Return : boolean
(define (unique-constraint-variables? problem)
  (all (map unique-list?
            (map constraint-variables (problem-constraints problem)))))

; Verifies that the given problem is valid, i.e. that is meets the following
; criteria:
; - variables assigned to given constraints do not contain a variable not
;   assigned to the problem
; - variable names assigned to problem are unique
; - variables assigned to each constraint are unique
; - that the number of variables assigned to each constraint matches the number
;   of arguments that function accepts
; - each variable only has integers in its domain.
; problem : Problem
; Return : Boolean
(define (valid-problem? problem)
  (and (variable-domains-integers? problem)
       (matching-constraints-arity? problem)
       (variable-overlap? problem)
       (unique-variables? problem)
       (unique-constraint-variables? problem)))

; Creates a variable with the given name whose domain is the range from start
; to end.
; name : Symbol
; start : Integer
; end : Integer
; Return : Variable
(define (variable-from-range name start end)
  (variable name (range start (add1 end))))
