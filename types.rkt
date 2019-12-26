#lang racket
(require racket/set)
(require racket/local)

; Returns if the given list of booleans is all true.
; bools : Listof Boolean
; Return : Boolean  
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
       (unique-constraint-variables? problem))

; Creates a variable with the given name whose domain is the range from start
; to end.
; name : Symbol
; start : Integer
; end : Integer
; Return : Variable
(define (variable-from-range name start end)
  (variable name (range start (add1 end))))

(define foo (problem (list (variable 'a '(1 2 3)) (variable 'b '(1 2 3)))
                     (list (constraint '(a b c) (lambda (x z y) #t))
                           (constraint '(a b) (lambda (x y) #t)))))

(valid-problem? foo)
