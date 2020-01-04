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

(define (all-func? func items)
  (all (map func items)))

; Returns if given list of items consists of all symbols.
; items : List of items
; Returns : Boolean
(define (all-symbols? items)
  (all-func? symbol? items))

(define (all-procedures? items)
  (all-func? procedure? items))

; Returns if the variable attribute of the given problem is valid, i.e. does it
; consist of a mapping of symbols to list of integers.
; problem : Problem
; Return : Boolean
(define (valid-variables-attribute? problem)
  (local (; Checks that the domain of the given variable only consists of
          ; integers.
          ; domain : List of Integer
          ; Return : Boolean
          (define (variable-domain-integers? domain)
            (all (map integer? domain)))

          (define (valid-domains? domains)
            (all (map variable-domain-integers? domains))))
    (and (all-symbols? (hash-keys (problem-variables problem)))
         (valid-domains? (hash-values (problem-variables problem))))))

; Returns if the constraint attribute of the given problem is valid, i.e. is it
; a mapping of List of symbols to functions.
; problem : Problem
; Return : boolean
(define (valid-constraints-attribute? problem)
  (and (all (map all-symbols? (hash-keys (problem-constraints problem))))
       (all (map all-procedures? (hash-values (problem-constraints problem))))
       ))

; Checks that for each constraint of the given problem, number of variables
; assigned to that constraint match the number of arguments expected by its
; associated function.
; problem : Problem
; Return : Boolean
(define (matching-constraints-arity? problem)
  (local (; Checks that the expected number of variables matches the expected
          ; number of arguments of the given function.
          ; constraint : Constraint
          ; Return : Boolean
          (define (verify-constraint num-of-vars constraint)
            (equal? num-of-vars
                    (procedure-arity constraint)))

          ; constraints : List of Pairs
          ; Return : Boolean
          (define (verify-constraints-helper constraints)
            (if (empty? constraints)
                #t
                (let* ([cur-constraint (first constraints)]
                   [num-of-vars (length (first cur-constraint))]
                   [constraints (rest cur-constraint)])
              (cond
                [(all (map (lambda (x) (verify-constraint num-of-vars x))
                           constraints))
                 (verify-constraints-helper (rest constraints))]
                [else (raise-argument-error 'mismatching-constraint-arity
                                            ""
                                            cur-constraint)])))))
          (verify-constraints-helper
           (hash->list (problem-constraints problem)))))

; Checks that for the given problem, the variables assigned in the constraints
; of this problem do not mention a variable not assigned to the problem.
; Possible for a variable to not be mentioned in a constraint, but no vice
; versa.
; problem : Problem
; Return : Boolean
(define (variable-overlap? problem)
  (let* ([assigned-variables
          (list->set (hash-keys (problem-variables problem)))]
         [constraint-variables
          (apply append
                 (hash-keys (problem-constraints problem)))]
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

; For the given problem, for each associated constraint, returns if variables
; are unique.
; problem : Problem
; Return : boolean
(define (unique-constraint-variables? problem)
  (all (map unique-list? (hash-keys (problem-constraints problem)))))

; Verifies that the given problem is valid, i.e. that is meets the following
; criteria:
; - variables attibute is mapping of symbols to domains
; - constraints attribute is mapping of a list of integers to a procedure
; - variables assigned to given constraints do not contain a variable not
;   assigned to the problem, under variable attribute
; - variable names assigned to problem are unique (implicit)
; - variables assigned to each constraint are unique
; - that the number of variables assigned to each constraint matches the number
;   of arguments that function accepts
; - each variable only has integers in its domain (under variables attribute)
; problem : Problem
; Return : Boolean
(define (valid-problem? problem)
  (and (valid-variables-attribute? problem)
       (valid-constraints-attribute? problem)
       (matching-constraints-arity? problem)
       (variable-overlap? problem)
       (unique-constraint-variables? problem)
       ))