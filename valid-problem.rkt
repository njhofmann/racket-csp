#lang racket
(provide valid-problem?)
(require racket/set)
(require racket/local)
(require "types.rkt")

; Returns if every item in the given list returns true for the given function.
; Function must take in one argument and return a boolean.
; func : Any -> Boolean
; items : Listof Any
(define (all-func? func items)
  (all (map func items)))

; Returns if the given list of booleans is all true.
; bools : Listof Boolean
; Return : Boolean  
(define (all bools)
  (foldr (λ (x y) (and x y)) #t bools))

; Returns if given list of items consists of all symbols, else throws an error.
; items : Listof Any
; Returns : Boolean
(define (all-symbols? items)
  (if (all-func? symbol? items)
      #t
      (raise-argument-error 'not-all-symbols
                            "not all items in given list are symbols"
                            items)))

; Returns if given list of items consists of all procedures, else throws an
; error.
; items : Listof Any
; Returns : Boolean
(define (all-procedures? items)
  (if (all-func? procedure? items)
      #t
      (raise-argument-error 'not-all-procedures
                            "not all items in given list are procedures"
                            items)))

; Returns if the variable attribute of the given problem is valid, i.e. does it
; consist of a mapping of symbols to list of integers.
; problem : Problem
; Return : Boolean
(define (valid-variables-attribute? problem)
  (local (; Checks that the domain of the given variable only consists of
          ; integers, else throws an error.
          ; domain : List of Integer
          ; Return : Boolean
          (define (variable-domain-integers? domain)
            (if (all (map integer? domain))
                #t
                (raise-argument-error 'non-integer-variable-domain
                                      "variable domain contains non integers"
                                      domain)))

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
       (all (map all-procedures? (hash-values (problem-constraints problem))))))

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
                    [(all (map (λ (x) (verify-constraint num-of-vars x))
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
          (filter (λ (x) (not (set-member? assigned-variables x)))
                  constraint-variables)])
    (if (empty? unassigned-constraint-vars)
        #t
        (raise-argument-error
         'unassigned-variables
         "constraints have variable(s) not assigned to problem"
         unassigned-constraint-vars))))

; For the given problem, for each associated constraint, returns if variables
; are unique.
; problem : Problem
; Return : boolean
(define (unique-constraint-variables? problem)
  (local (; Returns if the given list of items has no duplicate items, i.e. is
          ; it a set, else throws an error.
          ; items : List of T
          ; Return : boolean
          (define (unique-list? items)
            (if (equal? (length items) (length (set->list (list->set items))))
                #t
                (raise-argument-error 'duplicate-items
                                      "duplicate items in the given list"
                                      (get-dups (sort items <) '()))))

          ; Returns all items that appear more than once in the given list.
          ; List must be sorted.
          ; items : Listof T
          ; dups : Listof T
          ; Return : Listof T
          (define (get-dups items dups)
            (cond
              [(empty? items) dups]
              [(equal? (length items) 1) dups]
              [(and (equal? (first items) (second items))
                    (or (empty? dups)
                        (not (equal? (first items) (first dups)))))
               (get-dups (rest (rest items)) (cons (first items) dups))]
              [else (get-dups (rest items) dups)])))      
    (all (map unique-list? (hash-keys (problem-constraints problem))))))       

; Verifies that the given problem is valid, i.e. that is meets the following
; criteria, a specific error should be thrown if a criteria isn't met:
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
       (unique-constraint-variables? problem)))