#lang racket
(require "types.rkt" "valid-problem.rkt")
(require racket/set)
(provide simple-solver)

; Returns if the given solution is a complete solution to the problem, i.e. does
; it have a value for each variable assigned to the problem.
(define (complete-solution? problem solution)
  (equal? (list->set (hash-keys solution))
          (list->set (hash-keys (problem-variables problem)))))

; Chooses a new variable to add to the given problem and partial solution based
; on which unselected variable has the smallest remaining domain.
; problem : Problem
; solution : Hashtable of Symbol to Integer
; Return : Symbol
(define (assign-new-variable problem solution)
  (first
   (sort (filter (λ (x) (not (set-member? (list->set (hash-keys solution)) x)))
                 (hash-keys (problem-variables problem)))
         (λ (x y) (< (length (hash-ref (problem-variables problem) x))
                     (length (hash-ref (problem-variables problem) y)))))))

; Tries the given solution against the given list of problem constaints. Returns
; if all applicible constraints work with values assigned to the given solution.
; solution : Hashtable of Symbol to Integer
; constraints : Hashtable of Listof Symbol to Procedure
; Return : boolean
(define (try-against-constraints solution constraints)
  (if (empty? constraints)
      #t
      (let* ([cur-constraint-set (first constraints)]
             [cur-vars (first cur-constraint-set)]
             [cur-constraints (rest cur-constraint-set)])
        ; either current solution doesn't have all variables for current
        ; constraint set, or solution can be successfully be applied to 
        (if (or (not (all (map (λ (x) (hash-has-key? solution x)) cur-vars)))
                (all (map (λ (constraint)
                            (apply constraint
                                   (map (λ (x) (hash-ref solution x))cur-vars)))
                          cur-constraints)))
            (try-against-constraints solution (rest constraints))
            #f))))

; For a given problem, removes the first value of the domain of the given
; variable.
; problem : Problem
; var : Symbol
; Return : Poblem
(define (remove-first-domain-value prob var)
  (problem
   (hash-set (problem-variables prob) var
             (rest (hash-ref (problem-variables prob) var)))
   (problem-constraints prob)))

(define (try-variables problem solution new-var)
  ; out of values to try
  (if (empty? (hash-ref (problem-variables problem) new-var)) 
      #f
      (let* ([new-value (first (hash-ref (problem-variables problem) new-var))]
             [new-problem (remove-first-domain-value problem new-var)]
             [new-solution (hash-set solution new-var new-value)])
        (if (try-against-constraints new-solution
                                     (hash->list
                                      (problem-constraints new-problem)))
            (let* ([backtracking-result
                    (simple-solver-with-solution new-problem new-solution)])
              (if (equal? #f backtracking-result)
                  (try-variables new-problem solution new-var)
                  backtracking-result))
            (try-variables new-problem solution new-var)))))
        
; get next var, try against constraints, back track

(define (simple-solver-with-solution problem solution)
  (if (complete-solution? problem solution)
      solution
      (try-variables problem solution (assign-new-variable problem solution))))

; Attempts to solve the given CSP with naive backtracking, returns a solution if
; one is found else false.
; [roblem : Problem
; Return : Hashtable of Symbol to Integer, #f
(define (simple-solver problem)
  (if (valid-problem? problem)
      (simple-solver-with-solution problem (hash))
      (raise-argument-error
       'valid-problem-failed
       "given problem is invalid, but a more specific error was not raised"
       problem)))