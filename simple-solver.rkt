#lang racket
(require "types.rkt" "valid-problem.rkt")
(require racket/set)

(define (complete-solution? problem solution)
  (equal? (list->set (hash-keys solution))
          (list->set (hash-keys (problem-variables problem)))))
       
(define (assign-new-variable problem solution)
  (first
   (sort (set->list
          (set-remove (list->set (hash-keys problem))
                      (list->set (hash-keys solution))))
         (λ (x y) (< (length (hash-ref (problem-variables problem) x))
                     (length (hash-ref (problem-variables problem) y)))))))

(define (try-against-constraints solution constraints)
  (if (empty? constraints)
      #t
      (let* ([cur-constraint-set (first constraints)]
             [cur-vars (first cur-constraint-set)]
             [cur-constraints (rest cur-constraint-set)]
             [cur-constraint-values (map (λ (x) (hash-ref solution x))
                                         cur-vars)])
        ; either current solution doesn't have all variables for current
        ; constraint set, or solution can be successfully be applied to 
        (if (or (not (all (map (λ (x) (hash-has-key? solution x)) cur-vars)))
                (all (map
                      (λ (constraint) (apply constraint cur-constraint-values)
                        cur-constraints))))
            (try-against-constraints solution (rest constraints))
            #f))))

(define (remove-first-domain-value problem var)
  (problem
   (hash-set (problem-variables problem) var
             (rest (hash-ref (problem-variables problem) var)))
   (problem-constraints problem)))

(define (try-variables problem solution new-var)
  ; out of values to try
  (if (empty? (hash-ref (problem-variables problem) new-var)) 
      #f
      (let* ([new-value (first (hash-ref (problem-variables problem) new-var))]
             [new-problem (remove-first-domain-value problem new-var)]
             [new-solution (hash-set solution new-var new-value)])
        (if (try-against-constraints new-problem new-solution)
            (simple-solver-with-solution new-problem new-solution)
            (try-variables new-problem solution new-var)))))
        
; get next var, try against constraints, back track

(define (simple-solver-with-solution problem solution)
  (if (complete-solution? problem solution)
      solution
      (try-variables problem solution (assign-new-variable problem solution))))

(define (simple-solver problem)
  (if (valid-problem? problem)
      (simple-solver-with-solution problem (hash))
      (raise-argument-error
       'valid-problem-failed
       "given problem is invalid, but a more specific error was not raised"
       problem)))