#lang racket

(provide (struct-out variable) (struct-out constraint) (struct-out problem))

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