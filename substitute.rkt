#lang racket
(require racket/match)

; The substitution of a term for a free variable is term[var --> value]
(define (substitute term var value)
  (match term
    ; Primitive values
    [(? string?) term]
    [(? number?) term]
    ; Variable: replace all the free occurences of var in term with value IF var ≠ term
    ; term[var -> value]            = term
    [(? symbol?)                     (if (not (eq? var term))
                                         term
                                         value)]

    ; Abstraction: replace all free occurences of var in (λ v . body) with value IF var ≠ v && var∉FV(value).
    ; Assume that vars passed will not be a FV of value in this implementation.  It will be handled elsewhere
    ; (λv.body)[var -> value]      = λv.(body[var --> value])
    [`(λ (,v) ,body)                 (if (not (eq? v var))
                                         `(λ (,v) ,(substitute body var value))
                                         term)]
   
    ; Application: replace all free occurences of var in rator and rand with value
    ; [rator rand][var -> value]   = (rator[var --> value] rand[var --> value])
    [`(,rator ,rand)                `(,(substitute rator var value) ,(substitute rand var value))]))

(provide substitute substitute)