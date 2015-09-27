#lang racket
(require "substitute.rkt")

(define (eval-term term)
  (match term
      ; Primitive values
      [(? string?) term]
      [(? number?) term]
      [(? symbol?) term]

      ; Identity
      [`(λ (,v) ,v) term]

      ; Application
      [`((λ (,v1) ,b1) ,e)  (reduce-application (cons `(λ (,v1) ,b1) (cons (eval-term e) '() )))]

      ; Application-abstraction
      [`(,f ,e) (eval-term (cons (eval-term f) (cons (eval-term e) '() )))]
    ))

(define (reduce-application term)
  (match term
      ; Application: there are two scenarios to account for: the lhs or rhs are b-redexes.
  
      ; (reducible-term1            reducible-term2)      = (reducible-term1 reduced-term2)
      [`((λ (,v1) ,b1),   (and rhs `(λ (,v2) ,b2)))         (eval-term (substitute b1 v1 rhs))]
   
      ; (reducible-term1  reduced-term2)                  = (reduced-term1 reduced-term2) ==> evaluated-term
      [`((λ (,v) ,b)      ,e)                               (substitute b v e)]))


;TESTS for eval
(eval-term `((λ(y) (y a)) (λ(x)x)    ))
(eval-term `(λ(x)x) )
(eval-term `((λ(x)x)y))
(eval-term `((λ(x)x) (λ(x)x)) )
(eval-term `((λ(y) (y a)) (λ(x)x)    ))
(eval-term '((λ (x) x) a))
(eval-term `( (λ (x) (x x)) (λ (y) y) )  )
(eval-term `((λ(f) (f 7)) (λ (y) y)))
(eval-term `((λ(f) (f 7)) ((λ (x) (x x)) (λ (y) y)) ))
(eval-term `((λ(f) (f 7)) (λ(z) z)))
;(eval-term `((λ(x)(x x)) (λ(x)(x x))))
