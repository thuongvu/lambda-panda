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
       [`((λ (,v1) ,b1) ,e)  (reduce-application `(,(car term) ,(eval-term e)))]

      ; Application-abstraction
      [`(,f ,e) `(,(eval-term f) ,e)]
    ))

(define (reduce-application term)
  (match term
      ; Application: there are two scenarios to account for: the lhs or rhs are b-redexes.
  
      ; (reducible-term1            reducible-term2)      = (reducible-term1 reduced-term2)
      [`((λ (,v1) ,b1),   (and rhs `(λ (,v2) ,b2)))         (eval-term (substitute b1 v1 rhs))]
   
      ; (reducible-term1  reduced-term2)                  = (reduced-term1 reduced-term2) ==> evaluated-term
      [`((λ (,v) ,b)      ,e)                               (substitute b v e)]
    ))


;TESTS for eval
(eval-term `((λ(y) (y a)) (λ(x)x)    )) ; 'a
(eval-term `(λ(x)x) ) ;'(λ (x) x)
(eval-term `((λ(x)x)y)) ; 'y
(eval-term `((λ(x)x) (λ(x)x)) ) ;'(λ (x) x)
(eval-term `((λ(y) (y a)) (λ(x)x)    )) ;'a
(eval-term '((λ (x) x) a)) ;'a
(eval-term `( (λ (x) (x x)) (λ (y) y) )  ) ;'(λ (y) y)
(eval-term `((λ(f) (f 7)) (λ (y) y))) ;7
(eval-term `((λ(f) (f 7)) ((λ (x) (x x)) (λ (y) y)) )) ;7
(eval-term `((λ(f) (f 7)) (λ(z) z))) ;7
;(eval-term `((λ(x)(x x)) (λ(x)(x x))))
