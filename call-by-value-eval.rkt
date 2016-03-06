#lang racket
(require "substitute.rkt")

(define (cbv-reduce term)
  (match term
      ; Primitive values
      [(? string?) term]
      [(? number?) term]
      [(? symbol?) term]

      ; Identity
      [`(λ (,v) ,v) term]

      ; Application: there are two scenarios to account for: the lhs or rhs are b-redexes.
      ; (reducible-term1      reducible-term2)             =  (reducible-term1 reduced-term2)
      [`((λ (,v) ,b)         ,e)                              (substitute b v (cbv-reduce e))]

      ; (reducible-term1      reduced-term2)               =  (reduced-term1    reduced-term2) ==> evaluated-term
      [`(,f                   ,e)                            `(,(cbv-reduce f)  ,e)]
    ))

(define (recursive-eval term)
  (if (equal? term (cbv-reduce term))
              term
              (recursive-eval (cbv-reduce term))))


;TESTS for eval
;(recursive-eval `((λ(y) (y a)) (λ(x)x)    ))               ; 'a
;(recursive-eval `(λ(x)x) )                                 ;'(λ (x) x)
(recursive-eval `((λ(x)x)   y))                               ; 'y
;(recursive-eval `((λ(x)x) (λ(x)x)) )                       ;'(λ (x) x)
;(recursive-eval `((λ(y) (y a)) (λ(x)x)    ))               ;'a
;(recursive-eval '((λ (x) x) a))                            ;'a
;(recursive-eval `( (λ (x) (x x)) (λ (y) y) )  )            ;'(λ (y) y)
;(recursive-eval `((λ(f) (f 7)) (λ (y) y)))                 ;7
;(recursive-eval `((λ(f) (f 7)) ((λ (x) (x x)) (λ (y) y)))) ;7
;(recursive-eval `((λ(f) (f 7)) (λ(z) z)))                  ;7
;(recursive-eval `((λ(x)(x x)) (λ(x)(x x))))
