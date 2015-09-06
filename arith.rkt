#lang typed/racket

(require datatype)

(define-datatype Term
  [True-Term ()]
  [False-Term ()]
  [If-Then-Else (Term Term Term)]
  [Zero-Term ()]
  [Succ (Term)]
  [Pred (Term)]
  [Is-Zero (Term)])

(define-datatype Type
  [Nat ()]
  [Bool ()])

(: type-check (-> Term Type))
(define (type-check term)
  (match term 
    [(True-Term) 
     (Bool)]
    [(False-Term) 
     (Bool)]
    [(If-Then-Else t1 t2 t3)
     (let ([type-t1 (type-check t1)]
           [type-t2 (type-check t2)]
           [type-t3 (type-check t3)])
       (cond [(and (Bool? type-t1)
                   (equal? type-t2 type-t3))
              type-t2]
             [else (error "Type mismatch.")]))]
    [(Zero-Term) 
     (Nat)]
    [(Succ n)
     (match (type-check n)
       [(Nat) 
        (Nat)]
       [_ (error "Type mismatch.")])]
    [(Pred n)
     (match (type-check n)
       [(Nat)
        (Nat)])]
    [(Is-Zero n)
     (match (type-check n)
       [(Nat)
        (Bool)]
       [_ (error "Type mismatch.")])]
    [_ (error "No rule applies.")]))

;; (check-equal? (type-check (True-Term)) (Bool))
;; (check-equal? (type-check (False-Term)) (Bool))
;; (check-equal? (type-check (If-Then-Else (True-Term) (True-Term) (False-Term))) (Bool))