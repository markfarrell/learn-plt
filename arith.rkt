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

(: step (-> Term Term))
(define (step term)
  (match term 
    [(If-Then-Else t1 t2 t3)
     (match t1
       [(True-Term) t2]
       [(False-Term) t3]
       [_ (If-Then-Else (step t1) t2 t3)])]
    [(Succ t1)
     (Succ (step t1))]
    [(Pred t1)
     (match t1
       [(Zero-Term) 
        (Zero-Term)]
       [(Succ t2) t2]
       [_ (Pred (step t1))])]
    [(Is-Zero t1)
     (match t1
       [(Succ _)
        (False-Term)]
       [(Zero-Term)
        (True-Term)]
       [_ (Is-Zero (step t1))])]
    [_ (error "No rule applies.")]))

(: evaluate (-> Term Term))
(define (evaluate term)
  (with-handlers ([exn:fail? (lambda ([e : exn:fail]) term)])
    (evaluate (step term))))

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
        (Nat)]
       [_ (error "Type mismatch.")])]
    [(Is-Zero n)
     (match (type-check n)
       [(Nat)
        (Bool)]
       [_ (error "Type mismatch.")])]
    [_ (error "No rule applies.")]))