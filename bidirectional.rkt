#lang typed/racket

(require datatype)

(define-type Name Symbol)

(define-datatype Term
  [If-Then-Else (Term Term Term)]
  [True-Term ()]
  [False-Term ()]
  [Var (Name)]
  [Lam (Name Term)]
  [App (Term Term)]
  [Ann (Term Type)])

(define-datatype Type
  [Bool ()]
  [Function-Type (Type Type)])

(define-type Typing-Context (HashTable Name Type))

(: type-infer (-> Typing-Context Term Type))
(define (type-infer typing-context term)
  (match term
    [(True-Term) 
     (Bool)]
    [(False-Term)
     (Bool)]
    [(Var x)
     (hash-ref typing-context x)]
    [(Ann x t)
     (type-check typing-context term t)]
    [(App t1 t2)
     (match (type-infer typing-context t1)
       [(Function-Type ty1 ty2)
        (type-check typing-context t2 ty1)]
       [_ (error "Type mismatch.")])]
    [_ (error "No rule applies.")]))

(: type-check (-> Typing-Context Term Type Type))
(define (type-check typing-context term ty)
  (match term
    [(If-Then-Else t1 t2 t3)
     (let ([ty1 (type-check typing-context t1 ty)]
           [ty2 (type-check typing-context t2 ty)]
           [ty3 (type-check typing-context t3 ty)])
       (cond [(and (equal? ty1 (Bool))
                   (equal? ty2 ty)
                   (equal? ty3 ty))
              ty]
             [else (error "Type mismatch.")]))]
    [(Lam x t)
     (match ty
       [(Function-Type ty1 ty2)
        (type-check (hash-set typing-context x ty1) t ty2)]
       [_ (error "Type mismatch.")])]
    [_ (error "No rule applies.")]))