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

;; Statics

(: type-infer (-> Typing-Context Term Type))
(define (type-infer typing-context term)
  (match term
    [(True-Term) 
     (Bool)]
    [(False-Term)
     (Bool)]
    [(Var x)
     (hash-ref typing-context x)]
    [(Ann t ty)
     (type-check typing-context t ty)]
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
     (let ([ty1 (type-check typing-context t1 (Bool))]
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
        (Function-Type ty1 
                       (type-check (hash-set typing-context x ty1) t ty2))]
       [_ (error "Type mismatch.")])]
    [_ (cond [(equal? (type-infer typing-context term) ty) ty]
             [else (error "No rule applies.")])]))

(type-check (make-immutable-hash)
            (Ann (Lam 'x 
                      (If-Then-Else (Var 'x)
                                    (False-Term)
                                    (True-Term))) 
                 (Function-Type (Bool)
                                (Bool)))
            (Function-Type (Bool)
                           (Bool)))

;; Dynamics

(: substitute (-> Term Name Term Term))
(define (substitute s x t)
  (match t
    [(Var y)
     (cond [(eq? x y) s]
           [else (Var y)])]
    [(Lam y t1)
     (cond [(eq? x y)
            (Lam y t1)]
           [else
            (let ([z (gensym y)])
              (Lam z 
                   (substitute s x
                               (substitute (Var z) y t1))))])]
    [(App t1 t2)
     (App (substitute s x t1)
          (substitute s x t2))]
    [_ (error "No rule applies.")]))

(: is-value? (-> Term Boolean))
(define (is-value? t)
  (match t
    [(True-Term) true]
    [(False-Term) true]
    [(Lam _ _) true]
    [_ false]))

(: small-step (-> Term Term))
(define (small-step t)
  (match t
    [(If-Then-Else t1 t2 t3)
     (match t1
       [(True-Term) t2]
       [(False-Term) t3]
       [_ (If-Then-Else (small-step t1) t2 t3)])]
    [(App (Lam x t12)
          (? is-value? v2))
     (substitute v2 x t12)]
    [(App (? is-value? v1) t2)
     (App v1
          (small-step t2))]
    [(App t1 t2)
     (App (small-step t1) t2)]
    [_ (error "No rule applies.")]))

(: evaluate (-> Term Term))
(define (evaluate t)
  (with-handlers ([exn:fail? (lambda ([e : exn:fail]) t)])
    (evaluate (small-step t))))