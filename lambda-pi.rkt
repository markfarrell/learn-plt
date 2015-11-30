#lang typed/racket

(require datatype)

(define-type Name Symbol)
(define-type Index Nonnegative-Integer)

(define-datatype Term
  [Var (Name)]
  [Lam (Name Term)]
  [App (Term Term)]
  [Ann (Term Term)]
  [Pi  (Name Term Term)]
  [Type (Index)])

(define-type Typing-Context (HashTable Name Term))

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
              (Lam z (substitute s x
                                 (substitute (Var z) y t1))))])]
    [(App t1 t2)
     (App (substitute s x t1)
          (substitute s x t2))]
    [(Pi y T1 T2)
     (cond [(eq? x y)
            (Pi y T1 T2)]
           [else
            (let ([z (gensym y)])
              (Pi z T1 (substitute s x
                                   (substitute (Var z) y T2))))])]
    [(Type i)
     (Type i)]
    [_ (error "No rule applies.")]))

(: type-infer (-> Typing-Context Term Term))
(define (type-infer context term)
  (match term
    [(Var x)
     (hash-ref context x)]
    [(App t1 t2)
     (match (type-infer context t1)
       [(Pi x ty1 ty2)
        (cond [(equal? (type-infer context t2) ty1)
               (substitute t2 x ty2)]
              [else
               (error "Type mismatch.")])])]
    [(Ann t ty)
     (type-check context t ty)]
    [(Pi x ty1 ty2)
     (match (type-infer context ty1)
       [(Type i)
        (match (type-infer (hash-set context x ty1) ty2)
          [(Type j)
           (Type (min i j))]
          [_ (error "Type mismatch.")])]
       [_ (error "Type mismatch.")])]
    [(Type i)
     (Type (+ i 1))]
    [_ (error "No rule applies.")]))

(: type-check (-> Typing-Context Term Term Term))
(define (type-check context term ty)
  (match term
    [(Lam x t1)
     (match ty
       [(Pi x ty1 ty2)
        (Pi x ty1
            (type-check (hash-set context x ty1) t1 ty2))])]
    [_ (cond [(equal? (type-infer context term) ty) ty]
             [else (error "No rule applies.")])]))