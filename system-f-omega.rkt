#lang typed/racket

(require datatype)

(define-type Name Symbol)
(define-type Index Nonnegative-Integer)

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
  [Type-Var (Name)]
  [For-All (Name Type Type)]
  [Kind (Index)])

(define-type Typing-Context (HashTable Name Type))

(: type-substitute (-> Type Name Type Type))
(define (type-substitute s x t)
  (match t
    [(Bool)
     (Bool)]
    [(Type-Var y)
     (cond [(eq? x y) s]
           [else (Type-Var y)])]
    [(For-All y T1 T2)
     (cond [(eq? x y)
            (For-All y T1 T2)]
           [else
            (let ([z (gensym y)])
              (For-All z T1 (type-substitute s x
                                             (type-substitute (Type-Var z) y T2))))])]
    [(Kind i)
     (Kind i)]
    [_ (error "No rule applies.")]))
    
(: type-infer (-> Typing-Context Term Type))
(define (type-infer context term)
  (match term
    [(True-Term) 
     (Bool)]
    [(False-Term)
     (Bool)]
    [(Var x)
     (hash-ref context x)]
    [(App t1 t2)
     (match (type-infer context t1)
       [(For-All x ty1 ty2)
        (cond [(equal? (type-infer context t2) ty1)
               (type-substitute ty1 x ty2)]
              [else
               (error "Type mismatch.")])])]
    [(Ann t ty)
     (type-check context t ty)]
    [(For-All x ty1 ty2)
     (match (type-infer context ty1)
       [(Kind i)
        (match (type-infer (hash-set context x ty1) ty2)
          [(Kind j)
           (Kind (min i j))]
          [_ (error "Type mismatch.")])]
       [_ (error "Type mismatch.")])]
    [(Kind i)
     (Kind (+ i 1))]
    [_ (error "No rule applies.")]))

(: type-check (-> Typing-Context Term Type Type))
(define (type-check context term ty)
  (match term
    [(If-Then-Else t1 t2 t3)
     (let ([ty1 (type-check context t1 (Bool))]
           [ty2 (type-check context t2 ty)]
           [ty3 (type-check context t3 ty)])
       (cond [(and (equal? ty1 (Bool))
                   (equal? ty2 ty)
                   (equal? ty3 ty))
              ty]
             [else (error "Type mismatch.")]))]
    [(Lam x t1)
     (match ty
       [(For-All x ty1 ty2)
        (For-All x ty1
                 (type-check (hash-set context x ty1) t1 ty2))])]
    [_ (cond [(equal? (type-infer context term) ty) ty]
             [else (error "No rule applies.")])]))

(type-check (make-immutable-hash)
            (Ann (Lam 'x 
                      (If-Then-Else (Var 'x)
                                    (False-Term)
                                    (True-Term))) 
                 (For-All 'x
                          (Bool)
                          (Bool)))
            (For-All 'x
                     (Bool)
                     (Bool)))