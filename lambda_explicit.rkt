#lang typed/racket

(require datatype)
(require (for-syntax syntax/parse))

(provide term small-step evaluate)

(define-type Name Symbol)

(define-datatype Term
  [Zero-Term ()]
  [Succ (Term)]
  [Var (Name)]
  [Lam (Name Type Term)]
  [Let-Term (Name Term Term)]
  [Let-Type (Name Type Term)]
  [App (Term Term)])

(define-datatype Type
  [Nat ()]
  [Function-Type (Type Type)])

(define-type Typing-Context (HashTable Name Type))

(: substitute (-> Term Name Term Term))
(define (substitute s x t)
  (match t
    [(Var y)
     (cond [(eq? x y) s]
           [else (Var y)])]
    [(Lam y T t1)
     (cond [(eq? x y)
            (Lam y T t1)]
           [else
            (let ([z (gensym y)])
              (Lam z T 
                   (substitute s x
                               (substitute (Var z) y t1))))])]
    [(Let-Term y t1 t2)
     (cond [(eq? x y)
            (Let-Term y t1 t2)]
           [else
            (let ([z (gensym y)])
              (Let-Term z t1 
                   (substitute s x
                               (substitute (Var z) y t2))))])]
    [(Let-Type y T t1)
     (Let-Type y T 
               (substitute s x t1))]
    [(App t1 t2)
     (App (substitute s x t1)
          (substitute s x t2))]
    [_ (error "No rule applies.")]))

(: is-value? (-> Term Boolean))
(define (is-value? t)
  (match t
    [(Zero-Term) true]
    [(Succ _) true]
    [(Lam _ _ _) true]
    [_ false]))

(: small-step (-> Term Term))
(define (small-step t)
  (match t
    [(App (Lam x _ t12)
          (? is-value? v2))
     (substitute v2 x t12)]
    [(App (? is-value? v1) t2)
     (App v1
          (small-step t2))]
    [(App t1 t2)
     (App (small-step t1) t2)]
    [(Let-Term x 
               (? is-value? v1)
               t2)
     (substitute v1 x t2)]
    [(Let-Term x t1 t2)
     (Let-Term x 
               (small-step t1)
               t2)]
    [(Let-Type _ _ t1) t1]
    [_ (error "No rule applies.")]))

(: evaluate (-> Term Term))
(define (evaluate t)
  (with-handlers ([exn:fail? (lambda ([e : exn:fail]) t)])
    (evaluate (small-step t))))

(: type-check (-> Typing-Context Term Type))
(define (type-check typing-context term)
  (match term
    [(Zero-Term)
     (Nat)]
    [(Succ t1)
     (match (type-check typing-context t1)
       [(Nat)
        (Nat)]
       [_ (error "Type mismatch.")])]
    [(Lam x t t1)
     (Function-Type t
                    (type-check (hash-set typing-context x t) t1))]
    [(App t1 t2)
     (let ([type-1 (type-check typing-context t1)]
           [type-2 (type-check typing-context t2)])
       (match type-1
         [(Function-Type type-a type-b) type-b]
         [_ (error "Type mismatch.")]))]
    [(Let-Term x t1 t2)
     (let* ([type-1 (type-check typing-context t1)]
            [type-2 (type-check (hash-set typing-context x type-1) t2)])
       type-2)]
    [(Let-Type x T t1)
     (type-check (hash-set typing-context x T) t1)]
    [(Var x)
     (hash-ref typing-context x)]
    [_ (error "No rule applies.")]))

(define-syntax (term stx)
  (letrec ([parse (lambda (stx)
                    (syntax-parse stx
                      [(_ (~literal Z))
                       (syntax (Zero-Term))]
                      [(_ ((~literal S) n:id))
                       (quasisyntax (Succ (unsyntax (syntax->datum (parse (syntax (term n)))))))]
                      [(_ (lambda ((~literal :) x:id T:id) t1))
                       (quasisyntax (Lam 'x 
                                         (T) 
                                         (unsyntax (syntax->datum (parse (syntax (term t1)))))))]
                      [(_ (let ([x:id t1]) t2))
                       (quasisyntax (Let-Term 'x 
                                              (unsyntax (syntax->datum (parse (syntax (term t1)))))
                                              (unsyntax (syntax->datum (parse (syntax (term t2)))))))]
                      [(_ (type ([x:id T:id]) t1))
                       (quasisyntax (Let-Type 'x 
                                              (T)
                                              (unsytax (syntax->datum (parse (syntax (term t1)))))))]
                      [(_ x:id)
                       (syntax (Var 'x))]
                      [(_ (t1 t2))
                       (quasisyntax (App (unsyntax (syntax->datum (parse (syntax (term t1))))) 
                                         (unsyntax (syntax->datum (parse (syntax (term t2)))))))]))])
    (parse stx)))