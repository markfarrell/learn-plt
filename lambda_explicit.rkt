#lang typed/racket

(require datatype)

(define-type Name Symbol)

(define-datatype Term
  [Zero-Term ()]
  [Succ (Term)]
  [Var (Symbol)]
  [Abs (Symbol Type Term)]
  [App (Term Term)])

(define-datatype Type
  [Nat ()]
  [Function-Type (Type Type)])

(define-type Typing-Context (Listof (Vector Name Type)))

(: fresh (-> Name Name))
(define (fresh x)
  (gensym x))

(: substitute (-> Term Name Term Term))
(define (substitute s x t)
  (match t
    [(Var y)
     (cond [(eq? x y) s]
           [else (Var y)])]
    [(Abs y t t1)
     (cond [(eq? x y)
            (Abs y t t1)]
           [else
            (let ([z (fresh y)])
              (Abs z t (substitute s x
                                   (substitute (Var z) y t1))))])]
    [(App t1 t2)
     (App (substitute s x t1)
          (substitute s x t2))]
    [_ (error "No rule applies.")]))

(: is-value? (-> Term Boolean))
(define (is-value? t)
  (match t
    [(Zero-Term) true]
    [(Succ _) true]
    [(Abs _ _ _) true]
    [_ false]))

(: small-step (-> Term Term))
(define (small-step t)
  (match t
    [(App (Abs x _ t12)
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

(: type-check (-> Typing-Context Term Type))
(define (type-check typing-context term)
  (: typing-context-find (-> Typing-Context Name Type))
  (define (typing-context-find typing-context name)
    (vector-ref
     (first
      (filter (lambda ([pair : (Vector Name Type)])
                (equal? (vector-ref pair 0) name))
              typing-context))
     1))
  (match term
    [(Zero-Term)
     (Nat)]
    [(Succ t1)
     (match (type-check typing-context t1)
       [(Nat)
        (Nat)]
       [_ (error "Type mismatch.")])]
    [(Abs x t t1)
     (Function-Type t
                    (type-check (cons (vector x t) typing-context) t1))]
    [(App t1 t2)
     (let ([type-1 (type-check typing-context t1)]
           [type-2 (type-check typing-context t2)])
       (match type-1
         [(Function-Type type-a type-b) type-b]
         [_ (error "Type mismatch.")]))]
    [(Var x)
     (typing-context-find typing-context x)]
    [_ (error "No rule applies.")]))
