#lang typed/racket

(require datatype)

(define-type Name Symbol)

(define-datatype Term
  [Var (Name)]
  [Lam (Name Term Term)]
  [App (Term Term)]
  [Pi  (Name Term Term)]
  [Type-0 ()])

(define-type Typing-Context (HashTable Name Term))

(: substitute (-> Term Name Term Term))
(define (substitute s x t)
  (match t
    [(Var y)
     (cond [(eq? x y) s]
           [else (Var y)])]
    [(Lam y T1 t1)
     (cond [(eq? x y)
            (Lam y T1 t1)]
           [else
            (let ([z (gensym y)])
              (Lam z T1 (substitute s x
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
    [(Type-0)
     (Type-0)]
    [_ (error "No rule applies.")]))

(: type-check (-> Typing-Context Term Term))
(define (type-check context term)
  (match term
    [(Var x)
     (hash-ref context x)]
    [(Lam x T1 t1)
     (let ([T2 (type-check (hash-set context x T1) t1)]
           [T1-type (type-check context T1)])
       (match T1-type
         [(Type-0)
          (Pi x T1 T2)]
         [_ (error "Type mismatch.")]))]
    [(App t1 t2)
     (let ([t1-type (type-check context t1)]
           [t2-type (type-check context t2)])
       (match t1-type
         [(Pi x T1 T2)
          (cond [(equal? t2-type T1)
                 (substitute t2 x T2)]
                [else
                 (error "Type mismatch.")])]))]
    [(Pi x T1 T2)
     (let ([T1-type (type-check context T1)]
           [T2-type (type-check (hash-set context x T1) T2)])
       (match T1-type
         [(Type-0)
          (match T2-type
            [(Type-0)
             (Type-0)]
            [_ (error "Type mismatch.")])]
         [_ (error "Type mismatch.")]))]
    [(Type-0)
     (Type-0)]
    [_ (error "No rule applies.")]))
