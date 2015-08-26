#lang typed/racket

(require datatype)

(define-type Name Symbol)
(define-type Index Integer)
(define-type Context (Listof (Vector Name Index)))

(define-datatype Term
  [Var (Name)]
  [Abs (Name Term)]
  [App (Term Term)])

(define-datatype Nameless-Term
  [Nameless-Var (Index)]
  [Nameless-Abs (Nameless-Term)]
  [Nameless-App (Nameless-Term Nameless-Term)])

(: remove-names (-> Context Term Nameless-Term))
(define (remove-names context term)
  (match term
    [(Var x)
     (Nameless-Var (context-find context x))]
    [(App t1 t2)
     (Nameless-App (remove-names context t1)
                   (remove-names context t2))]
    [(Abs x t)
     (Nameless-Abs (remove-names (context-add context x) t))]))

(: context-find (-> Context Name Index))
(define (context-find context name)
  (vector-ref 
   (first
    (filter (lambda ([pair : (Vector Name Index)])
              (equal? (vector-ref pair 0) name))
            context))
   1))

(: context-add (-> Context Name Context))
(define (context-add context x)
  (append 
   (map (lambda ([pair : (Vector Name Index)])
          (vector (vector-ref pair 0)
                  (+ (vector-ref pair 1) 1)))
        context)
   (list (vector x 0))))

(: shift (-> Index Index Nameless-Term Nameless-Term))
(define (shift d c nameless-term)
  (match nameless-term
    [(Nameless-Var k)
     (Nameless-Var
      (cond [(< k c) k]
            [else 
             (+ k d)]))]
    [(Nameless-Abs t1)
     (Nameless-Abs (shift d 
                          (+ c 1)
                          t1))]
    [(Nameless-App t1 t2)
     (Nameless-App (shift d c t1)
                   (shift d c t2))]))

(: substitute (-> Nameless-Term Index Nameless-Term Nameless-Term))
(define (substitute s j t)
  (match t
    [(Nameless-Var k)
      (cond [(= k j) s]
            [else (Nameless-Var k)])]
    [(Nameless-Abs t1)
     (Nameless-Abs (substitute (shift 1 0 s)
                               (+ j 1)
                               t1))]
    [(Nameless-App t1 t2)
     (Nameless-App (substitute s j t1)
                   (substitute s j t2))]))

(: is-value? (-> Nameless-Term Boolean))
(define (is-value? t)
  (match t
    [(Nameless-Abs _) true]
    [_ false]))
    
(: step (-> Nameless-Term Nameless-Term))
(define (step t)
  (match t
    [(Nameless-App (Nameless-Abs t12)
                   (? is-value? v2))
     (shift -1 0 
            (substitute (shift 1 0 v2) 0 t12))]
    [(Nameless-App (? is-value? v1) t2)
     (Nameless-App v1
                   (step t2))]
    [(Nameless-App t1 t2)
     (Nameless-App (step t1) t2)]
    [_ (error "No rule applies.")]))

(: evaluate (-> Nameless-Term Nameless-Term))
(define (evaluate t)
  (with-handlers ([exn:fail? (lambda ([e : exn:fail]) t)])
    (evaluate (step t))))
     
