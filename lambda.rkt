#lang typed/racket

(require (for-syntax syntax/parse))
(require datatype)

(define-type Name Symbol)
(define-type Index Integer)
(define-type Context (Listof (Vector Name Index)))

(define-datatype Term
  [Var (Name)]
  [Abs (Name Type Term)]
  [App (Term Term)])

(define-datatype Type
  [Function-Type (Type Type)])

(define-type Typing-Context (Listof (Vector Name Type)))

(define-datatype Nameless-Term
  [Nameless-Var (Index)]
  [Nameless-Abs (Nameless-Term)]
  [Nameless-App (Nameless-Term Nameless-Term)])

(: remove-names (-> Context Term Nameless-Term))
(define (remove-names context term)
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
  (match term
    [(Var x)
     (Nameless-Var (context-find context x))]
    [(App t1 t2)
     (Nameless-App (remove-names context t1)
                   (remove-names context t2))]
    [(Abs x _ t)
     (Nameless-Abs (remove-names (context-add context x) t))]))

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

(define-syntax (parse stx)
  (syntax-parse stx
    [(_ ((~literal lambda) (~var x id) (~var T id))) #''ok] 
    [(_ (~var x id))  #'(Var 'x)]))

(parse y)
(parse (lambda x T))
     
