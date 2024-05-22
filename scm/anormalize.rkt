#lang racket

;; anormalize: A simple A-Normalizer for a subset of Scheme

;; Author: Matt Might
;; Site:   http://matt.might.net/

;; Input language:

;; <prog> ::= <dec> ...

;; <dec> ::= (define (<var> <name> ...) <exp>)
;;        |  (define <var> <exp>)
;;        |  <exp>

;; <exp> ::= (let ([<var> <exp>] ...) <exp>)
;;        |  (if <exp> <exp> <exp>)
;;        |  (set! <var> <exp>)
;;        |  (λ (<name> ...) <exp>)
;;        |  <number>
;;        |  <boolean>
;;        |  <string>
;;        |  <var>


;; Output language:

;; <prog> ::= <dec> ...

;; <dec> ::= (define <var> <exp>)
;;        |  (begin <dec> ...)
;;        |  <exp>

;; <aexp> ::= (λ (<name> ...) <exp>)
;;         |  <number>
;;         |  <boolean>
;;         |  <string>
;;         |  <var>
;;         |  (void)

;; <cexp> ::= (<aexp> <aexp> ...)
;;         |  (if <aexp> <exp> <exp>)
;;         |  (set! <var> <exp>)

;; <exp> ::= (let ([<var> <cexp>]) <exp>)
;;        |  <aexp>
;;        |  <cexp>

(define (atomic? exp)
  (match exp
    [`(quote ,_)         #t]
    [(? number?)         #t]
    [(? boolean?)        #t]
    [(? string?)         #t]
    [(? char?)           #t]
    [(? symbol?)         #t]
    [(or '+ '- '* '/ '=) #t]
    [else                #f]))


;; Expression normalization:
(define (normalize-term exp) (normalize exp (λ (x) x)))

(define (normalize exp k)
  (match exp
    [`(λ ,params ,body)   
      (k `(λ ,params ,(normalize-term body)))]
    
    [`(let () ,exp)
      (normalize exp k)]

    [`(let ([,x ,exp1] . ,clause) ,exp2) 
      (normalize exp1 (λ (aexp1) 
       `(let ([,x ,aexp1])
         ,(normalize `(let (,@clause) ,exp2) k))))]
    
    [`(if ,exp1 ,exp2 ,exp3)    
      (normalize-name exp1 (λ (t) 
       (k `(if ,t ,(normalize-term exp2) 
                  ,(normalize-term exp3)))))]
    
    [`(set! ,v ,exp)
      (normalize-name exp (λ (t)
       `(let ([,(gensym '_) (set! ,v ,t)])
          ,(k '(void)))))]
    
    [`(,f . ,e*) 
      (normalize-name f (λ (t) 
       (normalize-name* e* (λ (t*)
        (k `(,t . ,t*))))))]
    
    [(? atomic?)            
     (k exp)]))

(define (normalize-name exp k)
  (normalize exp (λ (aexp) 
    (if (atomic? aexp) (k aexp) 
        (let ([t (gensym)]) 
         `(let ([,t ,aexp]) ,(k t)))))))

(define (normalize-name* exp* k)
  (if (null? exp*)
      (k '())
      (normalize-name (car exp*) (λ (t) 
       (normalize-name* (cdr exp*) (λ (t*) 
        (k `(,t . ,t*))))))))


;; Top-level normalization:
(define (normalize-define def)
  (match def
    [`(define (,f . ,params) ,body)
     `(define ,f ,(normalize-term `(λ ,params ,body)))]
    
    [`(define ,v ,exp)
     `(begin ,@(flatten-top (normalize-term exp) v))]))
                    

(define (flatten-top exp v)
  (match exp
    [`(let ([,x ,cexp]) ,exp)
     (cons `(define ,x ,cexp) 
            (flatten-top exp v))]
    
    [else
     `((define ,v ,exp))]))


(define (normalize-program decs)
  (match decs
    ['() 
     '()]
    
    [(cons `(define . ,_) rest)
     (cons (normalize-define (car decs))
           (normalize-program rest))]
    
    [(cons exp rest)
     (cons (normalize-term exp)
           (normalize-program rest))]))


;; Tests:
(normalize-term
 '(λ (z)
    (λ (x)
      (set! z (f x)))))

(normalize-term
 '(+ (if (f x) 0 1) 2))

(normalize-term
 '(let ((id (λ (x) x)))
    (let ((apply (λ (f x) (f x))))
      ((id apply) (id 3)))))

(normalize-term
 '(let ([id (λ (x) x)]
        [apply (λ (f x) (f x))])
      ((id apply) (id 3))))

(normalize-term 
 '((f g) (h 3) #f))

(normalize-define
 '(define v ((f g) (h 3) #f)))

(normalize-program
 '{(define (f x) (h x))
   (define (h x) (+ 1 x))
   (f 20)})

(display
 (pretty-format
  (normalize-program
   '{(define (f n)
       (if (= n 0)
           1 
           (* n (f (- n 1)))))
     
     (f 20)}) 45))
