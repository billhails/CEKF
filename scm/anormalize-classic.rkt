#lang racket

;; Input language, Core Scheme:

; <M> ::= <V>
;      |  (let ([<x> <M>]) <M>) 
;      |  (if <M> <M> <M>)
;      |  (<M> <M> ...)
;      |  (<O> <M> ...)

; <V> ::= <c> | <x> | (λ (<x> ...) <M>)

; <x> is a variable
; <c> is a constant
; <O> is a primitive operation


;; Output language:

; <aexp> ::= <c> | <O>
;         |  (lambda (<x> ...) <exp>)

; <cexp> ::= (<aexp> <aexp> ...)
;         |  (if <aexp> <exp> <exp>)

; <exp>  ::= (let ([<x> <cexp>]) <exp>)
;         |  <cexp>
;         |  <aexp>


(define (Value? M)
  (match M
    [`(quote ,_)   #t]
    [(? number?)   #t]
    [(? boolean?)  #t]
    [(? string?)   #t]
    [(? char?)     #t]
    [(? symbol?)   #t]
    [(or '+ '- '* '/ '=) #t]
    [else          #f]))


(define (normalize-term M) (normalize M (λ (x) x)))

(define (normalize M k)
  (match M
    [`(λ ,params ,body)   
      (k `(λ ,params ,(normalize-term body)))]
    
    [`(let ([,x ,M1]) ,M2) 
      (normalize M1 (λ (N1) 
       `(let ([,x ,N1])
         ,(normalize M2 k))))]
     
    [`(if ,M1 ,M2 ,M3)    
      (normalize-name M1 (λ (t) 
       (k `(if ,t ,(normalize-term M2) 
                  ,(normalize-term M3)))))]
    
    [`(,Fn . ,M*) 
      (normalize-name Fn (λ (t) 
       (normalize-name* M* (λ (t*)
        (k `(,t . ,t*))))))]
    
    [(? Value?)             (k M)]))

(define (normalize-name M k)
  (normalize M (λ (N) 
    (if (Value? N) (k N) 
        (let ([t (gensym)]) 
         `(let ([,t ,N]) ,(k t)))))))

(define (normalize-name* M* k)
  (if (null? M*)
      (k '())
      (normalize-name (car M*) (λ (t) 
       (normalize-name* (cdr M*) (λ (t*) 
        (k `(,t . ,t*))))))))

;; tests


(define t1
 '(let ((id (λ (x) x)))
    (let ((apply (λ (f x) (f x))))
      ((id apply) (id 3)))))

(pretty-print t1)

(pretty-print (normalize-term t1))


(define t2
  '(let ([x (let ([y 20]) y)])
     x))


(pretty-print t2)

(pretty-print (normalize-term t2))



(define t3
  '(let ([x (if #t 1 2)])
     x))


(pretty-print t3)

(pretty-print (normalize-term t3))


  