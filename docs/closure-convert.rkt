#lang racket

(require test-engine/racket-tests)

;; <exp> ::= (lambda (<var> ...) <exp>)
;;        |  (<exp> <exp> ...)
;;        |  <var>

;;        ;  added for closure conversion:
;;        |  (lambda* (<var> <var> ...) <exp>)
;;        |  (make-closure <exp> <exp>)
;;        |  (make-env (<var> <exp>) ...)
;;        |  (env-ref <exp> <var>)
;;        |  (apply-closure <exp> <exp> ...)

; free : exp => set[var]
(define (free exp)
  
  (match exp
    [`(lambda ,params ,body)
     (set-subtract (free body) (apply set params))]
    
    [`(lambda* ,params ,body)
     (set-subtract (free body) (apply set params))]
    
    [(? symbol?)
     (set exp)]
    
    [`(make-closure ,proc, env)
     (set-union (free proc) (free env))]
    
    [`(make-env (,vs ,es) ...)
     (apply set-union (map free es))]
    
    [`(env-ref ,env ,v)
     (free env)]
    
    [`(apply-closure ,f ,args ...)
     (apply set-union (map free `(,f . ,args)))]
    
    [`(,f ,args ...)
     (apply set-union (map free `(,f . ,args)))]))
    

(check-expect (free '(make-closure (lambda (env x) ((env-ref env f) x))
                                   (make-env (f f))))
              (set 'f))


; substitute : hash[var,exp] exp => exp
(define (substitute sub exp)
  (match exp
    [`(lambda ,params ,body)
      (define params* (apply set params))
      (define sub* 
        (for/hash ([(k v) sub] #:when (not (set-member? params* k)))
          (values k v)))
      `(lambda ,params ,(substitute sub* body))]
    
    [`(lambda* ,params ,body)
     ; should not have free variables
      (define params* (apply set params))
      (define sub* 
        (for/hash ([(k v) sub] #:when (not (set-member? params* k)))
          (values k v)))
      `(lambda* ,params ,(substitute sub* body))]
    
    [(? symbol?)
     (if (hash-has-key? sub exp)
         (hash-ref sub exp)
         exp)]
    
    [`(make-closure ,lam ,env)
     `(make-closure ,(substitute sub lam) ,(substitute sub env))]
    
    [`(make-env (,vs ,es) ...)
     `(make-env ,@(map list vs (map (substitute-with sub) es)))]
    
    [`(env-ref ,env ,v)
     `(env-ref ,(substitute sub env) ,v)]
    
    [`(apply-closure ,f ,args ...)
     `(apply-closure ,@(map (substitute-with sub) `(,f . ,args)))]
    
    [`(,f ,args ...)
     (map (substitute-with sub) `(,f . ,args))]))

(define (substitute-with sub)
  (lambda (exp)
    (substitute sub exp)))


(check-expect 
 (substitute (make-hash '((f . +) (x . 4)))
             '(make-closure (lambda (env x) ((env-ref env f) x))
                            (make-env (f f))))
 '(make-closure (lambda (env x) ((env-ref env f) x))
                (make-env (f +))))
             


(define (closure-convert exp)
  (match exp
    
    [`(lambda ,params ,body)
      (define $env (gensym 'env))
      (define params* (cons $env params))
      (define fv (free exp))
      (define env (for/list ((v fv))
                    (list v v)))
      (define sub (for/hash ((v fv))
                    (values v `(env-ref ,$env ,v))))
      (define body* (substitute sub body))
     `(make-closure (lambda* ,params* ,body*)
                    (make-env ,@env))]
    
    [`(lambda* ,params ,body)
     exp]
    
    [(? symbol?)
     exp]
    
    [`(make-closure ,lam ,env)
     exp]
    
    [`(make-env (,vs ,es) ...)
     exp]
    
    [`(env-ref ,env ,v)
     exp]
    
    [`(apply-closure ,f ,args ...)
     exp]

    [`(,f ,args ...)
     `(apply-closure ,f . ,args)]))
   
     
                    

(pretty-write
 (closure-convert '(lambda (x) (+ x a b))))
                         

; transform/bottom-up : applies a bottom-up tree transform
(define (transform/bottom-up f exp)
  
  (define (t e) (transform/bottom-up f e))
  
  (let ([exp* (match exp
                [`(lambda ,params ,body)
                 `(lambda ,params ,(t body))]
                
                [`(lambda* ,params ,body)
                 `(lambda* ,params ,(t body))]
                
                [(? symbol?) 
                 exp]
                
                [`(make-closure ,lam ,env)
                 `(make-closure ,(t lam) ,(t env))]
                
                [`(make-env (,vs ,es) ...)
                 `(make-env ,@(map list vs (map t es)))]
                
                [`(env-ref ,env ,v)
                 `(env-ref ,(t env) ,v)]
                
                [`(apply-closure ,f ,args ...)
                 `(apply-closure ,(t f) ,(map t args))]
                
                [`(,f ,args ...)
                 `(,(t f) ,@(map t args))])])
    (f exp*)))


; transform/top-down : applies a bottom-up tree transform
(define (transform/top-down f exp)
  
  (define (t e) (transform/top-down f e))
  
  (match (f exp)
    [`(lambda ,params ,body)
     `(lambda ,params ,(t body))]
    
    [`(lambda* ,params ,body)
     `(lambda* ,params ,(t body))]
                    
    [(? symbol?) 
     exp]
    
    [`(make-closure ,lam ,env)
     `(make-closure ,(t lam) ,(t env))]
    
    [`(make-env (,vs ,es) ...)
     `(make-env ,@(map list vs (map t es)))]
    
    [`(env-ref ,env ,v)
     `(env-ref ,(t env) ,v)]
    
    [`(apply-closure ,f ,args ...)
     `(apply-closure ,(t f) ,@(map t args))]
    
    [`(,f ,args ...)
     `(,(t f) ,@(map t args))]))



(define (flat-closure-convert exp)
  (transform/bottom-up closure-convert exp))

(define (shared-closure-convert exp)
  (transform/top-down closure-convert exp))
                 

(define example
  '(lambda (f)
     (lambda (z)
       (lambda (x)
         (f x z a)))))


(pretty-write (flat-closure-convert example))

(pretty-write (shared-closure-convert example))
                    
(test)