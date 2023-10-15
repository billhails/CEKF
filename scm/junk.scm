; fn map {
;   (_, nil()) => { (make-vec 0) }
;   (f, cons(h, cons(t, nil()))) => { (make-vec 1 (f h) (map f t)) }
; }
(define map
        (lambda (arg0 arg1)
                (amb (match (vec 0 arg1)
                            ((0) (cut (make-vec 0)))
                            ((1) (back)))
                     (amb (let (f arg0)
                               (match (vec 0 arg1)
                                      ((1) (let (h (vec 1 arg1))
                                                (match (vec 0 (vec 2 arg1))
                                                       ((1) (let (t (vec 1 (vec 2 arg1)))
                                                                 (match (vec 0 (vec 2 (vec 2 arg1)))
                                                                        ((0) (cut (make-vec 1 (f h) (map f t))))
                                                                        ((1) (back)))))
                                                       ((0) (back)))))
                                      ((0) (back))))
                          (error "patterns exhausted in function map")))))

; fn factorial {
;   (0) => { 1 }
;   (n) => { (* n (factorial (- n 1))) }
; }
(define factorial
        (lambda (arg0)
                (amb (if (eq arg0 0)
                         (cut 1)
                         (back))
                     (amb (let (n arg0)
                               (cut (* n (factorial (- n 1)))))
                          (error "patterns exhausted in function factorial")))))

; fn member {
;   (_, nil()) => { false }
;   (x, cons(x, _)) => { true }
;   (x, cons(_, t)) => { (member x t) }
; }
(define member
        (lambda (arg0 arg1)
                (amb (match (vec 0 arg1)
                            ((0) (cut false))
                            ((1) (back)))
                     (amb (let (x arg0)
                               (match (vec 0 arg1)
                                      ((1) (if (eq x (vec 1 arg1))
                                               (cut true)
                                               (back)))
                                      ((0) (back))))
                          (amb (let (x arg0)
                                    (match (vec 0 arg1)
                                           ((1) (let (t (vec 2 arg1))
                                                     (cut (member x t))))
                                           ((0) (back))))
                               (error "patterns exhausted in function member"))))))

; fn test {
;   (cons(1, cons(1, nil()))) => { a }
;   (cons(1, cons(2, nil()))) => { b }
;   (cons(2, cons(1, nil()))) => { c }
;   (cons(2, cons(2, nil()))) => { d }
;   (_) => { e }
; }
(define test
        (lambda (arg0)
                (amb (match (vec 0 arg0)
                            ((1) (if (eq (vec 1 arg0) 1)
                                     (match (vec 0 (vec 2 arg0))
                                            ((1) (if (eq (vec 1 (vec 2 arg0)) 1)
                                                     (match (vec 0 (vec 2 (vec 2 arg0)))
                                                            ((0) (cut a))
                                                            ((1) (back)))
                                                     (back)))
                                            ((0) (back)))
                                     (back)))
                            ((0) (back)))
                     (amb (match (vec 0 arg0)
                                 ((1) (if (eq (vec 1 arg0) 1)
                                          (match (vec 0 (vec 2 arg0))
                                                 ((1) (if (eq (vec 1 (vec 2 arg0)) 2)
                                                          (match (vec 0 (vec 2 (vec 2 arg0)))
                                                                 ((0) (cut b))
                                                                 ((1) (back)))
                                                          (back)))
                                                 ((0) (back)))
                                          (back)))
                                 ((0) (back)))
                          (amb (match (vec 0 arg0)
                                      ((1) (if (eq (vec 1 arg0) 2)
                                               (match (vec 0 (vec 2 arg0))
                                                      ((1) (if (eq (vec 1 (vec 2 arg0)) 1)
                                                               (match (vec 0 (vec 2 (vec 2 arg0)))
                                                                      ((0) (cut c))
                                                                      ((1) (back)))
                                                               (back)))
                                                      ((0) (back)))
                                               (back)))
                                      ((0) (back)))
                               (amb (match (vec 0 arg0)
                                           ((1) (if (eq (vec 1 arg0) 2)
                                                (match (vec 0 (vec 2 arg0))
                                                       ((1) (if (eq (vec 1 (vec 2 arg0)) 2)
                                                                (match (vec 0 (vec 2 (vec 2 arg0)))
                                                                       ((0) (cut d))
                                                                       ((1) (back)))
                                                                (back)))
                                                       ((0) (back)))
                                                (back)))
                                           ((0) (back)))
                                    (amb (cut e)
                                         (error "patterns exhausted in function test"))))))))

; fn test2 {
;   (x=cons('a', nil()), cons(y=cons(2, nil()), nil())) => { a }
; }
(define test2
        (lambda (arg0 arg1)
                (amb (let (x arg0)
                          (match (vec 0 arg0)
                                 ((1) (if (eq (vec 1 arg0) 'a')
                                          (match (vec 0 (vec 2 arg0))
                                                 ((0) (match (vec 0 arg1)
                                                             ((1) (let (y (vec 1 arg1))
                                                                       (match (vec 0 (vec 1 arg1))
                                                                              ((1) (if (eq (vec 1 (vec 1 arg1)) 2)
                                                                                       (match (vec 0 (vec 2 (vec 1 arg1)))
                                                                                              ((0) (match (vec 0 (vec 2 arg1))
                                                                                                   ((0) (cut a))
                                                                                                   ((1) (back))))
                                                                                              ((1) (back)))
                                                                                       (back)))
                                                                              ((0) (back)))))
                                                             ((0) (back))))
                                                 ((1) (back)))
                                          (back)))
                                 ((0) (back))))
                     (error "patterns exhausted in function test2"))))

; fn colourToChar {
;   (red) => { 'r' }
;   (green) => { 'g' }
;   (blue) => { 'b' }
; }
(define colourToChar
        (lambda (arg0)
                (amb (match arg0
                            ((0) (cut 'r'))
                            ((1 2) (back)))
                     (amb (match arg0
                                 ((1) (cut 'g'))
                                 ((0 2) (back)))
                          (amb (match arg0
                                      ((2) (cut 'b'))
                                      ((0 1) (back)))
                               (error "patterns exhausted in function colourToChar"))))))
