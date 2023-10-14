; fn {
;   (['x', ['x']]) { a() }
;   (['x', ['y']]) { b() }
;   (['y', ['x']]) { c() }
;   (['y', ['y']]) { d() }
;   (_)            { e() }
; }
(lambda ($1)
  (escape
    (amb (match (kind $1)
                (0 (back))
                (1 (if (eq (field $1 0) 'x')
                       (match (kind (field $1 1))
                              (0 (back))
                              (1 (if (eq (field (field $1 1) 0) 'x')
                                     (match (kind (field (field $1 1) 1))
                                            (0 (amb (a) (cut)))
                                            (1 (back)))
                                     (back))))
                       (back))))
         (amb (match (kind $1)
                     (0 (back))
                     (1 (if (eq (field $1 0) 'x')
                            (match (kind (field $1 1))
                                   (0 (back))
                                   (1 (if (eq (field (field $1 1) 0) 'y')
                                          (match (kind (field (field $1 1) 1))
                                                 (0 (amb (b) (cut)))
                                                 (1 (back)))
                                          (back))))
                            (back))))
              (amb (match (kind $1)
                          (0 (back))
                          (1 (if (eq (field $1 0) 'y')
                                 (match (kind (field $1 1))
                                        (0 (back))
                                        (1 (if (eq (field (field $1 1) 0) 'x')
                                               (match (kind (field (field $1 1) 1))
                                                      (0 (amb (c) (cut)))
                                                      (1 (back)))
                                               (back))))
                                 (back))))
                   (amb (match (kind $1)
                               (0 (back))
                               (1 (if (eq (field $1 0) 'y')
                                      (match (kind (field $1 1))
                                             (0 (back))
                                             (1 (if (eq (field (field $1 1) 0) 'y')
                                                    (match (kind (field (field $1 1) 1))
                                                           (0 (amb (d) (cut)))
                                                           (1 (back)))
                                                    (back))))
                                      (back))))
                        (amb (e) (cut))))))))

; fn map {
;   (_, []) { [] }
;   (f, h @ t) { f(h) @ map(f, t) }
; }
(define map
  (lambda ($1 $2)
    (escape
      (amb (match (kind $2)
                  (0 (amb nil (cut)))
                  (1 (back))
           (amb (let (f $1)
                  (match (kind $2)
                         (1 (let (h (field $2 0))
                              (let (t (field $2 1))
                                (amb (pair (f h) (map f t)) (cut)))))
                         (0 (back))))
                (error "patterns exhausted in function map")))))))

; fn member {
;   (_, []) { false }
;   (x, x @ _) { true }
;   (x, _ @ t) { member(x, t) }
; }
(define member
  (lambda ($1 $2)
    (escape
      (amb (match (kind $2)
                  (0 (amb false (cut)))
                  (1 (back)))
           (amb (let (x $1)
                  (match (kind $2)
                         (1 (if (eq x (field $2 0))
                                (amb true (cut))
                                (back)))
                         (0 (back))))
                (amb (let (x $1)
                       (match (kind $2)
                              (1 (let (t (field $2 1))
                                   (amb (member x t) (cut))))
                              (0 (back))))
                     (error "patterns exhausted in function member")))))))
