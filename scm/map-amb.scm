; fn map {
;   (_, []) { [] }
;   (f, h @ t) { f(h) @ map(f, t) }
; }
(letrec
  ((map (lambda (a1 a2)
          (amb (match (vec 0 a2)
                 ((0) (cut (make-vec 0)))
                 ((1) (back)))
               (amb (let (f a1)
                      (match (vec 0 a2)
                        ((0) (back))
                        ((1) (cut (let (h (vec 1 a2))
                                    (let (t (vec 2 a2))
                                      (let (v1 (f h))
                                        (let (v2 (map f t))
                                          (make-vec 1 v1 v2)))))))))
                    (make-vec 0))))))
  (amb
    (map (lambda (x) (back)) (make-vec 1 2 (make-vec 0)))
    (make-vec 1 1 (make-vec 0)))) ; should be the final result
