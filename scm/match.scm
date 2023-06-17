(letrec
    ((map (lambda (fn lst)
            (match (vec 0 lst)
                (0 (let (l (fn (vec 1 lst)))
                    (let (r (map fn (vec 2 lst)))
                        (make-vec 0 l r))))
                (1 (make-vec 1))))))
    (map (lambda x (+ 1 x))
         (make-vec 0 1
             (make-vec 0 2
                 (make-vec 0 3
                     (make-vec 1))))))
