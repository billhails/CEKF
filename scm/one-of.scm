(letrec (
    (require
        (lambda (condition) (or condition (back))))
    (one-of
        (lambda (lst)
            (let (f (require lst))
                (amb (car lst) (one-of (cdr lst))))))
    )
    (let (a (one-of (list 1 2 3 4 5)))
        (let (f (require (== a 3)))
            a)))
   
