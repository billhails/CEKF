(letrec (
    (member
        (lambda (item lst)
            (if lst
                (or (== item (car lst))
                    (member item (cdr lst)))
                #f)))
    (exclude
        (lambda (items lst)
            (if lst
                (let (mem (member (car lst) items))
                   (if mem
                       (exclude items (cdr lst))
                       (let (excluded (exclude items (cdr lst)))
                            (cons (car lst) excluded))))
                nil)))
    )
    (let (a (list 2 3))
        (let (b (list 1 2 3 4 5))
            (exclude a b))))
