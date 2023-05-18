(letrec (

    (barrels-of-fun
        (lambda (ignored) ; currently functions are required to take an argument
            (let (barrels (list 30 32 36 38 40 62))
             (let (beer (one-of barrels))
              (let (wine (exclude (list beer) barrels))
               (let (barrel-1 (one-of wine))
                (let (except-barrel-1 (exclude (list barrel-1) wine))
                 (let (barrel-2 (one-of except-barrel-1))
                  (let (except-barrels-1-and-2 (exclude (list barrel-1 barrel-2) wine))
                   (let (purchase (some-of except-barrels-1-and-2))
                    (let (purchase-quantity (sum purchase))
                     (let (f (print (list barrel-1 barrel-2 purchase-quantity)))
                      (let (f (require (== (* 2 (+ barrel-1 barrel-2)) purchase-quantity)))
                       beer)))))))))))))

    (one-of
        (lambda (lst)
         (let (f (require lst))
          (amb (car lst) (one-of (cdr lst))))))

    (require
        (lambda (condition)
            (or condition (back))))

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

    (some-of
        (lambda (lst)
            (let (f (require lst))
                (amb (list (car lst))
                     (amb (some-of (cdr lst))
                          (let (rest (some-of (cdr lst)))
                               (cons (car lst) rest)))))))

    (sum
        (lambda (lst)
            (if lst
                (let (rest (sum (cdr lst)))
                     (+ (car lst) rest))
                0)))

) (barrels-of-fun 0))
