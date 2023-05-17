(let (one-to-five (list 1 2 3 4 5))
     (letrec ((liars
               (lambda (x)
                 (let (betty (amb 1 (amb 2 (amb 3 (amb 4 5)))))
                  (let (not-betty (exclude (list betty) one-to-five))
                   (let (ethel (one-of not-betty))
                    (let (not-betty-or-ethel (exclude (list betty ethel) one-to-five))
                     (let (joan (one-of not-betty-or-ethel))
                      (let (not-betty-or-ethel-or-joan (exclude (list betty ethel joan) one-to-five))
                       (let (kitty (one-of not-betty-or-ethel-or-joan))
                        (let (not-betty-or-ethel-or-joan-or-kitty (exclude (list betty ethel joan kitty) one-to-five))
                         (let (mary (car not-betty-or-ethel-or-joan-or-kitty))
                          (let (f (require (xor (== kitty 2) (== betty 3))))
                           (let (f (require (xor (== ethel 1) (== joan  2))))
                            (let (f (require (xor (== joan  3) (== ethel 5))))
                             (let (f (require (xor (== joan  3) (== ethel 5))))
                              (let (f (require (xor (== kitty 2) (== mary  4))))
                               (let (f (require (xor (== mary  4) (== betty 1))))
                                    (list betty ethel joan kitty mary))))))))))))))))))

              (one-of
               (lambda (lst)
                 (let (f (require lst))
                      (amb (car lst) (one-of (cdr lst))))))

              (member
               (lambda (item lst)
                 (if lst
                     (or (== item (car lst))
                         (member item (cdr lst)))
                     #f)))

              (require
                (lambda (condition)
                   (or condition (back))))

              (exclude
               (lambda (items lst)
                (if lst
                    (let (mem (member (car lst) items))
                        (if mem
                            (exclude items (cdr lst))
                            (let (excluded (exclude items (cdr lst)))
                                (cons (car lst) excluded))))
                    nil))))
       (liars 0)))
