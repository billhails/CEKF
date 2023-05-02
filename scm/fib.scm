(letrec ((fib
          (lambda (n)
                  (if (< n 2)
                      1
                      (let (fib1 (fib (- n 1)))
                           (let (fib2 (fib (- n 2)))
                                (+ fib1 fib2)))))))
        (fib 20))

