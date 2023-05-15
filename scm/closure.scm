(let (test (lambda (x) (lambda (y) (+ x y)))) (let (add2 (test 2)) (add2 3)))
