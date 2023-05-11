
(let (x (call/cc (lambda (ret) (let (y (call/cc ret)) (if y (lambda (x) 6) (lambda (x) 7)))))) (x #t))

