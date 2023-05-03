(let (x (call/cc (lambda (ret) (let (y (ret 5)) (if y 6 7))))) x)
