; 1, override bound by param.
((lambda (x)
     ((lambda (x)
        x) 1)) 2)

;2, override by bound.
((lambda (x)
     ((lambda (x)
        ((lambda (y)
           (+ x y)) 1)) 2)) 3)
