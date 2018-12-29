((lambda (x) (begin (set! x 3) x)) 4)

(define x 2)
((lambda () (set! x 3)))
x

((lambda () ((lambda (y) y) 2)))
