;(((lambda (x y) (lambda () (+ y x))) 1))

(((lambda (x y) (lambda () (set! x y) (+ (+ x x) y))) 1 2))


