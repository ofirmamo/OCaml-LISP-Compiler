; First test, simple test define lambda with 1 paramter
((lambda (x) (begin (set! x 3) x)) 4)

; Second define global and override..
(define x 2)
((lambda () (set! x 3)))
x

; Third use param within lambda as parent..
((lambda () ((lambda (y) y) 2)))

; 4, Simple test for getting param.
((lambda (x) x) 1)
((lambda (x y) (+ x y)) 2.5 4.5)
((lambda (p) p) "HEllo")
((lambda (str) (string-set! str  0 #\x) str) "h")
((lambda (vec) (vector-length vec)) '#(1 2 3 4 5))
