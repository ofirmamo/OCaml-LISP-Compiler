; Define 2 global functions..
(define (g x) (+ x 1.5))
(define f (lambda (y) y))

(g 1.5)
(f 'hello)
(f "hello")
(f (g 1.5))
(g (f 2.5))
