;; First test simple Box test for x
((lambda (x)
     (set! x 1)
     ((lambda ()
        x))) 2)

;; Second tests simple Box test for x
((lambda (x)
     ((lambda ()
        (set! x 'hello)))
	x) 2)

; Third test 1 box and 1 not
(define (f x y) x ((lambda () (set! x y) x)))
(f 1 2)

; Fourth test 2 boxes..
(define (g x y) x (set! y 3.5) ((lambda () (set! x 2.5) (+ x y))))
(g 1 2)
