; 1, Box simple lambda for lazy return 1
((lambda (x)
          x
          ((lambda (y)
                  (set! x (lambda () 1))
                  (+ y (x))) 2) )1)

; 2, Box lazy lambda
((lambda (x)
          x
          ((lambda (y)
                  (set! x (lambda () +))
                  ((x) y 1)) 2) )1)

