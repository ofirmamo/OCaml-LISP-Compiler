; First test, simple define and print out..
(define x 2)
x

; Second test, LambdaSimple and apply on number..
(define (y l) l)
(y 2)

; Third test, override define global funtction...
(define + *)
(+ 2 3)

; Fourth again simple one..
(define x (lambda (y) y))
(x 3)

; Last one override define free var in run-time..
(define x x)
(x 3)
