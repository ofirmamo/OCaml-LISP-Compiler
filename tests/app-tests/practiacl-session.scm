; 1.1
(define f (lambda () 1))
(f)

; 1.2
(define x 0)
(set! x 3)
(if #f 1 x)
(if #t (set! x 2) 2)

; 1.3 
((lambda ()
     ((lambda (a b c d e)
        e) 'a 'b 'c 'd 'e)))



