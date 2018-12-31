(define list
    (lambda l
      ((lambda (x) (cons x l)) 4)))

(list 1 2 3 4 5)
(list)

(define list
    (lambda (a . l)
      ((lambda () l))))

(list 1 2 3 4 5)

(list 1)
