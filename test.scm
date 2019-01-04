

(apply + 1 '(2 3 4 5))

(define flat
	(lambda (lst)
		(if (null? (cdr lst))
				(car lst)
				(cons (car lst) (flat (cdr lst)))))) 

(apply list 1 2 '(3 4 5))

(apply list 1 2 3 '(4))
(+ 1 2)
(* 1 2 3 4)
(- 8 2 2 2)
(/ 10 2 2)

(+ 1 2 3 4)
(((lambda (x)
      (lambda (y) x))
    2) 3)