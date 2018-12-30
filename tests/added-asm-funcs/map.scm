; Define map function
(define map
    (lambda (f lst)
      (if (null? lst)
          '()
          (cons (f 2 (car lst)) (map f (cdr lst))))))

(map - '(1 2 3))
(map * '(1 2 3))
