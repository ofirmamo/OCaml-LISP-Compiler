; filter function
(define filter
    (lambda (pred lst)
      (if (null? lst)
          '()
          (if (pred (car lst))
              (cons (car lst) (filter pred (cdr lst)))
              (filter pred (cdr lst))))))

(filter pair? '('(1 2) 3 4))
(filter vector? '('#(1) 1.5 2 '#(3)))

