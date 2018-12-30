(define get-nth
    (lambda (lst n)
      (if (null? lst)
          '()
          (if (= n 0)
              (car lst)
              (get-nth (cdr lst) (- n 1))))))

(get-nth '(1 2 3 4 5) 2)
(get-nth '(1 2 3) 8)
