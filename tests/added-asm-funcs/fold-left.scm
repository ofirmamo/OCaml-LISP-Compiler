(define fold-left
    (lambda (f acc lst)
      (if (null? lst)
          acc
          (fold-left f (f acc (car lst)) (cdr lst)))))

(fold-left + 0 '(1 2 3 4))
(fold-left * 1 '(1 2 3 4))
