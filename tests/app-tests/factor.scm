(define factor
    (letrec ((fact 
               (lambda (n)
                 (if (= n 0)
                     1
                     (* n (fact (- n 1)))))))
      (lambda (n)
        (fact n))))

(factor 5)
(factor 6)
(factor 7)
