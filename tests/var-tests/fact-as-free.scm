; Define fact function, tested by chez..
(define fact
    (lambda (n)
      (if (= n 0)
          1
          (* n (fact (- n 1))))))

(fact 5) ; should return 120
(fact 4) ; should return 24
(fact 6) ; Dont remmber....
