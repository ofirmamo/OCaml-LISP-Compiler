(define fibo
    (lambda (n)
      (if (= n 0)
          1
          (if (= n 1)
              1
              (+ (fibo (- n 1)) (fibo (- n 2)))))))

(fibo 5)
(fibo 6)
(fibo 7)
(fibo 8)
