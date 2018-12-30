(define counter
    (let ((n 1))
      (lambda ()
        (set! n (+ n 1))
          n)))

(counter) ;; 2
(counter) ;; 3
(counter) ;; 4
