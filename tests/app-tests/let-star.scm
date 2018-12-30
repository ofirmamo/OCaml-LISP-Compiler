; 1, Sanity test
(let* ((x 1)
         (y (+ 2 x)))
    (+ x y))

; 2, if macro expansion
(let* ((dit (lambda () 'hi))
        (dif (lambda () 'bye))
        (val #t))
    ((or (and val dit) (and val-test dif))))
