;(define list (lambda (a . l) l))

;(list 1 2 3 4 5)

;(define cari 
;    (lambda (a b c d . e)
;        (car e)))

;(cari 1 2 3 4 5 6 7 8)

(define list2
    (lambda (a . l)
        ((lambda () l))))
    
(list2 1 2 3 4 5)

;(+ 1 2)