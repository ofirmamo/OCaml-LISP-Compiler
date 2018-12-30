; First car of simple list
(define lst '(1 2 3))
(car lst)
(set-car! lst 4)
(car lst)

; 2, again simple string list
(define lst2 '("hello" 'bye "helloa"))
(car lst2)
(set-car! lst2 4)
(car lst2) 

;3, car of lists
(define lst3 '((1.5 2 3) 1 2))
(car lst3)
(set-car! lst3 "hello")
(car lst3)

;4, car of vector
(define lst4 '(#(1 'a) 2 3))
(car lst4)
(set-car! lst4 1)
(car lst4)

