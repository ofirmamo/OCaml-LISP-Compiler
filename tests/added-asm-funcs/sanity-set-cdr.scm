; First car of simple list
(define lst '(1 2 3))
(cdr lst)
(set-cdr! lst '(1 1 2))
(cdr lst)

; 2, again simple string list
(define lst2 '("hello" 'bye "helloa"))
(cdr lst2)
(set-cdr! lst2 '("bye"))
(cdr lst2)

;3, car of lists
(define lst3 '((1.5 2 3) 1 2))
(cdr lst3)
(set-cdr! lst3 4)
(cdr lst3)

;4, car of vector
(define lst4 '(#(1 'a) 2 3))
(cdr lst4)
(set-cdr! lst4 1)
(cdr lst4)


