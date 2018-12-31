(define list
	(lambda l
		l))

;; With params
(list 1 2 3)

;; without params
(list)

;;; list with mandatory params
(define list2 (lambda (x . l) l))

(list2 1 2 3)
(list2 1.5 2 1.7)
(list2 "Hello" 'world "Bye")
(list2 1)

