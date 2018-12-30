; 1, simple test summarize 2 bounds params..
(((lambda (x y) (lambda () (+ y x))) 1.5 2.5))

; 2, Summarize and set! for bounds..
(((lambda (x y) 
	(lambda () (set! x y) 
		(+ (+ x x) y))) 
	1 2))

; 3, override param and aply on 2 bounds..
((lambda (x y) 
	((lambda (z) 
		(set! z +) 
		(z x y)) 
		1)) 
	1.5 4.5)


