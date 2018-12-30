; String set
(define x "h")
(string-set! x 0 #\x)
x

; String length
(string-length "hellllllo")

; make string
(make-string 4 #\y)

; vector length
(vector-length '#(1 2 3 4 5))

; vector set
(define vec '#(1 2 3 4 5))
(vector-set! vec 2 6)
vec



