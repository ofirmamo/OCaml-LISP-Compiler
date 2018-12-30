; 1, triple lambdas.. 
((lambda (x)
     ((lambda (y)
        ((lambda (z)
           (* (+ x y) z)) 2)) 3)) 4)

; 2, four lamds..
((lambda (x)
     ((lambda (y)
        ((lambda (z)
           ((lambda (a)
              (* a (+ y (* z x)))) 2)) 3)) 4)) 5)

; 3, five lamds..
((lambda (a)
     ((lambda (b)
        ((lambda (c)
           ((lambda (d)
              ((lambda (e)
                 (+ a (- b (+ c (- e d))))) 1)) 2)) 3)) 4)) 5)
