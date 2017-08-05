;Han Wei Assignment1

;#1
(define Fahrenheit->Celsius
  (lambda (n)
    (/(* (- n 32) 5) 9)))

;#2
(define interval-contains?
  (lambda (interval number)
    (if (< number (car interval))
        #f
        (if(> number (cadr interval))
            #f
            #t))))

;#3
(define interval-intersects?
  (lambda (i1 i2)
    (if ( > (car i1) (car i2))
        (if (interval-contains? i2 (car i1))
            #t
            #f)
        (if (interval-contains? i1 (car i2))
            #t
            #f))))

;#4
(define interval-union
  (lambda (i1 i2)
    (if (interval-intersects? i1 i2)
        (list (list (min (car i1) (car i2)) (max (cadr i1) (cadr i2))))
        (list i1 i2))))

;#5
(define divisible-by-7?
  (lambda (num)
    (eq? 0 (modulo num 7))))

;#6
(define ends-with-7?
  (lambda (num)
    (eq? 0 (modulo (- num 7) 10))))

;#7
(define 1st
  (lambda (ls)
    (car ls)))

(define 2nd
  (lambda (ls)
    (cadr ls)))

(define 3rd
  (lambda (ls)
    (caddr ls)))