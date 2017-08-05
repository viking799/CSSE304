;Han Wei Assignment6a

;#1
(define curry2
  (lambda (prec)
    (lambda (a)
      (lambda (b)
        (prec a b)))))

;#2
(define curried-compose
  (lambda (pre1)
    (lambda (pre2)
      (lambda (ls)
        (pre1 (pre2 ls))))))

;#3
(define compose
  (lambda list-of-functions
      (if(equal? (length list-of-functions) 1)
        (car list-of-functions)
        (lambda (n) 
          ((car list-of-functions) ((apply compose (cdr list-of-functions)) n))))))
    
;#4
(define make-list-c
  (lambda (n)
    (if(zero? n)
      (lambda (ls)
        '())
      (lambda (ls)
        (cons ls ((make-list-c (- n 1)) ls))))))

;#5
(define let->application
  (lambda (ls)
    (cons  (cons 'lambda (cons (map car (cadr ls)) (list (cadr (cdr ls))))) (map cadr (cadr ls)) )))

;#6
(define let*->let
  (letrec ([add-let (lambda (ls n)
                      (if(null? ls)
                        n
                        (list 'let (list (car ls)) (add-let (cdr ls) n) )))])
    (lambda (ls)
    (add-let (cadr ls) (caddr ls)))))

