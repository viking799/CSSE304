;Han Wei Assignment6b

;#7
(define filter-in
  (lambda (proc ls)
    (if(null? ls)
      '()
      (if(proc (car ls))
        (cons (car ls) (filter-in proc (cdr ls)))
        (filter-in proc (cdr ls))))))

;#8
(define filter-out
  (lambda (proc ls)
    (if(null? ls)
      '()
      (if (not (proc (car ls)))
        (cons (car ls) (filter-out proc (cdr ls)))
        (filter-out proc (cdr ls))))))

;#9
(define sort-list-of-symbols
  (lambda (los)
    (map string->symbol (sort string<? (map symbol->string los)))))

;#10
(define invert 
  (lambda (ls)
    (if(null? ls)
      '()
      (cons (list (cadr (car ls)) (car (car ls))) (invert (cdr ls)) ))))

;#11
(define vector-index 
  (lambda (prec ls)
    (list-index prec (vector->list ls))))

(define list-index
   (letrec ([first-true (lambda (n)
                         (if(car n)
                           0
                           (+ 1 (first-true (cdr n)))))]
            [exist? (lambda (ls)
                      (if(member #t ls)
                        (first-true ls)
                        #f))])
  (lambda (prec ls)
    (exist? (map prec ls)))))

;#12
(define ribassoc
  (letrec ([split (lambda (n m p)
                  (if(number? n)
                    (get n m)
                    p))]
           [get (lambda (n m)
             (if(zero? n)
               (car m)
               (get (- n 1) (cdr m))))])
  (lambda (c los v fail-value)
    (split (list-index (lambda (x) (equal? x c)) los) (vector->list v) fail-value))))
      
