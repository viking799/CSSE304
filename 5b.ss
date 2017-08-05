;Han Wei Assignment5b

;#6
(define max-edges 
  (lambda (n)
    (if(< n 2)
      0
      (+ (- n 1) (max-edges (- n 1))))))

;#7
(define complete?
  (lambda (g)
    (not (list? (member #f (map (lambda (n) (equal? (- (length g) 1) n)) (map (lambda(n) (length (cadr n))) g)))))))

;#8
(define complete
  (letrec([to-all-point 
            (lambda (obj points)
              (if(null? obj)
                '()
                (cons (list (car obj) (remove (car obj) points)) (to-all-point (cdr obj) points))))])
    (lambda (n)
      (to-all-point n n))))

;#9
(define replace
  (lambda (a b ls)
    (if(null? ls)
      '()
      (if(equal? a (car ls))
        (cons b (replace a b (cdr ls)))
        (cons (car ls) (replace a b (cdr ls)))))))

;#10
(define remove-first
  (lambda (n ls)
    (if(null? ls)
      '()
      (if(equal? n (car ls))
        (cdr ls)
        (cons (car ls) (remove-first n (cdr ls)))))))

;#11
(define remove-last
  (lambda (n ls)
    (if (member n ls)
        (if (member n (cdr ls))
            (cons (car ls) (remove-last n (cdr ls)))
            (cdr ls))
        ls)))
