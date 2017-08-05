;Han Wei Assignment5


;#1
(define minimize-interval-list
  (letrec ([min-f (lambda (n)
                   (if(equal? (length n) 1)
                     n
                     (if(< (car (cadr n)) (cadr(car n)))
                       (cons car(n) (min-f cdr(n)))
                       (min-f (append (interval-union (car n) (cadr n)) (cddr n))))
                     
                     
                     
                     ))]
           [sort-f (lambda (a b)
                     (< (car a) (car b)))]
    (lambda (n)
      (min-f (list-sort sort-f n))))))


;#1
(define set?
  (lambda (ls)
    
    (if (null? ls)
        #t
        (if (member (car ls) (cdr ls))
            #f
            (set? (cdr ls))
            ))))


(define relation?
  (lambda (ls)
    (if(list? ls)
      (if(set? ls)
        (check-relation? ls)
        #f)
      #f)))

(define check-relation?
  (lambda (ls)
    (if(equal? (length ls) 0)
      #t
      (if(list? (car ls))
          (if(equal? (length (car ls)) 2)
            (check-relation? (cdr ls))
            #f)
          #f))))



(define multi-set?
  (lambda (obj)
    (if(relation? obj)
      (if(member #f (map format? obj))
        #f
        (set? (map car obj)))
      #f)))

(define format? 
  (lambda (ls)
    (and (symbol? (car ls)) (if(number? (cadr ls)) (positive? (cadr ls)) #f))))

;#2
(define ms-size
  (lambda (obj)
    (apply + (map cadr obj))))

;#3
(define matrix-ref
  (lambda (m i j)
    (list-ref (list-ref m i) j)))

;#4
(define matrix?
  (lambda (m)
    (if(list? m)
      (if(member #f (map list? m))
         #f
         (if(null? (car m))
           #f
         (not(member #f (map (lambda (x) (equal? x (length (car m)))) (map length m))))))
      #f)))
      
;#5
(define matrix-transpose
  (lambda (m)
    (if (null? (car m))
        '()
        (cons (map car m) (matrix-transpose (map cdr m))))))
  
;#6
(define last
  (lambda (ls)
    (if(equal? (length ls) 1) (car ls) (last (cdr ls)))))

;#7
(define all-but-last
  (lambda (ls)
    (if(equal? (length ls) 1)
      '()
      (cons (car ls) (all-but-last (cdr ls))))))


