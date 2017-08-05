;Han Wei Text1

;#1
(define max-diff 
  (lambda (ls)
    (- (apply max ls) (apply min ls))))

;#2
(define member-n-times?
  (lambda (ele ls times)
    (let ([next (member ele ls)])
      (if next
          (member-n-times? ele (cdr next) (sub1 times))
          (<= times 0)))))

;#3
(define most-frequent
  (letrec ([get-same (lambda (sample times ls)
                       (cond[(null? ls) (list sample times ls)]
                            [(equal? sample (car ls)) (get-same sample (add1 times) (cdr ls))]
                            [else (list sample times ls)]))]
           [compare (lambda (ls)
                      (if(null? ls)
                        '(DNE -1)
                        (let ([current (get-same (car ls) 1 (cdr ls))])
                           (let ([next (compare (caddr current))])
                             (if(>= (cadr current) (cadr next))
                               (list (car current) (cadr current))
                                next)))))])
    (lambda (ls)
      (if(null? ls)
        #f
        (car (compare (list-sort < ls)))))))

;#4
(define slist-same?
  (lambda (s1 s2 prec)
    (cond [(and (null? s1) (null? s2)) #t]
          [(or (null? s1) (null? s2)) #f]
          [(and (symbol? (car s1)) (symbol? (car s2))) 
           (if (prec (car s1) (car s2))
               (slist-same? (cdr s1) (cdr s2) prec)
               #f)]
          [(and (list? (car s1)) (list? (car s2)))
           (if(slist-same? (car s1) (car s2) prec)
             (slist-same? (cdr s1) (cdr s2) prec)
             #f)]
          [else #f])))

;#5
(define make-lyst
  (letrec ([getnumber (lambda (num ls)
    (if (equal? num 1)
        (car ls)
        (getnumber (sub1 num) (cdr ls))))])
  (lambda ()
    (let ([lyst '()])
      (lambda (msg . args)
        (case msg
          [(len) (length lyst)]
          [(first) (car lyst)]
          [(last) (getnumber (length lyst) lyst)]
          [(get) (getnumber (add1 (car args)) lyst)]
          [(add-first) (set! lyst (cons (car args) lyst))]
          [(add-last) (set! lyst (append lyst args))]))))))