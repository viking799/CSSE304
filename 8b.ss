
;Han Wei Assignment8b

;#1
(define group-by-n
  (letrec([get-n 
            (lambda (ls num)
              (if(zero? num)
                  '()
                (cons (car ls) (get-n (cdr ls) (sub1 num)))))]
          [remain-n 
            (lambda (ls num)
              (if(zero? num)
                  ls
                (remain-n (cdr ls) (sub1 num))))]
          )
  (lambda (ls num)
    (cond[(null? ls) '()]
         [(<= (length ls) num) (list ls)]
         [else (cons (get-n ls num) (group-by-n (remain-n ls num) num) )]))))

;#2
;worked with Linjie Zha, we came up with the idea about adding a boolean in front of the slist. 
(define subst-leftmost
  (letrec ([helper
             (lambda (new old slist eq-f)
               (cond [(null? slist) (cons #f '())]
                     [(symbol? (car slist)) (if (eq-f (car slist) old)
                                                (cons #t (cons new (cdr slist)))
                                                (let([next (helper new old (cdr slist) eq-f)])
                                                  (cons (car next) (cons (car slist) (cdr next)))))]
                     [else (let ([next (helper new old (car slist) eq-f)])
                        (if (car next)
                            (cons #t (cons (cdr next) (cdr slist)))
                            (let([next+ (helper new old (cdr slist) eq-f)])
                                                  (cons (car next+) (cons (cdr next) (cdr next+))))))]))])
  (lambda (new old slist eq-f)
    (cdr (helper new old slist eq-f)))))