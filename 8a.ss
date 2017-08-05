;Han Wei Assignment8a

;#1
(define (slist-map prec slist)
  (let s-mapping ([slist slist])
    (cond [(null? slist) '()]
          [(symbol? (car slist)) (cons (prec (car slist)) (s-mapping (cdr slist)))]
          [else (cons (s-mapping (car slist)) (s-mapping (cdr slist)))])))

(define (slist-reverse slist)
  (let s-reverse ([slist slist])
    (cond [(null? slist) '()]
          [(symbol? (car slist)) (append (s-reverse (cdr slist)) (list(car slist)))]
          [else (append (s-reverse (cdr slist)) (list(s-reverse (car slist))))])))

(define (slist-paren-count slist)
  (let s-pc ([slist slist])
    (cond [(null? slist) 2]
          [(symbol? (car slist)) (s-pc (cdr slist))]
          [else (+ (s-pc (car slist)) (s-pc (cdr slist)))])))

(define (slist-depth slist)
  (let s-depth ([slist slist])
    (cond [(null? slist) 1]
          [(symbol? (car slist)) (s-depth (cdr slist))]
          [else (max  (+ 1 (s-depth (car slist))) (s-depth (cdr slist)))])))

(define (slist-symbols-at-depth slist num)
  (let s-ad ([slist slist] [depth 1])
    (cond [(null? slist) '()]
          [(symbol? (car slist)) (if (equal? depth num) 
                                     (cons (car slist) (s-ad (cdr slist) depth))
                                     (s-ad (cdr slist) depth))]
          [else (append (s-ad (car slist) (+ 1 depth)) (s-ad (cdr slist) depth))])))

;#2
(define group-by-two
  (lambda (ls)
    (cond [(null? ls) ls]
          [(equal? (length ls) 1) (list ls)]
          [else (cons (list (car ls) (cadr ls)) (group-by-two (cddr ls)))])))

;#3

(define make-stack
(lambda ()
 (let ([stk '()])
   (lambda (msg . args )
     (case msg 
       [(empty?) (null? stk)]
       [(push) (set! stk (cons (car args) stk))]
       [(pop) (let ([top (car stk)])
                (set! stk (cdr stk))
                top)]
 [else (errorf 'stack "illegal message to stack object: ~a" msg)])))))
                

(define make-slist-leaf-iterator
    (lambda (slist)
      (let ([stack (make-stack)])
          (letrec ([next
              (lambda ()
                (cond [(stack 'empty?) #f]
                      [else (let ([current (stack 'pop)])
                              (cond [(null? current) (next)]
                                    [(symbol? current) current]
                                    [else (begin 
                                            (stack 'push (cdr current))
                                            (stack 'push (car current))
                                            (next))]))]))])
            (begin
              (stack 'push slist)
              (lambda (msg . args)
                (case msg
                  [(next) (next)])))))))
                