;Han Wei Assignment15

;#2 In this way, we do not need to consider the size of the variable we need. Therefore in this way, our function can work for other calculation too.

(define apply-continuation
  (lambda (k v)
    (k v)))

;#1
;(a)
(define member?-cps 
  (lambda (ele ls k)
    (cond [(null? ls) (apply-continuation k #f)]
          [(eq? ele (car ls)) (apply-continuation k #t)]
          [else (member?-cps ele (cdr ls) k)])))

;(b)
(define set?-cps
  (lambda (ls k)
    (cond [(null? ls) (apply-continuation k #t)]
      [(not (pair? ls)) (apply-continuation k #f)]
      [else (member?-cps (car ls) (cdr ls) (lambda (condi)
                                             (if condi 
                                                (apply-continuation k #f)
                                                (set?-cps (cdr ls) k))))])))

(define 1st-cps 
  (lambda (ls k) 
    (apply-continuation k (car ls))))

(define set-of-cps
  (lambda (ls k)
    (cond [(null? ls) (apply-continuation k '())]
          [else (set-of-cps (cdr ls) (lambda (rls)
                                       (member?-cps (car ls) rls (lambda (condi)
                                                                   (if condi
                                                                         (apply-continuation k rls)
                                                                         (apply-continuation k (cons (car ls) rls)))))))])))
(define map-cps
  (lambda (proc ls k)
    (cond [(null? ls) (apply-continuation k '())]
          [else (map-cps proc (cdr ls) (lambda (rls)
                                         (proc (car ls) (lambda (result)
                                                          (apply-continuation k (cons result rls))))))])))

(define domain-cps
  (lambda (rel k)
    (map-cps 1st-cps rel (lambda (result)
                           (set-of-cps result k)))))


;(c)
(define make-cps
  (lambda (proc)
    (lambda (x k)
      (apply-continuation k (proc x)))))

;(d)
(define andmap-cps
  (lambda (proc ls k)
    (cond[(null? ls) (apply-continuation k #t)]
         [else (proc (car ls) (lambda (condi)
                          (if condi
                              (andmap-cps proc (cdr ls) k)
                              (apply-continuation k #f))))])))

;(e)
(define cps-snlist-recur
  (lambda (seed item-proc list-proc)
    (letrec ([helper
               (lambda (ls k)
                 (cond [(null? ls) (apply-continuation k seed)]
                       [(let ([c (car ls)])
                          (if (or (pair? c) (null? c))
                              (helper c (lambda (cls)
                                            (helper (cdr ls)
                                              (lambda (rls)
                                                (list-proc cls rls k)))))
                              (helper (cdr ls)
                                (lambda (rls)
                                  (item-proc c rls k)))))]))])
    helper)))

(define +-cps
 (lambda (a b k)
 (apply-continuation k (+ a b))))

(define append-cps
  (lambda (ls1 ls2 k)
    (cond [(null? ls2) (apply-continuation k ls1)]
          [else (append-cps (reverse (cons (car ls2) (reverse ls1))) (cdr ls2) k)])))

(define sn-list-reverse-cps
  (cps-snlist-recur '()
    (lambda (ele1 ele2 k)
      (append-cps ele2 (list ele1) k))
    (lambda (ele1 ele2 k)
      (append-cps ele2 (list ele1) k))))

(define sn-list-occur-cps
  (lambda (s sls k)
    ((cps-snlist-recur 0
      (lambda (e count k)
        (if (eq? e s)
            (+-cps 1 count k)
            (apply-continuation k count)))
      +-cps) sls k)))

(define max-cps
  (lambda (num1 num2 k)
    (apply-continuation k (max num1 num2))))

(define sn-list-depth-cps
     (cps-snlist-recur 1
       (lambda (ele sls k)
         (apply-continuation k sls))
       (lambda (sls1 sls2 k)
         (max-cps (+ 1 sls1) sls2 k))))


;2
(define memoize
  (lambda (f hash eqp)
    (let ([htb (make-hashtable hash eqp)])
      (lambda args
        (let ([check (hashtable-ref htb args #f)])
          (if check
              check
              (let ([result (apply f args)])
                (hashtable-set! htb args result)
                result)))))))

;3
(define subst-leftmost
  (letrec ([helper
             (lambda (new old slist eq-f)
               (cond [(null? slist) (values #f '())]
                     [(symbol? (car slist)) (if (eq-f (car slist) old)
                                                (values #t (cons new (cdr slist)))
                                                (call-with-values (lambda () (helper new old (cdr slist) eq-f))
                                                  (lambda (bool body)
                                                    (values bool (cons (car slist) body)))))]
                     [else (call-with-values (lambda () (helper new old (car slist) eq-f))
                             (lambda (boolf bodyf)
                               (if boolf
                                   (values #t (cons bodyf (cdr slist)))
                                   (call-with-values (lambda () (helper new old (cdr slist) eq-f))
                                     (lambda (boolb bodyb)
                                       (values boolb (cons bodyf bodyb)))))))]))])
  (lambda (new old slist eq-f)
    (call-with-values (lambda () (helper new old slist eq-f)) (lambda (a b) b)))))