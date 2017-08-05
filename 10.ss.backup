;Han Wei Assignment10

;#1
(define free-vars
  (lambda (ls)
    (to-set(free-var-helper ls '()))))

(define bound-vars
  (lambda (ls)
    (to-set(bound-var-helper ls '()))))

(define list-remove 
  (letrec ([list-remove-helper (lambda (mls ele)
                                 (if (null? mls)
                                     '()
                                     (if (equal? (car mls) ele)
                                         (list-remove-helper (cdr mls) ele)
                                         (cons (car mls) (list-remove-helper (cdr mls) ele)))))])
  (lambda (mls rls)
    (if (null? rls)
      mls
      (list-remove (list-remove-helper mls (car rls)) (cdr rls))))))

(define free-var-helper
  (lambda (ls var)
    (cond [(null? ls) '()]
          [(symbol? ls) (if (member ls var) '() (list ls))]
          [(equal? (car ls) 'lambda)
            (free-var-helper (caddr ls) (append (cadr ls) var))]
          [(equal? (car ls) 'if)
            (free-var-helper (cons (cadr ls) (cddr ls)) var)]
          [(equal? (car ls) 'let)
            (append (free-var-helper (map cadr (cadr ls)) var) (list-remove (free-var-helper (cddr ls) var) (map car (cadr ls))))]
          [(equal? (car ls) 'let*)
            (list-remove (append (map cadr (cadr ls)) (free-var-helper (cddr ls) var)) (map car (cadr ls)))]
          [(equal? (car ls) 'set!)
           (cddr ls)]
          [else (append (free-var-helper (car ls) var) (free-var-helper (cdr ls) var))])))
      
(define bound-var-helper
  (lambda (ls var)
    (cond [(null? ls) '()]
          [(symbol? ls) (if (member ls var) (list ls) '())]
          [(equal? (car ls) 'lambda)
            (bound-var-helper (caddr ls) (append (cadr ls) var))]
          [(equal? (car ls) 'if)
            (bound-var-helper (cons (cadr ls) (cddr ls)) var)]
          [(equal? (car ls) 'let)
            (append (map car (cadr ls)) (bound-var-helper (cddr ls) var))]
          [(equal? (car ls) 'let*)
            (bound-var-helper (append (map car (cadr ls)) (cddr ls)) (append (map cadr (cadr ls)) var))]
          [(equal? (car ls) 'set!)
           (cddr ls)]
          [(equal? (car ls) 'set!)
           (cdr ls)]
          [else (append (bound-var-helper (car ls) var) (bound-var-helper (cdr ls) var))])))

(define to-set
  (lambda (ls)
    (if (null? ls)
        '()
        (if (member (car ls) (cdr ls)) (cdr ls) (cons (car ls) (to-set (cdr ls)))))))

;#2
(define occurs-free?
  (lambda (ele ls)  
    (if (member ele (free-vars ls)) #t #f)))

(define occurs-bound?
  (lambda (ele ls)
    (if (member ele (bound-vars ls)) #t #f)))

;#3
(define lexical-address
  (lambda (exp)
    (letrec ([helper (lambda (exp vars)
                       (cond[(symbol? exp) (buildformat exp vars 0)]
                            [(eqv? 'lambda (car exp))
                             (list 'lambda (cadr exp) (helper (caddr exp) (cons (cadr exp) vars)))]
                            [(eqv? 'if (car exp))
                             (cons 'if (helper (cdr exp) vars))]
                            [(eqv? 'let (car exp))
                             (list 'let (map (lambda (x) (list (car x) (helper (cadr x) vars))) (cadr exp)) (helper (caddr exp) (cons (map car (cadr exp)) vars)))]
                            [(eqv? 'set! (car exp))
                             (list 'set! (cadr exp) (helper (caddr exp) vars))]
                            [else
                              (map (lambda (x) (helper x vars)) exp)]
                         ))])
      (helper exp '()))))
                         
(define buildformat
  (lambda (sym vars depth)
    (if (null? vars)
        (list ': 'free sym)
        (let ((index (getIndex sym (car vars))))
          (if (= -1 index)
              (buildformat sym (cdr vars) (+ depth 1))
              (list ': depth index))))))
          
          
(define getIndex
  (lambda (item vars)
    (if (null? vars)
        -1
        (if (equal? (car vars) item)
            0
            (let ((remain (getIndex item (cdr vars))))
              (if (= remain -1)
                  -1
                  (add1 remain)))))))

;#4 (work with Linjie Zha)
(define un-lexical-address
            (letrec ([helper (lambda (exp vars)
                               (cond[(= (length exp) 1)
                                     (list (helper (car exp) vars))]
                                    [(eqv? (cadr exp) 'free)
                                     (caddr exp)]
                                    [(number? (cadr exp))
                                     (list-ref (list-ref vars (cadr exp)) (caddr exp))]
                                    [(eqv? (car exp) 'lambda)
                                     (list 'lambda (cadr exp) (helper (caddr exp) (cons (cadr exp) vars)))]
                                    [(eqv? (car exp) 'if)
                                     (cons 'if (cons (helper (cadr exp) vars) (helper (cddr exp) vars)))]
                                    [(eqv? (car exp) 'let)
                                     (list 'let (map (lambda (x) (list (car x) (helper (cadr x) vars))) (cadr exp))
                                       (helper (caddr exp) (cons (map car (cadr exp)) vars)))]
                                    [(eqv? (car exp) 'set!)
                                     (cons 'set! (cons (cadr exp) (helper (cddr exp) vars)))]
                                    [else
                                     (map (lambda (x) (helper x vars)) exp)]))])
              (lambda (exp)
              (helper exp '()))))