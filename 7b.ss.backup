;Han Wei Assignment7b

;#6
(define map-by-position 
  (lambda (fl al)
    (map apply fl (map list al))))

;#7
(define bt-leaf-sum
  (lambda (bt)
    (if(number? bt)
      bt
      (+ (bt-leaf-sum (cadr bt)) (bt-leaf-sum (caddr bt))))))

(define bt-inorder-list
  (lambda (bt)
    (if(number? bt)
      '()
      (append (bt-inorder-list (cadr bt))  (list (car bt)) (bt-inorder-list (caddr bt))))))

(define bt-max
  (lambda (bt)
    (if(number? bt)
      bt
      (max (bt-max (cadr bt)) (bt-max (caddr bt))))))

(define bt-max-interior
  (lambda (bt)
    (caar (bt-pick bt))))

(define bt-pick
  (lambda (bt)
    (cond [(and (number? (cadr bt)) (number? (caddr bt))) (list (list (car bt) (+ (cadr bt) (caddr bt))) (list (car bt) (+ (cadr bt) (caddr bt))))]
      [(and (list? (cadr bt)) (list? (caddr bt))) (list-picka (bt-pick (cadr bt)) (bt-pick (caddr bt)) (car bt))]
      [(and (list? (cadr bt)) (number? (caddr bt))) (list-pickb (bt-pick (cadr bt)) (caddr bt) (car bt))]
      [(and (number? (cadr bt)) (list? (caddr bt))) (list-pickc (bt-pick (caddr bt)) (cadr bt) (car bt))])))

(define list-picka
  (lambda (l1 l2 s)
    (if (< (cadar l1) (cadar l2))
      (if(> (cadar l2) (+ (cadadr l1) (cadadr l2)))
        (list (car l2) (list s (+ (cadadr l1) (cadadr l2))))
        (list (list s (+ (cadadr l1) (cadadr l2))) (list s (+ (cadadr l1) (cadadr l2)))))
      (if(< (cadar l1) (+ (cadadr l1) (cadadr l2)))
        (list (list s (+ (cadadr l1) (cadadr l2))) (list s (+ (cadadr l1) (cadadr l2))))
        (list (car l1) (list s (+ (cadadr l1) (cadadr l2)))))
        )))

(define list-pickb
  (lambda (ls num k)
    (if(< (cadar ls) (+ num (cadadr ls)))
      (list (list k (+ num (cadadr ls))) (list k (+ num (cadadr ls))))
      (list (car ls) (list k (+ num (cadadr ls)))))))

(define list-pickc
  (lambda (ls num k)
    (if(> (cadar ls) (+ num (cadadr ls)))
      (list (car ls) (list k (+ num (cadadr ls))))
      (list (list k (+ num (cadadr ls))) (list k (+ num (cadadr ls)))))))