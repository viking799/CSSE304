;Han Wei Assignment7a

;#1
(define vector-append-list
  (letrec ([copy-from-vector (lambda (nv ov n)
                               (if (< n (vector-length ov))
                                   (and (vector-set! nv n (vector-ref ov n))
                                   (copy-from-vector nv ov (add1 n)))))]
           [copy-from-list (lambda (nv ls n)
                               (if (< n (vector-length nv))
                                   (and (vector-set! nv n (car ls))
                                   (copy-from-list nv (cdr ls) (add1 n)))))])
    (lambda (vec ls)
      (let ([new-vector (make-vector (+ (vector-length vec) (length ls)))])
        (copy-from-vector new-vector vec 0)
        (copy-from-list new-vector ls (vector-length vec))
        new-vector))))

;#2
(define qsort
  (letrec ([add-from-list-t (lambda (prec piv ls)
                            (cond [(null? ls) '()]
                                  [(prec (car ls) piv) (cons (car ls) (add-from-list-t prec piv (cdr ls)))]
                                  [else (add-from-list-t prec piv (cdr ls))]))]
           [add-from-list-f (lambda (prec piv ls)
                            (cond [(null? ls) '()]
                                  [(prec (car ls) piv) (add-from-list-f prec piv (cdr ls))]
                                  [else (cons (car ls) (add-from-list-f prec piv (cdr ls)))]))])
    (lambda (prec ls)
      (if(< (length ls) 2)
        ls
      (append (qsort prec (add-from-list-t prec (car ls) (cdr ls))) (list (car ls))
        (qsort prec (add-from-list-f prec (car ls) (cdr ls))))))))

;#3
(define merge
(lambda (ls new)
                    (cond [(null? ls) (list new)]
                          [(interact? (car ls) new) (merge (cdr ls) (union (car ls) new))]
                          [else (cons (car ls) (merge (cdr ls) new))])))

(define union-set
  (lambda (ls)
                    (union-helper (list (car ls)) (cdr ls))))
(define union-helper (lambda (ans ls)
                           (cond [(null? ls) ans]
                                 [(member #t (map (lambda (x) (interact? x (car ls))) ans)) (union-helper (merge ans (car ls)) (cdr ls))]
                                 [else (union-helper (cons (car ls) ans) (cdr ls))])))
(define interact?
(lambda (l1 l2)
                        (cond [(null? l2) #f]
                              [(list?(member (car l2) l1)) #t]
                              [else (interact? l1 (cdr l2))])))

(define union
  (lambda (l1 l2)
                    (cond [(null? l2) l1]
                          [(list?(member (car l2) l1)) (union (cdr l2) l1)]
                          [else (cons (car l2) (union (cdr l2) l1))])))

(define connected?
    (lambda (ls)
      (equal? 1 (length (union-set (map (lambda (t) (cons (car t) (cadr t))) ls))))))
             
;#4
(define reverse-it
  (letrec([back (lambda (l1 l2)
                 (cond[(null? l1) l2]
                      [(back (cdr l1) (cons (car l1) l2))]))])
    (lambda (ls)
      (back ls '()))))

;#5
(define empty-BST
  (lambda ()
    '()))

(define empty-BST?
  (lambda (obj)
    (null? obj)))

(define BST-insert
  (lambda (num bst)
    (cond [(empty-BST? bst) (list num '() '())]
          [(= (car bst) num) bst]
          [(> (car bst) num) (list (car bst) (BST-insert num (cadr bst)) (caddr bst))]
          [(< (car bst) num) (list (car bst) (cadr bst) (BST-insert num (caddr bst)))])))

(define BST-inorder
  (lambda (bst)
    (if (empty-BST? bst)
        '()
        (append (BST-inorder (cadr bst)) (list (car bst)) (BST-inorder (caddr bst))))))

(define BST?
  (lambda (obj)
    (cond [(empty-BST? obj) #t]
          [(not (list? obj)) #f]
          [(not (equal? (length obj) 3)) #f]
          [(not (number? (car obj))) #f]
          [(not (BST? (cadr obj))) #f]
          [(not (BST? (caddr obj))) #f]
          [(not (apply < (BST-inorder obj))) #f]
          [else #t])))

(define BST-element
  (lambda (obj)
    (car obj)))

(define BST-left
  (lambda (obj)
    (cadr obj)))

(define BST-right
  (lambda (obj)
    (caddr obj)))

(define BST-insert-nodes
  (lambda (bst nums)
    (if(null? nums)
      bst
      (BST-insert-nodes (BST-insert (car nums) bst) (cdr nums)))))

(define BST-contains?
  (lambda (bst num)
    (cond [(empty-BST? bst) #f]
          [(= (car bst) num) #t]
          [(> (car bst) num) (BST-contains? (cadr bst) num)]
          [(< (car bst) num) (BST-contains? (caddr bst) num)])))