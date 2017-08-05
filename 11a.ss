;Han Wei Assignment11a
;; Binary trees using define-datatype
(load "chez-init.ss") ; chez-init.ss should be in the same folder as this code.
;; from EoPL, page 50
(define-datatype bintree bintree?
 (leaf-node
 (num integer?))
 (interior-node
 (key symbol?)
 (left-tree bintree?)
 (right-tree bintree?)))


;#1a
(define-syntax my-let
  (syntax-rules ()
    [(_ ((x v) ...) e1)
       ((lambda (x ...) e1) v ...)]
    [(_ n ((x v) ...) e1)
      (letrec([n
                (lambda (x ...)
                  e1)])
        (n v ...))]))
  
;#1b
(define-syntax my-or
  (syntax-rules ()
    [(_) #f]
    [(_ e1) e1]
    [(_ e1 e2 ...) (let ((next e1))
                     (if next next (my-or e2 ...)))]))

;#1c
(define-syntax +=
  (syntax-rules ()
    [(_ e1 e2) (begin (set! e1 (+ e1 e2))
                 e1)]))

;#1d
(define-syntax return-first
  (syntax-rules ()
    [(_ e1 e2 ...) (let ((return e1))
                     (begin e2 ...)
                       return)
     ]))


;#2
(define bintree-to-list
  (lambda (tree)
    (cases bintree tree
      [leaf-node (datum)
        (list 'leaf-node datum)]
      [interior-node (key l-tree r-tree)
        (list 'interior-node key (bintree-to-list l-tree) (bintree-to-list r-tree))]))) 

;#3
(define max-interior
  (lambda (tree)
    (caar (max-interior-helper tree))))

(define max-interior-helper
  (lambda (tree)
    (cases bintree tree
      [leaf-node (datum)
        datum]
      [interior-node (key l-tree r-tree)
        (let ((l-result (max-interior-helper l-tree)) (r-result (max-interior-helper r-tree)))
          (cond[(and (number? l-result) (number? r-result)) (list (list key (+ l-result r-result)) (list key (+ l-result r-result)))]
               [(and (list? l-result) (number? r-result)) (list-pickb l-result r-result key)]
               [(and (list? l-result) (list? r-result)) (list-picka l-result r-result key)]
               [(and (number? l-result) (list? r-result)) (list-pickc r-result l-result key)]))])))



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