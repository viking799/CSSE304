;Han Wei Assignment2

;#1
(define fact
  (lambda (n)
    (if (< n 1)
        1
        (* (fact (- n 1)) n))))

(define choose
  (lambda (n k)
    (/ (fact n) (fact k) (fact (- n k)))))

;#2
(define range
  (lambda (n m)
    (if (> m n)
        (cons* n (range (+ n 1) m))
        (list))))

;#3 
(define not-contains?
  (lambda (n ls)
    (if (null? ls)
        #t
        (if (equal? n (car ls))
            #f
            (not-contains? n (cdr ls))))))

(define set?
  (lambda (ls)
    
    (if (null? ls)
        #t
        (if (not-contains? (car ls) (cdr ls))
            (set? (cdr ls))
            #f))))

;#4
(define sum-of-squares
  (lambda (ls)
    (if (null? ls)
        0
        (+ (* (car ls) (car ls)) (sum-of-squares (cdr ls))))))

;#5
(define make-vec-from-points
  (lambda (l1 l2)
    (list (- (car l2) (car l1)) (- (cadr l2) (cadr l1)) (- (caddr l2) (caddr l1)))))

;#6
(define dot-product
  (lambda (l1 l2)
    (+ (* (car l2) (car l1)) (* (cadr l2) (cadr l1)) (* (caddr l2) (caddr l1)))))
    

;#7
(define vec-length
  (lambda (ls)
    (sqrt (+ (* (car ls) (car ls)) (* (cadr ls) (cadr ls)) (* (caddr ls) (caddr ls))))))

;#8
(define distance
  (lambda (l1 l2)
    (vec-length(make-vec-from-points l1 l2))))
    
;#9
(define cross-product
  (lambda (l1 l2)
    (list (- (* (cadr l1) (caddr l2)) (* (caddr l1) (cadr l2))) (- (* (caddr l1) (car l2)) (* (car l1) (caddr l2))) (- (* (car l1) (cadr l2)) (* (cadr l1) (car l2))))))

;#10

(define parallel?
  (lambda (l1 l2)
    (equal? (cross-product l1 l2) '(0 0 0))))

    
;#11
(define collinear? 
  (lambda (p1 p2 p3)
    (parallel? (make-vec-from-points p1 p2) (make-vec-from-points p2 p3))))