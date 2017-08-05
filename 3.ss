;Han Wei Assignment3


;#1
(define vec-length
  (lambda (ls)
    (sqrt (+ (* (car ls) (car ls)) (* (cadr ls) (cadr ls)) (* (caddr ls) (caddr ls))))))

(define make-vec-from-points
  (lambda (l1 l2)
    (list (- (car l2) (car l1)) (- (cadr l2) (cadr l1)) (- (caddr l2) (caddr l1)))))

(define distance
  (lambda (l1 l2)
    (vec-length(make-vec-from-points l1 l2))))

(define nearest-point 
  (lambda (n p)
    (if (equal? (length p) 1)
        (car p)
        (if (<= (distance n (car p)) (distance n (nearest-point n (cdr p))))
            (car p)
            (nearest-point n (cdr p))))))


;#2
(define not-contains?
  (lambda (n ls)
    (if (null? ls)
        #t
        (if (equal? n (car ls))
            #f
            (not-contains? n (cdr ls))))))

(define union
  (lambda (l1 l2)
    (if (equal? (length l2) 0)
        l1
        (if (not-contains? (car l2) l1)
            (union (append l1 (list(car l2))) (cdr l2))
            (union l1 (cdr l2))))))
            

;#3
(define intersection
  (lambda (l1 l2)
    (intersec '() l1 l2)))

(define intersec 
  (lambda (re l1 l2)
    (if (equal? (length l1) 0)
        re
        (if(not-contains? (car l1) l2)
          (intersec re (cdr l1) l2)
          (intersec (append re (list (car l1))) (cdr l1) l2)))))

;#4
(define subset?
  (lambda (l1 l2)
    (equal? l1 (intersection l1 l2))))
      
;#5
(define set?
  (lambda (ls)
    
    (if (null? ls)
        #t
        (if (not-contains? (car ls) (cdr ls))
            (set? (cdr ls))
            #f))))


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

;#6
(define domain
  (lambda (ls)
    (add-domain '() ls)))

(define add-domain
  (lambda (dos ls)
    (if(equal? (length ls) 0)
      dos
      (add-domain (if(not-contains? (car (car ls)) dos) (union (list (car (car ls))) dos) dos ) (cdr ls)))))

;#7
(define reflexive?
  (lambda (ls)
    (check-ref (get-element '() ls) ls)))

(define get-element
  (lambda (ele ls)
    (if (equal? (length ls) 0)
        ele
        (get-element (union (cdr(car ls)) (union (list(car (car ls))) ele)) (cdr ls)))))
        
(define check-ref
  (lambda (ele ls)
    (if (equal? (length ele) 0)
        #t
        (if(not-contains? (list (car ele) (car ele)) ls)
          #f
          (check-ref (cdr ele) ls)))))

    
;#8
(define hailstone-step-count 
  (lambda (n)
    (if(equal? n 1)
      0
      (+ 1 (hailstone-step-count (hailstone n))))))

(define hailstone
  (lambda (n)
    (if(equal? (modulo n 2) 0)
      (/ n 2)
      (+ (* n 3) 1))))