


(define list-recur
  (lambda (base-value list-proc)
    (letrec
      ([helper
         (lambda (ls)
           (if (null? ls)
               base-value
               (list-proc (car ls)
                 (helper (cdr ls)))))])
      helper)))

(define Average
  (lambda (ls)
    (/ ((list-recur 0 +) ls) (length ls)))) 
     
(define Varience 
  (lambda (ls)
    (let ([ave (Average ls)])
      (/ ((list-recur 0 (lambda (a b) (+ ((lambda (x) (expt (- x ave) 2)) a) b))) ls) (length ls)))))

(define Varience1 
  
  (lambda (ls)
    (let ([ave (Average ls)])
      ((list-recur 0 (lambda (x) (expt (- x 1) 2))) ls))))

(define Varience2
  (lambda (ls)
    (let ([ave (Average ls)])
      (expt (- ave 1) 2))))

(define list-sum (list-recur 0 +))