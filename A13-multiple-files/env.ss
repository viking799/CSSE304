; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and  2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are "callback procedures, 
    (cases environment env       ;  succeed is appluied if sym is found, otherwise 
      [empty-env-record ()       ;  fail is applied.
        (fail)]
      [extended-env-record (syms vals env)
		(let ((pos (list-find-position sym syms)))
      	  (if 	(number? pos)
				(succeed (list-ref vals pos))
				(apply-env env sym succeed fail)))])))

