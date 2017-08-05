
;; Parsed expression datatypes

(define-datatype expression expression?
  [var-exp        ; variable references
   (id symbol?)]
  [lit-exp        ; "Normal" data.  Did I leave out any types?
   (datum
    (lambda (x)
      (ormap 
       (lambda (pred) (pred x))
       (list number? vector? boolean? symbol? string? pair? null?))))]
  [lambda-exp
   (id (lambda (v) (or (symbol? v) (list-of expression?))))
   (body (list-of expression?))]
  [let-exp
    (vari (list-of (list-of expression?)))
    (body (list-of expression?))]
  [letrec-exp
    (vari (list-of (list-of expression?)))
    (body (list-of expression?))]
  [if-exp
    (bool expression?)
    (truecase expression?)]
  [if-else-exp
    (bool expression?)
    (truecase expression?)
    (falsecase expression?)]
  [let*-exp
    (vari (list-of (list-of expression?)))
    (body (list-of expression?))]
  [set!-exp
    (id symbol?)
    (body expression?)]
  [app-exp        ; applications
   (rator expression?)
   (rands (list-of expression?))]  
  )

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
    (ids (list-of symbol?))
    (bodies (list-of expression?))
    (env (lambda (x) #t))])
	 	 
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))