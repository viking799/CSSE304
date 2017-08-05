;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process

;(load "chez-init.ss") 
;(load "C:/Users/weih/Desktop/304/chez-init.ss")
;(load "C:/Users/weih/Desktop/304/A18-test-code.ss")
;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+

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
  [lambda-sym-exp
    (id symbol?)
    (body (list-of expression?))]
  [lambda-pair-exp
    (id pair?)
    (body (list-of expression?))]
  [lambda-ref-exp
    (vars list?)
    (body (list-of expression?))]
  [let-exp-top
    (vari (list-of (list-of expression?)))
    (body (list-of expression?))]
  [let-exp
    (vari (list-of (list-of expression?)))
    (body (list-of expression?))]
  [letrec-exp
    (proc-names (list-of symbol?))
    (idss (list-of (lambda (v) (or (pair? v) (list-of symbol? v)))))
    (bodies (list-of (list-of expression?)))
    (letrec-bodies (list-of expression?))]
  [if-exp
    (bool expression?)
    (truecase expression?)]
  [if-else-exp
    (bool expression?)
    (truecase expression?)
    (falsecase expression?)]
  [begin-exp
    (body (list-of expression?))]
  [cond-exp
    (body (list-of (list-of expression?)))]
  [and-exp
    (body (list-of expression?))]
  [or-exp
    (body (list-of expression?))]
  [case-exp
    (condi expression?)
    (body (list-of (lambda (x) (expression? (2nd x)) )))] 
  [let*-exp
    (vari (list-of (list-of expression?)))
    (body (list-of expression?))]
  [set!-exp
    (sym symbol?)
    (body expression?)]
  [define-exp
    (sym symbol?)
    (body expression?)]
  [app-exp        ; applications
   (rator expression?)
   (rands (list-of expression?))]
  [named-let-exp
    (name symbol?)
    (idss (list-of symbol?))
    (bodies (list-of expression?))
    (named-let-bodies (list-of expression?))]
  )

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)]
  [recursively-extended-env-record
    (proc-names (list-of symbol?))
    (idss (list-of (lambda (v) (or (pair? v) (list-of symbol? v)))))
    (bodies (list-of (list-of expression?)))
    (env environment?)])

(define-datatype continuation continuation?
  [apply-k 
    (proc procedure?)]
  [exit-k 
    ]
  [if-k
    (truecase expression?)
    (env environment?)
    (k continuation?)]
  [if-else-k
    (truecase expression?)
    (falsecase expression?)
    (env environment?)
    (k continuation?)]
  [rator-k 
    (rands (list-of expression?))
    (env list?)
    (k continuation?)]
  [rands-k 
    (proc-value proc-val?) 
    (k continuation?)]
  [let-k
    (vars (list-of symbol?))
    (bodies (list-of expression?))
    (env list?)
    (k continuation?)]
  [set!-k
    (exp expression?)
    (env environment?)
    (k continuation?)]
  [box-k
    (bx box?)
    (k continuation?)]
  [or-k
    (body (list-of expression?))
    (env environment?)
    (k continuation?)]
  [and-k
    (body (list-of expression?))
    (env environment?)
    (k continuation?)]
  [eval-bodies-k
    (bodies (list-of expression?))
    (env environment?)
    (k continuation?)]
  [add-global-k 
    (id symbol?)
    (k continuation?)]
  )
  
  

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [k-proc
    (k continuation?)]
  [exit-proc]
  [closure
    (ids (list-of symbol?))
    (bodies (list-of expression?))
    (env environment?)]
   [closure-sym
    (ids (list-of symbol?))
    (bodies (list-of expression?))
    (env environment?)]
  [closure-pair
    (ids pair?)
    (bodies (list-of expression?))
    (env environment?)]
  [closure-ref
    (vars list?)
    (bodies (list-of expression?))
    (env environment?)]
  )
	 	 

	 
	

;-------------------+
;                   |
;    PARSER         |
;                   |
;-------------------+

; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)]
     [(number? datum) (lit-exp datum)]
     [(vector? datum) (lit-exp datum)]
     [(string? datum) (lit-exp datum)]
     [(boolean? datum) (lit-exp datum)]
     [(char? datum) (lit-exp datum)]
     [(null? datum) (lit-exp datum)] 
     [(list? datum)
      (cond
       [(andmap (lambda (x)
                  (ormap 
                    (lambda (pred) (pred x))
                    (list number? vector? boolean? string?))) datum) (lit-exp datum)]
       [(eqv? (1st datum) 'quote) (lit-exp (2nd datum))]
       [(eqv? (1st datum) 'lambda)
        (cond[(not (> (length datum) 2)) (eopl:error 'parse-exp "  Error in parse-exp: lambda expression: incorrect length:" datum)]
             [(symbol? (2nd datum)) (lambda-sym-exp (2nd datum) (map parse-exp (cddr datum)))]
             [(and (not (list? (2nd datum))) (pair? (2nd datum))) (if (letrec ([check (lambda (x) (if (symbol? (car x)) (cond [(symbol? (cdr x)) #t] [(pair? (cdr x)) (check (cdr x))] [else #f]) #f))]) (check (2nd datum)))
                                      (lambda-pair-exp (2nd datum) (map parse-exp (cddr datum)))
                                      (eopl:error 'parse-exp "    Error in parse-exp: invalid arguement " datum))]
             [(and (andmap (lambda (x) (if (list? x) (eqv? (1st x) 'ref) (symbol? x))) (2nd datum)) (ormap list? (2nd datum)))
                                              (lambda-ref-exp (2nd datum) (map parse-exp (cddr datum)))]
             [(not (andmap symbol? (2nd datum))) (eopl:error 'parse-exp "    Error in parse-exp: invalid arguement " datum)]
             [else (lambda-exp (map parse-exp (2nd  datum)) (map parse-exp (cddr datum)))])]
       [(eqv? (1st datum) 'if)
        (cond[(< (length datum) 3) (eopl:error 'parse-exp "  Error in parse-exp: if expression: incorrect length:" datum)]
             [(> (length datum) 4) (eopl:error 'parse-exp "  Error in parse-exp: if expression: incorrect length:" datum)]
             [(= (length datum) 3) (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
             [else (if-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (cadddr datum)))])]
       [(eqv? (1st datum) 'begin)
        (begin-exp (map parse-exp (cdr datum)))]
       [(eqv? (1st datum) 'cond)
        (cond-exp (map (lambda (x) (list (parse-exp (1st x)) (parse-exp (2nd x)))) (cdr datum)))]
       [(eqv? (1st datum) 'and)
        (and-exp (map parse-exp (cdr datum)))]
       [(eqv? (1st datum) 'or)
        (or-exp (map parse-exp (cdr datum)))]
       [(eqv? (1st datum) 'case)
        (case-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))] 
       [(eqv? (1st datum) 'set!)
        (cond[(not (= (length datum) 3)) (eopl:error 'parse-exp "  Error in parse-exp: set! expression: incorrect length:" datum)]
             [else (set!-exp (2nd datum) (parse-exp (3rd datum)))])]
       [(eqv? (1st datum) 'define)
        (define-exp (2nd datum) (parse-exp (3rd datum)))]
       [(eqv? (1st datum) 'let)
        (cond[(not (> (length datum) 2)) (eopl:error 'parse-exp "  Error in parse-exp: let expression: incorrect length:" datum)]
             [(and (symbol? (2nd datum)) (andmap symbol? (map 1st (3rd datum)))) 
              (named-let-exp (2nd datum) (map 1st (3rd datum)) (map parse-exp (map 2nd (3rd datum))) (map parse-exp (cdddr datum)))]
             [(not (list? (2nd datum))) (eopl:error 'parse-exp "  Error in parse-exp: let expression: invalid arguement:" datum)]
             [(not (andmap list? (2nd datum))) (eopl:error 'parse-exp "  Error in parse-exp: let expression: not all proper list:" datum)]
             [(not (andmap (lambda (x) (= 2 (length x))) (2nd datum))) (eopl:error 'parse-exp "  Error in parse-exp: let expression: incorrect arguement in list:" datum)]
             [(not (andmap symbol? (map 1st (2nd datum)))) (eopl:error 'parse-exp "  Error in parse-exp: let expression: first number must be symbol:" datum)]
             [else (let-exp (map list (map parse-exp (map 1st (2nd datum))) (map parse-exp (map 2nd (2nd datum)))) (map parse-exp (cddr datum)))])]
       [(eqv? (1st datum) 'letrec)
        (cond[(not (> (length datum) 2)) (eopl:error 'parse-exp "  Error in parse-exp: let expression: incorrect length:" datum)]
             [(not (list? (2nd datum))) (eopl:error 'parse-exp "  Error in parse-exp: let expression: invalid arguement:" datum)]
             [(not (andmap list? (2nd datum))) (eopl:error 'parse-exp "  Error in parse-exp: let expression: incorrect arguement:" datum)]
             [(not (andmap (lambda (x) (= 2 (length x))) (2nd datum))) (eopl:error 'parse-exp "  Error in parse-exp: let expression: incorrect length:" datum)]
             [(not (andmap symbol? (map 1st (2nd datum)))) (eopl:error 'parse-exp "  Error in parse-exp: letrec expression: first number must be symbol:" datum)]
             [else (letrec-exp (map 1st (2nd datum)) (map 2nd (map 2nd (2nd datum))) (map list (map parse-exp (map 3rd (map 2nd (2nd datum))))) (map parse-exp (cddr datum)))])]
       [(eqv? (1st datum) 'let*)
        (cond[(not (> (length datum) 2)) (eopl:error 'parse-exp "  Error in parse-exp: let expression: incorrect length:" datum)]
             [(not (list? (2nd datum))) (eopl:error 'parse-exp "  Error in parse-exp: let expression: invalid arguement:" datum)]
             [(not (andmap list? (2nd datum))) (eopl:error 'parse-exp "  Error in parse-exp: let expression: incorrect arguement:" datum)]
             [(not (andmap (lambda (x) (= 2 (length x))) (2nd datum))) (eopl:error 'parse-exp "  Error in parse-exp: let expression: incorrect length:" datum)]
             [(not (andmap symbol? (map 1st (2nd datum)))) (eopl:error 'parse-exp "  Error in parse-exp: let* expression: first number must be symbol:" datum)]
             [else (let*-exp (map list (map parse-exp (map 1st (2nd datum))) (map parse-exp (map 2nd (2nd datum)))) (map parse-exp (cddr datum)))])]
      [else (app-exp (parse-exp (1st datum))
		     (map parse-exp (cdr datum)))])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))


(define unparse-exp
  (lambda (exp)
    (cases expression exp
      [var-exp (id) id]
      [lit-exp (id) id]
      [lambda-exp (id body)
        (cond[(null? id) (cons 'lambda (cons (map unparse-exp id) (map unparse-exp body)))]
             [(list? (car id)) (cons 'lambda (cons (map unparse-exp id) (map unparse-exp body)))]
             [else (cons 'lambda (cons (unparse-exp id) (map unparse-exp body)))])]
      [if-exp (bool truecase)
        (list 'if (unparse-exp bool) (unparse-exp truecase))]
      [if-else-exp (bool truecase falsecase)
        (list 'if (unparse-exp bool) (unparse-exp truecase) (unparse-exp falsecase))]
      [let-exp (vari body)
        (cons 'let (cons (map list (map unparse-exp (map 1st vari)) (map unparse-exp (map 2nd vari))) (map unparse-exp body)))]
      [letrec-exp (vari body)
        (cons 'letrec (cons (map list (map unparse-exp (map 1st vari)) (map unparse-exp (map 2nd vari))) (map unparse-exp body)))]
      [let*-exp (vari body)
        (cons 'let* (cons (map list (map unparse-exp (map 1st vari)) (map unparse-exp (map 2nd vari))) (map unparse-exp body)))]
      [set!-exp (sym body)
        (list 'set! (unparse-exp sym) (unparse-exp body))]
      [app-exp (rator rand)
        (cons (unparse-exp rator)
          (map unparse-exp rand)
          )])))






;-------------------+
;                   |
;   ENVIRONMENTS    |
;                   |
;-------------------+


; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and  2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (map box vals) env)))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
            (recursively-extended-env-record
              proc-names idss bodies old-env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (x) (eqv? sym x)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-init-env
  (lambda (sym fail)
    (let ([pos (list-find-position sym (2nd init-env))])
      (if (number? pos)
          (list-ref (3rd init-env) pos)
          (fail)
          ))))


(define apply-env-ref
  (lambda (env sym k fail)
    (cases environment env
      [empty-env-record ()
        (find-global-env sym (lambda (x) (apply-init-env x fail)) global-env)]
      [extended-env-record (syms vals env)
	(let ((pos (list-find-position sym syms)))
      	  (if 	(number? pos)
            	(list-ref vals pos)
            	(apply-env-ref env sym k fail)))]
      [recursively-extended-env-record (procnames idss bodies old-env)
        (let ([pos (list-find-position sym procnames)])
          (if (number? pos)
              (let ([id (list-ref idss pos)])
                (if (and (pair? id) (not (list? id)))
                    (box (closure-pair id (list-ref bodies pos) env))
                    (box (closure id (list-ref bodies pos) env))))
              (apply-env-ref old-env sym k fail)))])))


(define apply-env
  (lambda (env sym k fail)
    (apply-continuation k (unbox (apply-env-ref env sym k fail)))))

(define global-env '())

(define reset-global-env
  (lambda ()
    (set! global-env '())))

(define add-global-env
  (lambda (sym exp)
    (set! global-env (cons (list sym (box exp)) global-env))))

(define find-global-env
  (lambda (sym fail g-env)
    (if (null? g-env)
        (fail sym)
        (if (equal? sym (car (car g-env)))
            (cadr (car g-env))
            (find-global-env sym fail (cdr g-env))))))
     





;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+


(define syntax-expand
  (lambda (exp)
    (cases expression exp
      [let-exp (vari body)
        (app-exp [lambda-exp (map 1st vari) (map syntax-expand body)] (map 2nd vari))]
      [let*-exp (vari body)
        (syntax-expand (expand-let* vari body))]
      [begin-exp (bodies)
        (app-exp [lambda-exp '() (map syntax-expand (2nd exp))] '())] 
      [cond-exp (body)
        (expand-cond body)]
      [case-exp (condi body)
        (syntax-expand (let-exp (list (list (var-exp 'x) condi)) (list(expand-case condi body))))]
      [named-let-exp (names idss bodies named-let-bodies)
        (letrec-exp (list names) (list idss) (list named-let-bodies) (list (app-exp (var-exp names) bodies)))]
      [else exp])))

(define expand-cond
  (lambda (body)
    (if (= 1 (length body))
        (if-exp (caar body) (cadar body))
        (if-else-exp (caar body) (cadar body) (expand-cond (cdr body))))))

(define expand-case
  (lambda (condi body)
    (if (= 1 (length body))
        (1st (caddar body))
        (if-else-exp [app-exp (var-exp 'member) (list (var-exp 'x) (cadar body))] (1st (caddar body)) (expand-case condi (cdr body))))))
          
(define expand-let*
  (lambda (vari body)
      (if (null? vari) 
        [let-exp '() body]
        [let-exp (list(car vari)) (list (syntax-expand (let*-exp (cdr vari) body)))])))

      









;-------------------+
;                   |
;   INTERPRETER    |
;                   |
;-------------------+




; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env) (exit-k))))

; eval-exp is the main component of the interpreter




(define eval-exp
  (lambda (exp env k)
    (cases expression exp
      [lit-exp (datum) (apply-continuation k datum)]
      [var-exp (id)
	(apply-env env id
      	   k 
           (lambda () (eopl:error 'apply-env "variable not found in environment: ~s" id)))] 
      [lambda-exp (id body)
        (apply-continuation k (closure (map 2nd id) body env))]
      [lambda-sym-exp (id body)
        (apply-continuation k (closure-sym (list id) body env))]
      [lambda-pair-exp (id body)
        (apply-continuation k (closure-pair id body env))]
      [lambda-ref-exp (id body)
        (apply-continuation k (closure-ref id body env))]
      [if-exp (bool truecase)
        (if (equal? bool '(var-exp else))
            (eval-exp truecase env k) 
            (eval-exp bool env (if-k truecase env k)))]
      [if-else-exp (bool truecase falsecase)
        (eval-exp bool env (if-else-k truecase falsecase env k))]
      [app-exp (rator rands)
        (eval-exp rator env (rator-k rands env k))]
      [let-exp (vari body)
        (eval-exp (syntax-expand exp) env k)]
      [let*-exp (vari body)
        (eval-exp (syntax-expand exp) env k)]
      [begin-exp (body)
        (eval-exp (syntax-expand exp) env k)]
      [or-exp (body)
        (if (null? body)
           (apply-continuation k #f)
           (eval-exp (car body) env (or-k (cdr body) env k)))]
      [and-exp (body)
        (if (null? body)
           (apply-continuation k #t)
           (eval-exp (car body) env (and-k (cdr body) env k)))]
      [letrec-exp (proc-names idss bodies letrec-bodies)
       (eval-bodies letrec-bodies
         (extend-env-recursively
           proc-names idss (map list (map syntax-expand (map car bodies))) env) k)]
      [set!-exp (sym body)
        (apply-continuation (set!-k body env k)
            (apply-env-ref
                    env
                    sym
                    (set!-k body env k)
                    (lambda () (eopl:error 'apply-env for set!-exp "variable not found in environment: ~s" id))))]
      [define-exp (sym body)
        (eval-exp (syntax-expand body) env (add-global-k sym k))]
      [named-let-exp (name idss bodies name-letbody)
        (eval-exp (syntax-expand exp) env k)]
                    
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-bodies
  (lambda (bodies env k)
    (if (null? (cdr bodies))
        (eval-exp (syntax-expand (car bodies)) env k)
        (eval-exp (syntax-expand (car bodies)) env (eval-bodies-k (cdr bodies) env k)))))

(define eval-rands
  (lambda (rands env k)
    (map-cps (lambda (x kk) (eval-exp (syntax-expand x) env kk)) rands k)))
    
(define eval-rands-ref
  (lambda (rands env vars)
    (if (null? vars)
        '()
        (if (list? (1st vars))
            (cons (1st rands) (eval-rands-ref (cdr rands) env (cdr vars)))
            (cons (apply-env-ref env (2nd (1st rands))
                    (lambda (x) x)
                    (lambda () (eopl:error "variable not found in environment ~s" rands)))
              (eval-rands-ref (cdr rands) env (cdr vars)))
            ))))

(define map-cps
  (lambda (proc ls k)
    (if (null? ls) 
        (apply-continuation k '())
        (map-cps proc (cdr ls) 
          (apply-k (lambda (rls)
                           (proc (car ls) 
                             (apply-k (lambda (result)
                                       (apply-continuation k (cons result rls)))))))))))




;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args k)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args k)]
      [k-proc (kp) (apply-continuation kp (1st args))]
      [exit-proc () (apply-continuation (exit-k) args)]
      [closure (id bodies env)
        (eval-bodies bodies (extend-env id args env) k)]
      [closure-sym (id bodies env)
        (eval-bodies bodies (extend-env id (list args) env) k)]
      [closure-pair (id bodies env)
        (let ([my-list (letrec ([group-up (lambda (p ls)
                                      (cond [(symbol?(cdr p)) (list (list (car p) (cdr p)) (list (car ls) (cdr ls)))]
                                            [(pair? (cdr p)) (let ([result (group-up (cdr p) (cdr ls))])
                                                              (list (cons (car p) (car result)) (cons (car ls) (cadr result))))]))])
                     (group-up id args))])
          (eval-bodies bodies (extend-env (1st my-list) (2nd my-list) env) k))]
      [closure-ref (vars bodies env)
          (eval-bodies bodies (extend-env (map (lambda (x) (if (list? x) (2nd x) x)) vars) args env) k)]
      
      
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * add1 sub1 cons = / quotient zero? not member < > <= >=  car cdr list null? assq eq? equal? atom? length list->vector list? pair?
                            procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! display
                            newline caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr map apply list-tail eqv? append call/cc exit-list))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args k)
    (case prim-proc
      [(+) (apply-continuation k (apply + args))]
      [(-) (apply-continuation k (apply - args))]
      [(*) (apply-continuation k (apply * args))]
      [(/) (apply-continuation k (apply / args))]
      [(quotient) (apply-continuation k (apply quotient args))]
      [(add1) (apply-continuation k (+ (1st args) 1))]
      [(sub1) (apply-continuation k (- (1st args) 1))]
      [(cons) (apply-continuation k (cons (1st args) (2nd args)))]
      [(=) (apply-continuation k (= (1st args) (2nd args)))]
      [(zero?) (apply-continuation k (zero? (1st args)))]
      [(not) (apply-continuation k (not (1st args)))]
      [(member) (apply-continuation k (member (1st args) (2nd args)))];
      [(<) (apply-continuation k (< (1st args) (2nd args)))]
      [(>) (apply-continuation k (> (1st args) (2nd args)))]
      [(<=) (apply-continuation k (<= (1st args) (2nd args)))]
      [(>=) (apply-continuation k (>= (1st args) (2nd args)))]
      [(car) (apply-continuation k (car (1st args)))]
      [(cdr) (apply-continuation k (cdr (1st args)))]
      [(list) (apply-continuation k (apply list args))]
      [(null?) (apply-continuation k (null? (1st args)))]
      [(assq) (apply-continuation k (assq (1st args) (2nd args)))]
      [(eq?) (apply-continuation k (eq? (1st args) (2nd args)))]
      [(equal?) (apply-continuation k (equal? (1st args) (2nd args)))]
      [(atom?) (apply-continuation k (atom? (1st args)))]
      [(length) (apply-continuation k (length (1st args)))]
      [(list->vector) (apply-continuation k (list->vector (1st args)))]
      [(list?) (apply-continuation k (list? (1st args)))]
      [(pair?) (apply-continuation k (pair? (1st args)))]
      [(procedure?) (apply-continuation k (apply proc-val? args))]
      [(vector->list) (apply-continuation k (vector->list (1st args)))]
      [(vector) (apply-continuation k (apply vector args))]
      [(make-vector) (apply-continuation k (make-vector (1st args) (2nd args)))]
      [(vector-ref) (apply-continuation k (vector-ref (1st args) (2nd args)))]
      [(vector?) (apply-continuation k (vector? (1st args)))]
      [(number?) (apply-continuation k (number? (1st args)))]
      [(symbol?) (apply-continuation k (symbol? (1st args)))]
      [(set-car!) (apply-continuation k (set-car! (1st args) (2nd args)))]
      [(set-cdr!) (apply-continuation k (set-cdr! (1st args) (2nd args)))]
      [(vector-set!) (apply-continuation k (vector-set! (1st args) (2nd args) (3rd args)))]
      [(display) (apply-continuation k (apply display args))]
      [(newline) (apply-continuation k (apply newline args))]
      [(caar) (apply-continuation k (caar (1st args)))]
      [(cadr) (apply-continuation k (cadr (1st args)))]
      [(cdar) (apply-continuation k (cdar (1st args)))]
      [(cddr) (apply-continuation k (cddr (1st args)))]
      [(caaar) (apply-continuation k (caaar (1st args)))]
      [(caadr) (apply-continuation k (caadr (1st args)))]
      [(cadar) (apply-continuation k (cadar (1st args)))]
      [(caddr) (apply-continuation k (caddr (1st args)))]      
      [(cdaar) (apply-continuation k (cdaar (1st args)))]
      [(cdadr) (apply-continuation k (cdadr (1st args)))]
      [(cddar) (apply-continuation k (cddar (1st args)))]
      [(cdddr) (apply-continuation k (cdddr (1st args)))]
      [(map) (map-p-cps (1st args) (map list (2nd args)) k)] 
      ;(lambda (x) (apply-proc (1st args) (list x) k)) (cdr args))]
      [(apply) (apply-continuation k (apply apply-proc (append args (list (exit-k)))))]
      [(list-tail) (apply-continuation k (apply list-tail args))]
      [(eqv?) (apply-continuation k (apply eqv? args))]
      [(append) (apply-continuation k (apply append args))]
      [(call/cc) (apply-proc (1st args) (list (k-proc k)) k)]
      [(exit-list) (apply-proc (exit-proc) args (exit-k))] 
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

(define map-p-cps
  (lambda (proc ls k)
    (if (null? ls) 
        (apply-continuation k '())
        (map-p-cps proc (cdr ls) 
          (apply-k (lambda (rls)
                           (apply-proc proc (car ls) 
                             (apply-k (lambda (result)
                                 (apply-continuation k (cons result rls)))))))))))
    
(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))

;-------------------+
;                   |
; continuation      |
;                   |
;-------------------+

(define apply-continuation
  (lambda (k val)
    (cases continuation k
      [apply-k (proc) 
        (proc val)]
      [exit-k ()
        val]
      [if-k (truecase env k)
        (if val (eval-exp truecase env k) (apply-continuation k (void)))]
      [if-else-k (truecase falsecase env k)
        (if val (eval-exp truecase env k) (eval-exp falsecase env k))]
      [rator-k (rands env k)
        (eval-rands rands env (rands-k val k))]
      [rands-k (proc k)
        (apply-proc proc val k)]
      [let-k (vars bodies env k)
        (eval-bodies bodies (extend-env vars val env) k)]
      [set!-k (exp env k)
        (eval-exp exp env (box-k val k))]
      [box-k (bx k)
        (apply-continuation k (set-box! bx val))]
      [or-k (body env k)
        (if val
            (apply-continuation k val)
            (if (null? body)
                (apply-continuation k #f)
                (eval-exp (1st body) env (or-k (cdr body) env k))))]
      [and-k (body env k)
        (if val
            (if (null? body)
                (apply-continuation k #t)
                (eval-exp (car body) env (and-k (cdr body) env k)))
            #f)]
      [add-global-k (id k)
        (apply-continuation k (add-global-env id val))]
      [eval-bodies-k (bodies env k)
        (eval-bodies bodies env k)]
      [else
        val]
      )))


 