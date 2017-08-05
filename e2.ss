
;c1:
(define succ
  (lambda (ls)
    (if (null? ls)
        '(1)
        (let ([cur (+ 1 (car ls))])
          (if (= cur 10)
              (cons 0 (succ (cdr ls)))
              (cons cur (cdr ls)))))))















;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process



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
  [for-exp
    (init expression?)
    (checkcase expression?)
    (update (list-of expression?))
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
  [while-exp
    (condi expression?)
    (body expression?)]
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
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))


(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
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
    (env environment?)])
	 	 

	 
	

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
       [(eqv? (1st datum) 'for)
        (for-exp (if (null? (1st (2nd datum))) (parse-exp (1st (2nd datum))) (begin-exp (map parse-exp (1st (2nd datum))))) (parse-exp (3rd (2nd datum))) (map parse-exp (cddddr (2nd datum))) (map parse-exp (cddr datum)))]
       [(eqv? (1st datum) 'case)
        (case-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))]
       [(eqv? (1st datum) 'while)
        (while-exp (parse-exp (2nd datum)) (begin-exp (map parse-exp (cddr datum))))] 
       [(eqv? (1st datum) 'set!)
        (cond[(not (= (length datum) 3)) (eopl:error 'parse-exp "  Error in parse-exp: set! expression: incorrect length:" datum)]
             [else (set!-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))])]
       [(eqv? (1st datum) 'let)
        (cond[(not (> (length datum) 2)) (eopl:error 'parse-exp "  Error in parse-exp: let expression: incorrect length:" datum)]
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
             [else (letrec-exp (map list (map parse-exp (map 1st (2nd datum))) (map parse-exp (map 2nd (2nd datum)))) (map parse-exp (cddr datum)))])]
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
      [set!-exp (id body)
        (list 'set! (unparse-exp id) (unparse-exp body))]
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
      [cond-exp (body)
        (expand-cond body)]
      [case-exp (condi body)
        (syntax-expand (let-exp (list (list (var-exp 'x) condi)) (list(expand-case condi body))))]
      [for-exp (init check update body)
        (if (equal? init '(lit-exp ()))
            [while-exp check [begin-exp (append body update)]]
            [begin-exp (list init [while-exp check [begin-exp (append body update)]])])]
      [else exp]
         
        
      
      
      )))

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
        [let-exp (list(car vari)) (list(syntax-expand (let*-exp (cdr vari) body)))])))
        









;-------------------+
;                   |
;   INTERPRETER    |
;                   |
;-------------------+




; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env)))

; eval-exp is the main component of the interpreter

(define eval-bodies
  (lambda (bodies env)
    (if (null? (cdr bodies))
        (eval-exp (car bodies) env)
        (begin
          (eval-exp (1st bodies) env)
          (eval-bodies (cdr bodies) env)))))


(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
	(apply-env env id
      	   (lambda (x) x) 
           (lambda () (eopl:error 'apply-env "variable not found in environment: ~s" id)))] 
      [lambda-exp (id body)
        (closure (map 2nd id) body env)]
      [lambda-sym-exp (id body)
        (closure-sym (list id) body env)]
      [lambda-pair-exp (id body)
        (closure-pair id body env)]
      [if-exp (bool truecase)
        (if (equal? bool '(var-exp else)) (eval-exp truecase env)
        (if (eval-exp bool env) (eval-exp truecase env)))]
      [if-else-exp (bool truecase falsecase)
        (if (eval-exp bool env) (eval-exp truecase env) (eval-exp falsecase env))]
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands  rands env)])
          (apply-proc proc-value args))]
      [let-exp (vari body)
        (eval-exp (syntax-expand exp) env)]
      [begin-exp (body)
        (for-each (lambda (x) (eval-exp x env)) (map syntax-expand body))]
      [or-exp (body)
        (if (null? body)
           #f
           (let ([next (eval-exp (syntax-expand (car body)) env)])
             (if next next (eval-exp [or-exp (cdr body)] env))))]
      [and-exp (body)
        (if (null? body)
           #t
           (let ([next (eval-exp (syntax-expand (car body)) env)])
             (if next (eval-exp [and-exp (cdr body)] env) #f)))]
      [while-exp (test body)
        (letrec ([loop (lambda ()
                         (if (eval-exp test env)
                             (begin 
                               (eval-exp (syntax-expand body) env)
                               (loop))
                             ))])
          (loop))]
      [for-exp (init check update body)
        (eval-exp (syntax-expand exp) env)]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      [closure (id bodies env)
        (eval-bodies bodies (extend-env id args env))]
      [closure-sym (id bodies env)
        (eval-bodies bodies (extend-env id (list args) env))]
      [closure-pair (id bodies env)
        (let ([my-list (letrec ([group-up (lambda (p ls)
                                      (cond [(symbol?(cdr p)) (list (list (car p) (cdr p)) (list (car ls) (cdr ls)))]
                                            [(pair? (cdr p)) (let ([result (group-up (cdr p) (cdr ls))])
                                                              (list (cons (car p) (car result)) (cons (car ls) (cadr result))))]))])
                     (group-up id args))])
          (eval-bodies bodies (extend-env (1st my-list) (2nd my-list) env)))]
      
      
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * add1 sub1 cons = / quotient zero? not member < > <= >=  car cdr list null? assq eq? equal? atom? length list->vector list? pair?
                            procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! display
                            newline caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr map apply))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(quotient) (apply quotient args)]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (= (1st args) (2nd args))]
      [(zero?) (zero? (1st args))]
      [(not) (not (1st args))]
      [(member) (member (1st args) (2nd args))]
      [(<) (< (1st args) (2nd args))]
      [(>) (> (1st args) (2nd args))]
      [(<=) (<= (1st args) (2nd args))]
      [(>=) (>= (1st args) (2nd args))]
      [(car) (car (1st args))]
      [(cdr) (cdr (1st args))]
      [(list) (apply list args)]
      [(null?) (null? (1st args))]
      [(assq) (assq (1st args) (2nd args))]
      [(eq?) (eq? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(atom?) (atom? (1st args))]
      [(length) (length (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(list?) (list? (1st args))]
      [(pair?) (pair? (1st args))]
      [(procedure?) (apply proc-val? args)]
      ;[(procedure?) (or (if (list? (1st args)) (if (or (eqv? 'prim-proc (1st (1st args))) (eqv? 'closure (1st (1st args)))) #t #f) #f) (apply procedure? args))]
      [(vector->list) (vector->list (1st args))]
      [(vector) (apply vector args)]
      [(make-vector) (make-vector (1st args) (2nd args))]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(vector?) (vector? (1st args))]
      [(number?) (number? (1st args))]
      [(symbol?) (symbol? (1st args))]
      [(set-car!) (set-car! (1st args) (2nd args))]
      [(set-cdr!) (set-cdr! (1st args) (2nd args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(display) (apply display args)]
      [(newline) (apply newline args)]
      [(caar) (caar (1st args))]
      [(cadr) (cadr (1st args))]
      [(cdar) (cdar (1st args))]
      [(cddr) (cddr (1st args))]
      [(caaar) (caaar (1st args))]
      [(caadr) (caadr (1st args))]
      [(cadar) (cadar (1st args))]
      [(caddr) (caddr (1st args))]      
      [(cdaar) (cdaar (1st args))]
      [(cdadr) (cdadr (1st args))]
      [(cddar) (cddar (1st args))]
      [(cdddr) (cdddr (1st args))]
      [(map) (apply map (lambda (x) (apply-proc (1st args) (list x))) (cdr args))]
      [(apply) (apply apply-proc args)]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

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
















