;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process

;(load "chez-init.ss") 

;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+

(define-datatype expression expression?
  [var-exp        ; variable references
   (id (lambda (v) (or (symbol? v) (lexical? v))))]
  [lex-exp
    (id lexical?)]
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
  [while-exp
    (condi expression?)
    (body expression?)]
  [let*-exp
    (vari (list-of (list-of expression?)))
    (body (list-of expression?))]
  [set!-exp
    (sym (lambda (v) (or (symbol? v) (expression? v))))
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

(define lexical?
  (lambda (x)
    (if (list x)
        (if (= (length x) 3)
            (if (equal? ': (1st x))
                (if (number? (2nd x))
                    (if (number? (3rd x))
                        #t
                        #f)
                    (if  (equal? 'free (2nd x))
                        #t
                        #f))
                #f)
            #f)
        #f)))

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
       [(eqv? (1st datum) 'while)
        (while-exp (parse-exp (2nd datum)) (begin-exp (map parse-exp (cddr datum))))] 
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


(define parse-lex         
  (lambda (datum)
    (cond
     [(lexical? datum) (lex-exp datum)]
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
       [(eqv? (1st datum) 'while)
        (while-exp (parse-exp (2nd datum)) (begin-exp (map parse-exp (cddr datum))))] 
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
  (lambda (sym succeed fail)
    (let ([pos (list-find-position sym (2nd init-env))])
      (if (number? pos)
      (succeed (list-ref (3rd init-env) pos))
      (fail)
          ))))


(define apply-env-ref
  (lambda (env sym succeed fail)
    (cases environment env
      [empty-env-record ()
        (find-global-env sym succeed (lambda (x) (apply-init-env x succeed fail)) global-env)]
      [extended-env-record (syms vals env)
	(let ((pos (list-find-position sym syms)))
      	  (if 	(number? pos)
            	(succeed (list-ref vals pos))
            	(apply-env-ref env sym succeed fail)))]
      [recursively-extended-env-record (procnames idss bodies old-env)
        (let ([pos (list-find-position sym procnames)])
          (if (number? pos)
              (let ([id (list-ref idss pos)])
                (if (and (pair? id) (not (list? id)))
                    (box (closure-pair id (list-ref bodies pos) env))
                    (box (closure id (list-ref bodies pos) env))))
              (apply-env-ref old-env sym succeed fail)))])))


(define apply-env
  (lambda (env sym succeed fail)
    (unbox (apply-env-ref env sym succeed fail))))

(define global-env '())

(define reset-global-env
  (lambda ()
    (set! global-env '())))

(define add-global-env
  (lambda (sym exp)
    (set! global-env (cons (list sym (box exp)) global-env))))

(define find-global-env
  (lambda (sym succeed fail g-env)
    (if (null? g-env)
        (fail sym)
        (if (equal? sym (car (car g-env)))
            (succeed (cadr (car g-env)))
            (find-global-env sym succeed fail (cdr g-env))))))
     





;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+


(define syntax-expand
  (lambda (exp)
    (cases expression exp
      [let-exp-top (vari body)
        (app-exp [lambda-exp (map 1st vari) (map syntax-expand body)] (map 2nd vari))]
      [let-exp (vari body)
        (syntax-expand (expand-let vari body))]
      [let*-exp (vari body)
        (syntax-expand (expand-let* vari body))]
      [cond-exp (body)
        (expand-cond body)]
      [case-exp (condi body)
        (syntax-expand (let-exp (list (list (var-exp 'x) condi)) (list(expand-case condi body))))]
      [named-let-exp (names idss bodies named-let-bodies)
        (letrec-exp (list names) (list idss) (list named-let-bodies) (list (app-exp (var-exp names) bodies)))]
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
        [let-exp-top '() body]
        [let-exp-top (list(car vari)) (list (syntax-expand (let*-exp (cdr vari) body)))])))

(define expand-let
  (lambda (vari body)
    (if (null? vari)
        [let-exp-top '() body]
        [let-exp-top (list(car vari)) (list (syntax-expand (let-exp (cdr vari) body)))])))
        









;-------------------+
;                   |
;   INTERPRETER    |
;                   |
;-------------------+




; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env))))

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
      [lambda-ref-exp (id body)
        (closure-ref id body env)]
      [if-exp (bool truecase)
        (if (equal? bool '(var-exp else)) (eval-exp (syntax-expand truecase) env)
        (if (eval-exp bool env) (eval-exp (syntax-expand truecase) env)))]
      [if-else-exp (bool truecase falsecase)
        (if (eval-exp bool env) (eval-exp (syntax-expand truecase) env) (eval-exp (syntax-expand falsecase) env))]
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)])
          (cases proc-val proc-value
            [closure-ref (vars body envir)
              (let ([args (eval-rands-ref rands env vars)])
                (apply-proc proc-value args))]
            [else 
              (let ([args (eval-rands  rands env)])
                (apply-proc proc-value args))]))]
      [let-exp (vari body)
        (eval-exp (syntax-expand exp) env)]
      [let*-exp (vari body)
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
      [letrec-exp (proc-names idss bodies letrec-bodies)
       (eval-bodies letrec-bodies
         (extend-env-recursively
           proc-names idss (map list (map syntax-expand (map car bodies))) env))]
      [set!-exp (sym body)
        (if (not (expression? (eval-exp (var-exp sym) env)))
            (set-box! (apply-env-ref
                    env
                    sym
                    (lambda (x) x)
                    (lambda () (eopl:error 'apply-env for set!-exp "variable not found in environment: ~s" id)))
              (eval-exp (syntax-expand body) env))
        
          
        (set-box! (apply-env-ref
                    (if (equal? (eval-exp (var-exp sym) env) (var-exp (2nd (eval-exp (var-exp sym) env)))) (cadddr env) env)
                    (2nd (eval-exp (var-exp sym) env))
                    (lambda (x) x)
                    (lambda () (eopl:error 'apply-env for set!-exp "variable not found in environment: ~s" id)))
          (let ([curr (let ([cur (eval-exp (syntax-expand body) env)])
                        (if (expression? cur) (eval-exp cur env) cur))])
            (if (box? curr) (unbox curr) curr)))
          )]
      [define-exp (sym body)
        (add-global-env sym (eval-exp (syntax-expand body) env))]
      [named-let-exp (name idss bodies name-letbody)
        (eval-exp (syntax-expand exp) env)]
                    
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (let ([current (eval-exp x env)]) (if (expression? current)  (eval-exp current env) current ))) rands)))
    
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
      [closure-ref (vars bodies env)
          (eval-bodies bodies (extend-env (map (lambda (x) (if (list? x) (2nd x) x)) vars) args env))]
      
      
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * add1 sub1 cons = / quotient zero? not member < > <= >=  car cdr list null? assq eq? equal? atom? length list->vector list? pair?
                            procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! display
                            newline caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr map apply list-tail eqv? append))

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
      [(list-tail) (apply list-tail args)]
      [(eqv?) (apply eqv? args)]
      [(append) (apply append args)]
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
  (lambda (x) (top-level-eval (syntax-expand (parse-lex (lexical-address x))))))

(define eval-one-exp-ori
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))



;-------------------+
;                   |
; LEXICAL ADDRESS   |
;                   |
;-------------------+



(define lexical-address
  (lambda (exp)
    (letrec ([helper (lambda (exp vars)
                       (cond[(symbol? exp) (buildformat exp vars 0)]
                            [(number? exp) (buildformat exp vars 0)]
                            [(boolean? exp) (buildformat exp vars 0)]
                            [(eqv? 'lambda (car exp))
                             (list 'lambda (cadr exp) (helper (caddr exp) (cons (cadr exp) vars)))]
                            [(eqv? 'if (car exp))
                             (cons 'if (helper (cdr exp) vars))]
                            [(eqv? 'let (car exp))
                             (list 'let (map (lambda (x) (list (car x) (helper (cadr x) vars))) (cadr exp)) (helper (caddr exp) (cons (map car (cadr exp)) vars)))]
                            [(eqv? 'set! (car exp))
                             (list 'set! (cadr exp) (helper (caddr exp) vars))]
                            [else
                              (map (lambda (x) (helper x vars)) exp)]
                         ))])
      (helper exp '()))))
                         
(define buildformat
  (lambda (sym vars depth)
    (if (null? vars)
        sym
        (let ((index (getIndex sym (car vars))))
          (if (= -1 index)
              (buildformat sym (cdr vars) (+ depth 1))
              (list ': depth index))))))
          
          
(define getIndex
  (lambda (item vars)
    (if (null? vars)
        -1
        (if (equal? (car vars) item)
            0
            (let ((remain (getIndex item (cdr vars))))
              (if (= remain -1)
                  -1
                  (add1 remain)))))))
