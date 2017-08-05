;Assignment 19

;(load "chez-init.ss") 
;(load "C:/Users/weih/Desktop/304/chez-init.ss")
;(load "C:/Users/weih/Desktop/304/A19-test-code.ss")

(define-datatype kontinuation kontinuation?
  [init-k]
  [flatten-cdr-k (ls list?) (k kontinuation?)]
  [flatten-car-k  (flattened-cdr list?)
		  (k kontinuation?)]
  [append-k (car-L1 symbol?) (k kontinuation?)]
)


(define k)
(define v)
(define L1)
(define L2)
(define ls)

(define apply-k 
  (lambda ()
	 (cases kontinuation k
	    [init-k ()
	       v]
	    [flatten-cdr-k (kls kk)
	       (if (list? (car kls))
                   (begin (set! ls (car kls))
                          (set! k (flatten-car-k v kk))
                          (flatten-cps))
                   (begin (set! k kk)
                          (set! v (cons (car kls) v))
                          (apply-k)))]
            [flatten-car-k (fcdr kk)
               (begin (set! L1 v)
                      (set! L2 fcdr)
                      (set! k kk)
                      (append-cps))]
	    [append-k (carL1 kk)
               (begin (set! k kk)
                      (set! v (cons carL1 v))
                      (apply-k))])))


(define append-cps 
  (lambda ()
    (if (null? L1)
        (begin (set! v L2)
	       (apply-k))
        (begin (set! k (append-k (car L1) k))
               (set! L1 (cdr L1))
               (append-cps)))))
	



(define flatten-cps
  (lambda ()
    (if (null? ls)
        (begin (set! v ls)
         	     (apply-k))
        (begin (set! k (flatten-cdr-k ls k))
               (set! ls (cdr ls))
               (flatten-cps)))))