
;c1:
(define succ
  (lambda (ls)
    (if (null? ls)
        '(1)
        (let ([cur (+ 1 (car ls))])
          (if (= cur 10)
              (cons 0 (succ (cdr ls)))
              (cons cur (cdr ls)))))))




