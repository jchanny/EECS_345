;;#1: insert: places number into a list in the correct location
;;params: n-number to be inserted; lis-list number to inserted into
(define insert-cps
  (lambda (n lis return)
	(cond
	 ((null? lis) (return (cons n '())))
         ((> (car lis) n) (return (cons n (cons (car lis) (cdr lis)))))
         (else (insert-cps n (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

(define insert
  (lambda (n lis)
	(insert-cps n lis (lambda (v) v))))

;;#2: merge: combines 2 lists together, sorted in correct oder
;;params: l-list 1; r-list 2
