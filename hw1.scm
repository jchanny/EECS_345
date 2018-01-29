;;Jeremy Chan, jsc126

;;#1: Insert: returns lis with n inserted in the correct location 
;; params: number n, sorted list lis
(define insert
  (lambda (n lis)
	(cond
	 ((null? lis) '())
	 ((< (car lis) n) (cons (car lis) (insert n (cdr lis))))
	 ((>= (car lis) n) (cons n (cons (car lis) (cdr lis)))))))

;;#2: Merge: returns a sorted combination of two lists
;; params: l, list 1; r, list 2
(define merge
  (lambda (l r)
    (cond
      ((null? l)
       (if(null? r) '()
          (cons (car r) (cdr r))))
      (else (if (< (car l) (car r))
                (cons (car l) (merge (cdr l) (car r)))
               
                ))
      )))
      

