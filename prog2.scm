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
(define merge-cps
  (lambda (l r return)
    (cond
     ((null? l) (return r))
     ((null? r) (return l))
     ((< (car l) (car r)) (merge-cps (cdr l) r (lambda (v) (return (cons (car l) v)))))
     (else (merge-cps l (cdr r) (lambda (v) (return (cons (car r) v))))))))

(define merge
  (lambda (l r)
    (merge-cps l r (lambda (v) v))))

;;#3: removedups: removes successive repeating elements from a list
;;params: lis: list of atoms
(define removedups-cps
  (lambda (lis return)
    (cond
     ((null? lis) (return lis))
     ((null? (cdr lis)) (return lis))
     ((eq? (car lis) (cadr lis)) (removedups-cps (cdr lis) (lambda (v) (return v))))
     (else (removedups-cps (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

(define removedups
  (lambda (lis)
    (removedups-cps lis (lambda (v) v))))

;;#4: numparens: returns number of parentheses pairs in a list
;;params: lis: list containing atoms and parentheses
(define numparens-cps
  (lambda (lis return)
    (cond
     ((null? lis) (return 1))
     ((list? (car lis)) (numparens-cps (car lis) (lambda (v1) (numparens-cps (cdr lis) (lambda (v2) (return (+ v1 v2)))))))
     (else (numparens-cps (cdr lis) return)))))

(define numparens
  (lambda (lis)
    (numparens-cps lis (lambda (v) v))))

;;#5: dup*: duplicates contents and sublists
;;params: lis: list of atoms, including sublists
(define dup*-cps
  (lambda (lis return)
    (cond
     ((null? lis) (return lis))
     ((list? (car lis)) (dup*-cps (car lis) (lambda (v1) (dup*-cps (car lis) (lambda (v2) (dup*-cps (cdr lis) (lambda (v3) (return (cons v1 (cons v2 v3))))))))))
     (else (dup*-cps (cdr lis) (lambda (v) (return (cons (car lis) (cons (car lis) v)))))))))

(define dup*
  (lambda (lis)
    (dup*-cps lis (lambda (v) v))))

;;#6: removedups*: removes repeating adjacent atoms in the same sublist
;;params: lis: list of atoms, including sublists
(define removedups*-cps
  (lambda (lis return)
    (cond
	 ((null? lis) (return lis))
	 ((null? (cdr lis)) (return lis))
	 ((list? (car lis)) (removedups*-cps (car lis) (lambda (v1) (removedups*-cps (cdr lis) (lambda (v2) (return (cons v1 v2)))))))
	 ((eq? (car lis) (cadr lis)) (removedups*-cps (cdr lis) (lambda (v) (return v))))
	 (else (removedups*-cps (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

(define removedups*
  (lambda (lis)
	(removedups*-cps lis (lambda (v) v))))

;;#7: mergesort: sorts a list of integers
;;params: lis, list of integers, unsorted
(define split-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return '() '()))
      ((null? (cdr lis)) (return lis '()))
      (else (split-cps (cddr lis) (lambda (v1 v2) (return (cons (car lis) v1) (cons (cadr lis) v2))))))))

split continuation: (lambda (v1 v2) (list v1 v2))
