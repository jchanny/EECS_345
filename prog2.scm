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

(define merge-cps
  (lambda (l r return)
    (cond
     ((null? l) (return r))
     ((null? r) (return l))
     ((< (car l) (car r)) (merge-cps (cdr l) r (lambda (v) (return (cons (car l) v)))))
     (else (merge-cps l (cdr r) (lambda (v) (return (cons (car r) v))))))))

(define mergesort-cps
  (lambda (lis return)
    (cond
      ((null? lis) lis)
      ((null? (cdr lis)) lis)
      (else (merge-cps (mergesort-cps (car (split-cps lis (lambda (v1 v2) (list v1 v2)))) (lambda (v) v))
                       (mergesort-cps (cadr (split-cps lis (lambda (v1 v2) (list v1 v2)))) (lambda (v) v))
                       (lambda (v) v))))))

(define mergesort
  (lambda (lis)
    (mergesort-cps lis (lambda (v) v))))

;;8: replaceatoms: replaces list2.length # of atoms in list1 with those of list 2
;;params: l: list 1; r: list 2
(define replaceatoms-cps
  (lambda (l r return)
    (cond
      ((null? l) (return '()))
      ((null? r) (return l))
      ((list? (car l)) (replaceatoms-cps (car l) r (lambda (v) (replaceatoms-cps (cdr l) (cdr r) (lambda (v2) (return (cons v v2)))))))
      (else (replaceatoms-cps (cdr l) (cdr r) (lambda (v) (return (cons (car r) v))))))))

(define replaceatoms
  (lambda (l r)
    (replaceatoms-cps l r (lambda (v) v))))

;;9: suffix: returns list containing elements after last occurence of a specified atom
;;params: lis: list of atoms, x: specified atom
(define suffix-helper
  (lambda (x lis break)
    (cond
	 ((null? lis) lis)
	 ((eq? x (car lis)) (break (suffix x (cdr lis))))
	 (else (cons (car lis) (suffix-helper x (cdr lis) break))))))

(define suffix
  (lambda (x lis)
    (call/cc
     (lambda (break)
       (suffix-helper x lis break)))))

;;10: removes all elements from lists and sublists if they contain a certain atom
;;params: lis: list of atoms, including sublists; x: specified atom
(define emptysublists-helper
  (lambda (x lis break)
    (cond
	 ((null? lis) lis)
	 ((list? (car lis)) (cons (emptysublists x (car lis)) (emptysublists x (cdr lis))))
	 ((eq? (car lis) x) (break '()))
	 (else (cons (car lis) (emptysublists-helper x (cdr lis) break))))))

(define emptysublists
  (lambda (x lis)
    (call/cc
     (lambda (break)
       (emptysublists-helper x lis break)))))
