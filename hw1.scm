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
      (null? r) '()
      r)
     ((< (car l) (car r))
      (cons (car l) (merge (cdr l) r)))
     ((>= (car l) (car r))
      (cons (car r) (merge (cdr r) l)))
     )))

;;#3: removedups: removes repeating atoms that appear next to each other
;; params: lis: list of atoms
(define removedups
  (lambda (lis)
    (cond
     ((null? lis)'())
     ((null? (cdr lis)) lis)
     ((eq? (car lis) (car (cdr lis)))
      (removedups(cdr lis)))
     (else (cons (car lis) (removedups (cdr lis))))
     )))

;;#5: deepcons: places letter in front of leftmost non-list atom
;; params: x: letter to be placed; lis: list containing sublists and atoms
(define deepcons
  (lambda (x lis)
    (cond
     ((null? lis) '())
     ((list? (car lis))
      ( cons (deepcons x (car lis)) (cdr lis)))
     (else (cons x lis))
     )))

;;#6: numparens: counts number of balanced parantheses in list
;;params: lis: list containing atoms
(define numparens
  (lambda (lis)
    (cond
     ((null? lis) 1)
     ((list? (car lis))
      (+ (+ 0 (numparens(car lis))) (+ 0 (numparens(cdr lis)))))
     (else (numparens (cdr lis)))
     )))

;;#7: dup*: returns a list with all atoms in the list duplicated
;; params: lis: list of atoms, may include sublists
(define dup*
  (lambda (lis)
    (cond
     ((null? lis) '())
     ((list? (car lis))
      (cons (cons (dup* (car lis)) (dup* (car lis))) (dup* (cdr lis))))
     (else (cons (car lis) (cons (car lis) (dup* (cdr lis)))))
     )))

;;#8: removedups* INCOMPLETE: removes repeating atoms from sublists
;;params: lis: list of atoms, may include sublists
(define removedups*
  (lambda (lis)
    (cond
     ((null? lis)'())
     ((null? (cdr lis)) lis)
     ((list?(car lis))
      (cons(removedups*(car lis))(removedups*(cdr lis))))
     ((eq? (car lis)(car (cdr lis)))
      (removedups*(cdr lis)))
     (else (cons (car lis)(removedups*(cdr lis))))
     )))

;;#10: removedups**
