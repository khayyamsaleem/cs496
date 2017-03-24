#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Video 6: Inductive Sets as Datatypes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Recall from above:
;;
;; <BTree> ::= leaf(<Nat>) | node(<BTree>,<BTree>)
;;
;;

(require eopl/eopl)

(define-datatype btree btree?
  (leaf (n number?))
  (node (l btree?)
        (r btree?)))

;; btree
(define t1
  (node
   (node (leaf 1) (leaf 2))
   (node (leaf 3) (leaf 4))))

;; btree -> num
(define (sumBT t)
  (cases btree t
    (leaf (n) n)
    (node (l r) (+ (sumBT l) (sumBT r)))))

;; btree -> btree
(define (incBT t)
  (cases btree t
    (leaf (n) (leaf (+ n 1)))
    (node (l r) (node (incBT l) (incBT r)))))

;; btree -> [num]
(define (poBT t)
  (cases btree t
    (leaf (n) (list n))
    (node (l r) (append (poBT l) (poBT r)))))

;; { num->num, btree} -> btree
(define (mapBT f t)
  (cases btree t
    (leaf (n) (leaf (f n)))
    (node (l r) (node (mapBT f l) (mapBT f r)))))

;; btree -> btree
(define (mirrorBT t)
  (cases btree t
    (leaf (n) (leaf n))
    (node (l r) (node (mirrorBT r) (mirrorBT l)))))

;; { num -> b, {b,b} -> b, btree } -> b
(define (foldBT f g t)
  (cases btree t
    (leaf (n) (error "not complete"))
    (node (l r) (error "not complete"))))

