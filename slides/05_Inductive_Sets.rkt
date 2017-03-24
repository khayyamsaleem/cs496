#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Video 5: Inductive Sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User defined datatypes:
;; 1. Mathematical viewpoint (inductive sets)
;; 2. The PL viewpoint (inductive datatypes)
;;
;; Example 1: Natural Numbers (Nat)
;;
;;
;;  ------------- (Rule 1)
;;    0 \in Nat
;;
;;    n \in Nat
;; --------------- (Rule 2)
;;   s(n)\in Nat
;;
;;  s(s(0)) \in Nat? Yes, here is a derivation of that fact:
;;
;;    ------------ (Rule 1)
;;      0 \in Nat
;;   --------------- (Rule 2)
;;     s(0) \in Nat
;;   ------------------ (Rule 2)
;;    s(s(0)) \in Nat
;;
;;
;;
;; Example 2: Binary trees with natural numbers at the leaves (BTree)
;;
;;        n \in Nat
;;    ------------------- (Rule 1)
;;      leaf(n) \in BTree
;;
;;    l \in BTree,  r \in BTree
;;  ----------------------------- (Rule 2)
;;      node(l,r) \in BTree
;;
;;
;;  node(leaf(s(0)),leaf(s(s(0)))) \in BTree
;;
;;
;;     s(0) \in Nat                            s(s(0)) \in Nat
;;    --------------------- (Rule 1)        -------------------------- (Rule 1)
;;     leaf(s(0)) \in BTree                   leaf(s(s(0))) \in BTree
;;   ------------------------------------------------------------------- (Rule 2)
;;          node(leaf(s(0)), leaf(s(s(0)))) \in BTree
;;
;;
;; Examples above have been given in "rule notation".
;; Alternative notation: grammars.
;;
;; Example 1 (revisited using "grammar notation")
;;
;; <Nat> ::= 0 | s(<Nat>)
;;
;; <Nat> is called a non-terminal. Symbols 0, s, "(", ")" are called terminals.
;; <Nat> ::= 0  is called a production
;; <Nat> ::= s(<Nat>) is another production
;;
;; <Nat> ::= 0 | s(<Nat>)  is shorthand for
;;
;; <Nat> ::= 0 
;; <Nat> ::= s(<Nat>)
;;
;; s(s(0)) \in <Nat>? Yes, here is a derivation of that fact.
;;
;; <Nat> --> s(<Nat>) --> s(s(<Nat>)) --> s(s(0))
;;
;; Example 2 (revisited using grammar notation)
;;
;; <BTree> ::= leaf(<Nat>) | node(<BTree>,<BTree>)
;;
;; Notation: <BTree> and <Nat> are non-terminals. leaf, "(", ")", ",", node are terminals.
;;
;;  node(leaf(s(0)),leaf(s(s(0)))) \in <BTree>
;;
;;  <BTree> --> node(<BTree>, <BTree>) --> node(leaf(<Nat>),<BTree>)
;;;         --> node(leaf(s(0)),<BTree>)
;;          --> node(leaf(s(0)),leaf(<Nat>))
;;          --> node(leaf(s(0)),leaf(s(s(0))))
;;
;;
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

