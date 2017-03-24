#lang racket

;; Binary trees as lists

;; tree num
(define t1
  '(empty))

;; tree num
(define t2
  '(node 1 (empty) (empty)))

;; tree num
(define t3
  '(node 1 (node 2 (empty) (empty))
           (node 3 (empty) (empty))))

;; {} -> tree num
(define mkEmpty
  (lambda ()
    '(empty)))

;; {num, tree num, tree num} -> tree num
(define (mkNode n l r)
  (list 'node n l r))

;; tree num -> tree num
(define (succT t)
  (match t
    [(list 'empty) '(empty)]
    [(list 'node n l r) (list 'node (+ 1 n) (succT l) (succT r))]))

;; tree num -> tree bool
(define (evenT? t)
  (match t
    [(list 'empty) '(empty)]
    [(list 'node n l r) (list 'node (even? n) (evenT? l) (evenT? r))]))

;; { a->b,tree a } -> tree b
(define (mapT f t)
  (match t
    [(list 'empty) '(empty)]
    [(list 'node n l r) (list 'node (f n) (mapT f l) (mapT f r))]))

;; tree num -> tree num
(define (succT2 t)
  (mapT (lambda (x) (+ x 1)) t))

;; tree num -> tree bool
(define (evenT2? t)
  (mapT even? t))

;; tree a -> tree num
(define (resetT t)
  (mapT (lambda (x) 0) t))

;; tree a -> tree num
(define resetT2
  ((curry mapT) (lambda (x) 0)))



