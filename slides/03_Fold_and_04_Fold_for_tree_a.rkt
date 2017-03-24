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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Video 3 - Fold
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. Motivate fold with examples
;; 2. Define fold itself (foldr)
;; 3. Redefine the operations used to motivate fold, using foldr

;; [num] -> bool
(define (evenL? xs)
  (match xs
    ['() #t]
    [(cons h ys) (and (even? h) (evenL? ys))]))

;; [[a]] -> [a]
(define (concat xss)
  (match xss
    ['() '()]
    [(cons h ys) (append h (concat ys))]))

;; { {a,b} -> b , b , [a] } -> b
(define (foldr2 f e xs)
  (match xs
    ['() e]
    [(cons h t) (f h (foldr2 f e t))]))

;; (foldr2 f e '(x1 x2 x3))
;;
;; (f x1 (f x2 (f x3 e))
;; (+ x1 (+ x2 (+ x3 0))
;; (* x1 (* x2 (* x3 1))
;;
;;

;; { {a,b} -> b , b , [a]} -> b
(define (foldl2 f e xs)
  (match xs
    ['() e]
    [(cons h t) (foldl2 f (f e h) t)]))

;; (foldrl f e '(x1 x2 x3))
;;
;; (f (f (f e x1) x2) x3)
;;
;;


;; [num] -> num
(define (sumL xs)
  (match xs
    ['() 0]
    [(cons h ys) (+ h (sumL ys))]))

;; [num] -> num
(define (sumL2 xs)
  (foldr2 (lambda (h r) (+ h r)) 0 xs))

;; [num] -> bool
(define (evenL2? xs)
  (foldr2 (lambda (h r) (and (even? h) r)) #t xs))

;; [[a]] -> [a]
;;(define (concat2 xss)
;;  (foldr2 aFunction aBaseCase xss))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Video 4 - Fold for tree a
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tree num -> num
(define (sumT t)
  (match t
    [(list 'empty) 0]
    [(list 'node n l r) (+ n (sumT l) (sumT r))]))

;; tree string
(define t4
  '(node "a" (node "b" (empty) (empty))
             (node "c" (empty) (empty))))

;; { {a,b,b} -> b , b , tree a } -> b
(define (foldT f e t)
  (match t
    [(list 'empty) e]
    [(list 'node d l r) (f d (foldT f e l) (foldT f e r))]))

;; tree num -> num
(define (sumT2 t)
  (foldT (lambda (d rl rr) (+ d rl rr)) 0 t))
  
;; tree string -> string
(define (appendT t)
  (match t
    [(list 'empty) ""]
    [(list 'node s l r ) (string-append s (appendT l) (appendT r))]))

;; tree string -> string
(define (appendT2 t)
  (foldT (lambda (d rl rr) (string-append d rl rr)) "" t))

;; Exercise: define a function that takes a tree num and returns a new tree in which
;; each data item in the argument tree is incremented by 1. Use foldT.

;; tree num -> tree num
(define (incT2 t)
  (foldT (lambda (d rl rr) (list 'node (+ d 1) rl rr)) '(empty) t))
