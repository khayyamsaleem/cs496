; Khayyam Saleem
; HW2
; I pledge my honor that I have abided by the Stevens Honor System.
;

#lang racket
(require eopl/eopl)

;; dTree -> node-t || leaf-t
(define-datatype dTree dTree?
    (leaf-t
      (datum number?))
    (node-t
      (key symbol?)
      (left dTree?)
      (right dTree?)))

;; dTree
(define tLeft
  (node-t 'w
    (node-t 'x (leaf-t 2) (leaf-t 5))
    (leaf-t 8)))

;; dTree
(define tRight
  (node-t 'w (node-t 'x (leaf-t 2) (leaf-t 5)) (node-t 'y (leaf-t 7) (leaf-t 5))))

;; dTree -> num
(define (dTree-height t)
  (cases dTree t
    (node-t (n l r) (+ 1 (max (dTree-height l) (dTree-height r))))
    (leaf-t 0)))

;; dTree -> num
(define (dTree-size t)
  (cases dTree t
    (node-t (n l r) (+ 1 (dTree-size l) (dTree-size r)))
    (leaf-t 1)))

;; dTree -> [a]
(define (dTree-paths t)
  (cases dTree t
    (node-t (n l r)
            (append (map (lambda (x) (append '(0) x)) (dTree-paths l))
                    (map (lambda (x) (append '(1) x)) (dTree-paths r)) ))
    (leaf-t (n) '(()))))

;; dTree -> bool
(define (dTree-perfect? t)
  (cases dTree t
    (node-t (n l r)
            (and
             (dTree-perfect? l)
             (dTree-perfect? r)
             (equal? (dTree-height l) (dTree-height r))))
    (leaf-t (n) #t)))


(define symbol-upcase
  (compose string->symbol (compose string-upcase symbol->string)))


(define (succ n)
  (+ 1 n))

;; (sym -> sym, num -> num) -> dTree
(define (dTree-map f g t)
  (cases dTree t
    (node-t (n l r) (node-t (f n) (dTree-map f g l) (dTree-map f g r)))
    (leaf-t (n) (leaf-t (g n)))))

;; [a] -> dTree
(define (list->tree l)
  (match l
    [(cons h t) (node-t h (list->tree t) (list->tree t))]
    ['() (leaf-t 0)]
    ))


(define graphtest '(
((0 0 0) . 0)
((0 0 1) . 1)
((0 1 0) . 1)
((0 1 1) . 0)
((1 0 0) . 1)
((1 0 1) . 0)
((1 1 0) . 0)
((1 1 1) . 1)
))


(define gt2 '((x y z) .(
((0 0 0) . 0)
((0 0 1) . 1)
((0 1 0) . 1)
((0 1 1) . 0)
((1 0 0) . 1)
((1 0 1) . 0)
((1 1 0) . 0)
((1 1 1) . 1)
)))

;; [a] -> [a]
(define (rla_help x) (cons (cdar x) (cdr x)))

;; (dTree, [([num], num))] -> dTree
(define (replaceLeafAt t f)
  (cases dTree t
    (node-t (n l r)
            (node-t
             n
             (replaceLeafAt l (map rla_help (filter (lambda (y) (equal? (caar y) 0)) f)))
             (replaceLeafAt r (map rla_help (filter (lambda (y) (equal? (caar y) 1)) f)))))
    (leaf-t (n) (leaf-t (cdar f)))))

;; [a] -> dTree
(define (bf->dTree l)
  (replaceLeafAt (list->tree (car l)) (cdr l)))

;; (replaceLeafAt (list->tree '(x y z)) graphtest)