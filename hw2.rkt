; Khayyam Saleem
; HW2
; I pledge my honor that I have abided by the Stevens Honor System.
;

#lang racket
(require eopl/eopl)

(define-datatype dTree dTree?
    (leaf-t
      (datum number?))
    (node-t
      (key symbol?)
      (left dTree?)
      (right dTree?)))


(define tLeft
  (node-t 'w
    (node-t 'x (leaf-t 2) (leaf-t 5))
    (leaf-t 8)))

(define tRight
  (node-t 'w (node-t 'x (leaf-t 2) (leaf-t 5)) (node-t 'y (leaf-t 7) (leaf-t 5))))

(define (dTree-height t)
  (cases dTree t
    (node-t (n l r) (+ 1 (max (dTree-height l) (dTree-height r))))
    (leaf-t 0)))

(define (dTree-size t)
  (cases dTree t
    (node-t (n l r) (+ 1 (dTree-size l) (dTree-size r)))
    (leaf-t 1)))

(define (dTree-paths t)
  (cases dTree t
    (node-t (n l r)
            (append (map (lambda (x) (append '(0) x)) (dTree-paths l))
                    (map (lambda (x) (append '(1) x)) (dTree-paths r)) ))
    (leaf-t (n) '(()))))

(define (dTree-perfect? t)
  (cases dTree t
    (node-t (n l r) (equal? (dTree-size l) (dTree-size r)))
    (leaf-t (n) #t)))

(define symbol-upcase
  (compose string->symbol (compose string-upcase symbol->string)))

(define (succ n)
  (+ 1 n))

(define (dTree-map f g t)
  (cases dTree t
    (node-t (n l r) (node-t (f n) (dTree-map f g l) (dTree-map f g r)))
    (leaf-t (n) (leaf-t (g n)))))

(define (list->tree l)
  (car ))
