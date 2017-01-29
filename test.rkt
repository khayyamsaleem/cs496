#lang racket

;; num -> num
(define succ2
  (lambda (n)
    (+ 1 n)))

;; (num -> a) -> a
(define f
  (lambda (g)
    (g 2)))

;; (num -> num) -> num
(define f2
  (lambda (g)
    (+ 1 (g 2))))

(define (add x y)
  (+ x y))

(add 5 7)

(define (succL xs)
  (match xs
         ['() '()]
         [(cons h t) (cons (+ h 1) (succL t))]))

(succL '(1 2 3))

(define (fact n)
  (match n
      [0 1]
      [n (* n (fact (- n 1)))]))

(define (addL xs)
  (match xs
         ['() 0]
         [(cons h t) (+ h (addL t))]))

(define (myCar xs)
  (match xs
         ['() (error "myCar: cannot compute the head of an empty list")]
         [(cons h t) h]))

(provide (all-defined-out))



