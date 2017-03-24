#lang racket

(define (seven x) 7)

(define (sign x)
  (cond ((> x 0) 1)
        ((< x 0) -1)
        (else 0)))

(define (absolute x)
  (cond ((< x 0) (* x -1))
        (else x)))

(define (andp x y)
  (cond ((= x 0) 0)
        ((= y 0) 0)
        ((= x 1) (cond ((= y 1) 1)))))


(define (swap xs)
  (match xs
    [(cons x y) (cons y x)]))

(define (split xs)
  (match xs
    [(cons h t) (cons (list h) (list t))]))

(define (allOdd xs)
  (cond ((empty? xs) #t)
        ((= (remainder (car xs) 2) 0) #f)
        (else (allOdd (cdr xs)))))

(define (member? x xs)
  (cond ((empty? xs) #f)
        ((= x (car xs)) #t)
        (else (member? x (cdr xs)))))

(define (rem x xs)
  (cond ((empty? xs) '())
        ((= x (car xs)) (rem x (cdr xs)))
        (else (cons (car xs) (rem x (cdr xs))))))

(define (remAdjDups lst)
  (match lst
    ['() '()]
    [(cons h (cons h t)) (remAdjDups (cons h t))]
    [_ (cons (car lst) (remAdjDups (cdr lst)))]))

(split '(1 2 3))
(allOdd '(1 1 5 5 3))
(remAdjDups '(1 1 1 5 5 1 3))


