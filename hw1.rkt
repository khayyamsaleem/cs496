#lang racket
;;Khayyam Saleem
;;Homework 1
;;I pledge my honor that I have abided by the Stevens Honor System.

;; a -> num
(define (seven n)
  7)

;; num -> num
(define (sign n)
  (cond ((> n 0) 1)
        ((= n 0) 0)
        ((< n 0) -1)))

;; num -> num
(define (absolute n)
  (cond ((>= n 0) n)
        ((< n 0) (- n))))

;; {bool, bool} -> bool
(define (andp a b)
  (if a (if b #t #f) #f))

;; {bool, bool} -> bool
(define (orp a b)
  (if a #t (if b #t #f)))

;; bool -> bool
(define (notp a)
  (if a #f #t))

;; {bool, bool} -> bool
(define (xorp a b)
  (notp (equal? a b)))

;; {num, num} -> bool
(define (dividesBy a b)
  (if (zero? b) (error "Can't divide by zero!! Try again.")
    (zero? (remainder a b))))

;; [a] -> bool
(define (singleton? lst)
  (match lst
         [(cons a '()) #t]
         ['() #f]
         [(cons h t) #f]))

;; [a] -> bool
(define (singleton1? lst)
  (if (empty? lst) #f
    (if (empty? (cdr lst)) #t #f)))

;; [a] -> [a]
(define (swap p)
  (match p
         [(cons h t) (cons t h)]))

;; {(a -> b), a} -> b
(define (app f a)
  (f a))

;; {(a -> a), a} -> a
(define (twice f a)
  (f (f a)))

;; {(b -> c), (a -> b), a} -> c
(define (compose f1 f2 a)
  (f1 (f2 a)))

(define (remove-duplicates lon)
  (foldr (lambda (x y) (cons x (filter (lambda (z) (not (= x z))) y))) empty lon))

(define (element? item list-of-items)
  (if (null? list-of-items)
    #f
    (if (equal? item (car list-of-items))
      #t
      (element? item (cdr list-of-items)))))


(define (unionHelp a b)
  (cond ((null? b) a)
        ((element? (car b) a)
         (union a (cdr b)))
        (#t (union (cons (car b) a) (cdr b)))))

;; {[a], [a]} -> [a]
(define (union a b)
  (remove-duplicates (unionHelp a b)))

;; {(a->bool), (a->bool)} -> (a->bool)
(define (union1 f1 f2)
  (lambda (a) (orp (f1 a) (f2 a))))

(define (contains? a lst)
  (match (member a lst)
    [#f #f]
    [(cons h t) #t]))

;; {[a], [a]} -> [a]
(define (intersection a b)
  (cond ((null? a) '())
         ((contains? (car a) b) (cons (car a) (intersection (cdr a) b)))
         (else (intersection (cdr a) b))))

;; {(a->bool), (a->bool)} -> (a -> bool)
(define (intersection1 f1 f2)
  (lambda (a) (andp (f1 a) (f2 a))))

;; {a, [a]} -> bool
(define (belongsTo? a lst)
  (contains? a lst))

;; {a, (a -> bool)} -> bool
(define (belongsTo1? a f1)
  (f1 a))

;; [a] -> [a]
(define (remDups lst)
  (match lst
    ['() '()]
    [(cons h (cons h t)) (remDups (cons h t))]
    [_ (cons (car lst) (remDups (cdr lst)))]))

;; [a] -> [[a]]
(define (sublists lst)
  (if (empty? lst) '(())
    (append 
      (map 
        (lambda (x) (cons (car lst) x))
        (sublists (cdr lst))) 
      (sublists (cdr lst)))))

(define e1
  '(const 2))

(define e2
  '(add (sub (const 2) (const 3)) (const 4)))

;; {(a -> b), calc} -> calc
(define (mapC f e)
  (match e
    [(list 'const a) (list 'const (f a))]
    [(list a b c) (list a (mapC f b) (mapC f c))]))


;; {(a->b), (a->b), (a->b), (a->b), (a->b), calc} -> num
(define (foldC f1 f2 f3 f4 f5 e)
  (match e
    [(list 'const a) (f1 a)]
    [(list 'add b c) (f2 (foldC f1 f2 f3 f4 f5 b) 
                         (foldC f1 f2 f3 f4 f5 c))]
    [(list 'sub b c) (f3 (foldC f1 f2 f3 f4 f5 b)
                         (foldC f1 f2 f3 f4 f5 c))]
    [(list 'mult b c) (f4 (foldC f1 f2 f3 f4 f5 b)
                          (foldC f1 f2 f3 f4 f5 c))]
    [(list 'div b c) (f5 (foldC f1 f2 f3 f4 f5 b)
                         (foldC f1 f2 f3 f4 f5 c))]))

;; calc -> num
(define (numAdd e)
  (foldC 
    (lambda (x) 0)
    (lambda (x y) (+ 1 x y))
    (lambda (x y) (+ x y))
    (lambda (x y) (+ x y))
    (lambda (x y) (+ x y))
    e))

(define e3 
  '(add (sub (add (const 2) (const 3)) (const 6)) (add (const 4) (const 2))))

;; calc -> calc
(define (replaceAddWithMult e)
  (foldC 
    (lambda (x) (list 'const x))
    (lambda (x y) (list 'mult x y))
    (lambda (x y) (list 'sub  x y))
    (lambda (x y) (list 'mult x y))
    (lambda (x y) (list 'div x y))
    e))

;; calc -> num
(define (evalC e)
  (match e
    [(list 'const a)  a]
    [(list 'add b c)  (+ (evalC b) (evalC c))]
    [(list 'sub b c)  (- (evalC b) (evalC c))]
    [(list 'mult b c) (* (evalC b) (evalC c))]
    [(list 'div b c)  (/ (evalC b) (evalC c))]))

;; calc -> num
(define (evalCf e)
  (foldC (lambda (x) x)
    (lambda (x y) (+ x y))
    (lambda (x y) (- x y))
    (lambda (x y) (* x y))
    (lambda (x y) (/ x y))
    e))


;; the function returns the number of even numbers in list
;; [a] -> num
(define (f xs)
  (let ((g (lambda (x r) (if (even? x) (+ r 1) r))))
    (foldr g 0 xs)))

;; [a] -> ([a] -> [a])
(define (app1 xs)
  (let ((g (lambda (x h) (lambda (y) (cons x (h y))))))
    (foldr g identity xs)))

(display "\ntesting all non-trivial functions\n")
(display "----------------------------------\n\n")
(display "testing union: (union '(1 2 3) '(2 3 4 5 6))\n")
(union '(1 2 3) '(2 3 4 5 6))
(display "\ntesting union w/ characteristic fn: ((union1 even? odd?) 3)\n")
((union1 even? odd?) 3)
(display "\ntesting contains, should return #t, then #f\n")
(contains? 5 '(3 4 5 6))
(contains? 2 '(3 4 5 6))
(display "\ntesting intersection: (intersection '(1 2 3 4) '(3 4 5 6))\n")
(intersection '(1 2 3 4) '(3 4 5 6))
(display "\ntesting intersection w/ characteristic fn: ((intersection1 even? zero?) 0)\n")
((intersection1 even? zero?) 0)
(display "\ntesting sublists: (sublists '(1 2 3))\n")
(sublists '(1 2 3))
(display "\nlearning about calculator expressions: e1, e2\n")
e1
e2
(display "\ntesting mapC\n")
(mapC (lambda (x) (+ x 1)) e2)
(display "\ntesting foldC\n")
(foldC (lambda (x) 0)
       (lambda (x y) (+ 1 x y))
       (lambda (x y) (+ 1 x y))
       (lambda (x y) (+ 1 x y))
       (lambda (x y) (+ 1 x y))
       e2)
(display "\ntesting numAdd\n")
(numAdd e2)
(numAdd e3)
(display "\ntesting replaceAddWithMult\n")
(replaceAddWithMult e2)
(display "\ntesting evalC\n")
(evalC e1)
(evalC e2)
(evalC e3)
(display "\ntesting evalCf\n")
(evalCf e1)
(evalCf e2)
(evalCf e3)
(display "\ntesting Ex. 4 #1\n")
(f '(1 2 3 4 5 6 6 6))
(display "\ntesting app1 (Ex. 4 #3\n")
((app1 '(1 2 3)) '(4 5 6))

(provide (all-defined-out))

