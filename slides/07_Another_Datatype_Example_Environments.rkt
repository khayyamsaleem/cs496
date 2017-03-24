#lang racket

;; Environments
;;
;; Function that associates values to variables.
;;
;; Example of an environment e.
;; e(x) = 3
;; e(y) = -5
;; e(z) = 2
;;
;;
;; Are environments inductive sets?
;;
;;  --------------------- (Rule 1)
;;    empty-env \in Env
;;
;;   s\in Symbol, n \in Int, e \in Env
;; ----------------------------- (Rule 2)
;;   (extend-env s n e) \in Env
;;
;; Env is the inductive set determined by the above two rules
;;
;;

(require eopl/eopl)

(define-datatype env env?
  (empty-env)
  (extend-env (s symbol?)
              (n number?)
              (old-env env?)))
;; env
(define e1
  (extend-env 'x 3 (extend-env 'y (- 5) (extend-env 'z 2 (empty-env)))))

;; {sym, num, env} -> env
(define (extend-environment s n e)
  (extend-env s n e))

;; {env, sym} -> num
(define (apply e s)
  (cases env e
    (empty-env () #f)
    (extend-env (t n old-e) (if (eqv? s t)
                                n
                                (apply old-e s)))))

;; Alternative, more general, representation of environments

(define-datatype genv genv?
  (empty-genv)
  (extend-genv (s (list-of symbol?))
              (n (list-of number?))
              (old-env genv?)))

(define e2
  (extend-genv '(x y) '(2 4) (extend-genv '(u v w) '(1 2 3) (empty-genv))))

(define (applyg e s)
  (error 'applyg "undefined"))

(define (extend-genvironment e xs ns)
  (error 'extend-genvironment "undefined"))

;; Digression: comments on let

(define let1
  (let ((x 2))
       (+ x x)))

(define let2
  (let ((x 2)
        (y 3))
       (+ x y)))

(define let3
  (let ((x 2))
       (let ((y 3))
            (+ x y))))

(define let4
  (let ((x 2))
       (let ((x 3))
            (+ x x))))

(define let5
  (let* ((x 2)
        (y (+ x 3)))
     (+ x y)))
