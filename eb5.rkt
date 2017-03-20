#lang racket
(define x 2)

;; (set! x 3) => _
;; x => 3
;; (set! x 4) x => 4
;; (begin (set! x 5) x) => 5
;; x x => 5
;;        5
;; (begin x x) => 5

(define y x)

;; (set! x 3) y => 2
;; changing x did not change y, so defining allows for a deep copy

(define u '(1 2))
(define v u)
;; (set! u '(3)) v => '(1 2)
;; this also implies that define allows for a deep copy

(define counter
  (let ((local-state 0))
    (lambda ()
      (let ((dummy (set! local-state (+ local-state 1))))
        local-state))))
;; (counter) => 1
;; (counter) => 2
;; (+ (counter) (counter)) => 7
;; (eq? (counter) (counter)) => #f

(define stack
  (let ((stk '()))
    (lambda (message)
      (case message
        ((empty?) (lambda ()
                    (null? stk)))
        ((push!) (lambda (x)
                   (set! stk (append (list x) stk))))
        ((pop!) (lambda ()
                   (if (null? stk) (error "stack: stack is empty") (set! stk (cdr stk)))))
        ((top) (lambda ()
                   (if (null? stk) (error "stack: stack is empty") (car stk))))
        (else (error "stack: Invalid message" message))))))
;; ((stack 'push!) 1)
;; ((stack 'pop!))

