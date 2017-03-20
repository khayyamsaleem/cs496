#lang racket

(define queue
  (let ((data '()))
    (lambda (message)
      (case message
        ((empty?) (lambda ()
                          (null? data)))
        ((add!) (lambda (x)
                  (set! data (append data (list x)))))
        ((remove!) (lambda ()
                     (if (null? data)
                         (error "remove!: queue is empty")
                         (set! data (cdr data)))))
        ((peek) (lambda ()
                   (if (null? data)
                        (error "peek: queue is empty")
                       (car data))))
        (else (error "queue: Invalid" message))))))
                  
; factorial
(define (f x)
  (if (zero? x)
      1
      (* x (f (- x 1)))))

