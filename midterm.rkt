;; Midterm Spring 2017 Solutions
; 
; Problem 1 --> Derivation
;
;    <Number> ::= n  n ∈ N
;       <Dir> ::= d ∈ {North, East, South, West}
; <SimpleCmd> ::= move(<Dir>, <Number>)
;              |  pencilDown
;              |  pencilUp
;       <Cmd> ::= <SimpleCmd>
;              |  <SimpleCmd>;<Cmd>
;
; Problem 3 --> Result of Evaluating expression in Explicit Refs
;
; let x = newref(22) in proc (y) -(deref(x), y)
; 
; (proc-val
;    (closure 'y
;             (diff-exp (deref-exp (var-exp 'x))
;                       (var-exp 'y))
;             [x | ref-val 0]
;    )
; )
; STORE: [0 | (num-val 22)]
;
;
; (pair-exp (left expression?) (right expression?))
; (fst-exp (e expression?))
; (snd-exp (e expression?))
;
; (pair-val 
;     (left expval?) 
;     (right expval?))
;
; (pair-exp (e1 e2)
;     (pair-val (value-of e1 env) (value-of e2 env)))
;
; (fst-exp (e)
;     (let ((v (value-of e env)))
;          (car (expval->pair v))))
;
; (snd-exp (e)
;     (let ((v (value-of e env)))
;     (cadr (expval->pair v))))
;
; (define (expval->pair v)
;   (cases expval? v
;     (pair-val (v1 v2) (cons v1 v2))
;     (else (error "Not a pair!"))))
;
