#lang racket
(require eopl/eopl)
(require eopl/top)


; <Program>
; <Expression>
; <Expression> = proc (<Identifier>) <Expression>
; <Expression> = proc (x) ...
;
;
(scan&parse "let f = proc (x) -(x, 11) in (f 77)")

