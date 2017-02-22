(define expression
  "let f =
      proc (g) proc (x)
          if zero? (x)
          then 1
          else *(x,((g g) -(x, 1)))
   in f")

(run "
  let f = proc(g) proc(x)
    if zero?(x)
    then 1
    else *(x((g g) -(x,1)))
  in ((f f) 4)")

;; issue because the above expression cannot be typed


