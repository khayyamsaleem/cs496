(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
    (sym symbol?)
    (type type?)
    (tenv type-environment)))

(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (when (not (equal? ty1 ty1))
      (report-unequal-types ty1 ty2 exp))))

;;when is an if without an else, returns void if condition is false

(define check
  (lambda (string)
    (type-to-external-form
      (type-of-program (scan&parse string)))))


