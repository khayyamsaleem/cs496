(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?
  (require "store.scm")                 ; for reference?
  
  (require racket/contract/base)        ; for list-of
  
  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean, a procval, or a
;;; reference. 

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))
    (ref-val
      (ref reference?))
    (list-val
      (list (listof expval?)))
    (node-val
      (data expval?)
      (left expval?)
      (right expval?))
    (emptytree-val
      (emptytree type?))
    (unit-val)
    (pair-val
      (fst expval?)
      (snd expval?))
    )

;;; extractors:

  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval->ref
    (lambda (v)
      (cases expval v
	(ref-val (ref) ref)
	(else (expval-extractor-error 'reference v)))))

  (define expval->fst
    (lambda (v)
      (cases expval v
             (pair-val (e1 e2) e1)
             (else (expval-extractor-error 'pairFst v)))))

  (define expval->elem
    (lambda (v)
      (cases expval v
        (node-val (e l r) e)
        (else (expval-extractor-error 'node v)))))

  (define expval->left
    (lambda (v)
      (cases expval v
        (node-val (e l r) l)
        (else (expval-extractor-error 'node v)))))

  (define expval->right
    (lambda (v)
      (cases expval v
        (node-val (e l r) r)
        (else (expval-extractor-error 'node v)))))

  (define cons-expval
    (lambda (v xs)
      (let ((ls (expval->list xs)))
        (list-val (cons v ls)))))

  (define expval->snd
    (lambda (v)
      (cases expval v
             (pair-val (e1 e2) e2)
             (else (expval-extractor-error 'pairSnd v)))))

  (define expval->list
    (lambda (v)
      (cases expval v
        (list-val (list) list)
        (else (expval-extractor-error 'list v)))))

  ;;; trees

  (define-datatype bTree bTree?
    (node-t
     (key expval?)
     (left bTree?)
     (right bTree?)))
  

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))
  
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env-rec*
      (proc-names (list-of symbol?))
      (b-vars (list-of symbol?))
      (proc-bodies (list-of expression?))
      (saved-env environment?)))

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
	(empty-env () '())
	(extend-env (sym val saved-env)
	  (cons
	    (list sym (expval->printable val))
	    (env->list saved-env)))
	(extend-env-rec* (p-names b-vars p-bodies saved-env)
	  (cons
	    (list 'letrec p-names '...)
	    (env->list saved-env))))))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned
  ;; up with env->list 
  (define expval->printable
    (lambda (val)
      (cases expval val
	(proc-val (p)
	  (cases proc p
	    (procedure (var body saved-env)
	      (list 'procedure var '... (env->list saved-env)))))
	(else val))))


)
