(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

  ;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

  ;;;;;;;;;;;;;;;; Declared ADTs ;;;;;;;;;;;;;;;;
  
  (define global-adts '())

  ;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
                   (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        (showstore-exp () (write "Store is") (newline)
                       (write (get-store-as-list)) (newline)
                       (write "Env is") (newline)
                       (write (env->list env)) (newline))

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (num-val
                       (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
                   (let ((val1 (value-of exp1 env)))
                     (let ((num1 (expval->num val1)))
                       (if (zero? num1)
                           (bool-val #t)
                           (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
                (let ((val1 (value-of exp1 env)))
                  (if (expval->bool val1)
                      (value-of exp2 env)
                      (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
                 (let ((val1 (value-of exp1 env)))
                   (value-of body
                             (extend-env var val1 env))))
        
        (proc-exp (var type body)
                  (proc-val (procedure var body env)))

        (call-exp (rator rand)
                  (let ((proc (expval->proc (value-of rator env)))
                        (arg (value-of rand env)))
                    (apply-procedure proc arg)))

        (letrec-exp (reType name bVar bVarType body letrec-body)
                    (value-of letrec-body
                              (extend-env-rec* (list name) (list bVar) (list body) env)))

        (begin-exp (exp1 exps)
                   (letrec 
                       ((value-of-begins
                         (lambda (e1 es)
                           (let ((v1 (value-of e1 env)))
                             (if (null? es)
                                 v1
                                 (value-of-begins (car es) (cdr es)))))))
                     (value-of-begins exp1 exps)))

        (newref-exp (exp1)
                    (let ((v1 (value-of exp1 env)))
                      (ref-val (newref v1))))

        (deref-exp (exp1)
                   (let ((v1 (value-of exp1 env)))
                     (let ((ref1 (expval->ref v1)))
                       (deref ref1))))

        (setref-exp (exp1 exp2)
                    (let ((ref (expval->ref (value-of exp1 env))))
                      (let ((v2 (value-of exp2 env)))
                        (begin
                          (setref! ref v2)
                          (num-val 23)))))

        (for-exp (var lbe ube exp1)
                 (let* ((v1 (value-of lbe env))
                        (varRef (newref v1))
                        (env2 (extend-env var (ref-val varRef) env))
                        (v2 (value-of ube env2)))
                   ( if (<= (expval->num v1) (expval->num v2)) 
                        (do ((i (expval->num v1) (+ i 1)))
                          ((= i (+ 1 (expval->num v2))) )   
                          (begin (setref! varRef (num-val i))
                                 ; (write "value of var")
                                 ; (write (deref varRef))
                                 ; (write (value-of exp1 env2))
                                 (value-of exp1 env2)
                                 )
                          )
                        (num-val 23))))
        
        (constr-exp (id args) 
                (let ((vals (map (lambda (e) (value-of e env)) args)))
                  (tu-val id vals)))
        
        (type-exp (id conss paramss) (eopl:error "not implemented!"))
        
        (case-exp (e bs) (eopl:error "not implemented!"))
        
        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
                   (let ((r arg))
                     (let ((new-env (extend-env var r saved-env)))
                       (when (instrument-let)
                         (begin
                           (eopl:printf
                            "entering body of proc ~s with env =~%"
                            var)
                           (pretty-print (env->list new-env))
                           (eopl:printf "store =~%")
                           (pretty-print (store->readable (get-store-as-list)))
                           (eopl:printf "~%")))
                       (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
       (lambda (p)
         (cons
          (car p)
          (expval->printable (cadr p))))
       l)))
 
  )
  


  
