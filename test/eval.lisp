(defun my-eval (Sexpr Env)       
  (cond
    ((null Sexpr)            nil)                
    ((numberp Sexpr)         Sexpr)              
    ((stringp Sexpr)         Sexpr)              
    ((eq t Sexpr)            Sexpr)              
    ((atom Sexpr)            (lookup Sexpr Env)) 
    ((eq (car Sexpr) 'quote) (car (cdr Sexpr)))  
    ((eq (car Sexpr) 'cond)  (eval-cond (cdr Sexpr) Env))     
    ((eq (car Sexpr) 'function) 
      (cond                     
        ((eq (caadr Sexpr) 'lambda) (list 'closed (cadr Sexpr) Env))
        (t (error "Cannot build closure with non-atom or non-lambda!"))
      ) 
    ) 
    (t
      (my-apply (car Sexpr) (eval-list (cdr Sexpr) Env) Env)  
    ) 
  ) 
) 

(defun my-apply (Func Params Env) 
  (cond
    ((atom Func)
      (cond
        ((eq Func 'car) (car (car Params)))
        ((eq Func 'cdr) (cdr (car Params)))
        ((eq Func 'cons) (cons (car Params) (car (cdr Params))))
        ((eq Func 'get) (get (car Params) (car (cdr Params))))
        ((eq Func 'atom) (atom (car Params)))
        ((eq Func 'error) (error (string Params)))
        ((eq Func 'eq) (eq (car Params) (car (cdr Params))))
        ((eq Func 'null) (eq (car Params) nil)) 
        ((eq Func '+) (apply '+ Params)) 
        ((eq Func '*) (apply '* Params)) 
        ((eq Func 'funcall)              
          (apply-closure                 
            (car Params)
            (eval-list (cdr Params) Env)
          ) 
        ) 
        (t (cond                         
            ((get Func 'EXPR)            
             (my-apply (get Func 'EXPR) Params Env) Params Env)
            (t (my-apply (lookup Func Env) Params Env)))
        ) 
      ) 
    ) 
    ((eq (car Func) 'lambda)
      (my-eval (car (cdr (cdr Func)))
               (update-env (car (cdr Func)) Params Env)
      )
    ) 
    (t (my-apply (my-eval Func Env) Params Env))
  ) 
) 

(defun apply-closure (Closure Params)
  (cond
    ((eq (sel-closure-header Closure) 'closed) 
      (my-apply
        (sel-closure-lambda Closure)
        Params
        (sel-closure-environment Closure)
      ) 
    ) 
    (t (error "Can't call apply-closure on a non-closure."))
  ) 
) 
  
(defun sel-closure-header (Sexpr) (car Sexpr))               
(defun sel-closure-lambda (Sexpr) (cadr Sexpr))              
(defun sel-closure-environment (Sexpr) (caddr Sexpr))        

(defun eval-cond (Conds Env)
  (cond
    ((null Conds) nil)                      
    ((my-eval (car (car Conds)) Env)        
      (my-eval (car (cdr (car Conds))) Env) 
    ) 
    (t (eval-cond (cdr Conds) Env))         
  ) 
) 

(defun eval-list (Sexpr Env)
  (cond
    ((null Sexpr) nil)
    (t
      (cons (my-eval (car Sexpr) Env)
            (eval-list (cdr Sexpr) Env)
      )
    ) 
  ) 
) 

(defun lookup (Ident Env)
  (cond
    ((null Env) (error "Unbound identifier"))
    ((eq Ident (car (car Env))) (car (cdr (car Env))))
    (t (lookup Ident (cdr Env)))
  ) 
) 

(defun update-env (Formals Actuals Env)
  (cond
    ((null Formals)
      (cond
        ((null Actuals) Env)
        (t (error "Bad argument count"))
      ) 
    ) 
    ((null Actuals) (error "Bad argument count")) 
    (t
      (cons (list (car Formals) (car Actuals))
            (update-Env (cdr Formals) (cdr Actuals) Env)
      ) 
    ) 
  ) 
) 
