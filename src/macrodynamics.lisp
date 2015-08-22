(in-package :macrodynamics)

(defparameter *var-space* nil)
(defparameter *fun-space* nil)
(defparameter *within-captured-dynenv* nil)

(defun get-dynenv-var (var)
  (let ((dynenv-var (get var 'dynenv-var)))
    (cond
      (*within-captured-dynenv*
       (multiple-value-bind (dvalue foundp)
           (get-assoc var *var-space*)
         (cond
           (foundp
            dvalue)
           ((boundp dynenv-var)
            (symbol-value dynenv-var))
           (t
            (error 'unbound-dynenv-macro-var :var var)))))
      (t
       (error
        "No macrodynamic environment has been captured to look up variable ~A"
        var)))))

(defun (setf get-dynenv-var) (val var)
  (declare (ignore val))
  (error
   "Illegal attempt to assign to macrodynamic variable ~A"
   var))

(defmacro def-dynenv-var (var &optional (val nil val-supplied))
  (let ((gvar (gensym (symbol-name var))))
    `(progn
       (defvar ,gvar ,@(when val-supplied (list val)))
       (setf (get ',var 'dynenv-var) ',gvar)
       (define-symbol-macro ,var (get-dynenv-var ',var)))))

(defun dynenv-function% (symbol)
  (let ((dynenv-fun (get symbol 'dynenv-fun)))
    (cond
      ((null dynenv-fun)
       (error "The macrodynamic function ~A is undefined." symbol))
      ;;UNDEFINED-FUNCTION
      (*within-captured-dynenv*
       (multiple-value-bind (dvalue foundp)
           (get-assoc symbol *fun-space*)
         (cond
           (foundp
            dvalue)
           ((boundp dynenv-fun)
            (symbol-value dynenv-fun))
           (t
            (error 'unbound-dynenv-macro-fun :fun symbol)))))
      (t
       (error
        "No macrodynamic environment has been captured to look up function ~A"
        symbol)))))

(defmacro dynenv-function (name)
  `(dynenv-function% ',name))

(defmacro def-unbound-dynenv-fun (name)
  (let ((call-args (make-symbol "ARGS"))
        (gname (gensym (symbol-name name))))
    `(progn
       (defvar ,gname)
       (setf (get ',name 'dynenv-fun) ',gname)
       (defmacro ,name (&rest ,call-args)
         `(funcall (dynenv-function ,',name) ,@,call-args)))))

(defmacro def-dynenv-fun (name args &body body)
  (let ((call-args (make-symbol "ARGS"))
        (gname (gensym (symbol-name name))))
    `(progn
       (defvar ,gname (lambda (,@args) ,@body))
       (setf (get ',name 'dynenv-fun) ',gname)
       (defmacro ,name (&rest ,call-args)
         `(funcall (dynenv-function ,',name) ,@,call-args)))))

(defmacro ct-let (bindings &body body)
  (with-gensyms (new-var-space accum item name value)
    (let ((gnames (loop for binding in bindings
                     collect (gensym (symbol-name (first binding))))))
      `(let (,@(loop for binding in bindings
                  for gname in gnames
                  collect `(,gname ,(second binding))))
         (let ((,new-var-space
                (reduce (lambda (,accum ,item)
                          (destructuring-bind (,name . ,value) ,item
                            (update-alist ,name ,value ,accum)))
                        (list
                         ,@(mapcar (lambda (binding gname)
                                     `(cons ',(first binding) ,gname))
                                   bindings
                                   gnames))
                        :initial-value *var-space*)))
           `(symbol-macrolet ((var-space ,,new-var-space))
              ,(let ((*var-space* ,new-var-space))
                    ,@body)))))))

(defmacro ct-let* (bindings &body body)
  (with-gensyms (new-var-space)
    (cond
      ((endp bindings)
       `(progn ,@body))
      (t
       (destructuring-bind (name value) (first bindings)
         `(let ((,new-var-space
                 (update-alist ',name ,value) *var-space*))
            `(symbol-macrolet ((var-space ,,new-var-space))
               ,(let ((*var-space* ,new-var-space))
                     (ct-let* (,@(rest bindings))
                       ,@body)))))))))

(defmacro ct-flet (definitions &body body)
  (with-gensyms (new-fun-space)
    (cond
      ((endp definitions)
       `(progn ,@body))
      (t
       (destructuring-bind (name args &body fun-body) (first definitions)
         `(let ((,new-fun-space
                 (update-alist ',name (lambda (,@args) ,@fun-body) *fun-space*)))
            `(symbol-macrolet ((fun-space ,,new-fun-space))
               ,(let ((*fun-space* ,new-fun-space))
                     (ct-flet (,@(rest definitions))
                       ,@body)))))))))

;; mutual recursion? would that make sense with essentially dynamic funs?
;; how about call-next-fun capability?
(defmacro ct-labels (definitions &body body)
  (with-gensyms (new-fun-space)
    (cond
      ((endp definitions)
       `(progn ,@body))
      (t
       (destructuring-bind (name args &body fun-body) (first definitions)
         `(let ((,new-fun-space
                 (update-alist ',name (named-lambda ,name (,@args) ,@fun-body)
                               *fun-space*)))
            `(symbol-macrolet ((fun-space ,,new-fun-space))
               ,(let ((*fun-space* ,new-fun-space))
                     (ct-flet (,@(rest definitions))
                       ,@body)))))))))

(defmacro with-dynenv (environment &body body)
  "Macro for capturing a dynenv within another macro's body."
  (with-gensyms (var-expansion var-expanded-p fun-expansion fun-expanded-p)
    `(multiple-value-bind (,var-expansion ,var-expanded-p)
         (macroexpand-1 'var-space ,environment)
       (multiple-value-bind (,fun-expansion ,fun-expanded-p)
           (macroexpand-1 'fun-space ,environment)
         (let ((*var-space* (when ,var-expanded-p ,var-expansion))
               (*fun-space* (when ,fun-expanded-p ,fun-expansion))
               (*within-captured-dynenv* t))
           ,@body)))))

(defmacro def-dynenv-macro (name lambda-list &body body)
  (let* ((env-param (cadr (member '&environment lambda-list)))
         (actual-env-param (or env-param (gensym "ENV"))))
    (multiple-value-bind (remaining-forms declarations docstring)
        (parse-body body :documentation t :whole t)
      `(defmacro ,name (,@lambda-list
                        ,@(unless env-param `(&environment ,actual-env-param)))
         ,@declarations
         ,@(ensure-list docstring)
         (with-dynenv ,actual-env-param
           ,@remaining-forms)))))

(define-condition unbound-dynenv-macro-var ()
  ((var
    :initarg :var
    :accessor var)))

(define-condition unbound-dynenv-macro-fun ()
  ((fun
    :initarg :fun
    :accessor fun)))
