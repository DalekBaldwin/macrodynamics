(in-package :macrodynamics)

(defparameter *var-space* nil)
(defparameter *fun-space* nil)
(defparameter *dynenv* nil)

(defun get-assoc (item alist &rest keys &key key test test-not)
  (declare (ignore key test test-not))
  (let ((result (apply #'assoc item alist keys)))
    (cond
      ((null result)
       (values nil nil))
      (t
       (values (cdr result) t)))))

(defmacro ct-let (bindings &body body)
  (with-gensyms (new-var-space accum item name value)
    `(let ((,new-var-space
            (reduce (lambda (,accum ,item) (list* ,item ,accum))
                    (list
                     ,@(mapcar (lambda (binding)
                                 (destructuring-bind (name value) binding
                                   `(cons ',name ,value)))
                               bindings))
                    :initial-value *var-space*)))
       `(symbol-macrolet ((var-space ,,new-var-space))
          ,(let ((*var-space* ,new-var-space))
                ,@body)))))

(defmacro ct-let* (bindings &body body)
  (with-gensyms (new-var-space)
    (cond
      ((endp bindings)
       `(progn ,@body))
      (t
       (destructuring-bind (name value) (first bindings)
         `(let ((,new-var-space
                 (list* (cons ',name ,value) *var-space*)))
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
                 (list* (cons ',name (lambda (,@args) ,@fun-body)) *fun-space*)))
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
                 (list* (cons ',name (named-lambda ,name (,@args) ,@fun-body))
                        *fun-space*)))
            `(symbol-macrolet ((fun-space ,,new-fun-space))
               ,(let ((*fun-space* ,new-fun-space))
                     (ct-flet (,@(rest definitions))
                       ,@body)))))))))

(defmacro def-dynenv-macro (name lambda-list &body body)
  (let* ((env-param (cadr (member '&environment lambda-list)))
         (actual-env-param (or env-param (gensym "ENV"))))
    (multiple-value-bind (remaining-forms declarations docstring)
        (parse-body body :documentation t :whole t)
      (with-gensyms (var-expansion var-expanded-p fun-expansion fun-expanded-p)
        `(defmacro ,name (,@lambda-list
                          ,@(unless env-param `(&environment ,actual-env-param)))
           ,@(ensure-list docstring)
           ,@declarations
           (multiple-value-bind (,var-expansion ,var-expanded-p)
               (macroexpand-1 'var-space ,actual-env-param)
             (multiple-value-bind (,fun-expansion ,fun-expanded-p)
                 (macroexpand-1 'fun-space ,actual-env-param)
               (let ((*var-space* (when ,var-expanded-p ,var-expansion))
                     (*fun-space* (when ,fun-expanded-p ,fun-expansion)))
                 ,@remaining-forms))))))))

(define-condition unbound-dynenv-macro-var ()
  ((var
    :initarg :var
    :accessor var)))

(define-condition unbound-dynenv-macro-fun ()
  ((fun
    :initarg :fun
    :accessor fun)))

(defmacro ct-get (var &optional (default nil default-supplied))
  (with-gensyms (value foundp)
    `(progn
       (multiple-value-bind (,value ,foundp)
           (get-assoc ',var *var-space*)
         (cond
           (,foundp
            ,value)
           (t
            ,(cond
              (default-supplied
                  default)
              (t
               `(error 'unbound-dynenv-macro-var :var ',var)))))))))

(defmacro ct-get-fun (function)
  (with-gensyms (value foundp)
    `(multiple-value-bind (,value ,foundp)
         (get-assoc ',function *fun-space*)
       (cond
         (,foundp
          ,value)
         (t
          (error 'unbound-dynenv-macro-fun :fun ',function))))))

(defmacro ct-call (function &rest arguments)
  (with-gensyms (fun)
    `(let ((,fun (ct-get-fun ,function)))
       (funcall ,fun ,@arguments))))

(defmacro ct-apply (function &rest arguments)
  (with-gensyms (fun)
    `(let ((,fun (ct-get-fun ,function)))
       (apply ,fun ,@arguments))))
