(in-package :macrodynamics)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-interface-specialized-functions
      (pure:<hash-table> :key-interface <equal>)
      pure:<map> :prefix md- :package :macrodynamics))

(defparameter *namespace* nil)

(defmacro ct-let (bindings &body body)
  (with-gensyms (new-namespace accum item name value)
    `(let ((,new-namespace
            (reduce (lambda (,accum ,item)
                      (destructuring-bind (,name ,value) ,item
                        (md-insert ,accum ,name ,value)))
                    (list
                     ,@(mapcar (lambda (binding)
                                 (destructuring-bind (name value) binding
                                   `(list (list 'var ',name) ,value)))
                               bindings))
                    :initial-value *namespace*)))
       
       `(symbol-macrolet ((namespace ,,new-namespace))
          ,(let ((*namespace* ,new-namespace))
                ,@body)))))

(defmacro ct-let* (bindings &body body)
  (with-gensyms (new-namespace)
    (cond
      ((endp bindings)
       `(progn ,@body))
      (t
       (destructuring-bind (name value) (first bindings)
         `(let ((,new-namespace
                 (md-insert *namespace* (list 'var ',name) ,value)))
            `(symbol-macrolet ((namespace ,,new-namespace))
               ,(let ((*namespace* ,new-namespace))
                     (ct-let* (,@(rest bindings))
                       ,@body)))))))))

(defmacro ct-flet (definitions &body body)
  (with-gensyms (new-namespace)
    (cond
      ((endp definitions)
       `(progn ,@body))
      (t
       (destructuring-bind (name args &body fun-body) (first definitions)
         `(let ((,new-namespace
                 (md-insert *namespace* (list 'fun ',name)
                            (lambda (,@args) ,@fun-body))))
            `(symbol-macrolet ((namespace ,,new-namespace))
               ,(let ((*namespace* ,new-namespace))
                     (ct-flet (,@(rest definitions))
                       ,@body)))))))))

;; mutual recursion? would that make sense with essentially dynamic funs?
;; how about call-next-fun capability?
(defmacro ct-labels (definitions &body body)
  (with-gensyms (new-namespace)
    (cond
      ((endp definitions)
       `(progn ,@body))
      (t
       (destructuring-bind (name args &body fun-body) (first definitions)
         `(let ((,new-namespace
                 (md-insert *namespace* (list 'fun ',name)
                            (named-lambda ,name (,@args) ,@fun-body))))
            `(symbol-macrolet ((namespace ,,new-namespace))
               ,(let ((*namespace* ,new-namespace))
                     (ct-flet (,@(rest definitions))
                       ,@body)))))))))

(defmacro def-dynenv-macro (name lambda-list &body body)
  (let* ((env-param (cadr (member '&environment lambda-list)))
         (actual-env-param (or env-param (gensym "ENV"))))
    (multiple-value-bind (remaining-forms declarations docstring)
        (parse-body body :documentation t :whole t)
      (with-gensyms (expansion expanded-p)
        `(defmacro ,name (,@lambda-list
                          ,@(unless env-param `(&environment ,actual-env-param)))
           ,@declarations
           ,docstring
           (multiple-value-bind (,expansion ,expanded-p)
               (macroexpand-1 'namespace ,actual-env-param)
             (cond
               (,expanded-p
                (let ((*namespace* ,expansion))
                  ,@remaining-forms))
               (t
                (progn ,@remaining-forms)))))))))

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
    `(multiple-value-bind (,value ,foundp)
         (md-lookup *namespace* (list 'var ',var))
       (cond
         (,foundp
          ,value)
         (t
          ,(cond
            (default-supplied
                default)
            (t
             `(error 'unbound-dynenv-macro-var :var ',var))))))))

(defmacro ct-get-fun (function)
  `(md-lookup *namespace* (list 'fun ',function)))

(defmacro ct-call (function &rest arguments)
  `(funcall (md-lookup *namespace* (list 'fun ',function)) ,@arguments))

(defmacro ct-apply (function &rest arguments)
  (with-gensyms (actual-fun)
    `(let ((,actual-fun
            (md-lookup *namespace* (list 'fun ',function))))
       (apply ,actual-fun ,@arguments))))
