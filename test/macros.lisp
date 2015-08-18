(in-package :macrodynamics-test)

(def-dynenv-macro with-generators (bindings &body body)
  (let ((symbols (mapcar #'first bindings)))
    `(let (,@bindings)
       ,(ct-let ((generator-symbols
                  (union symbols (ct-get generator-symbols nil))))
                `(progn ,@body)))))

(def-dynenv-macro let-map (bindings &body body)
  (let ((binding-symbols
         (loop for (sym form) in bindings collect sym))
        (binding-gensyms
         (loop for (sym form) in bindings collect (gensym (symbol-name sym)))))
    (let* ((generator-symbols (ct-get generator-symbols nil))
           (mappings (ct-get mappings nil))
           (mapping-symbols (mapcar #'first mappings))
           (new-mappings
            (loop for symbol in binding-symbols
               for gensym in binding-gensyms
               collect (list symbol gensym
                             ;; a mapping can depend on any generator or mapping bound above
                             (append generator-symbols mapping-symbols)))))
      `(flet (,@(loop for (name . map-body) in bindings
                   for gensym in binding-gensyms
                   collect `(,gensym (,@generator-symbols
                                      ,@mapping-symbols)
                                     (declare (ignorable ,@generator-symbols
                                                         ,@mapping-symbols))
                                     ,@map-body)))
         ,(ct-let ((mappings
                    (union new-mappings mappings :key #'first)))
                  `(progn ,@body))))))

(def-dynenv-macro check-that
    (expr
     &rest keys
     &key
     examples
     (shrink-failures t)
     (random-state t random-state-supplied)
     (regression-id nil regression-id-supplied)
     (regression-file nil regression-file-supplied))
  (declare (ignore examples shrink-failures random-state
                   random-state-supplied regression-id regression-id-supplied
                   regression-file regression-file-supplied))
  (with-gensyms (agg)
    (let ((mappings (ct-get mappings nil))
          (generator-symbols (ct-get generator-symbols nil)))
      `(check-it::check-it%
        ',expr
        (generator (tuple ,@generator-symbols))
        (lambda (,agg)
          ;; here the generator vars are rebound from generators to generated values
          (destructuring-bind (,@generator-symbols) ,agg
            (declare (ignorable ,@generator-symbols))
            ;; bind mappings from innermost to outermost
            (let* (,@(loop for (sym fun args) in (reverse mappings)
                        collect
                        ;; generated values and previously-bound mappings
                        ;; are passed to mapping funs
                          `(,sym (,fun ,@args))))
              (declare (ignorable ,@(mapcar #'first mappings)))
              ,expr)))
        ,@keys))))
