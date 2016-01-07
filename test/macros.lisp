(in-package :macrodynamics-test)

(def-dynenv-var **generator-symbols** nil)
(def-dynenv-var **mappings** nil)

(def-dynenv-fun **blurf** (&rest x) (apply #'union x))

(def-dynenv-var **test-var** 0)

#+nil
(def-dynenv-macro with-generators (bindings &body body)
  (let ((symbols (mapcar #'first bindings)))
    `(let (,@bindings)
       ,(ct-let ((**generator-symbols**
                  (**blurf** symbols **generator-symbols**)))
                (ct-flet ((**blurf** (&rest x) (apply #'gensym x)))
                    `(progn ,@body))))))

(defmacro with-generators (bindings &body body &environment env)
  (with-dynenv env
    (let ((symbols (mapcar #'first bindings)))
      `(let (,@bindings)
         ,(ct-let ((**generator-symbols**
                    (**blurf** symbols **generator-symbols**)))
                  (ct-flet ((**blurf** (&rest x) (apply #'gensym x)))
                    `(progn ,@body)))))))

#+nil
(def-dynenv-macro let-map (bindings &body body)
  (let ((binding-symbols
         (loop for (sym form) in bindings collect sym))
        (binding-gensyms
         (loop for (sym form) in bindings collect (**blurf** (symbol-name sym)))))
    (let* ((mapping-symbols (mapcar #'first **mappings**))
           (new-mappings
            (loop for symbol in binding-symbols
               for gensym in binding-gensyms
               collect
                 (list symbol gensym
                       ;; a mapping can depend on any generator or mapping bound above
                       (append **generator-symbols** mapping-symbols)))))
      `(flet (,@(loop for (name . map-body) in bindings
                   for gensym in binding-gensyms
                   collect `(,gensym (,@**generator-symbols**
                                      ,@mapping-symbols)
                                     (declare (ignorable ,@**generator-symbols**
                                                         ,@mapping-symbols))
                                     ,@map-body)))
         ,(ct-let ((**mappings**
                    (union new-mappings **mappings** :key #'first)))
                  `(progn ,@body))))))

(defmacro let-map (bindings &body body &environment env)
  (with-dynenv env
    (let ((binding-symbols
           (loop for (sym form) in bindings collect sym))
          (binding-gensyms
           (loop for (sym form) in bindings collect (**blurf** (symbol-name sym)))))
      (let* ((mapping-symbols (mapcar #'first **mappings**))
             (new-mappings
              (loop for symbol in binding-symbols
                 for gensym in binding-gensyms
                 collect
                   (list symbol gensym
                         ;; a mapping can depend on any generator or mapping bound above
                         (append **generator-symbols** mapping-symbols)))))
        `(flet (,@(loop for (name . map-body) in bindings
                     for gensym in binding-gensyms
                     collect `(,gensym (,@**generator-symbols**
                                        ,@mapping-symbols)
                                       (declare (ignorable ,@**generator-symbols**
                                                           ,@mapping-symbols))
                                       ,@map-body)))
           ,(ct-let ((**mappings**
                      (reduce (lambda (accum item)
                                (destructuring-bind (symbol . data) item
                                  (macrodynamics::update-alist symbol data accum)))
                              new-mappings
                              :initial-value **mappings**)))
                    `(progn ,@body)))))))

#+nil
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
    `(check-it::check-it%
      ',expr
      (generator (tuple ,@**generator-symbols**))
      (lambda (,agg)
        ;; here the generator vars are rebound from generators to generated values
        (destructuring-bind (,@**generator-symbols**) ,agg
          (declare (ignorable ,@**generator-symbols**))
          ;; bind mappings from innermost to outermost
          (let* (,@(loop for (sym fun args) in (reverse **mappings**)
                      collect
                      ;; generated values and previously-bound mappings
                      ;; are passed to mapping funs
                        `(,sym (,fun ,@args))))
            (declare (ignorable ,@(mapcar #'first **mappings**)))
            ,expr)))
      ,@keys)))

(defmacro check-that
    (expr
     &rest keys
     &key
       examples
       (shrink-failures t)
       (random-state t random-state-supplied)
       (regression-id nil regression-id-supplied)
       (regression-file nil regression-file-supplied)
     &environment env)
  (declare (ignore examples shrink-failures random-state
                   random-state-supplied regression-id regression-id-supplied
                   regression-file regression-file-supplied))
  (with-dynenv env
    (with-gensyms (agg)
      `(check-it::check-it%
        ',expr
        (generator (tuple ,@**generator-symbols**))
        (lambda (,agg)
          ;; here the generator vars are rebound from generators to generated values
          (destructuring-bind (,@**generator-symbols**) ,agg
            (declare (ignorable ,@**generator-symbols**))
            ;; bind mappings from innermost to outermost
            (let* (,@(loop for (sym fun args) in (reverse **mappings**)
                        collect
                        ;; generated values and previously-bound mappings
                        ;; are passed to mapping funs
                          `(,sym (,fun ,@args))))
              (declare (ignorable ,@(mapcar #'first **mappings**)))
              ,expr)))
        ,@keys))))
