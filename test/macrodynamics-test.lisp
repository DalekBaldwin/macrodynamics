(in-package :macrodynamics-test)

(in-root-suite)

(defsuite* test-all)

(defmacro wrap-let-map (bindings &body body)
  `(let-map ,bindings ,@body))

(deftest test-contextual-dsl ()
  (with-generators ((x (generator (integer)))
                    (y (generator (integer))))
    (let-map ((a (+ x x)))
      (let ((z 3))
        (wrap-let-map ((b (+ a a)))
          (let ((check-it:*num-trials* 10))
            (is
             (check-that (progn
                           (format t "~&~A ~A ~A ~A~%" x y a b)
                           (= a a))))))))))

(def-dynenv-var **test-var** 0)

(deftest test-dynenv-macrolet ()
  (is
   (equal
    (dynenv-macrolet ((herp (&body body)
                            (ct-let ((**test-var** (1+ **test-var**)))
                              `(progn ,@body)))
                      (derp (thing)
                            `(list* ,**test-var** ,thing)))
      (derp (herp (derp (herp (derp (herp (derp (herp (derp nil))))))))))
    (list 0 1 2 3 4))))
