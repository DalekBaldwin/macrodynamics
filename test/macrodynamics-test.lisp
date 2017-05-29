(in-package :macrodynamics-test)

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

(def-dynenv-var **test-var-same-file** 0)

(deftest test-dynenv-macrolet-same-file ()
  (is
   (equal
    (dynenv-macrolet ((herp (&body body)
                            (ct-let ((**test-var-same-file**
                                      (1+ **test-var-same-file**)))
                              `(progn ,@body)))
                      (derp (thing)
                            `(list* ,**test-var-same-file** ,thing)))
      (derp (herp (derp (herp (derp (herp (derp (herp (derp nil))))))))))
    (list 0 1 2 3 4))))

(deftest test-dynenv-macrolet-different-file ()
  (is
   (equal
    (dynenv-macrolet ((herp (&body body)
                            (ct-let ((**test-var-different-file**
                                      (1+ **test-var-different-file**)))
                              `(progn ,@body)))
                      (derp (thing)
                            `(list* ,**test-var-different-file** ,thing)))
      (derp (herp (derp (herp (derp (herp (derp (herp (derp nil))))))))))
    (list 0 1 2 3 4))))
