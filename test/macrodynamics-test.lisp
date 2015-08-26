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

#+nil
(with-generators ((x (generator (integer)))
                  (y (generator (integer))))
  (let-map ((a (+ x x)))
    (let ((z 3))
      (wrap-let-map ((b (+ a a)))
        (let ((check-it:*num-trials* 10))
          (is
           (check-that (progn
                         (format t "~&~A ~A ~A ~A~%" x y a b)
                         (= a a)))))))))
