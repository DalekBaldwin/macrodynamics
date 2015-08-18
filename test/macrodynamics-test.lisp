(in-package :macrodynamics-test)

(in-root-suite)

(defsuite* test-all)

(deftest test-contextual-dsl ()
  (with-generators ((x (generator (integer)))
                    (y (generator (integer))))
    (let-map ((a (+ x x)))
      (let-map ((b (+ a a)))
        (let ((check-it:*num-trials* 10))
          (is
           (check-that (progn
                         (format t "~&~A ~A ~A ~A~%" x y a b)
                         (= a a)))))))))
