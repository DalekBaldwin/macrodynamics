;;;; macrodynamics.asd

(defpackage :macrodynamics-system
  (:use :cl :asdf))
(in-package :macrodynamics-system)

(defsystem :macrodynamics
  :name "macrodynamics"
  :serial t
  :components
  ((:static-file "macrodynamics.asd")
   (:module :src
            :components ((:file "package")
                         (:file "macrodynamics"))
            :serial t))
  :depends-on (:alexandria :lisp-interface-library)
  :in-order-to ((test-op (load-op :macrodynamics-test)))
  :perform (test-op :after (op c)
                    (funcall
                     (intern #.(string '#:run-all-tests)
                             :macrodynamics-test))))

(defsystem :macrodynamics-test
  :name "macrodynamics-test"
  :serial t
  :description "Tests for macrodynamics."
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "macros")
                         (:file "macrodynamics-test"))))
  :depends-on (:macrodynamics :stefil :check-it))
