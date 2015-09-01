;;;; macrodynamics.asd

(defpackage :macrodynamics-system
  (:use :cl :asdf))
(in-package :macrodynamics-system)

(defsystem :macrodynamics
  :name "macrodynamics"
  :serial t
  :author "Kyle Littler"
  :license "LLGPL"
  :description "A language extension for creating bindings scoped to the entire expansion process of a region of code."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :components
  ((:static-file "macrodynamics.asd")
   (:module :src
            :components ((:file "package")
                         (:file "util")
                         (:file "macrodynamics"))
            :serial t))
  :depends-on (:alexandria)
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
