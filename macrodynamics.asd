;;;; macrodynamics.asd

(defsystem "macrodynamics"
  :serial t
  :author "Kyle Littler"
  :license "LLGPL"
  :version "0.1"
  :description "A language extension for creating bindings scoped to the entire expansion process of a region of code."
  :homepage "https://github.com/DalekBaldwin/macrodynamics"
  :components
  ((:static-file "macrodynamics.asd")
   (:module :src
            :components ((:file "package")
                         (:file "util")
                         (:file "macrodynamics"))
            :serial t))
  :depends-on ("alexandria")
  :in-order-to ((test-op (test-op "macrodynamics/test"))))

(defsystem "macrodynamics/test"
  :serial t
  :author "Kyle Littler"
  :license "LLGPL"
  :version "0.1"
  :description "Tests for macrodynamics."
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "macros")
                         (:file "macrodynamics-test"))))
  :depends-on ("macrodynamics"
               "fiasco"
               "check-it")
  :perform (test-op (op c) (symbol-call :macrodynamics/test :test-all)))
