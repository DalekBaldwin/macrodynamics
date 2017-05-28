;;;; macrodynamics.asd

(defsystem "macrodynamics"
  :name "macrodynamics"
  :serial t
  :author "Kyle Littler"
  :license "LLGPL"
  :description "A language extension for creating bindings scoped to the entire expansion process of a region of code."
  ;;:long-description #.(uiop:read-file-string (uiop:subpathname *load-pathname* "README.md"))
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
  :name "macrodynamics-test"
  :serial t
  :author "Kyle Littler"
  :license "LLGPL"
  :description "Tests for macrodynamics."
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "macros")
                         (:file "macrodynamics-test"))))
  :depends-on ("macrodynamics" "hu.dwim.stefil" "check-it")
  :perform (test-op (op c) (symbol-call :macrodynamics-test :test-all)))
