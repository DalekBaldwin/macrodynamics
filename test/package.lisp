(in-package :cl-user)

(defpackage :macrodynamics/test
  (:use :cl
        :macrodynamics
        :fiasco
        :alexandria
        :check-it))

(fiasco:defsuite
    (fiasco-suites::macrodynamics/test :bind-to-package :macrodynamics/test
                                       :in fiasco-suites::all-tests))

(in-package :macrodynamics/test)

(defparameter *system-directory* (asdf:system-source-directory "macrodynamics/test"))
