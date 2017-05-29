(in-package :cl-user)

(fiasco:define-test-package :macrodynamics-test
  (:use :cl
        :macrodynamics
        :fiasco
        :alexandria
        :check-it))

(in-package :macrodynamics-test)

(defparameter *system-directory* macrodynamics::*system-directory*)
