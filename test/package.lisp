(in-package :cl-user)

(defpackage :macrodynamics-test
  (:use :cl :macrodynamics :stefil :alexandria :check-it)
  (:export
   #:test-all))

(in-package :macrodynamics-test)

(defparameter *system-directory* macrodynamics::*system-directory*)
