(in-package :cl-user)

(defpackage :macrodynamics
  (:use :cl :alexandria)
  (:export #:ct-let
           #:ct-let*
           #:ct-flet
           #:ct-labels
           #:def-dynenv-macro
           #:with-dynenv
           #:def-dynenv-var
           #:def-dynenv-fun
           #:def-unbound-dynenv-fun
           #:dynenv-function
           #:unbound-dynenv-macro-var
           #:unbound-dynenv-macro-fun))

(in-package :macrodynamics)

(defparameter *system-directory*
  (make-pathname
   :directory
   (pathname-directory
    (asdf:system-definition-pathname "macrodynamics"))))
