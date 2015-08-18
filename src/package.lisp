(in-package :cl-user)

(defpackage :macrodynamics
  (:use :cl :alexandria)
  (:export #:ct-let
           #:ct-let*
           #:ct-flet
           #:ct-labels
           #:def-dynenv-macro
           #:unbound-dynenv-macro-var
           #:unbound-dynenv-macro-fun
           #:ct-get
           #:ct-get-fun
           #:ct-call
           #:ct-apply))

(in-package :macrodynamics)

(defparameter *system-directory*
  (make-pathname
   :directory
   (pathname-directory
    (asdf:system-definition-pathname "macrodynamics"))))
