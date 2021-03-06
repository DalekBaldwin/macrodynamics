(in-package :cl-user)

(defpackage :macrodynamics
  (:use :cl :alexandria)
  (:export #:ct-let
           #:ct-let*
           #:ct-flet
           #:call-next-dynenv-fun
           #:def-dynenv-macro
           #:dynenv-macrolet
           #:with-dynenv
           #:def-dynenv-var
           #:def-dynenv-fun
           #:def-unbound-dynenv-fun
           #:dynenv-function
           #:unbound-dynenv-macro-var
           #:unbound-dynenv-macro-fun))

(in-package :macrodynamics)

(defparameter *system-directory* (asdf:system-source-directory "macrodynamics"))
