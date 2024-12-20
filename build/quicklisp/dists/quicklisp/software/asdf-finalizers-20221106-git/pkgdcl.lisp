#+xcvb (module ())

(uiop:define-package :asdf-finalizers
  (:use :cl :asdf)
  (:import-from :asdf
   #:around-compile)
  (:export
   #:*warn-when-finalizers-off*
   #:eval-at-toplevel
   #:final-forms
   #:register-finalizer
   #:register-final-form
   #:no-finalizer-left-behind-p
   #:assert-no-finalizer-left-behind
   #:compile-check-finalizers
   #:check-finalizers-around-compile
   #:call-with-finalizers
   #:with-finalizers
   #:using-finalizers-p
   #:finalizers-off
   #:finalizers-off-error
   #:finalizers-off-simple-error
   #:finalizers-off-warning
   #:finalizers-off-simple-warning
   ))
