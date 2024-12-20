#+xcvb (module (:depends-on ("finalizers")))

(in-package :asdf-finalizers)

(defun compile-check-finalizers (input-file &rest keys &key &allow-other-keys)
  (declare (ignore keys))
  (let ((okp (no-finalizer-left-behind-p)))
    (unless okp
      (warn 'missing-final-forms
	    :format-control "Source file ~A uses finalizers but fails to ~
             include ~S between the last finalizer and the end of file"
	    :format-arguments `(,input-file (final-forms))))
    okp))

(defun check-finalizers-around-compile (fun)
  "Assuming your system :depends-on (:asdf-finalizers),
  you may use this function as your :around-compile function
  for an ASDF system, module or file, as in
	:around-compile \"asdf-finalizers:check-finalizers-around-compile\"
  This will allow you to use finalizers within covered source files,
  and will issue an error if you fail to evaluate (FINAL-FORMS)
  after the last finalizer was used and before the end of the file.
  Alternatively, you may use ASDF::FINALIZED-CL-SOURCE-FILE below.
  You may also have your own custom :around-compile hooks
  chain into CHECK-FINALIZERS-AROUND-COMPILE
  to achieve the same effect and more."
  (with-finalizers ()
    (funcall fun :compile-check 'compile-check-finalizers)))

(defclass asdf::finalized-cl-source-file (cl-source-file)
  ((around-compile :initform 'check-finalizers-around-compile))
  (:documentation "Assuming your system :defsystem-depends-on (:asdf-finalizers),
  you may use this class as your system's :default-component-class,
  or as the class of a component as in
	(:finalized-cl-source-file \"foo\" :depends-on (\"bar\" \"baz\"))
  This will automatically declare CHECK-FINALIZERS-AROUND-COMPILE
  as the relevant component's :around-compile hook."))
