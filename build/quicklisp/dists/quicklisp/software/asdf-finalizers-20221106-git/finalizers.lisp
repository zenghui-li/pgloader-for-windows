#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :asdf-finalizers)

(defvar *warn-when-finalizers-off* t
  "Flag to enable or disable the raising warnings
when finalizers are used outside of context.
Typically, you want that flag to be on while compiling your application, but
off when your application is done compiled and you're at runtime.")

(defvar *debug-finalizers* nil
  "Flag to enable debugging output for finalizers.")


(define-condition finalizers-off () ())
(define-condition finalizers-off-error (finalizers-off error) ())
(define-condition finalizers-off-simple-error (finalizers-off-error simple-error) ())
(define-condition finalizers-off-warning (finalizers-off warning) ())
(define-condition finalizers-off-simple-warning (finalizers-off-warning simple-warning) ())

(define-condition missing-final-forms (simple-warning) ())


;; UNBOUND by default: catch people using them outside of a proper with-finalizers form!
(defvar *finalizers*)
(defvar *finalizers-data* nil)

(defun using-finalizers-p ()
  (boundp '*finalizers*))

(defun reset-finalizers ()
  (setf *finalizers* nil
	*finalizers-data* (make-hash-table :test 'equal))
  (values))

(defun disable-finalizers ()
  (makunbound '*finalizers*)
  (makunbound '*finalizers-data*)
  (values))

(defmacro final-forms ()
  "This macro will expand into any final forms so far registered.
  The forms will be expanded inside an
    (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE) ...)
  but you can override that with your own EVAL-WHEN.
  You need to have finalizers enabled to use this macro (see WITH-FINALIZERS).
  In a file that uses finalizers, you MUST include (FINAL-FORMS)
  after the last finalizer was used and before the end of the file,
  or the compilation will fail.
  Typically, you will write (FINAL-FORMS) as the very last form in your file,
  or if you didn't use the asdf-finalizers package, you will instead write
  (ASDF-FINALIZERS:FINAL-FORMS)."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ;; This indirection is because some Lisps (e.g. CCL 1.8) may be confused
     ;; if the state from *finalizers* isn't properly flushed by this eval-when.
     (final-forms-internal)))

(defmacro final-forms-internal ()
  (expand-final-forms))

(defun expand-final-forms ()
  (cond
    ((using-finalizers-p)
     (let ((forms (reverse
		   (loop :while *finalizers*
			 :collect (let ((f (pop *finalizers*)))
				    (etypecase f
				      (function (funcall f))
				      (cons f)))))))
       (when *debug-finalizers*
	 (with-standard-io-syntax
	   (let ((*package* (find-package :cl))
		 (*print-readably* nil)
		 (*print-pretty* t))
	     (format *trace-output* "~&Final forms:~%~{  ~S~%~}~%" forms))))
       `(progn ,@forms)))
    (*warn-when-finalizers-off*
     (warn 'finalizers-off-simple-warning
	   :format-control "~S expanded outside of ~S"
	   :format-arguments '(final-forms with-finalizers))
     nil)
    (t
     nil)))

(defun register-finalizer (finalizer)
  "This function, to be used within a macro, reader-macro, deftype, etc.,
  will register a THUNK to be called during finalization.
  Dependencies may be enforced by thunk calling thunk dependencies.
  Any form returned by the THUNK will be included in the finalized code
  after the code from any previously registered thunk of constant code fragment,
  and after the code from any registered dependency."
  (check-type finalizer (or function cons))
  (unless (using-finalizers-p)
    (error 'finalizers-off-simple-error
	   :format-control "Trying to use finalizers outside of a (~S ...) form. ~
       You probably need to use ~
       :around-compile \"asdf-finalizers:check-finalizers-around-compile\" ~
       in your asdf defsystem"
	   :format-arguments '(with-finalizers)))
  (push finalizer *finalizers*))

(defun register-final-form (form)
  "This function, to be used within a macro, reader-macro, deftype, etc.,
  will register a constant piece of code to the evaluated at toplevel
  at the end of the current code fragment (e.g. file)."
  (check-type form cons)
  (register-finalizer form))

(defun no-finalizer-left-behind-p ()
  (null *finalizers*))

(defun assert-no-finalizer-left-behind ()
  (assert (no-finalizer-left-behind-p)))

(defmacro with-finalizers ((&key finalize) &body body)
  "Evaluate BODY in a context where finalizers are enabled.
  By default, don't finalize, because we want to catch code
  that fails to finalize in the same file that requires code finalization.
  This macro is typically used by ASDF when you configure it as below.
  For convenience, you may also use it to test code at the REPL;
  you may then pass an argument FINALIZE with true value,
  and WITH-FINALIZERS will evaluate finalization forms."
  `(call-with-finalizers #'(lambda () ,@body) :finalize ,finalize))

(defun call-with-finalizers (thunk &key finalize)
  (let ((*finalizers* '())
	(*finalizers-data* (make-hash-table :test 'equal)))
    (unwind-protect
	 (funcall thunk)
      (when finalize (eval '(final-forms)))
      (assert-no-finalizer-left-behind))))

(defun eval-at-toplevel (form &optional already-done-p-form warning &rest warning-arguments)
  "This function, to be used within a macro, deftype, reader-macro, etc.,
  will evaluate toplevel FORM now during the macroexpansion phase, but also
  register it to be evaluated at the toplevel as part of the FINAL-FORMS,
  so that assuming you use the FINAL-FORMS afterwards but before the end of current file,
  so it is available to whoever load the associated FASL or CFASL.
  If the FORM has already been registered, it is skipped.
  Either now or when loading the (C?)FASL, the evaluation of FORM will be skipped
  when ALREADY-DONE-P-FORM evaluates to a true value.
  When finalizers are not enabled, warn with given warning and arguments or
  with a default warning, unless ALREADY-DONE-P-FORM evaluated to a true value,
  at which point we trust the user to somehow have done the right thing,
  and a build from clean will hopefully catch him if he didn't."
  (let ((whole `(eval-at-toplevel ,form ,already-done-p-form))
	(already-done-p (eval already-done-p-form)))
    (unless already-done-p
      (eval form))
    (cond
      ((using-finalizers-p)
       (unless (gethash whole *finalizers-data*)
	 (setf (gethash whole *finalizers-data*) t)
	 (register-final-form
          (if already-done-p-form
            `(unless ,already-done-p-form ,form)
            form))))
      (already-done-p) ;; don't warn if it has already been done; it could be by design.
      ((not *warn-when-finalizers-off*)) ;; don't warn if warnings are off - e.g. at runtime.
      ((stringp warning)
       (warn 'finalizers-off-simple-warning
             :format-control warning :format-arguments warning-arguments))
      ((and warning (symbolp warning))
       (apply 'warn warning warning-arguments))
      (t
       (warn 'finalizers-off-simple-warning
	     :format-control "trying to ~S form ~S without finalizers enabled~@[ while not ~S~]"
	     :format-arguments whole))))
  nil)
