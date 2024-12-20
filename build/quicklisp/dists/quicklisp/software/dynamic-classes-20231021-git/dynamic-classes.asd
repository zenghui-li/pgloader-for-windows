(defpackage #:dynamic-classes-system (:use #:common-lisp #:asdf))
(in-package #:dynamic-classes-system)

(defsystem dynamic-classes
  :author "Gary Warren King <gwking@metabang.com>"
  :version "1.0.2"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license"
  :components ((:module 
		"dev"
		:serial t
		:components ((:file "package")
			     (:file "define-class")
			     (:file "dynamic-class"))))
  :in-order-to ((test-op (load-op dynamic-classes-test)))
  :perform (test-op :after (op c)
		    (funcall (intern (symbol-name '#:run-tests) :lift) 
			     :config :generic))
  :depends-on (:metatilities-base))

(defmethod operation-done-p 
           ((o test-op) (c (eql (find-system 'dynamic-classes))))
  (values nil))


