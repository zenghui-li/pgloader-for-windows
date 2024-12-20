(defpackage #:dynamic-classes-test-system (:use #:asdf #:cl))
(in-package #:dynamic-classes-test-system)

(defsystem dynamic-classes-test
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License; see file COPYING for details"
  :description "Tests for LIsp Framework for Testing"
  :components ((:module 
		"setup"
		:pathname "test/"
		:components ((:file "packages")
			     (:file "tests"
				    :depends-on ("packages"))))
	       (:module 
		"test"
		:pathname "test/"
		:depends-on ("setup")
		:components ()))  
  :depends-on (:lift
	       :dynamic-classes))

(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system 'dynamic-classes-test))))
  (values nil))


