#+xcvb (module (:depends-on ("asdf-finalizers" (:asdf "hu.dwim.stefil"))))

(defpackage :asdf-finalizers-test
  (:use :cl :fare-utils :asdf-finalizers :hu.dwim.stefil :list-of))

(in-package :asdf-finalizers-test)

;;; Testing the asdf-finalizers library.

(defsuite* (test-suite
            :in root-suite
            :documentation "Testing asdf-finalizers"))

(defun transpose (x)
  (check-type x (list-of (list-of integer)))
  (apply 'mapcar 'list x))

(deftest test-list-of ()
  (is (typep '(nil t t nil) '(list-of boolean)))
  (is (not (typep '(nil t 1 nil) '(list-of boolean))))
  (is (not (typep '(nil t t nil . 1) '(list-of boolean))))
  (is (typep '(1 2 3 4) '(list-of integer)))
  (is (not (typep '(1 2 3 4) '(list-of nil))))
  (is (typep nil '(list-of nil)))
  (is (equal (transpose '((1 2) (3 4))) '((1 3) (2 4))))
  nil)

(typep '(1 2 3) '(list-of string))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (typep '(1 2 3) '(list-of symbol)))

(final-forms)

#| Manual test: in a fresh Lisp,
(require "asdf")(asdf:load-system :asdf-finalizers :force t)(trace asdf-finalizers:eval-at-toplevel asdf-finalizers:register-final-form)(setf asdf-finalizers::*debug-finalizers* t)(asdf:load-system :asdf-finalizers-test :force '(:asdf-finalizers-test))
|#
