(in-package :common-lisp-user)

(require :asdf)

(asdf:initialize-output-translations
 `(:output-translations
   (t ,(merge-pathnames (make-pathname
                         :directory '(:relative "test" "cache" :wild-inferiors)
                         :name :wild :type :wild)
                        *load-truename*))
   :ignore-inherited-configuration
   ))

(asdf:load-asd (merge-pathnames (make-pathname
                                 :name "iterate"
                                 :type "asd")
                                *load-truename*))

(format t "~&Successfully loaded the iterate ASD file.~%")

#-sbcl
(asdf:load-asd (merge-pathnames (make-pathname
                                 :name "rt"
                                 :type "asd"
                                 :directory
                                 '(:relative "ext" "rt"))
                                *load-truename*))


(if (not (asdf:find-system "iterate"))
    (progn
     (format t "~&Unable to find the iterate ASDF system definition.~%")
     (uiop:quit 1))
    (format t "~&Found iterate ASDF system definition.~%"))



(asdf:load-system "iterate/tests")

(handler-bind ((iterate.test::unexpected-failures-error
                 #'(lambda (e)
                     (declare (ignorable e))
                     (format t "~&Catching unexpected failures error.~%")
                     (uiop:quit 2)))
               (error #'(lambda (e)
                          (format t "~&Caught unexpected error ~a~%" e)
                          (uiop:quit 3))))
  (asdf:test-system "iterate")
  (uiop:quit 0))
