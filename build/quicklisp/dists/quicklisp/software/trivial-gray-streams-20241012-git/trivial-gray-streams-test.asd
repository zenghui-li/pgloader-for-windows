;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; -*-

(defsystem :trivial-gray-streams-test
  :version "2.1"
  :depends-on (:trivial-gray-streams)
  :pathname #P"test/"
  :serial t
  :components ((:file "package")
               (:file "test-framework")
               (:file "test")))
