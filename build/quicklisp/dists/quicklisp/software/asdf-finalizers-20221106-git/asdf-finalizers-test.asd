;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(defsystem "asdf-finalizers-test"
  :description "Tests for asdf-finalizers"
  :author "Francois-Rene Rideau"
  :depends-on ("asdf-finalizers-test/1" "asdf-finalizers-test/2")
  :perform (test-op (o c) (call-function "asdf-finalizers-test::test-suite")))

(defsystem "asdf-finalizers-test/1"
  :description "Some tests for asdf-finalizers"
  :author "Francois-Rene Rideau"
  :defsystem-depends-on ("asdf-finalizers")
  :around-compile "asdf-finalizers:check-finalizers-around-compile"
  :depends-on ("list-of" "fare-utils" "hu.dwim.stefil")
  :components ((:file "asdf-finalizers-test")))

(defsystem "asdf-finalizers-test/2"
  :description "Some tests for asdf-finalizers"
  :author "Francois-Rene Rideau"
  :defsystem-depends-on ("asdf-finalizers")
  :depends-on ("list-of" "fare-utils" "hu.dwim.stefil")
  :components ((:finalized-cl-source-file "asdf-finalizers-test")))
