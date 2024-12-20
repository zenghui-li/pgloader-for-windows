;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(defsystem "asdf-finalizers"
  :description "Enforced calling of finalizers for Lisp code"
  :author "Francois-Rene Rideau"
  :depends-on ((:version "asdf" "3.0"))
  :components
  ((:file "pkgdcl")
   (:file "finalizers" :depends-on ("pkgdcl"))
   (:file "asdf-support" :depends-on ("finalizers")) ;; uses with-finalizers
   (:file "initialization" :depends-on ("pkgdcl")))
  :in-order-to ((test-op (test-op "asdf-finalizers-test"))))
