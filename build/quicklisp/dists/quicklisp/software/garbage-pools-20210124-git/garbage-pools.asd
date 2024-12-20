;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(defpackage :garbage-pools-asd
  (:use :cl :asdf))

(in-package :garbage-pools-asd)

(defsystem "garbage-pools"
  :description "GARBAGE-POOLS is Common Lisp re-implementation of the APR Pools
  for resource management."
  :version "0.1.2"
  :author "Andrey Moskvitin <archimag@gmail.com>"
  :homepage "https://github.com/archimag/garbage-pools"
  :license "MIT"
  :components ((:file "garbage-pools")))
