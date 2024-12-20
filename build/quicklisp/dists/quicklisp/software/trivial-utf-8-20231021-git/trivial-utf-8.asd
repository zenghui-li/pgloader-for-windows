(asdf:defsystem :trivial-utf-8
  :description "A small library for doing UTF-8-based input and output."
  :licence "ZLIB"
  :author "Marijn Haverbeke <marijnh@gmail.com>"
  :maintainer "Gábor Melis <mega@retes.hu>"
  :homepage "https://common-lisp.net/project/trivial-utf-8/"
  :bug-tracker "https://gitlab.common-lisp.net/trivial-utf-8/trivial-utf-8/-/issues"
  :source-control (:git "https://gitlab.common-lisp.net/trivial-utf-8/trivial-utf-8.git")
  :depends-on (#:mgl-pax-bootstrap)
  :components ((:file "trivial-utf-8")))

(asdf:defsystem :trivial-utf-8/tests
  :depends-on (:trivial-utf-8)
  :components ((:file "tests")))
