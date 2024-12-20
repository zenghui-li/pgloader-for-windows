;;;; cl-ifx.asd

(asdf:defsystem #:ixf
    :serial t
    :description "Tools to handle IBM PC version of IXF file format"
    :author "Dimitri Fontaine <dim@tapoueh.org>"
    :license "WTFPL"
    :version "0.1.0"
    :depends-on (#:split-sequence       ; split sequences
                 #:md5                  ; check archive checksums
                 #:alexandria           ; utils
                 #:babel                ; Encoding conversions
                 #:local-time           ; Parsing timestamps
                 #:cl-ppcre             ; Regular expressions
                 #:ieee-floats          ; Decoding IEEE Floats
                 )
    :components ((:file "package")
                 (:file "records"   :depends-on ("package"))
                 (:file "types"     :depends-on ("package"))
                 (:file "encodings" :depends-on ("package"))
                 (:file "structs"   :depends-on ("package"
                                                 "records"
                                                 "types"
                                                 "encodings"))
                 (:file "data"      :depends-on ("package"
                                                 "structs"
                                                 "records"
                                                 "types"))
                 (:file "ixf"       :depends-on ("package"
                                                 "records"
                                                 "types"
                                                 "structs"
                                                 "data"))))

