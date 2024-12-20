;;;
;;;IXF File format encodings
;;;
;;; See http://www-01.ibm.com/software/globalization/ccsid/ccsid_registered.html

(in-package #:ixf)

(defvar *ixf-encodings-mapping*
  '(("09580" . :GBK)
    ("00932" . :CP932)
    ("29626" . :EUCJP)
    ("13242" . :EUCJP)
    ("00952" . :EUCJP)
    ("00953" . :EUCJP)
    ("00954" . :EUCJP)
    ("01252" . :CP1252)
    ("01251" . :CP1251)
    ;; ( . :UCS-2BE)
    ;; ( . :UCS-2LE)
    ;; ( . :UCS-2)
    ("01233" . :UTF-32BE)
    ("01235" . :UTF-32LE)
    ("01237" . :UTF-32)
    ("01201" . :UTF-16BE)
    ("01203" . :UTF-16LE)
    ("01205" . :UTF-16)
    ;; ( . :UTF-8B)
    ("01208" . :UTF-8)                  ; UTF-8 with IBM PUA
    ("01209" . :UTF-8)
    ;; ( . :ISO-8859-16)
    ("00923" . :ISO-8859-15)
    ;; ( . :ISO-8859-14)
    ("00921" . :ISO-8859-13)
    ;; ( . :ISO-8859-11)
    ;; ( . :ISO-8859-10)
    ("00920" . :ISO-8859-9)
    ("05012" . :ISO-8859-8)
    ("00916" . :ISO-8859-8)
    ("00813" . :ISO-8859-7)
    ("09005" . :ISO-8859-7)
    ("01089" . :ISO-8859-6)
    ("00915" . :ISO-8859-5)
    ("00914" . :ISO-8859-4)
    ("00913" . :ISO-8859-3)
    ("00912" . :ISO-8859-2)
    ("00819" . :ISO-8859-1)
    ("04133" . :EBCDIC-US)
    ("00437" . :ASCII))
  "A alist of mapping from IBM CCSID to babel encodings.")

(defun babel-encoding-for-code-page (code-page)
  "Return a babel encoding for given CODE-PAGE."
  (when code-page
    (or (cdr (assoc code-page *ixf-encodings-mapping* :test #'string=))
        (error "Unknown Code Page ~s" code-page))))
