;;;
;;; Tools to handle IBM PC version of IXF file format
;;;
;;; http://www-01.ibm.com/support/knowledgecenter/SSEPGG_10.5.0/com.ibm.db2.luw.admin.dm.doc/doc/r0004669.html

(in-package :ixf)

(defconstant +bigint+          492)
(defconstant +blob+            404)
(defconstant +clob+            408)
(defconstant +blob-file+       916)
(defconstant +clob-file+       920)
(defconstant +dbclob-file+     924)
(defconstant +char+            452)
(defconstant +date+            384)
(defconstant +dbclob+          412)
(defconstant +decimal+         484)
(defconstant +decfloat+        996)
(defconstant +float+           480)
(defconstant +graphic+         468)
(defconstant +integer+         496)
(defconstant +longvarchar+     456)
(defconstant +longvargraphic+  472)
(defconstant +smallint+        500)
(defconstant +time+            388)
(defconstant +timestamp+       392)
(defconstant +varchar+         448)
(defconstant +vargraphic+      464)
(defconstant +blob-location-spec+   960)
(defconstant +dbclob-location-spec+ 964)
(defconstant +dbblob-location-spec+ 968) ; unnamed in the spec?

(defun parse-ixf-null (data pos)
  "Read a NULL indicator and returns t when the value is NULL."
  ;;
  ;; The null indicator is a two-byte value set to x'0000' for not null, and
  ;; x'FFFF' for null.
  ;;
  (and (= #xff (aref data pos))
       (= #xff (aref data (+ 1 pos)))))


;;;
;;; Reading numbers
;;;
(defun unsigned-to-signed (byte n)
  (declare (type fixnum n) (type unsigned-byte byte))
  (logior byte (- (mask-field (byte 1 (1- (* n 8))) byte))))

(defun parse-ixf-smallint (data pos)
  "Read a 2-byte integer."
  (unsigned-to-signed (logior (ash (aref data (+ 1 pos)) 8) (aref data pos)) 2))

(defun parse-ixf-unsigned-integer (data pos)
  "Read an unsigned 4-byte integer."
  (logior (ash (aref data (+ pos 3)) 24)
          (ash (aref data (+ pos 2)) 16)
          (ash (aref data (+ pos 1)) 8)
          (aref data pos)))

(defun parse-ixf-unsigned-bigint (data pos)
  "Read an unsigned 8-byte integer."
  (logior (parse-ixf-unsigned-integer data pos)
          (ash (parse-ixf-unsigned-integer data (+ 4 pos)) 32)))

(defun parse-ixf-integer (data pos)
  "Read a signed 4-byte integer."
  (unsigned-to-signed (parse-ixf-unsigned-integer data pos) 4))

(defun parse-ixf-bigint (data pos)
  "Read a signed 8-byte integer."
  (unsigned-to-signed (parse-ixf-unsigned-bigint data pos) 8))

(defun parse-ixf-decimal (data pos precision scale)
  "Read a DECIMAL BCD IBM format.

   The right documentation to be able to make sense of the data seems to be
   found at http://www.simotime.com/datapk01.htm, at least it allows
   progress to be made."

  (let* ((nbytes (floor (+ precision 2) 2))
         (bytes  (subseq data pos (+ pos nbytes)))
         (sign   (if (= #xD (ldb (byte 4 0) (aref bytes (- nbytes 1))))
                     -1 1)))
    (* sign
       (/
        (loop :for byte :across bytes
           :for num :from 1
           :for pow  := (expt 10 precision) :then (floor pow 100)
           :for high := (ldb (byte 4 4) byte)
           :for low  := (ldb (byte 4 0) byte)
           :when (= num nbytes) :sum (* high pow)
           :else :sum (+ (* high pow) (* low (/ pow 10))))
        (expt 10 scale)))))

(defun parse-ixf-float (data pos length)
  "Parse a FLOATING POINT machine IBM format."
  (cond
    ((= 4 length)
     (ieee-floats:decode-float32 (parse-ixf-unsigned-integer data pos)))

    ((= 8 length)
     (ieee-floats:decode-float64 (parse-ixf-unsigned-bigint data pos)))))


;;;
;;; Reading encoded strings
;;;
(defun parse-ixf-string (data pos length)
  "Read an encoded string in data from pos to length."
  (babel:octets-to-string data :start pos :end (+ pos length)))


;;;
;;; Reading ascii-encoded date and time strings
;;;
(defun parse-ixf-timestamp (data pos length)
  "Read an IXF timestamp string.

   From the docs:

   Each time stamp is a character string of the form
   yyyy-mm-dd-hh.mm.ss.nnnnnn (year month day hour minutes seconds
   fractional seconds).

   Starting with Version 9.7, the timestamp precision is contained in the
   IXFCLENG field of the column descriptor record, and cannot exceed 12.
   before Version 9.7, IXFCLENG is not used, and should contain blanks.

   Valid characters within TIMESTAMP are invariant in all PC ASCII code
   pages; therefore, IXFCSBCP and IXFCDBCP are not significant, and should
   be zero."
  (let ((datestring
         (map 'string #'code-char (subseq data pos (+ pos length 20)))))

    (cl-ppcre:register-groups-bind ((#'parse-integer year month day hour min sec frac))
        ("(....)-(..)-(..)-(..).(..).(..).(\\d+)" datestring)
      (let ((ns (* frac (expt 10 (- 9 length)))))
        (local-time:encode-timestamp ns sec min hour day month year)))))

(defun parse-ixf-time (data pos)
  "Read an IXF time ascii string."
  (let ((timestring (map 'string #'code-char (subseq data pos (+ pos 8)))))
    (substitute #\: #\. timestring)))

(defun parse-ixf-date (data pos)
  "Read an IXF date ascii string."
  (map 'string #'code-char (subseq data pos (+ pos 10))))


;;;
;;; external BLOB and CLOBs
;;;
(defun parse-ixf-lls (data pos
                      &key
                        relative-to
                        (element-type    '(unsigned-byte 8))
                        (external-format :ascii))
  "Parse a LOB Location Specifier."
  (let ((lls (babel:octets-to-string data :start pos)))
    (cl-ppcre:register-groups-bind (filename (#'parse-integer offset length))
        ("^(.*)\\.(\\d+)\\.(\\d+)/$" lls)
      (with-open-file (blob (make-pathname :defaults relative-to
                                           :name filename
                                           :type nil)
                            :direction :input
                            :element-type element-type
                            :external-format external-format)
        (file-position blob offset)
        (let ((bytes (make-array length :element-type element-type)))
          (read-sequence bytes blob)
          bytes)))))
