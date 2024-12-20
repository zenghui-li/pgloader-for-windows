;;;
;;; API to read IBM ixf files.
;;;

(in-package #:ixf)

(defmacro with-ixf-stream ((var stream) &body body)
  "Executes BODY with VAR an IXF-FILE instance made from STREAM."
  `(let ((,var (make-ixf-file :stream ,stream)))
     (read-headers ,var)
     ,@body))

(defmacro with-ixf-file ((var filename) &body body)
  "Executes BODY with VAR an IXF-FILE instance made from FILENAME stream
  contents."
  `(with-open-file (s ,filename :element-type '(unsigned-byte 8))
     (with-ixf-stream (,var s)
       ,@body)))

(defun map-data (ixf map-fn)
  "Call map-fn on each row of data read from STREAM given IXF definition."
  (let ((stream (ixf-file-stream ixf)))
    (loop :while (< (file-position stream) (file-length stream))
       :for record := (read-next-record stream)
       :when (data-record-p record)
       :do (funcall map-fn (read-next-row ixf record)))))

(defmethod read-data ((ixf ixf-file))
  "Return the data read from IXF as a list of vectors of values."
  (let ((stream (ixf-file-stream ixf)))
    (loop :while (< (file-position stream) (file-length stream))
       :for record := (read-next-record stream)
       :when (data-record-p record)
       :collect (read-next-row ixf record))))

(defun read-ixf-file (filename)
  "Read FILENAME as an IXF file and return the IXF definition (table name,
   columns names and types, etc) and all its content as a list of vectors,
   each vector being a table's row, as mutliple values."
  (with-ixf-file (ixf filename)
    (values ixf (read-data ixf))))

