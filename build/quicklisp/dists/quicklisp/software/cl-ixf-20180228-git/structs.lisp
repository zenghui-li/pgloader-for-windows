;;;
;;; API to read IBM ixf files.
;;;

(in-package #:ixf)

(defstruct ixf-header date time count code-page encoding)

(defstruct ixf-column
  name nullable has-default default pkey-pos type
  code-page encoding length d-id pos desc)

(defstruct ixf-table
  name creator source ncol columns pkey-name desc)

(defstruct ixf-file stream header table data-position)

(declaim (inline get-code-page))
(defun get-code-page (code-page)
  "Return CODE-PAGE or NIL if it's 00000"
  (if (string= "00000" code-page) nil code-page))

(defun parse-encoding (record single-cp-property double-cp-property &key strict)
  "Read the encoding from the RECORD with properties such as :IXFHSBCP
   and :IXFHDBCP, or :IXFCSBCP and :IXFCDBCP"
  (let ((single-byte-code-page
         (get-code-page (get-record-property single-cp-property record)))

        (double-byte-code-page
         (get-code-page (get-record-property double-cp-property record))))
    ;; we want to read only one value here.
    (when strict
      (assert (and (not (and (null single-byte-code-page)
                             (null double-byte-code-page)))
                   (not (and (null single-byte-code-page)
                             (null double-byte-code-page))))))

    (let ((cp (or single-byte-code-page double-byte-code-page)))
      (values cp (babel-encoding-for-code-page cp)))))

(defmethod parse-header ((ixf ixf-file) record)
  "Given a record alist, parse its definition into IXF."
  (let ((header (setf (ixf-file-header ixf) (make-ixf-header))))
    (setf (ixf-header-date header)  (get-record-property :IXFHDATE record))
    (setf (ixf-header-time header)  (get-record-property :IXFHTIME record))
    (setf (ixf-header-count header) (get-record-property :IXFHHCNT record))

    ;; read the encoding, either Single-Byte Code Page or Double-Byte Code Page
    (multiple-value-bind (code-page encoding)
        (parse-encoding record :IXFHSBCP :IXFHDBCP :strict t)
      (setf (ixf-header-code-page header) code-page
            (ixf-header-encoding header)  encoding))

    ;; return the ixf structure itself
    ixf))

(defmethod parse-table-definition ((ixf ixf-file) record)
  "Parse a Table definition from its record."
  (let ((table (setf (ixf-file-table ixf) (make-ixf-table))))
    (setf (ixf-table-name table)
          (subseq (get-record-property :IXFTNAME record)
                  0
                  (get-record-property :IXFTNAML record)))

    (setf (ixf-table-creator table)
          (subseq (get-record-property :IXFTQUAL record)
                  0
                  (get-record-property :IXFTQULL record)))

    (setf (ixf-table-source table)    (get-record-property :IXFTSRC record))
    (setf (ixf-table-ncol table)      (get-record-property :IXFTCCNT record))
    (setf (ixf-table-pkey-name table) (get-record-property :IXFTPKNM record))

    (setf (ixf-table-desc table)
          (string-trim '(#\Space) (get-record-property :IXFTDESC record)))

    ;; prepare a vector of columns of the right size
    (setf (ixf-table-columns table)
          (make-array (ixf-table-ncol table)
                      :element-type 'ixf-column))

    (loop :for i :below (ixf-table-ncol table)
       :do (setf (aref (ixf-table-columns table) i)
                 (make-ixf-column)))

    ;; return the ixf structure itself
    ixf))

(defmethod parse-column-definition ((col ixf-column) (header ixf-header) record)
  "Parse a Column definition from its record."
  (setf (ixf-column-name col)
        (subseq (get-record-property :IXFCNAME record)
                0
                (get-record-property :IXFCNAML record)))

  (setf (ixf-column-nullable col)
        (char= #\Y (get-record-property :IXFCNULL record)))

  (setf (ixf-column-has-default col)
        (char= #\Y (get-record-property :IXFCDEF record)))

  (when (ixf-column-has-default col)
    (setf (ixf-column-default col)
          (subseq (get-record-property :IXFCDEFV record)
                  0
                  (get-record-property :IXFCDEFL record))))

  (setf (ixf-column-pkey-pos col) (get-record-property :IXFCKPOS record))
  (setf (ixf-column-type col)     (get-record-property :IXFCTYPE record))
  (setf (ixf-column-length col)   (get-record-property :IXFCLENG record))
  (setf (ixf-column-d-id col)     (get-record-property :IXFCDRID record))
  (setf (ixf-column-pos col)      (get-record-property :IXFCPOSN record))

  (multiple-value-bind (code-page encoding)
      (parse-encoding record :IXFCSBCP :IXFCDBCP)
    (setf
     (ixf-column-code-page col) (or code-page (ixf-header-code-page header))
     (ixf-column-encoding col)  (or encoding (ixf-header-encoding header))))

  (setf (ixf-column-desc col)
        (string-trim '(#\Space)
                     (get-record-property :IXFCDESC record))))

(defmethod read-headers ((ixf ixf-file))
  "Return an IXF-FILE data structure filled with information read from FILENAME."
  (let* ((stream        (ixf-file-stream ixf))
         (header-record (read-next-record stream)))

    (parse-header ixf header-record)

    (loop :with col-number := 0 :with cols := nil
       :while (or (null cols) (< col-number cols))

       :for record := (read-next-record stream)

       ;; stop before data
       :until (char= #\D (get-record-property :type record))

       ;; analyze records
       :when (char= #\T (get-record-property :type record))
       :do (setf cols
                 (ixf-table-ncol
                  (ixf-file-table (parse-table-definition ixf record))))

       :when (char= #\C (get-record-property :type record))
       :do (let ((column
                  (aref (ixf-table-columns (ixf-file-table ixf)) col-number)))
             (parse-column-definition column (ixf-file-header ixf) record)
             (incf col-number))

       :finally (progn
                  (setf (ixf-file-data-position ixf) (file-position stream))
                  (return ixf)))))
