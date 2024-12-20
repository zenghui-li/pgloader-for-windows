;;;
;;; Tools to handle IBM PC version of IXF file format
;;;
;;; http://www-01.ibm.com/support/knowledgecenter/SSEPGG_10.5.0/com.ibm.db2.luw.admin.dm.doc/doc/r0004667.html

(in-package :ixf)

(defstruct (ixf-field
             (:conc-name field-)
             (:constructor make-field (name size type)))
  name size type)

#|
HEADER RECORD

   FIELD NAME     LENGTH    TYPE        COMMENTS
   ----------     -------   ---------   -------------
   IXFHRECL       06-BYTE   CHARACTER   record length
   IXFHRECT       01-BYTE   CHARACTER   record type = 'H'
   IXFHID         03-BYTE   CHARACTER   IXF identifier
   IXFHVERS       04-BYTE   CHARACTER   IXF version
   IXFHPROD       12-BYTE   CHARACTER   product
   IXFHDATE       08-BYTE   CHARACTER   date written
   IXFHTIME       06-BYTE   CHARACTER   time written
   IXFHHCNT       05-BYTE   CHARACTER   heading record count
   IXFHSBCP       05-BYTE   CHARACTER   single byte code page
   IXFHDBCP       05-BYTE   CHARACTER   double byte code page
   IXFHFIL1       02-BYTE   CHARACTER   reserved
|#

(defvar *ixf-header*
  (list (make-field :IXFHRECL  06 'integer)
        (make-field :IXFHRECT  01 'character)
        (make-field :IXFHID    03 'string)
        (make-field :IXFHVERS  04 'string)
        (make-field :IXFHPROD  12 'string)
        (make-field :IXFHDATE  08 'string)
        (make-field :IXFHTIME  06 'string)
        (make-field :IXFHHCNT  05 'integer)
        (make-field :IXFHSBCP  05 'string)
        (make-field :IXFHDBCP  05 'string)
        (make-field :IXFHFIL1  02 'string))
  "Definition of the IXF Header record.")

#|
TABLE RECORD

   FIELD NAME     LENGTH     TYPE        COMMENTS
   ----------     -------    ---------   -------------
   IXFTRECL       006-BYTE   CHARACTER   record length
   IXFTRECT       001-BYTE   CHARACTER   record type = 'T'
   IXFTNAML       003-BYTE   CHARACTER   name length
   IXFTNAME       256-BYTE   CHARACTER   name of data
   IXFTQULL       003-BYTE   CHARACTER   qualifier length
   IXFTQUAL       256-BYTE   CHARACTER   qualifier
   IXFTSRC        012-BYTE   CHARACTER   data source
   IXFTDATA       001-BYTE   CHARACTER   data convention = 'C'
   IXFTFORM       001-BYTE   CHARACTER   data format = 'M'
   IXFTMFRM       005-BYTE   CHARACTER   machine format = 'PC'
   IXFTLOC        001-BYTE   CHARACTER   data location = 'I'
   IXFTCCNT       005-BYTE   CHARACTER   'C' record count
   IXFTFIL1       002-BYTE   CHARACTER   reserved
   IXFTDESC       030-BYTE   CHARACTER   data description
   IXFTPKNM       257-BYTE   CHARACTER   primary key name
   IXFTDSPC       257-BYTE   CHARACTER   reserved
   IXFTISPC       257-BYTE   CHARACTER   reserved
   IXFTLSPC       257-BYTE   CHARACTER   reserved
|#

(defvar *ixf-table*
  (list (make-field :IXFTRECL   006   'integer)
        (make-field :IXFTRECT   001   'character)
        (make-field :IXFTNAML   003   'integer)
        (make-field :IXFTNAME   256   'string)
        (make-field :IXFTQULL   003   'integer)
        (make-field :IXFTQUAL   256   'string)
        (make-field :IXFTSRC    012   'string)
        (make-field :IXFTDATA   001   'character)
        (make-field :IXFTFORM   001   'character)
        (make-field :IXFTMFRM   005   'string)
        (make-field :IXFTLOC    001   'character)
        (make-field :IXFTCCNT   005   'integer)
        (make-field :IXFTFIL1   002   'string)
        (make-field :IXFTDESC   030   'string)
        (make-field :IXFTPKNM   257   'string)
        (make-field :IXFTDSPC   257   'string)
        (make-field :IXFTISPC   257   'string)
        (make-field :IXFTLSPC   257   'string))
  "Definition of the IXF Table record.")

#|
COLUMN DESCRIPTOR RECORD

   FIELD NAME     LENGTH     TYPE        COMMENTS
   ----------     -------    ---------   -------------
   IXFCRECL       006-BYTE   CHARACTER   record length
   IXFCRECT       001-BYTE   CHARACTER   record type = 'C'
   IXFCNAML       003-BYTE   CHARACTER   column name length
   IXFCNAME       256-BYTE   CHARACTER   column name
   IXFCNULL       001-BYTE   CHARACTER   column allows nulls
   IXFCDEF        001-BYTE   CHARACTER   column has defaults
   IXFCSLCT       001-BYTE   CHARACTER   column selected flag
   IXFCKPOS       002-BYTE   CHARACTER   position in primary key
   IXFCCLAS       001-BYTE   CHARACTER   data class
   IXFCTYPE       003-BYTE   CHARACTER   data type
   IXFCSBCP       005-BYTE   CHARACTER   single byte code page
   IXFCDBCP       005-BYTE   CHARACTER   double byte code page
   IXFCLENG       005-BYTE   CHARACTER   column data length
   IXFCDRID       003-BYTE   CHARACTER   'D' record identifier
   IXFCPOSN       006-BYTE   CHARACTER   column position
   IXFCDESC       030-BYTE   CHARACTER   column description
   IXFCLOBL       020-BYTE   CHARACTER   lob column length
   IXFCUDTL       003-BYTE   CHARACTER   UDT name length
   IXFCUDTN       256-BYTE   CHARACTER   UDT name
   IXFCDEFL       003-BYTE   CHARACTER   default value length
   IXFCDEFV       254-BYTE   CHARACTER   default value
   IXFCREF        001-BYTE   CHARACTER   reference type
   IXFCNDIM       002-BYTE   CHARACTER   number of dimensions
   IXFCDSIZ       varying    CHARACTER   size of each dimension
|#

(defvar *ixf-column*
  (list (make-field :IXFCRECL    006  'integer)
        (make-field :IXFCRECT    001  'character)
        (make-field :IXFCNAML    003  'integer)
        (make-field :IXFCNAME    256  'string)
        (make-field :IXFCNULL    001  'character)
        (make-field :IXFCDEF     001  'character)
        (make-field :IXFCSLCT    001  'character)
        (make-field :IXFCKPOS    002  'integer)
        (make-field :IXFCCLAS    001  'character)
        (make-field :IXFCTYPE    003  'integer)
        (make-field :IXFCSBCP    005  'string)
        (make-field :IXFCDBCP    005  'string)
        (make-field :IXFCLENG    005  'integer)
        (make-field :IXFCDRID    003  'integer)
        (make-field :IXFCPOSN    006  'integer)
        (make-field :IXFCDESC    030  'string)
        (make-field :IXFCLOBL    020  'integer)
        (make-field :IXFCUDTL    003  'integer)
        (make-field :IXFCUDTN    256  'string)
        (make-field :IXFCDEFL    003  'integer)
        (make-field :IXFCDEFV    254  'string)
        (make-field :IXFCREF     001  'character)
        (make-field :IXFCNDIM    002  'integer)
        (make-field :IXFCDSIZ    nil nil)))

#|
DATA RECORD

   FIELD NAME     LENGTH    TYPE        COMMENTS
   ----------     -------   ---------   -------------
   IXFDRECL       06-BYTE   CHARACTER   record length
   IXFDRECT       01-BYTE   CHARACTER   record type = 'D'
   IXFDRID        03-BYTE   CHARACTER   'D' record identifier
   IXFDFIL1       04-BYTE   CHARACTER   reserved
   IXFDCOLS       varying   variable    columnar data
|#

(defvar *ixf-data*
  (list
   (make-field :IXFDRECL    06  'integer)
   (make-field :IXFDRECT    01  'character)
   (make-field :IXFDRID     03  'integer)
   (make-field :IXFDFIL1    04  'string)
   (make-field :IXFDCOLS    nil nil))
  "Definition of the IXF Data record.")

#|
APPLICATION RECORD

   FIELD NAME     LENGTH    TYPE        COMMENTS
   ----------     -------   ---------   -------------
   IXFARECL       06-BYTE   CHARACTER   record length
   IXFARECT       01-BYTE   CHARACTER   record type = 'A'
   IXFAPPID       12-BYTE   CHARACTER   application identifier
   IXFADATA       varying   variable    application-specific data
|#

(defvar *ixf-application*
  (list
   (make-field :IXFDRECL    06  'integer)
   (make-field :IXFDRECT    01  'character)
   (make-field :IXFAPPID    12  'string)
   (make-field :IXFADATA    nil nil))
  "Definition of the IXF Application record.")


;;
;; Now read the bytes and give them required meaning
;;
(defvar *record-types* `((#\H . ,*ixf-header*)
                         (#\T . ,*ixf-table*)
                         (#\C . ,*ixf-column*)
                         (#\D . ,*ixf-data*)
                         (#\A . ,*ixf-application*))
  "All expected record types.")

(defun read-integer (stream size)
  "Read a character encoded integer of SIZE from binary STREAM."
  (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
    (read-sequence bytes stream)
    (parse-integer (map 'string #'code-char bytes) :junk-allowed t)))

(defun read-character (stream)
  "Read a single character from the binary STREAM."
  (code-char (read-byte stream)))

(defun read-ascii-string (stream size)
  "Read an ascii string of SIZE characters from STREAM."
  (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
    (read-sequence bytes stream)
    (string-trim '(#\Nul) (map 'string #'code-char bytes))))

(defun read-binary-data (stream size)
  "Read a bunch of SIZE bytes in STREAM."
  (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
    (read-sequence bytes stream)
    bytes))

(defun read-field (stream field start length)
  "Read the next bytes of STREAM according to field definition."
  (declare (type ixf-field field))

  (if (field-size field)
      (ecase (field-type field)
        (integer   (read-integer stream (field-size field)))
        (character (read-character stream))
        (string    (read-ascii-string stream (field-size field))))

      ;; varying field, read the rest of the column
      (read-binary-data stream (- (+ length 6) (- (file-position stream) start)))))

(defun read-record (stream record-definition start length)
  "Read the next bytes of STREAM according to record definition"
  (loop :for field :in record-definition
     :collect (cons (field-name field)
                    (read-field stream field start length))))

(defun read-next-record (stream)
  "Discover next record length and type, then read it."
  (let* ((start             (file-position stream))
         (length            (read-integer stream 6))
         (record-type       (read-character stream))
         (record-definition (cdr (assoc record-type *record-types*))))

    (unless record-definition
      (error "Unknown record-type ~s found at position ~s." record-type start))

    (prog1
        (append (list (cons :type record-type))
                ;; (list (cons :start start))
                ;; (list (cons :length length))
                (read-record stream (cddr record-definition) start length))

      ;; ensure we skip any unread data that pertains to that record.
      (file-position stream (+ start length 6)))))

(defun header-record-p (record) (char= #\H (get-record-property :type record)))
(defun table-record-p  (record) (char= #\T (get-record-property :type record)))
(defun column-record-p (record) (char= #\C (get-record-property :type record)))
(defun data-record-p   (record) (char= #\D (get-record-property :type record)))

(defun get-record-property (property record)
  "Return the property value for PROPERTY (a symbol) as found in RECORD."
  (cdr (assoc property record)))

(defun check-record (record)
  "Given a record, do some basic validity checking."
  (let ((record-type (get-record-property :type record)))
    (case record-type
      (#\H  (assert (string= "0002" (get-record-property :IXFHVERS record))))

      (#\T  (assert (char= #\C      (get-record-property :IXFTDATA record)))
            (assert (char= #\M      (get-record-property :IXFTFORM record)))
            (assert (string= "PC"   (get-record-property :IXFTMFRM record)))
            (assert (char= #\I      (get-record-property :IXFTLOC record))))

      (#\C  (assert (member (get-record-property :IXFCNULL record) '(#\Y #\N)))
            (assert (member (get-record-property :IXFCDEF record)  '(#\Y #\N)))
            (assert (member (get-record-property :IXFCREF record)  '(#\D #\R)))
            (assert (= (get-record-property :IXFCNDIM record) 0))))))

(defun validate-file (filename)
  "Validate that we can read FILENAME as an IXF file."
  (with-open-file (s filename :element-type '(unsigned-byte 8))
    (let ((length (file-length s)))
      (loop :while (< (file-position s) length)
         :do (check-record (read-next-record s))))))

(defun collect-records (filename)
  "Validate that we can read FILENAME as an IXF file."
  (with-open-file (s filename :element-type '(unsigned-byte 8))
    (let ((length (file-length s)))
      (loop :while (< (file-position s) length)
         :collect (read-next-record s)))))
