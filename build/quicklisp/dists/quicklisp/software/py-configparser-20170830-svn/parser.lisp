
(cl:in-package #:py-configparser)

(declaim (special *line-no* *current-section* *file-name*
                  *current-input*))

;; Errors for the parsing side

(define-condition parsing-error (configparser-error)
   ((line-no :initarg :line-no :initform *line-no* :reader line)
    (file :initarg :file :initform *file-name* :reader file)
    (section :initarg :section :initform *current-section* :reader section)
    (message :initarg :text :reader message))
   (:report (lambda (c stream)
               (format stream "~A at line ~A" (message c) (line c)))))
(define-condition missing-section-header-error (parsing-error) ())



;; The reader

(declaim (inline %read-char %unread-char))

(defun %read-char (stream)
  (let ((ch (read-char stream nil :eof)))
    (when (eql ch #\Newline)
      (incf *line-no*))
    (if (eq ch :eof) #\Newline ch)))

(defun ensure-section (config section-name)
  (handler-case
      (%get-section config section-name)
    (no-section-error ()
      (add-section config section-name))))

(defun is-whitespace (c)
  (or (eq c #\Space)
      (eq c #\Tab)
      (eq c #\Return)))

(defun is-comment-char (c)
  (or (eq c #\;)
      (eq c #\#)))

(defun skip-whitespace (s)
  (loop for c = (%read-char s)
        while (is-whitespace c)))

(defun skip-empty-line (s)
  (loop for c = (%read-char s)
        if (eq c #\Newline) do (return)
        else unless (is-whitespace c)
        do (error 'parsing-error
			:text "Non-empty line found where empty expected."))) ;; empty line expected

(defun skip-to-eol (s)
  (loop for c = (%read-char s)
        until (eq c #\Newline)))

(defun expect-char (s expect &key skip-whitespace)
  (let ((ch (%read-char s)))
    (when (and skip-whitespace
               (is-whitespace ch))
      (loop for c = (%read-char s)
            while (is-whitespace c)
            finally (setf ch c)))
    (unless (eq ch expect)
      (error 'parsing-error
		 :text (format nil "Character ~A expected, but ~A found instead."
				       expect ch))) ;; character expect expected, but ch found
    ch))

(defun expect-one-of (s expect-bag &key skip-whitespace)
  (let ((ch (%read-char s)))
    (when (and skip-whitespace
               (is-whitespace ch))
      (loop for c = (%read-char s)
            while (is-whitespace c)
            finally (setf ch c)))
    (unless (member ch expect-bag)
      ;; character ch found, but looking for EXPECT-BAG
      (error 'parsing-error
             :text (format nil "Character ~A found, but one of ~A expected."
					 ch expect-bag)))
      ch))

(defun make-input-buffer (p)
  (declare (ignore p))
  (make-array 20 :element-type 'cl:character :fill-pointer 0
              :adjustable t))

(declaim (inline extend-input))
(defun extend-input (p c)
  (vector-push-extend c *current-input* 20))

(defun finalize-input (p)
  (let ((cp *current-input*))
    (setf *current-input*
          (make-input-buffer p))
    cp))

(defun read-section-name (p s)
  (expect-char s #\[)
  (loop for c = (%read-char s)
        if (eq c #\Newline)
        do (error 'parsing-error
			:text "Premature end of line, or end of line in section name.")
			;; we can't have newlines in section names!
        else if (eq c #\])
        do (progn
             (skip-to-eol s)
             (return (finalize-input p)))
        else do (extend-input p c)))

(defun read-option-name (p s)
  (loop for c = (%read-char s)
        if (or (eq c #\:)
               (eq c #\=))
        do (let ((option-name (finalize-input p)))
             (when (= 0 (length option-name))
               (error 'parsing-error
			    :text "No option name found.")) ;; No option name found
             (return option-name))
        else if (is-whitespace c)
        do (unread-char (expect-one-of s '(#\: #\=) :skip-whitespace t) s)
        else do (extend-input p c)))
        
(defun read-option-value (p s &key (leading-white :skip))
  (let ((leading-mode t)
        (lead-detected nil))
    (loop for c = (%read-char s)
          unless (or (eql c #\Return)
                     (eql c #\Newline))
          do (if (and leading-mode
                      (is-whitespace c))
                 (setf lead-detected t)
                 (progn
                   (when (and (eq leading-white :fold)
                              leading-mode
                              lead-detected)
                     (extend-input p #\Space))
                   (setf leading-mode nil)
                   (extend-input p c)))
          
          if (and (eql c #\Newline)
                  (let ((ch (peek-char nil s nil nil)))
                    (or (eql ch #\Space)
                        (eql ch #\Tab))))
          do (return (read-option-value p s :leading-white :fold))
          until (eql c #\Newline)
          finally (return (finalize-input p)))))

(defun reading-driver (p s)
  (let ((*line-no* 0)
        (*current-section* nil)
        (*current-input* (make-input-buffer p)))
    (loop for c = (peek-char nil s nil :eof)
          until (eq c :eof)
          if (eql c #\[)
          do (setf *current-section*
                   (section-name (ensure-section p (read-section-name p s))))

          else if (is-whitespace c)
          do (skip-empty-line s)

          else if (is-comment-char c)
          do (skip-to-eol s)

          else if (eql c #\Newline)
          do (%read-char s) ;; skip over the newline character

          else do (if (null *current-section*)
                      (error 'missing-section-header-error
				     :text (format nil "Missing section header; found ~A instead." c))
                      (set-option p
                              *current-section*
                              (read-option-name p s)
                              (read-option-value p s))))))

;;
;; The API
;;

(defun read-files (config filenames)
  "Parses the files given in the list `filenames', if they exist.
The list is processed first to last, overwriting any pre-existing
values with the last value read.

The results are stored in `config' which is modified destructively.

Returns as values the configuration and the list of files actually read."
  (let (files-read)
    (dolist (filename (remove-if-not #'probe-file filenames)
             (values config files-read))
      (with-open-file (s filename
                         :direction :input
                         :if-does-not-exist :error)
        (read-stream config s :stream-name filename))
      (push filename files-read))))

(defun read-stream (config stream &key (stream-name "an unknown stream"))
  "Parses the content of `stream' as a configuration file,
storing any values in `config' which is modified destructively.

This function maps from the python 'readfp()' function."
  (let ((*file-name* stream-name))
    (reading-driver config stream)
    config))

(defun %format-value (value)
  (if (and (numberp value)
           (not (integerp value)))
      (format nil "~,,,,,,'eE" value)
      value))

(defun write-stream (config stream)
  "Writes the configuration file corresponding to the
in-memory config state. Reloading the file
with `read-stream' or `read-files' will restore config state."
  (flet ((write-section (section)
           (format stream "[~a]~%" (section-name section))
           (format stream "~:{~A = ~{~A~%~}~}~%"
                   (mapcar #'(lambda (option)
                               (list (car option)
                                     (list (%format-value (cdr option)))))
                           (section-options section)))))
    (let ((*print-radix* nil)
          (*print-base* 10))
      ;; set the printer output as expected by python
      (when (defaults config)
        ;; write the defaults too!!
        (write-section (config-defaults config)))
      (mapcar #'write-section (config-sections config)))))

