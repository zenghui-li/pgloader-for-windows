
(cl:in-package :py-configparser)

;; The conditions (errors)

(define-condition configparser-error (error) ())

;; Errors for the configuration management side
(define-condition config-error (configparser-error) ())
(define-condition no-section-error (config-error) ())
(define-condition duplicate-section-error (config-error) ())
(define-condition no-option-error (config-error) ())
(define-condition interpolation-error (config-error) ())
(define-condition interpolation-depth-error (interpolation-error) ())
(define-condition interpolation-missing-option-error (interpolation-error) ())
(define-condition interpolation-syntax-error (interpolation-error) ())


;;
;; Configuration storage and management routines
;;


;; The structures
;;   Note: because ABCL has issues with its CLOS support
;;         (as per 1-1-2008), we use structures below to
;;         be maximally portable.


(defstruct section
  name
  options)

(defstruct config
  (defaults (make-section :name "DEFAULT"))
  sections
  (option-name-transform-fn #'string-downcase)
  (section-name-transform-fn #'identity))

(defun norm-option-name (config option-name)
  (funcall (config-option-name-transform-fn config) option-name))

(defun norm-section-name (config section-name)
  (funcall (config-section-name-transform-fn config) section-name))

(defun %validate-section-name (name)
  (when (or (= 0 (length name))
            (find #\] name)
            (find #\Newline name)
            (find #\Return name))
    (error 'no-section-error)) ;; Invalid section name, signal so.
  name)

(defun %validate-option-name (name)
  (when (or (= 0 (length name))
            (eql (aref name 0) #\[)
            (find #\Space name)
            (find #\Tab name)
            (find #\Return name)
            (find #\Newline name))
    (error 'no-option-error));; No such option error
  name)

;; non-API
(defun %get-section (config section-name)
  (if (string= "DEFAULT" section-name)
      (config-defaults config)
      (let* ((norm-section-name (norm-section-name config section-name))
             (section (find norm-section-name (config-sections config)
                            :key #'section-name
                            :test #'string=)))
        (unless section
          (error 'no-section-error)) ;; no-such-section error
        section)))

;; non-API
(defun %get-option (config section-name option-name if-does-not-exist)
  (let* ((section (%get-section config section-name))
         (norm-option (norm-option-name config option-name))
         (option (or (assoc norm-option
                            (section-options section)
                            :test #'string=)
                     (assoc norm-option
                            (section-options (config-defaults config))
                            :test #'string=))))
    (if (null option)
        (if (eq if-does-not-exist :error)
            (error 'no-option-error) ;; no such option error
            (values (car (push (list (%validate-option-name norm-option))
                               (section-options section)))
                    section))
        (values option section))))

;;
;; The API
;;

(defun defaults (config)
  "Returns an alist containing instance wide defaults, where the
elements are 2-element dotted lists: the CDR is the value
associated with the key."
  (section-options (config-defaults config)))

(defun sections (config)
  "Returns a list of names of defined sections."
  (mapcar #'section-name (config-sections config)))

(defun has-section-p (config section-name)
  "Returns `NIL' when the section is not added to the config yet,
some other value if it is."
  (handler-case
      (%get-section config section-name)
    (no-section-error () nil)))

(defun add-section (config section-name)
  "Adds a new section to the config.

If the section exists, the `duplicate-section-error' is raised."
  (%validate-section-name section-name)
  (let ((norm-section-name (funcall (config-section-name-transform-fn config)
                                    section-name)))
    (when (has-section-p config section-name)
      (error 'duplicate-section-error))
    (car (push (make-section :name norm-section-name)
               (config-sections config)))))

(defun options (config section-name)
  "Returns a list of option names which are defined in the given section."
  (let ((section (%get-section config section-name)))
    (mapcar #'first (section-options section))))

(defun has-option-p (config section-name option-name)
  "Returns a generalised boolean with a value of `NIL' when
the specified option does not exist in the specified section
and some other value otherwise."
  (handler-case
      (%get-option config section-name option-name :error)
    (no-option-error () nil)))

;; non-API
(defun %extract-replacement (option-value)
  ;; Returns: (VALUES replacement-option start end) or NIL
  (let ((%-pos (position #\% option-value)))
    (when (and %-pos
             (< (+ 3 %-pos) (length option-value))
             (eql (aref option-value (1+ %-pos)) #\( ))
        (let ((paren-pos (position #\) option-value :start %-pos)))
          (unless (and paren-pos
                       (< (1+ paren-pos) (length option-value))
                       (eql (aref option-value (1+ paren-pos)) #\s))
            (error 'interpolation-syntax-error))
            ;; syntax error: %(..)s is minimally required
          (when (<= 0 (- paren-pos %-pos 2))
            (let ((replacement-name
                   (make-array (- paren-pos %-pos 2)
                               :element-type (array-element-type option-value)
                               :displaced-to option-value
                               :displaced-index-offset (+ 2 %-pos))))
              (when (= 0 (length replacement-name))
                ;; some preconditions on replacement-name
                (error 'interpolation-syntax-error))
              (values replacement-name %-pos (1+ paren-pos))))))))
        
;; non-API
(defun %option-value (config section option-name &key defaults)
  (if (string= option-name "__name__")
      (section-name section)
      (let* ((norm-option-name (norm-option-name config option-name))
             (option (has-option-p config (section-name section) option-name)))
        (if option
          (cdr option)
          (labels ((get-value (repositories)
                     (when (null repositories)
                       (error 'interpolation-missing-option-error))
                     ;; no such option error
                     (let ((value (assoc norm-option-name (car repositories)
                                         :test #'string=)))
                       (if value
                         (cdr value)
                         (get-value (cdr repositories))))))
          (get-value (list (section-options section)
                           defaults
                           (defaults config))))))))

;; non-API
(defun %expand-option-value (config section option-value defaults
                            &optional dependees)
  (multiple-value-bind
        (replacement-name start end)
      (%extract-replacement option-value)
    (unless replacement-name
      ;; nothing to do here...
      (return-from %expand-option-value option-value))

    (let ((norm-replacement (norm-option-name config replacement-name))
          (replacement-value (%option-value config section
                                            replacement-name
                                            :defaults defaults)))
      (when (member norm-replacement dependees :test #'string=)
        (error 'interpolation-depth-error)) ;; recursive dependency...
      (%expand-option-value
       config
       section
       (concatenate 'string
                   (subseq option-value 0 start)
                   (%expand-option-value config
                                         section
                                         replacement-value
                                         defaults
                                         (cons norm-replacement dependees))
                   (subseq option-value (1+ end) (length option-value)))
       defaults
       dependees))))

(defun get-option (config section-name option-name
                   &key (expand t) defaults type)
  "Returns the value of the specified option in the specified section.

If `expand' is `NIL', any options which depend on other options
won't be expanded and the raw configuration value is returned.

When `defaults' is an alist of which the elements are dotted lists of
key/value pairs, these values are used in the expansion of option values.

`type' may be one of `:boolean', `:number' or it may remain unspecified."
  (multiple-value-bind
        (option section)
      (%get-option config section-name option-name :error)
    (flet ((convert-boolean (v)
             (cond
               ((member v '("1" "yes" "true" "on") :test #'string=)
                T)
               ((member v '("0" "no" "false" "off") :test #'string=)
                NIL)
               (t
                (error 'not-a-boolean))))
           (convert-number (v)
             (parse-number:parse-number v)))
      (let ((string-value
             (if expand
                 (%expand-option-value config
                                       section (cdr option)
                                       defaults
                                       (list option-name))
                 (cdr option))))
        (cond
          ((eq type :boolean)
           (convert-boolean string-value))
          ((eq type :number)
           (convert-number string-value))
          ((null type)
           string-value)
          (t
           (error "Illegal `type' parameter value.")))))))

(defun set-option (config section-name option-name value)
  "Sets the value of the specified option in the specified section.

If the section does not exist, a `no-section-error' is raised. If the
option does not exist, it is created."
  (let ((option (%get-option config section-name option-name :create)))
    (setf (cdr option) value)))

(defun items (config section-name &key (expand t) defaults)
  "Returns an alist of which the items are dotted lists of key/value
pairs being the option names and values specified in the given section.

When `expand' is `NIL', options are returned in raw form. Otherwise
option values are expanded.

The definition of `defaults' is the same as for `get-option'."
  (let ((section (%get-section config section-name)))
    (if expand
        (mapcar #'(lambda (x)
                    (cons (car x) (get-option config section-name
                                              (car x) ;; option-name
                                              :expand expand
                                              :defaults defaults)))
                (section-options section))
        (section-options section))))

(defun remove-option (config section-name option-name)
  "Remove the specified option from the given section."
  (multiple-value-bind
        (option section)
      (%get-option config section-name option-name :error)
    (setf (section-options section)
          (remove option (section-options section)))))

(defun remove-section (config section-name)
  "Remove the specified section.

In case the section name equals the magic name `DEFAULT',
an error is raised, since this section can't be removed."
  (when (string= section-name "DEFAULT")
    (error 'no-section-error)) ;; no such section error
  (let ((section (%get-section config section-name)))
    (setf (config-sections config)
          (remove section (config-sections config)))))

