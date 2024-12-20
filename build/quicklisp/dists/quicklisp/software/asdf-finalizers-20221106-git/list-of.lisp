#+xcvb (module (:depends-on ("initialization")))

(defpackage :list-of
  (:use :cl :asdf-finalizers)
  (:export
   #:list-of
   #:vector-of))

(in-package :list-of)

(defun sequence-of-predicate-for (type &optional (sequence-type 'list))
  (with-standard-io-syntax
    (let ((*package* (find-package :list-of)))
      (intern (format nil "~S-OF-~S-P" sequence-type type) :list-of))))

(defun list-of-predicate-for (type)
  (sequence-of-predicate-for type 'list))

(defun vector-of-predicate-for (type)
  (sequence-of-predicate-for type 'vector))

(defun list-of-type-predicate (type)
  #'(lambda (x)
      (loop :for c = x :then (cdr c) :while (consp c) :always (typep (car c) type)
	    :finally (return (null c)))))

(defun vector-of-type-predicate (type)
  #'(lambda (x)
      (and (typep x 'vector)
           (every #'(lambda (e) (typep e type)) x))))

(defun ensure-list-of-predicate (type &optional predicate)
  (unless predicate
    (setf predicate (list-of-predicate-for type)))
  (check-type predicate symbol)
  (unless (fboundp predicate)
    (setf (symbol-function predicate) (list-of-type-predicate type)))
  nil)

(defun ensure-vector-of-predicate (type &optional predicate)
  (unless predicate
    (setf predicate (vector-of-predicate-for type)))
  (check-type predicate symbol)
  (unless (fboundp predicate)
    (setf (symbol-function predicate) (vector-of-type-predicate type)))
  nil)

(deftype list-of (type)
  (case type
    ((t) 'list) ;; a (list-of t) is the same as a regular list.
    ((nil) 'null) ;; a (list-of nil) can have no elements, it's null.
    (otherwise
     (let ((predicate (list-of-predicate-for type)))
       (eval-at-toplevel ;; now, and amongst final-forms if enabled
	`(ensure-list-of-predicate ',type ',predicate)
	`(fboundp ',predicate) ;; hush unnecessary eval-at-toplevel warnings
	"Defining ~S outside of finalized Lisp code" `(list-of ,type))
       `(and list (satisfies ,predicate))))))

(deftype vector-of (type)
  (let ((spec-type (upgraded-array-element-type type)))
    (if (equal type spec-type)
        `(vector ,spec-type)
        (let ((predicate (vector-of-predicate-for type)))
          (eval-at-toplevel ;; now, and amongst final-forms if enabled
           `(ensure-vector-of-predicate ',type ',predicate)
           `(fboundp ',predicate) ;; hush unnecessary eval-at-toplevel warnings
           "Defining ~S outside of finalized Lisp code" `(vector-of ,type))
          `(and (vector ,spec-type) (satisfies ,predicate))))))

;; These are available in case you prefer to explicitly call declare-list-of and
;; declare-vector-of in your code-base rather than rely on finalizers.
;; They are not exported because we do not encourage it, but you can import them.
(defmacro declare-list-of (type)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (ensure-list-of-predicate ',type)))

(defmacro declare-vector-of (type)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (ensure-vector-of-predicate ',type)))
