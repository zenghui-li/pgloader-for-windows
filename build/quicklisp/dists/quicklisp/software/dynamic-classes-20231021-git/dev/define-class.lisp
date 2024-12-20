(in-package #:metabang-dynamic-classes)

;;; some class defining functions

(defvar *define-class-form* 'metatilities:defclass*
  "The name of the form used to define a class. Usually, this will be bound to 'defclass* but when we are using GBBOpen, it will probably be bound to define-class or define-class*.")

#+test
(setf *define-class-form* 'metatilities:defclass*)

(defun simple-define-class 
    (superclasses 
     &optional (name (simple-define-class-name superclasses)))
  "Define a class on the fly..."
  (cond ((and (length-1-list-p superclasses)
               (find-class (first superclasses) nil))
         (values (first superclasses)))
        (t
	 (muffle-redefinition-warnings
           (eval `(progn
                    (when (find-class ',name nil)
                      (setf (find-class ',name) nil))
                    (defclass* ,name ,(ensure-list superclasses) nil))))
         (values name))))

(defun simple-define-class-name (superclasses &optional (package *package*)) 
  (intern (format nil "~{~a~^-AND-~}" superclasses) package))

(defun define-class (class-name superclasses slots &rest class-options)
  "Define a class with all the bells and whistles on the fly... See 
simple-define-class for the simpler version."
  (muffle-redefinition-warnings
    (eval `(,*define-class-form* 
            ,(or class-name 
                 (setf class-name
                       (simple-define-class-name (ensure-list superclasses))))
             ,(ensure-list superclasses) 
             (,@(ensure-list slots))
             ,@class-options)))
  (values class-name))

(defun map-subclasses (class fn &key proper?)
  "Applies fn to each subclass of class. If proper? is true, then
the class itself is not included in the mapping. Proper? defaults to nil."
  (let ((mapped (make-hash-table :test #'eq)))
    (labels ((mapped-p (class)
               (gethash class mapped))
             (do-it (class root)
               (unless (mapped-p class)
                 (setf (gethash class mapped) t)
                 (unless (and proper? root)
                   (funcall fn class))
                 (mapc (lambda (class)
                         (do-it class nil))
                       (class-direct-subclasses class)))))
      (do-it (get-class class) t))))

(defun superclasses (thing &key (proper? t))
  "Returns a list of superclasses of thing. Thing can be a class, object or symbol naming a class. The list of classes returned is 'proper'; it does not include the class itself."
  (let ((result (class-precedence-list 
		 (finalize-class-if-necessary (get-class thing)))))
    (if proper? (rest result) result)))

(defun find-existing-subclass (superclass superclasses)
  "Look through all the sub-classes of superclass and see if any of them descend
from every class in superclasses."
  (let ((results nil))
    (map-subclasses
     superclass
     (lambda (subclass)
       (let ((last-position -1))
         (when (every (lambda (superclass)
                        (let ((pos
                               (position 
                                superclass (superclasses subclass :proper? nil)
                                :key (lambda (x) (class-name x)))))
                          (prog1
                            (and pos (< last-position pos))
                            (setf last-position pos))))
                      superclasses)
           (push (class-name subclass) results)))))
    (values (first results))))

(defun find-or-create-class (root classes)
  "Try to find a class which is a subclass of root and all of the other `classes` as well. If no such class exists, then it will be created and returned."
  (or (find-existing-subclass root classes)
      (let ((superclasses (remove-redundant-classes classes)))
        (define-class (simple-define-class-name 
		       (remove-redundant-classes superclasses))
          classes nil))))

(defun remove-redundant-classes (classes)
  (loop for class in classes 
        unless (class-redundant-p class classes) collect
        class))

(defun class-redundant-p (class classes)
  (some
   (lambda (other-class)
     (and (not (eq class other-class))
          (subtypep other-class class)))
   classes))

