(in-package #:common-lisp-user)

(defpackage #:metabang-dynamic-classes
  (:nicknames #:dynamic-classes)
  (:use #:common-lisp)
  (:import-from #:metatilities
		#:ensure-list
		#:length-1-list-p
		#:class-precedence-list
		#:class-direct-subclasses
		#:get-class
		#:finalize-class-if-necessary
		#:muffle-redefinition-warnings)  
  (:export
   #:existing-subclass
   #:include-class-dependencies
   #:add-parameter->dynamic-class
   #:determine-dynamic-class
   ;;??
   #+(or)
   #:add-dynamic-class-for-parameters
   #:remove-parameter->dynamic-class
   #:empty-add-parameter->dynamic-class
   #:empty-all-add-parameter->dynamic-class
   #:parameter->dynamic-class
   #:find-existing-subclass
   #:find-or-create-class))

