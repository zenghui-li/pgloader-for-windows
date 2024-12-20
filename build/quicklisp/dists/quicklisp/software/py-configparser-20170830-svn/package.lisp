
;; This package is actuall two things:
;;  1) a configuration management utility
;;  2) a configuration file parser/writer in the .INI format
;;
;; But in the Python module this distinction hasn't been implemented
;; this stringently, meaning we're stuck to the current naming scheme.

;; There's no reason however that you can't create your own format
;; and parse that, storing it in the config object as defined in this
;; package. (However, if you already use this module, you might as well
;; use the INI format as persistent format.)


(cl:defpackage #:py-configparser
  (:use #:cl)
  (:export
           ;; common condition class
           #:configparser-error

           ;; configuration storage type
           #:config

           ;; Configuration management
           ;;  Error classes
           #:no-section-error
           #:duplicate-section-error
           #:no-option-error
           #:interpolation-error
           #:interpolation-depth-error
           #:interpolation-missing-option-error
           #:interpolation-syntax-error

           ;;  Functions
           #:make-config
           #:defaults
           #:sections
           #:has-section-p
           #:add-section
           #:options
           #:has-option-p
           #:get-option
           #:set-option
           #:items
           #:remove-option
           #:remove-section

           ;; Configuration file parsing
           ;;  Error classes
           #:parsing-error
           #:missing-section-header-error

           ;;  Functions
           #:read-stream
           #:read-files
           #:write-stream))
           
