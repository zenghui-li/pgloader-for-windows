
(defpackage #:py-configparser-tests
	(:use #:cl #:py-configparser #:rt))

(in-package :py-configparser-tests)

;; test 1
;; should succeed
(deftest basic.parser
  (typep (with-input-from-string (s "[n]
p=q
z=%(p)s
")
  (read-stream (make-config) s)) 'config)
  T)

(deftest basic.get-option.1
 (with-input-from-string (s "[n]
p=q
z=%(p)s
 and some more
")
  (equal (get-option (read-stream (make-config) s) "n" "z")
         "q and some more"))
  T)

(deftest basic.get-option.2
 (with-input-from-string (s "[n]
p=q
delta=%(gamma)s
z=%(p)s
 and some more
")
  (equal (get-option (read-stream (make-config) s) "n" "delta" :defaults '(("gamma" . "the gamma value")))
         "the gamma value"))
  T)

(deftest basic.get-option.3
 (with-input-from-string (s "[n]
p=15
delta=%(gamma)s
z=%(p)s
 and some more
")
  (equal (get-option (read-stream (make-config) s) "n" "p" :type :number)
         15))
  T)

(deftest basic.get-option.4
 (with-input-from-string (s "[n]
p=yes
delta=%(gamma)s
z=%(p)s
 and some more
")
  (equal (get-option (read-stream (make-config) s) "n" "p" :type :boolean)
         T))
  T)

(deftest basic.get-option.5
 (with-input-from-string (s "[n]
p=q
delta=%(gamma)s
z=%(p)s
 and some more

[DEFAULT]
gamma=the gamma value
")
  (equal (get-option (read-stream (make-config) s) "n" "delta")
         "the gamma value"))
  T)


(deftest basic.sections
         (with-input-from-string (s "[n] post-section header gunk ignored
p=q
z=%(p)s
")
  (equal (sections (read-stream (make-config) s))
         '("n")))
         T)

(deftest basic.comments-only
         (typep (with-input-from-string (s "#comments only
")
                  (read-stream (make-config) s)) 'config)
         T)

(deftest basic.no-newline
         (typep (with-input-from-string (s "#comments without trailing \#Newline")
                  (read-stream (make-config) s))
                'config)
         T)

(deftest basic.with-defaults
         (equal (with-input-from-string (s "[DEFAULT]
def-option = options without trailing newline")
                  (get-option (read-stream (make-config) s) "DEFAULT" "def-option"))
                "options without trailing newline")
         T)

;; newlines only
(deftest basic.newlines-only
         (with-input-from-string (s "


")
           (typep (read-stream (make-config) s)
                 'config))
         T)

;; empty lines only
(deftest basic.empty-lines-only
         (with-input-from-string (s "
# The next two lines intentionally contain spaces-only
  
      
")
           (typep (read-stream (make-config) s)
                 'config))
         T)


;; options
(deftest basic.options
  (equal (with-input-from-string (s "[n]
p=q
z=%(p)s
")
           (options (read-stream (make-config) s) "n")) '("z" "p"))
  T)

;; items
(deftest basic.items.1
         (equal (with-input-from-string (s "[n]
p=q
z=%(p)s
")
           (items (read-stream (make-config) s) "n" :expand nil))
           '(("z" . "%(p)s")
             ("p" . "q")))
  T)

(deftest basic.items.2
         (equal (with-input-from-string (s "[n]
p=q
z=%(p)s
")
                  (items (read-stream (make-config) s) "n" :expand t))
                '(("z" . "q") ("p" . "q")))
  T)

(deftest basic.items.3
         (equal (with-input-from-string (s "[n]
p=q
delta=%(gamma)s
z=%(p)s
")
           (items (read-stream (make-config) s) "n" :expand t
                                                    :defaults '(("gamma" . "the gamma"))))
           '(("z" . "q") ("delta" . "the gamma") ("p" . "q")))
  T)


;; sections
(deftest basic.sections.1
         (equal (with-input-from-string (s "[n]
p=q
z=%(p)s

[v]
[t]
")
           (sections (read-stream (make-config) s))) '("t" "v" "n"))
  T)

(deftest basic.sections.2
         (equal (with-input-from-string (s "[n]
p=q
z=%(p)s

[v]
[t]

[DEFAULT]
p=t
")
           (sections (read-stream (make-config) s))) '("t" "v" "n"))
  T)

;; add-section
(deftest basic.add-section
         (with-input-from-string (s "[n]
p=q
z=%(p)s

[t]

")
           (let ((c (read-stream (make-config) s)))
             (unless (has-section-p c "v")
               (add-section c "v")
               (not (null (has-section-p c "v"))))))
  T)

;; set-option
(deftest basic.set-option.1
         (with-input-from-string (s "[n]
p=q
z=%(p)s

[t]

")
           (let ((c (read-stream (make-config) s)))
             (unless (has-option-p c "t" "b")
               (set-option c "t" "b" "ok")
               (equal (get-option c "t" "b") "ok"))))
  T)

(deftest basic.set-option.2
         (with-input-from-string (s "[n]
p=q
z=%(p)s

[t]

")
           (let ((c (read-stream (make-config) s)))
             (set-option c "n" "p" "ok")
             (equal (get-option c "n" "p") "ok")))
  T)

;; remove-option
(deftest basic.remove-option
         (with-input-from-string (s "[n]
p=q
z=%(p)s

[t]

")
           (let ((c (read-stream (make-config) s)))
             (when (has-option-p c "n" "p")
               (remove-option c "n" "p")
               (null (has-option-p c "n" "p")))))
  T)

;; remove-section
(deftest basic.remove-section
         (with-input-from-string (s "[n]
p=q
z=%(p)s

[t]

")
           (let ((c (read-stream (make-config) s)))
             (when (has-section-p c "t")
               (remove-section c "t")
               (null (has-section-p c "t")))))
  T)



;; now the tests that fail
(deftest failures.no-header
         (with-input-from-string (s "option-before = section
[header]")
           (handler-case
               (progn
                 (read-stream (make-config) s)
                 nil)
             (missing-section-header-error () T)))
         T)

(deftest failures.no-spaced-option-names
         (with-input-from-string (s "[n]
option with space = not allowed
")
           (handler-case
               (progn
                 (read-stream (make-config) s)
                 nil)
             (parsing-error () T)))
         T)

(deftest failures.recursion
         (with-input-from-string (s "[n]
p=%(z)s
z=%(p)s
")
           (handler-case
               (get-option (read-stream (make-config) s)
                           "n" ;; section
                           "p" ;; option
                           :expand t)
             (interpolation-depth-error () T)))
         T)

;; non-erroring non-parsing tests
(deftest miscelaneous
         (with-input-from-string (s "[n]
p=%(__name__)s
q=%(z)s
z=hello
")
           (let ((p (read-stream (make-config) s)))
             (unless (string= (get-option p "n" "p" :expand t) "n")
               (error "Unexpected output"))
             (unless (string= (get-option p "n" "q" :expand nil) "%(z)s")
               (error "Unexpected output"))
             (unless (string= (get-option p "n" "q" :expand t) "hello")
               (error "Unexpected output"))
             (unless (string= (get-option p "n" "z") "hello")
               (error "Unexpected output"))
             NIL))
         NIL)