(defpackage weeds.util
  (:use :common-lisp :trivialtests :cl-ppcre)
  (:export :basename
           :comment
           :macos-open-file
           :hash-keys
           :massoc
           :mht
           :sorted
           :strcat))

(in-package :weeds.util)

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(defmacro comment (&rest body))  ;; muffle warning on unused body
(declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))

(defun strcat (&rest l)
  (format nil "~{~a~}"
          (loop for x in l
             when x
             collect x)))

(dotests
 (test= (strcat) "")
 (test= (strcat :a) "A")
 (test= (strcat 1 2 3) "123"))

(defun basename (file-name)
  (car (cl-ppcre:split "\\." file-name)))

;;; Misc unused stuff, delete it if I don't use it soon:
(defun hash-keys (m)
  (loop for k being the hash-keys of m collect k))

(defun mht () (make-hash-table :test #'equal))

(defun massoc (m k v)
  (setf (gethash k m) v)
  m)

(defun sorted (l)
  "
  Get a sorted list without changing the original. Hack, providing
  something like Clojure's ordering semantics in some cases at least.
  "
  (sort (copy-seq l) #'(lambda (a b) (string< (format nil "~a" a)
                                              (format nil "~a" b)))))

(defun macos-open-file (filename)
  (sb-ext:run-program "/usr/bin/open"
                      (list filename)
                      :input nil :output *standard-output*))

(defun curry (function &rest args)
  "
  http://cl-cookbook.sourceforge.net/functions.html#curry
  "
  (lambda (&rest more-args)
    (apply function (append args more-args))))

(test= (funcall (curry #'+ 3) 5)
       8)

(test= (funcall (curry #'+ 3 5) 5 9)
       22)

(defun getenv (name &optional default)
  ;; From http://cl-cookbook.sourceforge.net/os.html
  #+CMU
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+Allegro (sys:getenv name)
   #+CLISP (ext:getenv name)
   #+ECL (si:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   default))
