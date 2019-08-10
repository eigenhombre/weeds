(defpackage cl-blog.util
  (:use :common-lisp)
  (:export :slurp
           :test=
           :dotests
           :strcat
           :spit
           :slurp
           :comment))

(in-package :cl-blog.util)

(defmacro comment (&rest body))

(defun test= (a b)
  (assert (equal a b)))

(defun slurp (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defun spit (name s)
  (with-open-file (stream name
                          :direction :output
                          :if-exists :supersede)
    (write-string s stream)))

(defun strcat (&rest l)
  (format nil "~{~a~}"
          (loop for x in l
             when x
             collect x)))

(defmacro dotests (&rest body)
  `(progn ,@body))

(dotests
 (test= (strcat) "")
 (test= (strcat :a) "A")
 (test= (strcat 1 2 3) "123"))
