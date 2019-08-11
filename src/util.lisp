(defpackage cl-blog.util
  (:use :common-lisp)
  (:export :basename
           :comment
           :dotests
           :drop
           :hash-keys
           :massoc
           :mht
           :range
           :sorted
           :spit
           :strcat
           :slurp
           :take
           :test=))

(in-package :cl-blog.util)

(defmacro comment (&rest _))

(defun test= (a b)
  (assert (equal a b)))

(defmacro dotests (&rest body)
  `(progn ,@body))

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

(dotests
 (test= (strcat) "")
 (test= (strcat :a) "A")
 (test= (strcat 1 2 3) "123"))

(defun rand-nth (l)
  (nth (random (length l)) l))

(defun basename (file-name)
  (car (cl-ppcre:split "\\." file-name)))

(defun range (n)
  (loop for x upto (1- n) collect x))

(defun take (n l)
  (loop for x in l repeat n collect x))

(defun drop (n l)
  (nthcdr n l))

(dotests
 (test= (drop 3 (range 10))
        '(3 4 5 6 7 8 9)))


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
