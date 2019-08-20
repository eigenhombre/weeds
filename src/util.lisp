(defpackage weeds.util
  (:use :common-lisp)
  (:export :basename
           :comment
           :dotests
           :drop
           :macos-open-file
           :hash-keys
           :interpose
           :massoc
           :mht
           :range
           :sorted
           :spit
           :strcat
           :slurp
           :take
           :test=))

(in-package :weeds.util)

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(defmacro comment (&rest body))  ;; muffle warning on unused body
(declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))

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

(defun interpose (sep coll)
  (cdr (loop for x in coll append (list sep x))))

(dotests
 (test= (interpose :sep nil) nil)
 (test= (interpose :sep '(1)) '(1))
 (test= (interpose :sep '(1 2 3)) '(1 :SEP 2 :SEP 3)))
