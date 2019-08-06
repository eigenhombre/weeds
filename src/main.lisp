(defpackage cl-blog
  (:use :cl :arrow-macros :html-parse))
(in-package :cl-blog)

(defun slurp (name)
  "Slurps up file <name> and returns the data as a string.
   https://sourceforge.net/p/cl-cookbook/patches/4/"
  (let ((data nil))
    (with-open-file (file name :direction :input)
      (setf data (make-array (file-length file) :element-type
                             'character
                             :fill-pointer t :adjustable t))
      (setf (fill-pointer data) (read-sequence data file))
      (values data))))

(defun spit (name s)
  (with-open-file (stream name
                          :direction :output
                          :if-exists :supersede)
    (format stream s)))

(defun strcat (&rest l)
  (format nil "~{~a~}"
          (loop for x in l
             when x
             collect x)))

(defun wrap-with-pre (s)
  (strcat "<pre>" s "</pre>"))

(defun preview-file (filename)
  (sb-ext:run-program "/usr/bin/open"
                      (list filename)
                      :input nil :output *standard-output*))

(->> "/Users/jacobsen/Dropbox/org/sites/zerolib.com/bardo.html"
  slurp
  parse-html
  prin1-to-string
  wrap-with-pre
  (spit "/tmp/baz.html"))

;;(preview-file "/tmp/baz.html")
(defmacro comment (&rest body))
(defun translate (l)
  (cond
    ((not l) "")
    ((stringp l) l)
    ((listp (car l)) (qualified-tag l))
    ((symbolp (car l))
     (format nil "<~a>~{~a~}</~a>"
             (car l)
             (mapcar #'translate (cdr l))
             (car l)))
    (t (strcat "<" (car l) ">"
               (mapcan #'translate (cdr l))
               "</" (car l) ">"))))

(defun qualified-tag (l)
  (let* ((tagname (symbol-name (caar l)))
         (kvpairs (cdar l)))
    (format nil "<~a ~{~a~}>~{~a~}</~a>"
            tagname
            (loop for (key value) on kvpairs by #'cddr
               collect (strcat key "=\"" value "\" "))
            (mapcar #'translate (cdr l))
            tagname
            ;; (apply #'strcat
            ;;        `("<"
            ;;          ,tagname
            ;;          " "
            ;;          ,@(loop for (key value) on kvpairs by #'cddr
            ;;               collect (strcat key "=\"" value "\" "))
            ;;          ">"
            ;;          ,@(cdr l)
            ;;          "</"
            ;;          ,tagname
            ;;          ">"))
            )))


;; OK
(translate '(:body))
(translate '(:body (:hr)))
(translate '(:TITLE "Bardo"))
(translate '(:COMMENT " 2018-03-07 Wed 08:16 "))
(translate '((:META :NAME "generator" :CONTENT "Org-mode")))
(translate '((:META :HTTP-EQUIV "Content-Type" :CONTENT "text/html;charset=utf-8")))
(translate '((:STYLE :TYPE "text/css") " "))
(translate '(:body ((:div :id "content"))))
(translate '((:div :id "content")))
(translate '(:body (:hr) (:hr)))
;; Broked
(translate '(:BODY ((:DIV :ID "content")
                    ((:H1 :CLASS "title") "Bardo"))))

(translate '((:H1 :CLASS "title") "Bardo"))

(->> "/Users/jacobsen/Dropbox/org/sites/zerolib.com/bardo.html"
  slurp
  parse-html
  cdar
  translate
  (spit "/tmp/x.html"))
(preview-file "/tmp/x.html")
