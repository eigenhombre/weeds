(defpackage cl-blog
  (:use :cl :arrow-macros :html-parse)
  (:export :slurp
           :length
           :transform-html))
(in-package :cl-blog)

(defmacro comment (&rest body))
(defun test= (a b)
  (assert (equal a b)))

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

(test= (strcat) "")
(test= (strcat :a) "A")
(test= (strcat 1 2 3) "123")

(defun preview-file (filename)
  (sb-ext:run-program "/usr/bin/open"
                      (list filename)
                      :input nil :output *standard-output*))

(defun qualified-tag (l)
  (let* ((tagname (symbol-name (caar l)))
         (kvpairs (cdar l)))
    (format nil "<~a ~{~a~^ ~}>~{~a~}</~a>"
            tagname
            (loop for (key value) on kvpairs by #'cddr
               collect (strcat key "=\"" value "\""))
            (mapcar #'unparse (cdr l))
            tagname)))

(defun unparse (l)
  (cond
    ((not l) "")
    ((stringp l) l)
    ((symbolp l) (format nil "<~a/>" (symbol-name l)))
    ((listp (car l)) (qualified-tag l))
    ((symbolp (car l))
     (format nil "<~a>~{~a~}</~a>"
             (car l)
             (mapcar #'unparse (cdr l))
             (car l)))
    (t (format nil "<~a>~{~a~}</~a>"
               (car l)
               (mapcan #'unparse (cdr l))
               (car l)))))

; '(:|| :XML "xml" :VERSION "1.0" :ENCODING "utf-8"))
(test= (unparse '(:body))
       '"<BODY></BODY>")
(test= (unparse '(:body (:hr)))
       '"<BODY><HR></HR></BODY>")
(test= (unparse '(:TITLE "Bardo"))
       '"<TITLE>Bardo</TITLE>")
(test= (unparse '(:COMMENT " 2018-03-07 Wed 08:16 "))
       '"<COMMENT> 2018-03-07 Wed 08:16 </COMMENT>")
(test= (unparse '((:META :NAME "generator" :CONTENT "Org-mode")))
       '"<META NAME=\"generator\" CONTENT=\"Org-mode\"></META>")
(test= (unparse '((:META :HTTP-EQUIV "Content-Type" :CONTENT "text/html;charset=utf-8")))
       '"<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html;charset=utf-8\"></META>")
(test= (unparse '((:STYLE :TYPE "text/css") " "))
       '"<STYLE TYPE=\"text/css\"> </STYLE>")
(test= (unparse '(:body ((:div :id "content"))))
       '"<BODY><DIV ID=\"content\"></DIV></BODY>")
(test= (unparse '((:div :id "content")))
       '"<DIV ID=\"content\"></DIV>")
(test= (unparse '(:body (:hr) (:hr)))
       '"<BODY><HR></HR><HR></HR></BODY>")
(test= (unparse '(:BODY ((:DIV :ID "content")
                         ((:H1 :CLASS "title") "Bardo"))))
       '"<BODY><DIV ID=\"content\"><H1 CLASS=\"title\">Bardo</H1></DIV></BODY>")
(test= (unparse '((:H1 :CLASS "title") "Bardo"))
       '"<H1 CLASS=\"title\">Bardo</H1>")

(defun transform-html (raw-html)
  (->> raw-html
    parse-html
    cdar
    unparse))

(assert (< 1000
           (->> "/Users/jacobsen/Dropbox/org/sites/zerolib.com/bardo.html"
             slurp
             transform-html
             length)))

(in-package :common-lisp-user)
(defun main ()
  (loop for f in (directory "/Users/jacobsen/Dropbox/org/sites/zerolib.com/*.html")
     do (format t
                "~a: ~a chars~%"
                f
                (arrow-macros:->> f
                  cl-blog:slurp
                  cl-blog:transform-html
                  cl-blog:length))))

;; (spit "/tmp/x.html")
;; (preview-file "/tmp/x.html")
;; (loop for f in (directory "/Users/jacobsen/Dropbox/org/sites/zerolib.com/*.html")
;;    collect (->> f
;;              slurp
;;              transform-html
;;              length))
