(defpackage cl-blog
  (:use :cl :arrow-macros :html-parse :cl-ppcre)
  (:export :slurp
           :length
           :transform-html
           :parse-transform-and-write!))
(in-package :cl-blog)

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

(defun preview-file (filename)
  (sb-ext:run-program "/usr/bin/open"
                      (list filename)
                      :input nil :output *standard-output*))

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

(defun qualified-tag (l)
  (let* ((tagname (symbol-name (caar l)))
         (kvpairs (cdar l)))
    (format nil "<~a ~{~a~^ ~}>~{~a~}</~a>"
            tagname
            (loop for (key value) on kvpairs by #'cddr
               collect (strcat key "=\"" value "\""))
            (mapcar #'unparse (cdr l))
            tagname)))

(dotests
 ;; '(:|| :XML "xml" :VERSION "1.0" :ENCODING "utf-8"))
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
        '"<H1 CLASS=\"title\">Bardo</H1>"))

(defun tree-walk (fn tree)
  (cond ((atom tree) (funcall fn tree))
	(t (cons (tree-walk fn (first tree))
	         (tree-walk fn (rest tree))))))

(dotests
 (test= (tree-walk #'identity '(a b c (e f (g h)) d))
        '(A B C (E F (G H)) D))
 (test= (tree-walk #'(lambda (x)
                       (if (numberp x)
                           (* x x)
                           x))
                   '(1 2 3 (4 5 (6 7) 8)))
        '(1 4 9 (16 25 (36 49) 64))))

(defun tree-keep (fn tree)
  (cond ((atom tree) tree)
        ((funcall fn (first tree))
         (cons (tree-keep fn (first tree))
	       (tree-keep fn (rest tree))))
	(t (tree-keep fn (rest tree)))))

(dotests
 (test= (tree-keep #'identity '())
        ())
 (test= (tree-keep #'identity '(a b c))
        '(a b c))
 (test= (tree-keep #'identity '(a b (c d) e))
        '(a b (c d) e))
 (test= (tree-keep #'(lambda (x) (not (equal x 'b)))
                   '(a b (c d) e))
        '(a (c d) e))
 (test= (tree-keep #'(lambda (x) (or (not (numberp x))
                                     (evenp x)))
                   '(1 2 (3 4) 5 6))
        '(2 (4) 6)))

(defun tree-remove-tag (tag-kw tree)
  (tree-keep #'(lambda (x)
                 (or (not (listp x))
                     (not (listp (car x)))
                     (not (equal (caar x) tag-kw))))
             tree))

(dotests
 (let ((scr '((:SCRIPT :TYPE "text/javascript") " ... javascript ... ")))
   (test= '(a b c)          (tree-remove-tag :script `(a ,scr b c)))
   (test= '(a (:d :e) b c)  (tree-remove-tag :script '(a (:d :e) b c)))
   (test= '(a b c)          (tree-remove-tag :script `(a ,scr b c ,scr ,scr)))
   (test= '(a b c)          (tree-remove-tag :script '(a b c)))))

(defun tree-add (fn el tree &key (order :after) (once nil))
  (labels ((fff (done tree order once)
             (cond ((atom tree) tree)
                   ((and (or (not once)
                             (not done))
                         (funcall fn (first tree)))
                    (if (equal order :after)
                        (list* (fff t (first tree) order once)
                               el
	                       (fff t (rest tree) order once))
                        (list* el
                               (fff t (first tree) order once)
	                       (fff t (rest tree) order once))))
	           (t (cons (fff done (first tree) order once)
	                    (fff done (rest tree) order once))))))
    (fff nil tree order once)))

(dotests
 (let ((invec '(1 1 3 5 9 8 9 10 (1 16 1)))
       (pred #'(lambda (x) (and (numberp x)
                                (evenp x)))))
   (test= (tree-add pred 666 invec
                    :once nil
                    :order :before)
          '(1 1 3 5 9 666 8 9 666 10 (1 666 16 1)))

   (test= (tree-add pred 666 invec
                    :once t
                    :order :before)
          '(1 1 3 5 9 666 8 9 10 (1 16 1)))

   (test= (tree-add pred 666 invec
                    :once nil
                    :order :after)
          '(1 1 3 5 9 8 666 9 10 666 (1 16 666 1)))

   (test= (tree-add pred 666 invec
                    :once t
                    :order :after)
          '(1 1 3 5 9 8 666 9 10 (1 16 1)))))

(defun transform-html (raw-html)
  (->> raw-html
    parse-html
    ;; Garbage in the beginning... fix this...:
    cdar
    (tree-remove-tag :script)
    unparse))

(assert (< 1000
           (->> "/Users/jacobsen/Dropbox/org/sites/zerolib.com/bardo.html"
             slurp
             transform-html
             length)))

(defun join (sep coll)
  (format nil (format nil "~~{~~a~~^~a~~}" sep) coll))

(defun target-file-name (target-dir src-path)
  (strcat target-dir "/" (file-namestring src-path)))

(defun parse-transform-and-write! (target-dir src-file)
  (ensure-directories-exist (strcat target-dir "/"))
  (let ((target-file (target-file-name target-dir src-file)))
    (->> src-file
      slurp
      transform-html
      (spit target-file))))


(in-package :common-lisp-user)

(defun main ()
  (loop for f in (directory
                  "/Users/jacobsen/Dropbox/org/sites/zerolib.com/*.html")
     do (progn
          (format t "Processing ~a~%" (file-namestring f))
          (cl-blog:parse-transform-and-write! "/tmp/cl-blog-out" f))))

