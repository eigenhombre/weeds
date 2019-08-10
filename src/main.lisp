(defpackage cl-blog.main
  (:use
   :cl
   :arrow-macros
   :html-parse
   :cl-blog.util
   :cl-blog.tree
   :cl-ppcre)
  (:export :transform-html
           :parse-html
           :tree-remove-tag
           :parse-transform-and-write!))

(in-package :cl-blog.main)

(defmacro comment (&rest body))

(defun test= (a b)
  (assert (equal a b)))

(defun rand-nth (l)
  (nth (random (length l)) l))

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

(defun transform-html (raw-html)
  (->> raw-html
    parse-html
    ;; Garbage in the beginning... fix this...:
    cdar
    (tree-remove-tag :script)
    (tree-remove-tag :style)
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

(defun preview-file (filename)
  (sb-ext:run-program "/usr/bin/open"
                      (list filename)
                      :input nil :output *standard-output*))

(->> "/Users/jacobsen/Dropbox/org/sites/zerolib.com/auckland.html"
  slurp
  parse-html
  cdar
  (tree-remove-tag :script)
  (tree-remove-tag :style))

;;=>
'((:!DOCTYPE " html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"")
  ((:HTML :XMLNS "http://www.w3.org/1999/xhtml" :LANG "en" :|XML:LANG| "en")
   (:HEAD (:TITLE "Auckland") (:COMMENT " 2018-03-10 Sat 14:42 ")
    ((:META :HTTP-EQUIV "Content-Type" :CONTENT "text/html;charset=utf-8"))
    ((:META :NAME "generator" :CONTENT "Org-mode"))
    ((:META :NAME "author" :CONTENT "John Jacobsen")))
   (:BODY
    ((:DIV :ID "content") ((:H1 :CLASS "title") "Auckland")
     ((:DIV :ID "outline-container-sec-1" :CLASS "outline-2")
      ((:H2 :ID "sec-1") "&#xa0;&#xa0;&#xa0;"
       ((:SPAN :CLASS "tag") ((:SPAN :CLASS "southpole") "southpole"))))
     ((:DIV :ID "outline-container-sec-2" :CLASS "outline-2")
      ((:H2 :ID "sec-2") "Europe in Miniature")
      ((:DIV :CLASS "outline-text-2" :ID "text-2")
       (:P "
09:00h
")
       (:P "
Made it back to the Midwest of the South Pacific, where the most
threatening thing is the drug dog that will point you out to Customs
if you have had any fruit (or drugs) in your backpack at any time in
the last 10,000 years.
")
       (:P "
How is it that a 12 hour transpacific flight can seem to take forever,
and yet when it&rsquo;s over it&rsquo;s hard to say what exactly happened during
those 12 hours? The memory of too-small seats and futile attempts to
sleep sitting up is imprinted more on my body, which feels like the
747 actually rolled over it, than in my mind. The in-flight film
&ldquo;Collateral&rdquo; made more of a mental impression (Tom Cruise blowing away
various people on the LA streets I just went jogging on
&ldquo;yesterday&rdquo;). But the silent view from the back of the darkened plane
of hundreds of personal LCD screens embedded in the back of every
seat, all tuned to different movies, TV shows, games, etc. was
lovely&#x2026; something straight out of a contemporary art gallery. In the
21st Century, apparently, everyone gets their own in-flight media
smorgasbord.
")
       (:P "
At any rate, though I missed my flight to Christchurch, I&rsquo;m on the 10
AM flight, just an hour or so away. All I hope is that I don&rsquo;t have to
show up at the CDC to get my Ice clothing TODAY. I&rsquo;m hoping for a hot
bath and a nap at the Devon.
")
       (:P "
Everything is exactly as I remembered it so far.
"))))
    ((:DIV :ID "postamble" :CLASS "status")
     ((:P :CLASS "date") "Date: 2005-01-11")
     ((:P :CLASS "author") "Author: John Jacobsen")
     ((:P :CLASS "date") "Created: 2018-03-10 Sat 14:42")
     ((:P :CLASS "creator")
      ((:A :HREF "http://www.gnu.org/software/emacs/") "Emacs") " 24.5.1 ("
      ((:A :HREF "http://orgmode.org") "Org") " mode 8.2.10)")
     ((:P :CLASS "validation")
      ((:A :HREF "http://validator.w3.org/check?uri=referer") "Validate"))))))

(in-package :common-lisp-user)

(defun basename (file-name)
  (car (cl-ppcre:split "\\." file-name)))

(defun main ()
  (loop for f in (directory
                  "/Users/jacobsen/Dropbox/org/sites/zerolib.com/*.html")
     for i from 0
     do (progn
          (format t "~a~11t~a~%"
                  (if (= i 0) "Processing" "")
                  (basename (file-namestring f)))
          (cl-blog.main:parse-transform-and-write! "/tmp/cl-blog-out" f))))
