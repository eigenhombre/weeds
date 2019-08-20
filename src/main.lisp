(eval-when (:load-toplevel :compile-toplevel :execute)
  (load (merge-pathnames "util.lisp" *default-pathname-defaults*))
  (load (merge-pathnames "date.lisp" *default-pathname-defaults*))
  (load (merge-pathnames "tree.lisp" *default-pathname-defaults*)))

(defpackage weeds.main
  (:use
   :cl
   :arrow-macros
   :html-parse
   :weeds.date
   :weeds.util
   :weeds.tree
   :let-plus
   :cl-ppcre
   :cl-utilities)
  (:export :main
           :parse-html
           :srcfiles
           :transform-html
           :tree-remove-tag))

(in-package :weeds.main)

;; FIXME: Make this more general / configurable:
(defparameter *srcdir* "/Users/jacobsen/Dropbox/org/sites/zerolib.com")
(defparameter *outdir* "/tmp/weeds-out/")

(declaim (ftype (function (list) t) qualified-tag))

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
  (let* ((tagname (caar l))
         (kvpairs (cdar l)))
    (format nil "<~a ~{~a~^ ~}>~{~a~}</~a>"
            tagname
            (loop for (key value) on kvpairs by #'cddr
               collect (strcat key "=\"" value "\""))
            (mapcar #'unparse (cdr l))
            tagname)))

(dotests
 (test= (qualified-tag '((:p :class "date")))
        '"<P CLASS=\"date\"></P>"))

(dotests
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

(defun is-funky-xml-tag (x)
  "
  Strip out \"<?XML ... ?>\" business, handled poorly by HTML parser
  and not strictly needed...?
  "
  (and (listp x)
       (atom (car x))
       (equal (symbol-name (car x)) (symbol-name ':||))))

(defun transform-html-tree (raw-html)
  (->> raw-html
    ;; FIXME: Shouldn't need this...
    car
    (tree-remove #'is-funky-xml-tag)
    (tree-remove-tag :script)
    (tree-remove-tag :style)))

(defvar *example-post* (->> "/auckland.html"
                         (strcat *srcdir* )
                         slurp
                         parse-html
                         transform-html-tree))
*example-post*
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

(dotests
 (assert (< 1000
            (->> *example-post*
              unparse
              length))))

(apply #'strcat (interpose " " (split "\\s+" "a b  c")))
;;=>
'"a b c"

(defun words-from-paragraph (txt n)
  "
  Take at most n words from input paragraph to build a (possibly
  shorter) new paragraph. Return a list of the shortened text and the
  number of words taken.
  "
  (let* ((words (split "\\s+" txt))
         (subseq (take n words))
         (n-taken (length subseq))
         (rejoined (apply #'strcat
                          (interpose " " subseq))))
    (list rejoined n-taken)))

(dotests
 (labels
     ((tst (n-req src-txt out-txt-expected wc-expected)
        (let+ (((txt wc) (words-from-paragraph src-txt
                                               n-req)))
          (test= txt out-txt-expected)
          (test= wc wc-expected))))
   (tst 0 "Few words are better than more." "" 0)
   (tst 1 "Few words are better than more." "Few" 1)
   (tst 2 "Few words are better than more." "Few words" 2)
   (tst 6
        "Few words are better   than more."
        "Few words are better than more."
        6)
   (tst 7
        "Few words are better than more."
        "Few words are better than more."
        6)
   (tst 7
        "Few words    are better than more."
        "Few words are better than more."
        6)))

(defun limit-p-word-count (n tree)
  (cond
    ((not tree) (list n tree))
    ((atom tree) (list n tree))
    ((zerop n) (list n nil))
    ((and (equal (car tree) :p)
          (stringp (cadr tree)))
     (let+ (((&ign txt) tree)
            ((txt-taken n-taken) (words-from-paragraph txt n)))
       `(,(- n n-taken) (:p ,txt-taken))))
    (t
     (let+ ((c1 (car tree))
            ((n1 tree1) (limit-p-word-count n c1)))
       (if (< n1 0)
           (list n1 tree1)
           (let+ (((n2 tree2) (limit-p-word-count n1 (cdr tree))))
             (list n2 (cons tree1 tree2))))))))

(dotests
 (labels ((test-limit-wc (rhx rhn lhx lhn)
            (let+ (((out-n out-txt) (limit-p-word-count rhn rhx)))
              (test= out-n lhn)
              (test= out-txt lhx))))
   (test-limit-wc nil 2
                  nil 2)
   (test-limit-wc '(:p "some text") 2
                  '(:p "some text") 0)
   (test-limit-wc '(:p "some text") 1
                  '(:p "some") 0)
   (test-limit-wc '(:p "some text") 3
                  '(:p "some text") 1)
   (test-limit-wc '(:h1 "a header") 3
                  '(:h1 "a header") 3)
   (test-limit-wc '(:div (:p "some text") (:h1 "a header")) 3
                  '(:div (:p "some text") (:h1 "a header")) 1)
   (test-limit-wc '(:div (:p "some text") (:h1 "a header")) 1
                  '(:div (:p "some")) 0)
   (test-limit-wc '(:div (:p "some text")) 1
                  '(:div (:p "some")) 0)
   (test-limit-wc '(:p ((:a :href "foo" :class "baz") "xyz")) 10
                  '(:p ((:a :href "foo" :class "baz") "xyz")) 10)
   ;; FIXME: Handle structure inside of :p tags...
   ))

(defun tags-for-post (transformed-html)
  (->> transformed-html
    (tree-find
     #'(lambda (x)
         (when (listp x)
           (let ((cx (car x)))
             (and (listp cx)
                  (equal (list :SPAN :CLASS "tag")
                         (take 3 cx)))))))
    cdr
    (remove-if-not #'listp)
    (mapcar #'cadr)))

(dotests
 (test= (tags-for-post *example-post*)
        '("southpole")))

(defun post-date (transformed-html)
  (->> transformed-html
    (tree-find
     #'(lambda (x)
         (when (listp x)
           (let ((cx (car x)))
             (and (listp cx)
                  (equal (list :P :CLASS "date")
                         (take 3 cx)))))))
    last
    car
    post-date-str->date))

(test=
 (format nil "~a" (post-date *example-post*))
 ;;=>
 '"2005-01-10T18:00:00.000000-06:00")

(defun target-file-name (target-dir src-path)
  (strcat target-dir "/" (file-namestring src-path)))

(defun srcfiles ()
  (directory (strcat *srcdir* "/*.html")))

(->> (srcfiles)
  (mapcar (compose #'basename #'file-namestring))
  (take 4))
;;=>
'("a-bath" "a-nicer-guy" "a-place-that-wants-you-dead" "a-two-bit-decoder")

(defun post-title (transformed-html)
  (->> transformed-html
    (tree-find #'(lambda (x) (and (listp x)
                                  (equal (car x) :TITLE))))
    cdr))

(defun make-post-alist (path)
  (let* ((slug (basename (file-namestring path)))
         (outpath (strcat *outdir* slug ".html"))
         (html (slurp path))
         (parsed (parse-html path))
         (transformed (transform-html-tree parsed))
         (date (post-date transformed))
         (unparsed (unparse transformed))
         (title (post-title transformed))
         (tags (tags-for-post transformed)))
    `((:path . ,path)
      (:outpath . ,outpath)
      (:slug . ,slug)
      (:html . ,html)
      (:date . ,date)
      (:tags . ,tags)
      (:parsed . ,parsed)
      (:title . ,title)
      (:transformed . ,transformed)
      (:unparsed . ,unparsed))))

(defun posts-alist ()
  (loop for path in (srcfiles)
     collect (make-post-alist path)))

(defun post-links (posts)
  (loop for post in posts
     collect
       (let* ((title (cadr (assoc :title post)))
              (slug (cdr (assoc :slug post)))
              (tags (cdr (assoc :tags post)))
              (url (strcat *outdir* slug ".html")))
         `(:p ((a href ,url) ,title) ,(strcat " " tags)))))

(defun out-html (posts)
  (unparse
   `(:html
     (:body
      ,@(post-links posts)))))

(defun main ()
  (let ((posts (posts-alist))
        (indexfile (strcat *outdir* "index.html")))
    (loop for post in posts
       for i from 0
       do (let ((outpath (cdr (assoc :outpath post)))
                (slug (cdr (assoc :slug post)))
                (unparsed (cdr (assoc :unparsed post)))
                (datestr (local-time->yyyy-mm-dd (cdr (assoc :date post)))))
            (progn
              (format t
                      "~a~10t~10a ~a~%"
                      (if (= i 0) "Processed" "")
                      datestr
                      slug)
              (ensure-directories-exist outpath)
              (spit outpath unparsed))))
    (ensure-directories-exist indexfile)
    (spit indexfile
          (out-html posts))))

(comment
 (main)
 (macos-open-file (strcat *outdir* "index.html")))
