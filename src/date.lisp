(defpackage cl-blog.date
  (:use
   :cl
   :arrow-macros
   :cl-blog.util
   :cl-ppcre
   :cl-utilities
   :local-time)
  (:export :post-date-str->date
           :local-time->yyyy-mm-dd))

(in-package :cl-blog.date)

(defun post-date-str->date (dstr)
  (let ((b (->> dstr
             (scan-to-strings "Date: (.+)")
             (nth-value 1))))
    (when b
      (parse-timestring (subseq (aref b 0) 0 10)))))

(dotests
 (test= nil (post-date-str->date nil))
 (test= (format nil "~a" (post-date-str->date "Date: 2005-01-19"))
        '"2005-01-18T18:00:00.000000-06:00")
 (test= (format nil "~a" (post-date-str->date
                          "Date: 2005-01-19T00:00:00Z"))
        '"2005-01-18T18:00:00.000000-06:00"))

(defun local-time->yyyy-mm-dd (date)
  (when date
    (format-timestring
     nil
     date
     :format '((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2))
     :timezone +utc-zone+)))

(dotests
 (test= (local-time->yyyy-mm-dd
         (post-date-str->date "Date: 2005-01-19"))
        '"2005-01-19")
 (test= (local-time->yyyy-mm-dd (post-date-str->date nil))
        nil))
