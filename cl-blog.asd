(defsystem "cl-blog"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:arrow-macros
               :cl-html-parse)
  :components ((:module "src"
                        :components
                        ((:file "util")
                         (:file "tree" :depends-on "util")
                         (:file "main" :depends-on ("tree" "util")))))
  :description ""
  :in-order-to ((test-op (test-op "cl-blog/tests"))))

(defsystem "cl-blog/tests"
  :author ""
  :license ""
  :depends-on ("cl-blog"
               "rove")
  :components ((:module "tests"
                        :components
                        ((:file "main"))))
  :description "Test system for cl-blog"
  :perform (test-op (op c) (symbol-call :rove :run c)))
