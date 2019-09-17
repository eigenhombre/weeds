(defsystem "weeds"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("arrow-macros"
               "cl-html-parse"
               "cl-utilities"
               "local-time"
               "let-plus")
  :components ((:module "src"
                        :components
                        ((:file "util")
                         (:file "date" :depends-on ("util"))
                         (:file "tree" :depends-on ("util"))
                         (:file "main" :depends-on ("tree" "util" "date")))))
  :description ""
  :in-order-to ((test-op (test-op "weeds/tests"))))

(defsystem "weeds/tests"
  :author ""
  :license ""
  :depends-on ("weeds"
               "rove")
  :components ((:module "tests"
                        :components
                        ((:file "main"))))
  :description "Test system for weeds"
  :perform (test-op (op c) (symbol-call :rove :run c)))
