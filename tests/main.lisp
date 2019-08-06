(defpackage cl-blog/tests/main
  (:use :cl
        :cl-blog
        :rove))
(in-package :cl-blog/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-blog)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
