(defpackage cl-htmx/tests/main
  (:use :cl
        :cl-htmx
        :rove))
(in-package :cl-htmx/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-htmx)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
