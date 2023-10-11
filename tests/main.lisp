(defpackage cl-htmx/tests/main
  (:use :cl :cl-htmx :rove)
  )

(in-package :cl-htmx/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-htmx)' in your Lisp.

(deftest test-html-1
  (testing "basic render tags"
    (ok (equalp (render-html '(div) nil)
                "<div></div>"))
    (ok (equalp (render-html '(abc) nil)
                "<abc></abc>"))
    (ok (equalp (render-html '(input-) nil)
                "<input />"))
    (ok (equalp (render-html '(-) nil)
                "")))
  (testing "basic render tags with literals"
    (ok (equalp (render-html '(div "test") nil)
                "<div>test</div>"))
    (ok (equalp (render-html '(abc "abc") nil)
                "<abc>abc</abc>"))
    (ok (equalp (render-html '(- "test") nil)
                "test"))
    )
  )
