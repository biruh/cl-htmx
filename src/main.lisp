(defpackage cl-htmx
  (:use :cl)
  (:export render-html))

(in-package :cl-htmx)




(defun no-closing-tag (tag)
  (and (> (length tag) 1)
       (char-equal (aref tag (1- (length tag))) #\-)))

(defun render-tag (tag stream)
  (let ((parent-tag (string (first body))))
    (assert (not (and (no-closing-tag parent-tag)
                      (= (length body) 2)))
            nil
            "you cannot have children for ~a"
            parent-tag)
    (cond
      ((no-closing-tag parent-tag)
       (format stream "<~a />" (subseq parent-tag 0 (- (length parent-tag) 1))))
      ((string= "-" parent-tag) (format stream ""))
      (t (format stream "<~a></~a>" parent-tag parent-tag)))
    )
  )

(defun render-html (body stream)
  (assert (listp body) nil "top element needs to be a list: ~a" body)
  )

(render-html '(input-) nil)


(defmacro htmx (body)
  )
