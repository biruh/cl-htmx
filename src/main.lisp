(defpackage cl-htmx
  (:use :cl)
  (:export render-html))

(in-package :cl-htmx)

(defconstant +no-closing-designator+ #\-)

(defun no-closing-tag (tag)
  (char-equal (aref tag (1- (length tag))) +no-closing-designator+))


(defun p-list-to-a-list (p-list)
  (loop while p-list
        for key = (pop p-list)
        for value = (pop p-list)
        collect (cons key value)))

(defun extract-parts (form)
  (if (listp form)
      (let* ((len (length form)))
        (cond
          ((zerop len) nil)
          ((= len 1)
           (let ((tag (string (nth 0 form))))
             `(:tag ,tag
               :props nil
               :no-closing-tag ,(no-closing-tag tag)
               :child nil)))
          ((evenp len)
           (let ((tag (string (nth 0 form))))
             `(:tag ,tag
               :props ,(p-list-to-a-list (subseq form 1 (-  len 1)))
               :no-closing-tag ,(no-closing-tag tag)
               :child ,(extract-parts (nth (- len 1) form)))))
          ((oddp len)
           (let ((tag (string (nth 0 form))))
             `(:tag ,tag
               :props ,(p-list-to-a-list (subseq form 1 len))
               :no-closing-tag ,(no-closing-tag tag)
               :child nil)))))
      form))

(defun validate-parts (ip-form)
  (when (listp ip-form)
    (destructuring-bind (&key tag props no-closing-tag child) ip-form
      (if no-closing-tag
          (assert (null child) nil "an element with no closing tag can't have child: ~a" (list tag)))
      (loop :for (ident . value) :in props
            :do  (assert (keywordp ident)
                         nil
                         "property identifiers must be a keyword ~a is not"
                         ident))
      (validate-parts child))))


(defun extract-and-validate (form)
  (assert (listp form) nil "form must be list but is ~a" form)
  (let ((ir (extract-parts form)))
    (validate-parts ir)
    ir))

(extract-and-validate '(div :a b (input- :value "test")))

(defun render-tag (tag stream)
  (cond
    ((stringp tag) tag)
    ((null tag) nil)
    ((listp tag) )
    )

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
  (render-tag body stream))

(render-html '(input-) nil)


(defmacro htmx (body)
  )
