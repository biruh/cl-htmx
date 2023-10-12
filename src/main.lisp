(defpackage cl-htmx
  (:use :cl)
  (:export render-html))

(in-package :cl-htmx)

(defconstant +no-closing-designator+ #\-)

(defun no-closing-tag (tag)
  (char-equal (aref tag (1- (length tag))) +no-closing-designator+))


(defun p-list-to-a-list (p-list)
  (loop :while p-list
        :for key = (pop p-list)
        :for value = (pop p-list)
        :collect (cons key value)))

(defun extract-parts (form)
  (format t "~%processing ~a" form)
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
  (format t "~%validating: ~a" ip-form)

  (when (and (not (null ip-form))
             (listp ip-form))
    (destructuring-bind (&key tag props no-closing-tag child) ip-form
      (if no-closing-tag
          (assert (null child) nil "validation fail: an element with no closing tag can't have child: ~a" tag))
      (loop :for (ident . value) :in props
            :do  (assert (keywordp ident)
                         nil
                         "validation fail: property identifiers must be a keyword ~a is not"
                         ident))
      (validate-parts child))))


(defun extract-and-validate (form)
  (assert (listp form) nil "validation fail: top element must be list but is ~a" form)
  (let ((ir (extract-parts form)))
    (validate-parts ir)
    ir))


(defun render-ir (form stream)
  (let (ir (extract-and-validate form))
    (destructuring-bind (&key tag props no-closing-tag child) ir
      (cond
        ((no-closing-tag parent-tag)
         (format stream "<~a />" (subseq parent-tag 0 (- (length parent-tag) 1))))
        ((string= "-" parent-tag) (format stream ""))
        (t (format stream "<~a></~a>" parent-tag parent-tag))))))

(defun render-html (body stream)
  (assert (listp body) nil "top element needs to be a list: ~a" body)
  (render-tag body stream))

(render-html '(input-) nil)


(defmacro htmx (body)
  )
