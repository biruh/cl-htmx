(defpackage cl-htmx
  (:use :cl)
  (:export render-html))

(in-package :cl-htmx)




(defparameter *port* 4242)
(defvar *server-handler*)

(defun start ()
  (stop)
  (setf *server-handler*
        (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port *port*))))


(defun stop ()
  (when *server-handler*
    (hunchentoot:stop *server-handler*)
    (setf *server-handler* nil)))








(defconstant +no-closing-designator+ #\-)

(defun no-closing-tag-p (tag)
  (char-equal (aref tag (1- (length tag))) +no-closing-designator+))


(defun p-list-to-a-list (p-list)
  (loop :while p-list
        :for key = (pop p-list)
        :for value = (pop p-list)
        :collect (cons key value)))


(defun extract-parts (form)
  (cond
    ((and (listp form)
          (symbolp (nth 0 form)))
     (let* ((len (length form)))
       (cond
         ((zerop len) nil)
         ((= len 1)
          (let ((tag (string (nth 0 form))))
            `(:tag ,tag
              :props nil
              :no-closing-tag ,(no-closing-tag-p tag)
              :child nil)))
         ((evenp len)
          (let ((tag (string (nth 0 form))))
            `(:tag ,tag
              :props ,(p-list-to-a-list (subseq form 1 (-  len 1)))
              :no-closing-tag ,(no-closing-tag-p tag)
              :child ,(let ((child (nth (- len 1) form)))
                        (extract-parts child)
                        ))))
         ((oddp len)
          (let ((tag (string (nth 0 form))))
            `(:tag ,tag
              :props ,(p-list-to-a-list (subseq form 1 len))
              :no-closing-tag ,(no-closing-tag-p tag)
              :child nil))))))
    ((not (listp form)) form)
    (t (loop :for x :in form
             :collect (extract-parts x)))))

(defun validate-parts (ip-form)
  (when (and (not (null ip-form))
             (listp ip-form))
    (destructuring-bind (&key tag props no-closing-tag child) ip-form
      (if no-closing-tag
          (assert
           (null child) nil
           "validation fail: an element with no closing tag can't have child: ~a" tag))
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


(defun render-props (props)
  (cond
    ((null props) "")
    (t (format nil " ~{~a~^ ~}"
               (loop :for (x . y) :in props
                     :collect (format nil "~a=~a" x y))))))


(defun render-html (ir stream)
  (destructuring-bind (&key tag props no-closing-tag child) ir
    (cond
      (no-closing-tag (format stream "<~a~a />" tag (render-props props)))
      (t (format stream "<~a~a>~a</~a>"
                 tag
                 (render-props props)
                 (if (listp child)
                     (render-html child stream)
                     child)
                 tag)))))

(defun render-ir (form stream)
  (let ((ir (extract-and-validate form)))
    (render-html ir stream)))

(defun render-app ()
  (render-ir '(html ((head)
                     (body (div "test")))) nil))




(hunchentoot:define-easy-handler (app :uri "/app") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (render-app))

(defun render-html (body stream)
  (assert (listp body) nil "top element needs to be a list: ~a" body)
  (render-ir body stream))


(defmacro htmx (name body)
  (let ((render-name-str (format nil "render-~a" (string name))))
    `(progn
       (defun ,render-name-str ()
         (format nil "<a></a>"))

       (hunchentoot:define-easy-handler (app :uri "/app") ()
         (setf (hunchentoot:content-type*) "text/plain")
         (format nil "Hey~@[ ~A~]!" name))

       (defun render-html (body stream)
         (assert (listp body) nil "top element needs to be a list: ~a" body)
         (render-ir body stream)))))
