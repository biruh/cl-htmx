(defsystem "cl-htmx"
  :version "0.1.0"
  :author "Biruh Mekonnen"
  :license "MIT"
  :depends-on ("hunchentoot")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-htmx/tests"))))

(defsystem "cl-htmx/tests"
  :author "Biruh Mekonnen"
  :license "MIT"
  :depends-on ("cl-htmx"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-htmx"
  :perform (test-op (op c) (symbol-call :rove :run c)))
