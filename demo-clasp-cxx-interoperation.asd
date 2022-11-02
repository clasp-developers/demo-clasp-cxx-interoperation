(asdf:defsystem #:demo-clasp-cxx-interoperation
  :description "Demo clasp/cxx interoperation"
  :version "0.0.1"
  :author "Christian Schafmeister <chris.schaf@verizon.net>"
  :licence "LGPL-3.0"
  :depends-on ()
  :serial t
  :components ((:module "hello-world"
                :serial t
                :components ((:file "hello-world")))))
