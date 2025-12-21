;;;; example-swank-triangle.asd

(asdf:defsystem #:example-swank-triangle
  :description "Interactive triangle rendering example with Swank support"
  :author "cl-sokol contributors"
  :license "MIT/zlib"
  :version "0.1.0"
  :depends-on (#:cl-sokol
               #:bordeaux-threads
               #:swank)
  :serial t
  :components ((:file "triangle-shader")
               (:file "example-swank-triangle"))
  :in-order-to ((test-op (test-op #:example-swank-triangle/run))))

(asdf:defsystem #:example-swank-triangle/run
  :description "Run the interactive triangle example"
  :depends-on (#:example-swank-triangle)
  :perform (test-op (o c)
                    (uiop:symbol-call :example-swank-triangle :run-triangle-example-threaded)))
