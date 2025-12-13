;;;; example-wrapper-triangle.asd

(asdf:defsystem #:example-wrapper-triangle
  :description "Triangle rendering example using cl-sokol CLOS wrapper"
  :author "cl-sokol contributors"
  :license "MIT/zlib"
  :version "0.1.0"
  :depends-on (#:cl-sokol)
  :serial t
  :components ((:file "triangle-shader")
               (:file "example-wrapper-triangle"))
  :in-order-to ((test-op (test-op #:example-wrapper-triangle/run))))

(asdf:defsystem #:example-wrapper-triangle/run
  :description "Run the triangle example"
  :depends-on (#:example-wrapper-triangle)
  :perform (test-op (o c)
                    (uiop:symbol-call :example-wrapper-triangle :run-triangle-example)))
