;;;; example-events.asd

(asdf:defsystem #:example-events
  :description "Event handling example for cl-sokol"
  :author "cl-sokol contributors"
  :license "MIT/zlib"
  :version "0.1.0"
  :depends-on (#:cl-sokol
               #:example-wrapper-triangle) ; for shader
  :serial t
  :components ((:file "example-events"))
  :in-order-to ((test-op (test-op #:example-events/run))))

(asdf:defsystem #:example-events/run
  :description "Run the event handling example"
  :depends-on (#:example-events)
  :perform (test-op (o c)
                    (uiop:symbol-call :example-events :run-events-example)))
