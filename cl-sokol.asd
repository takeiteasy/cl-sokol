;;;; cl-sokol.asd

#+(and darwin sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))

(asdf:defsystem #:cl-sokol
  :description "Describe cl-sokol here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (:cffi
               :cffi-libffi
               :trivial-main-thread)
  :serial t
  :components ((:file "src/package")
               (:file "src/bindings")
               (:file "src/sokol")
               (:file "src/test")))
