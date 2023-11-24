;;;; cl-sokol.asd

#+(and darwin sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))

(asdf:defsystem #:cl-sokol
  :description "Common Lisp bindings + wrapper for sokol libraries"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (:cffi
               :cffi-libffi
               :trivial-main-thread)
  :serial t
  :components ((:file "src/package")
               (:file "src/bindings")
               (:file "src/sokol")
               (:file "src/test")))
