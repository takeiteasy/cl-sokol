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
               :cl-autowrap
               :trivial-main-thread)
  :serial t
  :pathname "src"
  :components ((:static-file "sokol/sokol_gfx.h")
               (:static-file "sokol/sokol_app.h")
               (:static-file "sokol/sokol_audio.h")
               (:static-file "sokol/sokol_fetch.h")
               (:static-file "sokol/sokol_time.h")
               (:file "test")))
