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
  :pathname "src/"
  :components ((:module #:cl-sokol/app
                :serial t
                :components ((:file "bindings")
                             (:file "wrapper")
                             (:static-file "sokol_app.h")))
               (:module #:cl-sokol/gfx
                :serial t
                :components ((:file "bindings")
                             (:file "wrapper")
                             (:static-file "sokol_gfx.h")))
               (:module #:cl-sokol/time
                :serial t
                :components ((:file "bindings")
                             (:file "wrapper")
                             (:static-file "sokol_time.h")))
               (:file "sokol"
                :depends-on (:cl-sokol/app
                             :cl-sokol/time))))
