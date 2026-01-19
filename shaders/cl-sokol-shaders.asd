(defsystem :cl-sokol-shaders
  :description "Write sokol shaders in Common Lisp using 3bgl-shader DSL"
  :license "MIT"
  :author "George"
  :depends-on (:cl-sokol-3bgl-shader :cl-sokol :cffi)
  :serial t
  :pathname ""
  :components ((:file "package")
               (:file "type-mapping")
               (:file "reflection")
               (:file "shader-desc")
               (:file "api")
               (:module "examples"
                :components ((:file "triangle")))))
