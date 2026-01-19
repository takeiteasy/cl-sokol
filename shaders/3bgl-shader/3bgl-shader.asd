(defsystem :cl-sokol-3bgl-shader
  :description "Forked 3bgl-shader for cl-sokol - CL-hosted DSL for generating GLSL/Metal/HLSL/WGSL"
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (alexandria bordeaux-threads)
  :serial t
  :pathname ""
  :components ((:file "package")
               (:file "ir")
               (:file "walker")
               (:file "types")
               (:file "infer")
               (:file "glsl-base")
               (:file "cl-functions")
               (:file "glsl")
               (:file "finalize-inference")
               (:file "printer")
               (:file "metal-printer")
               (:file "hlsl-printer")
               (:file "wgsl-printer")
               (:file "compiler")
               (:file "api")))

;; Optional OpenGL utilities (requires cl-opengl)
(defsystem :cl-sokol-3bgl-shader/gl-utils
  :description "Optional OpenGL shader management utilities"
  :depends-on (:cl-sokol-3bgl-shader :cl-opengl)
  :pathname ""
  :components ((:file "old-utils")
               (:file "utils")))
