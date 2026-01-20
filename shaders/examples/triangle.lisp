;;;; Example triangle shader using cl-sokol/shaders
;;;; This demonstrates writing sokol shaders in Lisp using clsl DSL

(defpackage #:sokol-triangle-shader
  (:use #:cl #:clsl-glsl)
  (:shadowing-import-from #:clsl-glsl
                          #:defun #:defconstant #:defmacro #:defstruct)
  (:export #:vertex #:fragment))

(in-package #:sokol-triangle-shader)

;;; Vertex inputs (attributes)
(input position :vec4 :location 0)
(input color0 :vec4 :location 1)

;;; Fragment output
(output frag-color :vec4 :stage :fragment)

;;; Interface block for vertex->fragment communication
(interface varyings (:out (:vertex outs) :in (:fragment ins))
  (color :vec4))

;;; Vertex shader entry point
(defun vertex ()
  "Simple pass-through vertex shader"
  (setf (@ outs color) color0)
  (setf gl-position position))

;;; Fragment shader entry point
(defun fragment ()
  "Simple pass-through fragment shader"
  (setf frag-color (@ ins color)))


;;; Now define the sokol shader using our API
(in-package #:cl-sokol-shaders)

;; Define the triangle shader
;; This creates a function TRIANGLE-SHADER-DESC that returns sg_shader_desc*
(define-sokol-shader triangle
  (:vertex sokol-triangle-shader::vertex)
  (:fragment sokol-triangle-shader::fragment))
