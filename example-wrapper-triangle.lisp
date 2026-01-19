;;;; example-wrapper-triangle.lisp

(defpackage #:example-wrapper-triangle
  (:use #:cl)
  (:export #:run-triangle-example))

(in-package :example-wrapper-triangle)

(cffi:define-foreign-library libsokol-app
  (:darwin "src/lib/libsokol_app.dylib")
  (:unix "src/lib/libsokol_app.so")
  (:windows "src/lib/sokol_app.dll")
  (t (:default "libsokol_app")))

(cffi:use-foreign-library libsokol-app)

(defvar *pipeline* nil "Graphics pipeline")
(defvar *vertex-buffer* nil "Vertex buffer")

(defmethod sokol:init ()
  "Initialize graphics and resources"
  ;; Setup sokol_gfx with default descriptor
  (sokol:setup-gfx)

  ;; Create vertex buffer - 3 vertices with position (x,y,z) and color (r,g,b,a)
  (let* ((vertices (vector
                    0.0  0.5 0.5   1.0 0.0 0.0 1.0    ; top (red)
                    0.5 -0.5 0.5   0.0 1.0 0.0 1.0    ; right (green)
                   -0.5 -0.5 0.5   0.0 0.0 1.0 1.0))  ; left (blue)
         (vertex-range (sokol:make-range-from-array vertices :float))
         (buffer-descriptor (make-instance 'sokol:buffer-desc
                                          :size (* 21 (cffi:foreign-type-size :float))
                                          :buffer-type :vertex-buffer
                                          :usage :immutable
                                          :data vertex-range)))
    (setf *vertex-buffer* (sokol:make-buffer buffer-descriptor)))

  ;; Create shader using cl-sokol-shaders (auto-detects backend)
  (let ((shader (cl-sokol-shaders:make-shader 'triangle)))

    ;; Create pipeline
    (let ((pipeline-descriptor (make-instance 'sokol:pipeline-desc
                                             :shader shader
                                             :vertex-layouts '(:sg-vertexformat-float3
                                                              :sg-vertexformat-float4))))
      (setf *pipeline* (sokol:make-pipeline pipeline-descriptor)))))

(defmethod sokol:frame ()
  "Render a frame"
  ;; Begin rendering pass
  (let ((pass (make-instance 'sokol:pass-desc
                            :action (make-instance 'sokol:pass-action
                                                  :clear-color (sokol:make-color 0.1 0.1 0.1 1.0)
                                                  :load-action :clear
                                                  :store-action :store))))
    (sokol:begin-pass pass))

  ;; Apply pipeline and draw
  (sokol:apply-pipeline *pipeline*)

  ;; Bind vertex buffer
  (let ((bindings-obj (make-instance 'sokol:bindings
                                     :vertex-buffers (list *vertex-buffer*))))
    (sokol:apply-bindings bindings-obj))

  ;; Draw triangle (3 vertices, 1 instance)
  (sokol:draw 0 3 1)

  ;; End pass and commit frame
  (sokol:end-pass)
  (sokol:commit))

(defmethod sokol:cleanup ()
  "Clean up resources"
  (sokol:shutdown-gfx))

(defmethod sokol:event (event-data)
  "Handle input events"
  (declare (ignore event-data)))

(defun run-triangle-example ()
  "Run the triangle example"
  (sokol:run-app :width 640
                 :height 480
                 :title "Triangle"))

;; (asdf:load-system :example-wrapper-triangle)
;; (example-wrapper-triangle:run-triangle-example)