;;;; example-swank-triangle.lisp
;;;; Interactive development example with Swank support

(defpackage :example-swank-triangle
  (:use :cl)
  (:export #:run-triangle-example
           #:run-triangle-example-threaded
           #:start-swank
           #:reload-resources))

(in-package :example-swank-triangle)

(cffi:define-foreign-library libsokol-app
  (:darwin "src/lib/libsokol_app.dylib")
  (:unix "src/lib/libsokol_app.so")
  (:windows "src/lib/sokol_app.dll")
  (t (:default "libsokol_app")))

(cffi:use-foreign-library libsokol-app)

;;; State management for interactive development
(defvar *pipeline* nil "Graphics pipeline")
(defvar *vertex-buffer* nil "Vertex buffer")
(defvar *rotation* 0.0 "Rotation angle for interactive modification")
(defvar *clear-color* (list 0.1 0.1 0.1 1.0) "Background color - modify at runtime!")
(defvar *app-thread* nil "Thread running the sokol app")

;;; Swank support
(defun start-swank (&optional (port 4005))
  "Start Swank server for SLIME/Sly connection.
  Connect from Emacs with: M-x slime-connect RET localhost RET 4005"
  (swank:create-server :port port :dont-close t)
  (format t "~%Swank server started on port ~A~%" port)
  (format t "Connect from Emacs: M-x slime-connect RET localhost RET ~A~%~%" port))

;;; Resource creation (can be called multiple times for hot-reloading)
(defun create-vertex-buffer ()
  "Create vertex buffer - can be called from REPL to reload"
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
    (sokol:make-buffer buffer-descriptor)))

(defun create-pipeline ()
  "Create graphics pipeline - can be called from REPL to reload"
  (let* ((shader-desc (example-wrapper-triangle:triangle-shader-desc :metal-macos))
         (shader (sokol:make-shader shader-desc))
         (pipeline-descriptor (make-instance 'sokol:pipeline-desc
                                            :shader shader
                                            :vertex-layouts '(:sg-vertexformat-float3
                                                             :sg-vertexformat-float4))))
    (sokol:make-pipeline pipeline-descriptor)))

(defun reload-resources ()
  "Reload all graphics resources - call from REPL to see changes.
  Example: (reload-resources) after modifying create-vertex-buffer"
  (format t "Reloading resources...~%")
  (setf *vertex-buffer* (create-vertex-buffer))
  (setf *pipeline* (create-pipeline))
  (format t "Resources reloaded!~%"))

;;; Sokol callbacks
(defmethod sokol:init ()
  "Initialize graphics and resources"
  (format t "Initializing sokol graphics...~%")
  ;; Setup sokol_gfx with default descriptor
  (sokol:setup-gfx)

  ;; Create initial resources
  (setf *vertex-buffer* (create-vertex-buffer))
  (setf *pipeline* (create-pipeline))

  (format t "Initialization complete!~%")
  (format t "~%TIP: Try modifying *clear-color* from the REPL:~%")
  (format t "  (setf example-swank-triangle::*clear-color* '(0.5 0.0 0.5 1.0))~%")
  (format t "Or increment rotation:~%")
  (format t "  (incf example-swank-triangle::*rotation* 0.5)~%~%"))

(defmethod sokol:frame ()
  "Render a frame - this can be modified interactively!"
  ;; Auto-increment rotation (or control it from REPL)
  (incf *rotation* 0.01)

  ;; Begin rendering pass with configurable clear color
  (let ((pass (make-instance 'sokol:pass-desc
                            :action (make-instance 'sokol:pass-action
                                                  :clear-color (apply #'sokol:make-color *clear-color*)
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
  (format t "Cleaning up...~%")
  (sokol:shutdown-gfx))

(defmethod sokol:event (event-data)
  "Handle input events"
  (declare (ignore event-data)))

(defun run-triangle-example ()
  "Run the triangle example (blocking - use run-triangle-example-threaded for interactive dev)"
  (sokol:run-app :width 640
                 :height 480
                 :title "Triangle (Swank)"))

(defun run-triangle-example-threaded ()
  "Run the triangle example with Swank for interactive development.

  IMPORTANT: On macOS, windows MUST run on the main thread.
  This function will:
  1. Start Swank on a background thread (if not already running)
  2. Run the sokol app on the MAIN thread (blocking)

  Before calling this, make sure you're ready to have your REPL blocked.
  Connect from Emacs AFTER the window appears.

  Usage:
    ;; Run this in your REPL (it will block)
    (run-triangle-example-threaded)

    ;; After the window appears, connect from Emacs:
    ;; M-x slime-connect RET localhost RET 4005

    ;; Then from the SLIME REPL you can modify:
    (setf example-swank-triangle::*clear-color* '(1.0 0.0 0.0 1.0))  ; Red background
    (setf example-swank-triangle::*rotation* 0.0)  ; Reset rotation
    (example-swank-triangle::reload-resources)     ; Reload graphics resources"

  ;; Start Swank in background if not already running
  (unless (find-if (lambda (thread)
                     (search "Swank" (bt:thread-name thread)))
                   (bt:all-threads))
    (format t "Starting Swank server in background...~%")
    (bt:make-thread
     (lambda ()
       (swank:create-server :port 4005 :dont-close t))
     :name "Swank-server-thread")
    (sleep 0.5)  ; Give Swank time to start
    (format t "Swank server started on port 4005~%")
    (format t "Connect from Emacs: M-x slime-connect RET localhost RET 4005~%~%"))

  (format t "Starting triangle example on MAIN thread (as required by macOS)...~%")
  (format t "The window will appear shortly. Your REPL will be blocked.~%")
  (format t "Connect from Emacs to interact while the app runs.~%~%")

  ;; Run on main thread (blocking)
  (run-triangle-example))

;;; Quick start guide (macOS compatible):
;;;
;;; 1. Load and run the example (this will start Swank and block your REPL)
;;;    (asdf:load-system :example-swank-triangle)
;;;    (in-package :example-swank-triangle)
;;;    (run-triangle-example-threaded)
;;;
;;; 2. After the window appears, connect from Emacs:
;;;    M-x slime-connect RET localhost RET 4005
;;;
;;; 3. In the NEW Emacs SLIME REPL, try interactive modifications:
;;;    (setf example-swank-triangle::*clear-color* '(0.2 0.0 0.3 1.0))  ; Dark purple
;;;    (setf example-swank-triangle::*clear-color* '(0.0 0.2 0.2 1.0))  ; Dark cyan
;;;    (setf example-swank-triangle::*rotation* 0.0)                     ; Reset rotation
;;;
;;; 4. Modify the sokol:frame method and recompile (C-c C-c in Emacs)
;;;    Changes take effect immediately!
;;;
;;; Note: On macOS, GUI windows MUST run on the main thread, so your original
;;;       REPL will be blocked. Use the Swank-connected REPL from Emacs instead.
