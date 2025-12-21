;;;; example-events.lisp
;;;; Demonstrates the event wrapper API for handling keyboard and mouse input

(defpackage :example-events
  (:use :cl)
  (:export #:run-events-example))

(in-package :example-events)

(cffi:define-foreign-library libsokol-app
  (:darwin "src/lib/libsokol_app.dylib")
  (:unix "src/lib/libsokol_app.so")
  (:windows "src/lib/sokol_app.dll")
  (t (:default "libsokol_app")))

(cffi:use-foreign-library libsokol-app)

;;; State
(defvar *pipeline* nil)
(defvar *vertex-buffer* nil)
(defvar *clear-color* (list 0.1 0.1 0.1 1.0))
(defvar *event-log* nil "Recent events for display")

;;; Graphics initialization (same as triangle example)
(defmethod sokol:init ()
  "Initialize graphics"
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

  ;; Create shader and pipeline
  (let* ((shader-desc (example-wrapper-triangle:triangle-shader-desc :metal-macos))
         (shader (sokol:make-shader shader-desc))
         (pipeline-descriptor (make-instance 'sokol:pipeline-desc
                                            :shader shader
                                            :vertex-layouts '(:sg-vertexformat-float3
                                                             :sg-vertexformat-float4))))
    (setf *pipeline* (sokol:make-pipeline pipeline-descriptor)))

  (format t "~%Event Handling Example~%")
  (format t "=====================~%")
  (format t "Try:~%")
  (format t "  - Press 1, 2, 3 to change background color~%")
  (format t "  - Press SPACE to reset color~%")
  (format t "  - Click mouse to see position~%")
  (format t "  - Press ESC to quit~%~%"))

(defmethod sokol:frame ()
  "Render a frame"
  (let ((pass (make-instance 'sokol:pass-desc
                            :action (make-instance 'sokol:pass-action
                                                  :clear-color (apply #'sokol:make-color *clear-color*)
                                                  :load-action :clear
                                                  :store-action :store))))
    (sokol:begin-pass pass))

  (sokol:apply-pipeline *pipeline*)
  (let ((bindings-obj (make-instance 'sokol:bindings
                                     :vertex-buffers (list *vertex-buffer*))))
    (sokol:apply-bindings bindings-obj))
  (sokol:draw 0 3 1)
  (sokol:end-pass)
  (sokol:commit))

(defmethod sokol:cleanup ()
  "Clean up resources"
  (sokol:shutdown-gfx))

(defmethod sokol:event (event-ptr)
  "Handle input events using the event wrapper API"
  ;; Wrap the raw pointer to get a nice CLOS object
  (let ((event (sokol:wrap-event event-ptr)))

    ;; Keyboard events
    (when (sokol:key-down-p event)
      (format t "Key pressed: ~A~%" (sokol:event-key-code event))

      (cond
        ;; Number keys change color
        ((sokol:key-pressed-p event :SAPP-KEYCODE-1)
         (setf *clear-color* '(0.5 0.0 0.0 1.0))
         (format t "  -> Red background~%"))

        ((sokol:key-pressed-p event :SAPP-KEYCODE-2)
         (setf *clear-color* '(0.0 0.5 0.0 1.0))
         (format t "  -> Green background~%"))

        ((sokol:key-pressed-p event :SAPP-KEYCODE-3)
         (setf *clear-color* '(0.0 0.0 0.5 1.0))
         (format t "  -> Blue background~%"))

        ;; Space resets
        ((sokol:key-pressed-p event :SAPP-KEYCODE-SPACE)
         (setf *clear-color* '(0.1 0.1 0.1 1.0))
         (format t "  -> Reset to dark gray~%"))

        ;; ESC to quit
        ((sokol:key-pressed-p event :SAPP-KEYCODE-ESCAPE)
         (format t "  -> ESC pressed, requesting quit~%")
         (sokol-app:sapp-request-quit))))

    ;; Mouse events
    (when (sokol:mouse-down-p event)
      (format t "Mouse ~A clicked at (~,1f, ~,1f)~%"
              (sokol:event-mouse-button event)
              (sokol:event-mouse-x event)
              (sokol:event-mouse-y event))

      ;; Different colors for different buttons
      (cond
        ((sokol:left-button-p event)
         (setf *clear-color* '(0.3 0.3 0.0 1.0))
         (format t "  -> Left button = Yellow~%"))

        ((sokol:right-button-p event)
         (setf *clear-color* '(0.3 0.0 0.3 1.0))
         (format t "  -> Right button = Magenta~%"))

        ((sokol:middle-button-p event)
         (setf *clear-color* '(0.0 0.3 0.3 1.0))
         (format t "  -> Middle button = Cyan~%"))))

    ;; Mouse scroll
    (when (sokol:mouse-scroll-p event)
      (format t "Mouse scrolled: (~,2f, ~,2f)~%"
              (sokol:event-scroll-x event)
              (sokol:event-scroll-y event)))

    ;; Window events
    (when (sokol:window-resized-p event)
      (format t "Window resized to ~Dx~D~%"
              (sokol:event-window-width event)
              (sokol:event-window-height event)))

    ;; Check modifiers (on any key press)
    (when (and (sokol:key-down-p event)
               (or (sokol:shift-pressed-p event)
                   (sokol:ctrl-pressed-p event)
                   (sokol:alt-pressed-p event)
                   (sokol:super-pressed-p event)))
      (format t "  Modifiers: ~{~A~^+~}~%"
              (remove nil
                      (list (when (sokol:shift-pressed-p event) "SHIFT")
                            (when (sokol:ctrl-pressed-p event) "CTRL")
                            (when (sokol:alt-pressed-p event) "ALT")
                            (when (sokol:super-pressed-p event) "SUPER")))))))

(defun run-events-example ()
  "Run the event handling example"
  (sokol:run-app :width 640
                 :height 480
                 :title "Event Handling Example"))

(export 'run-events-example)

;;; Usage:
;;;
;;; (asdf:load-system :example-events)
;;; (example-events:run-events-example)
;;;
;;; Try pressing keys 1, 2, 3, SPACE
;;; Click the mouse
;;; Press ESC to quit
