;;;; example-simple-window.lisp
;;;; bindings test

(defpackage :example-simple-window
  (:use :cl)
  (:export :run))

(in-package :example-simple-window)

;; Load the libraries
(cffi:define-foreign-library libsokol-app
  (:darwin "src/lib/libsokol_app.dylib")
  (:unix "src/lib/libsokol_app.so")
  (:windows "src/lib/sokol_app.dll")
  (t (:default "libsokol_app")))

(cffi:use-foreign-library libsokol-app)

(defvar *pass-action* nil "Pass action for clearing the screen")
(defvar *frame-count* 0 "Frame counter")

(defun init-cb ()
  "Initialize graphics"
  (format t "Initializing...~%")

  ;; Setup sokol_gfx
  (cffi:with-foreign-object (desc '(:struct sg:sg-desc))
    ;; Zero out the desc struct
    (loop for i from 0 below (cffi:foreign-type-size '(:struct sg:sg-desc))
          do (setf (cffi:mem-aref desc :uint8 i) 0))

    ;; Get environment from sokol_glue (via pointer to avoid CFFI struct-by-value issues)
    (let ((env-ptr (cffi:foreign-slot-pointer desc '(:struct sg:sg-desc) 'sg::environment)))
      (sglue:sglue-environment-ptr env-ptr))

    ;; Initialize graphics
    (sg:sg-setup desc))

  (format t "Graphics initialized~%")

  ;; Create pass action to clear to red
  (setf *pass-action* (cffi:foreign-alloc '(:struct sg:sg-pass-action)))

  ;; Zero out the structure
  (loop for i from 0 below (cffi:foreign-type-size '(:struct sg:sg-pass-action))
        do (setf (cffi:mem-aref *pass-action* :uint8 i) 0))

  ;; Set clear color to red (r=1.0, g=0.0, b=0.0, a=1.0)
  (let* ((colors-ptr (cffi:foreign-slot-pointer *pass-action*
                                                  '(:struct sg:sg-pass-action)
                                                  'sg::colors))
         (color0-ptr colors-ptr))

    ;; Set load action to CLEAR
    (setf (cffi:foreign-slot-value color0-ptr
                                     '(:struct sg:sg-color-attachment-action)
                                     'sg::load-action)
          1)  ; SG_LOADACTION_CLEAR = 1

    ;; Set clear color to red
    (let ((clear-value-ptr (cffi:foreign-slot-pointer color0-ptr
                                                       '(:struct sg:sg-color-attachment-action)
                                                       'sg::clear-value)))
      (setf (cffi:foreign-slot-value clear-value-ptr '(:struct sg:sg-color) 'sg::r) 1.0)
      (setf (cffi:foreign-slot-value clear-value-ptr '(:struct sg:sg-color) 'sg::g) 0.0)
      (setf (cffi:foreign-slot-value clear-value-ptr '(:struct sg:sg-color) 'sg::b) 0.0)
      (setf (cffi:foreign-slot-value clear-value-ptr '(:struct sg:sg-color) 'sg::a) 1.0)))

  (format t "Initialization complete!~%"))

(defun frame-cb ()
  "Render a frame"
  (handler-case
      (progn
        (incf *frame-count*)
        (when (zerop (mod *frame-count* 60))
          (format t "Frame ~A~%" *frame-count*))

        (cffi:with-foreign-object (pass '(:struct sg:sg-pass))
          ;; Zero out pass struct
          (loop for i from 0 below (cffi:foreign-type-size '(:struct sg:sg-pass))
                do (setf (cffi:mem-aref pass :uint8 i) 0))

          ;; Copy pass action struct directly (mem-to-mem copy)
          (let ((action-ptr (cffi:foreign-slot-pointer pass '(:struct sg:sg-pass) 'sg::action))
                (action-size (cffi:foreign-type-size '(:struct sg:sg-pass-action))))
            (dotimes (i action-size)
              (setf (cffi:mem-aref action-ptr :uint8 i)
                    (cffi:mem-aref *pass-action* :uint8 i))))

          ;; Get swapchain from sokol_glue (via pointer to avoid CFFI struct-by-value issues)
          (let ((sc-ptr (cffi:foreign-slot-pointer pass '(:struct sg:sg-pass) 'sg::swapchain)))
            (sglue:sglue-swapchain-ptr sc-ptr))

          ;; Render
          (sg:sg-begin-pass pass))

        (sg:sg-end-pass)
        (sg:sg-commit))
    (error (e)
      (format t "Error in frame-cb: ~A~%" e))))

(defun cleanup-cb ()
  "Cleanup resources"
  (format t "Cleaning up...~%")
  (when *pass-action*
    (cffi:foreign-free *pass-action*)
    (setf *pass-action* nil))
  (sg:sg-shutdown)
  (format t "Cleanup complete!~%"))

(defun event-cb (event)
  "Handle input events"
  ;; Check event type
  (let ((event-type (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::type)))
    ;; SAPP_EVENTTYPE_KEY_DOWN = 10
    (when (= event-type 10)
      (let ((keycode (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::key-code)))
        ;; SAPP_KEYCODE_ESCAPE = 256
        (when (= keycode 256)
          (format t "ESC pressed, requesting quit...~%")
          (sapp:sapp-request-quit))))))

;; Define CFFI callbacks
(cffi:defcallback init-callback :void ()
  (init-cb))

(cffi:defcallback frame-callback :void ()
  (frame-cb))

(cffi:defcallback cleanup-callback :void ()
  (cleanup-cb))

(cffi:defcallback event-callback :void ((event (:pointer (:struct sapp:sapp-event))))
  (event-cb event))

(defun run ()
  "Run the simple window example"
  (format t "~%Simple Window Example - cl-sokol~%")
  (format t "=================================~%~%")
  (format t "This will open a window with a red background.~%")
  (format t "Press ESC or close the window to exit.~%~%")

  ;; Disable floating point traps for sokol (it uses NaN for some initialization)
  #+sbcl
  (sb-int:set-floating-point-modes :traps nil)

  ;; Must run on main thread for macOS/Windows
  (trivial-main-thread:with-body-in-main-thread ()
    (cffi:with-foreign-object (desc '(:struct sapp:sapp-desc))
      ;; Zero out the descriptor
      (loop for i from 0 below (cffi:foreign-type-size '(:struct sapp:sapp-desc))
            do (setf (cffi:mem-aref desc :uint8 i) 0))

      ;; Set callbacks
      (setf (cffi:foreign-slot-value desc '(:struct sapp:sapp-desc) 'sapp::init-cb)
            (cffi:callback init-callback))
      (setf (cffi:foreign-slot-value desc '(:struct sapp:sapp-desc) 'sapp::frame-cb)
            (cffi:callback frame-callback))
      (setf (cffi:foreign-slot-value desc '(:struct sapp:sapp-desc) 'sapp::cleanup-cb)
            (cffi:callback cleanup-callback))
      (setf (cffi:foreign-slot-value desc '(:struct sapp:sapp-desc) 'sapp::event-cb)
            (cffi:callback event-callback))

      ;; Set window properties
      (setf (cffi:foreign-slot-value desc '(:struct sapp:sapp-desc) 'sapp::width) 800)
      (setf (cffi:foreign-slot-value desc '(:struct sapp:sapp-desc) 'sapp::height) 600)

      ;; Use cffi:with-foreign-string to keep the string alive for the duration
      (cffi:with-foreign-string (title "Simple Window - cl-sokol")
        (setf (cffi:foreign-slot-value desc '(:struct sapp:sapp-desc) 'sapp::window-title)
              title)

        (format t "Starting application...~%~%")

        ;; Run the application (blocks until window is closed)
        (sapp:sapp-run desc))))

  (format t "~%Application exited.~%"))
