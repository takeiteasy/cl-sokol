;;;; api.lisp
;;;; Copyright (C) 2025 George Watson
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package :sokol)

;;;; Graphics Setup/Teardown

(defun setup-gfx (&optional (desc (make-instance 'gfx-desc)))
  "Initialize the graphics subsystem.
  DESC is a GFX-DESC object (optional)."
  ;; Disable floating point traps for Metal (required on SBCL)
  #+sbcl
  (sb-int:set-floating-point-modes :traps nil)
  (with-foreign-struct (desc-ptr desc)
    (sokol-gfx:sg-setup desc-ptr)))

(defun shutdown-gfx ()
  "Shut down the graphics subsystem."
  (sokol-gfx:sg-shutdown))

(defun query-backend ()
  "Query the current graphics backend."
  (sokol-gfx:sg-query-backend))

;;;; Resource Creation

(defun make-buffer (desc)
  "Create a buffer from a BUFFER-DESC.
  Returns a foreign struct (sg_buffer) - store this for later use."
  (let ((buffer-ptr (foreign-alloc '(:struct sokol-gfx:sg-buffer))))
    (with-foreign-struct (desc-ptr desc)
      (setf (mem-ref buffer-ptr '(:struct sokol-gfx:sg-buffer))
            (sokol-gfx:sg-make-buffer desc-ptr)))
    buffer-ptr))

(defun make-shader (desc-ptr)
  "Create a shader from a foreign shader descriptor.
  DESC-PTR should be a pointer to sg_shader_desc (from generated shaders).
  Returns a foreign struct (sg_shader)."
  (sokol-gfx:sg-make-shader desc-ptr))

(defun make-image (desc)
  "Create an image/texture from an IMAGE-DESC.
  Returns a foreign struct (sg_image)."
  (let ((image-ptr (foreign-alloc '(:struct sokol-gfx:sg-image))))
    (with-foreign-struct (desc-ptr desc)
      (setf (mem-ref image-ptr '(:struct sokol-gfx:sg-image))
            (sokol-gfx:sg-make-image desc-ptr)))
    image-ptr))

(defun make-sampler (desc)
  "Create a sampler from a SAMPLER-DESC.
  Returns a foreign struct (sg_sampler)."
  (with-foreign-struct (desc-ptr desc)
    (sokol-gfx:sg-make-sampler desc-ptr)))

(defun make-view (desc)
  "Create a texture view from a VIEW-DESC.
  Returns a foreign struct (sg_view)."
  (with-foreign-struct (desc-ptr desc)
    (sokol-gfx:sg-make-view desc-ptr)))

(defun make-pipeline (desc)
  "Create a pipeline from a PIPELINE-DESC.
  Returns a foreign struct (sg_pipeline)."
  (let ((pipeline-ptr (foreign-alloc '(:struct sokol-gfx:sg-pipeline))))
    (with-foreign-struct (desc-ptr desc)
      (setf (mem-ref pipeline-ptr '(:struct sokol-gfx:sg-pipeline))
            (sokol-gfx:sg-make-pipeline desc-ptr)))
    pipeline-ptr))

;;;; Rendering

(defun begin-pass (desc)
  "Begin a rendering pass with the given PASS-DESC."
  (with-foreign-struct (desc-ptr desc)
    (sokol-gfx:sg-begin-pass desc-ptr)))

(defun end-pass ()
  "End the current rendering pass."
  (sokol-gfx:sg-end-pass))

(defun commit ()
  "Commit the current frame."
  (sokol-gfx:sg-commit))

(defun apply-pipeline (pipeline)
  "Apply a pipeline for rendering.
  PIPELINE is a foreign pointer to sg_pipeline."
  (sokol-gfx:sg-apply-pipeline (mem-ref pipeline '(:struct sokol-gfx:sg-pipeline))))

(defun apply-bindings (bindings-obj)
  "Apply resource bindings for rendering.
  BINDINGS-OBJ is a BINDINGS CLOS object."
  (with-foreign-struct (bindings-ptr bindings-obj)
    (sokol-gfx:sg-apply-bindings bindings-ptr)))

(defun draw (base-element num-elements num-instances)
  "Issue a draw call.
  BASE-ELEMENT: first element to draw
  NUM-ELEMENTS: number of elements to draw
  NUM-INSTANCES: number of instances to draw"
  (sokol-gfx:sg-draw base-element num-elements num-instances))

(defun apply-uniforms (stage-slot data-ptr data-size)
  "Apply shader uniforms.
  STAGE-SLOT: uniform block slot (typically 0)
  DATA-PTR: foreign pointer to uniform data
  DATA-SIZE: size of uniform data in bytes"
  (cffi:with-foreign-object (range '(:struct sokol-gfx:sg-range))
    (setf (cffi:foreign-slot-value range '(:struct sokol-gfx:sg-range) 'sokol-gfx::ptr) data-ptr)
    (setf (cffi:foreign-slot-value range '(:struct sokol-gfx:sg-range) 'sokol-gfx::size) data-size)
    (sokol-gfx:sg-apply-uniforms stage-slot range)))

;;;; Resource Destruction

(defun destroy-buffer (buffer)
  "Destroy a buffer resource.
  BUFFER can be either a foreign pointer (from make-buffer) or a raw sg_buffer struct."
  (if (cffi:pointerp buffer)
      ;; It's a pointer allocated by make-buffer
      (progn
        (sokol-gfx:sg-destroy-buffer (mem-ref buffer '(:struct sokol-gfx:sg-buffer)))
        (cffi:foreign-free buffer))
      ;; It's a raw struct (by value)
      (sokol-gfx:sg-destroy-buffer buffer)))

(defun destroy-pipeline (pipeline)
  "Destroy a pipeline resource.
  PIPELINE can be either a foreign pointer (from make-pipeline) or a raw sg_pipeline struct."
  (if (cffi:pointerp pipeline)
      ;; It's a pointer allocated by make-pipeline
      (progn
        (sokol-gfx:sg-destroy-pipeline (mem-ref pipeline '(:struct sokol-gfx:sg-pipeline)))
        (cffi:foreign-free pipeline))
      ;; It's a raw struct (by value)
      (sokol-gfx:sg-destroy-pipeline pipeline)))

(defun destroy-shader (shader)
  "Destroy a shader resource.
  SHADER can be either a foreign pointer or a raw sg_shader struct."
  (if (cffi:pointerp shader)
      ;; It's a pointer
      (progn
        (sokol-gfx:sg-destroy-shader (mem-ref shader '(:struct sokol-gfx:sg-shader)))
        (cffi:foreign-free shader))
      ;; It's a raw struct (by value)
      (sokol-gfx:sg-destroy-shader shader)))

(defun destroy-image (image)
  "Destroy an image/texture resource.
  IMAGE can be either a foreign pointer (from make-image) or a raw sg_image struct."
  (if (cffi:pointerp image)
      ;; It's a pointer allocated by make-image
      (progn
        (sokol-gfx:sg-destroy-image (mem-ref image '(:struct sokol-gfx:sg-image)))
        (cffi:foreign-free image))
      ;; It's a raw struct (by value)
      (sokol-gfx:sg-destroy-image image)))

(defun destroy-sampler (sampler)
  "Destroy a sampler resource."
  (sokol-gfx:sg-destroy-sampler sampler))

;;;; Utilities

(defun environment ()
  "Get the current graphics environment from sokol_glue."
  (sokol-glue:sglue-environment))

(defun swapchain ()
  "Get the current swapchain from sokol_glue."
  (sokol-glue:sglue-swapchain))

;;;; Application Control

(defun request-quit ()
  "Request the application to quit.
   This will send a QUIT_REQUESTED event to the event handler,
   allowing the application to optionally cancel the quit."
  (sokol-app:sapp-request-quit))

(defun quit-app ()
  "Immediately quit the application without sending a quit request event.
   Use this after confirming the user wants to quit."
  (sokol-app:sapp-quit))

(defun cancel-quit ()
  "Cancel a pending quit request.
   Call this from the event handler when receiving QUIT_REQUESTED event
   if you want to prevent the application from quitting."
  (sokol-app:sapp-cancel-quit))

;;;; Application Lifecycle

(defvar *user-app-instance* nil
  "Holds the user's application instance for callbacks.")

(defcallback %init-cb :void ()
  "Internal callback that dispatches to the generic INIT function."
  (when *user-app-instance*
    (init)))

(defcallback %frame-cb :void ()
  "Internal callback that dispatches to the generic FRAME function."
  (when *user-app-instance*
    (frame)))

(defcallback %cleanup-cb :void ()
  "Internal callback that dispatches to the generic CLEANUP function."
  (when *user-app-instance*
    (cleanup)))

(defcallback %event-cb :void ((event (:pointer (:struct sokol-app:sapp-event))))
  "Internal callback that dispatches to the generic EVENT function."
  (when *user-app-instance*
    (event event)))

(defun run-app (&key
                  (width 640)
                  (height 480)
                  (title "Sokol Application")
                  (high-dpi t)
                  (swap-interval 1)
                  (sample-count 1)
                  fullscreen
                  (app-instance t))
  "Run a Sokol application.

  WIDTH, HEIGHT: window dimensions
  TITLE: window title
  HIGH-DPI: enable high-DPI/Retina support (default T)
  SWAP-INTERVAL: vsync interval, 1 for vsync on (default 1)
  SAMPLE-COUNT: MSAA sample count (default 1, no MSAA)
  FULLSCREEN: start in fullscreen mode
  APP-INSTANCE: set to T to use generic functions, or provide an object whose methods will be called

  This function will call the generic functions INIT, FRAME, CLEANUP, and EVENT.
  You should define methods for these to implement your application logic.

  Example:
    (defmethod init ()
      (format t \"Initializing~%\"))

    (defmethod frame ()
      ;; Draw something
      )

    (run-app :title \"My Game\")"

  ;; Disable floating point traps on SBCL
  #+sbcl (sb-int:set-floating-point-modes :traps nil)

  ;; Store the app instance for callbacks
  (setf *user-app-instance* app-instance)

  ;; Create and fill the app descriptor
  (with-foreign-object (desc '(:struct sokol-app:sapp-desc))
    (zero-memory desc '(:struct sokol-app:sapp-desc))

    ;; Set callbacks
    (setf (foreign-slot-value desc '(:struct sokol-app:sapp-desc) 'sokol-app::init-cb)
          (callback %init-cb))
    (setf (foreign-slot-value desc '(:struct sokol-app:sapp-desc) 'sokol-app::frame-cb)
          (callback %frame-cb))
    (setf (foreign-slot-value desc '(:struct sokol-app:sapp-desc) 'sokol-app::cleanup-cb)
          (callback %cleanup-cb))
    (setf (foreign-slot-value desc '(:struct sokol-app:sapp-desc) 'sokol-app::event-cb)
          (callback %event-cb))

    ;; Set window properties
    (setf (foreign-slot-value desc '(:struct sokol-app:sapp-desc) 'sokol-app::width) width)
    (setf (foreign-slot-value desc '(:struct sokol-app:sapp-desc) 'sokol-app::height) height)
    (setf (foreign-slot-value desc '(:struct sokol-app:sapp-desc) 'sokol-app::window-title)
          (foreign-string-alloc title))
    (setf (foreign-slot-value desc '(:struct sokol-app:sapp-desc) 'sokol-app::high-dpi)
          (if high-dpi 1 0))
    (setf (foreign-slot-value desc '(:struct sokol-app:sapp-desc) 'sokol-app::swap-interval)
          swap-interval)
    (setf (foreign-slot-value desc '(:struct sokol-app:sapp-desc) 'sokol-app::sample-count)
          sample-count)
    (when fullscreen
      (setf (foreign-slot-value desc '(:struct sokol-app:sapp-desc) 'sokol-app::fullscreen)
            1))

    ;; Run the application
    (sokol-app:sapp-run desc)))

;;;; Event Wrapper API

(defun event-type (event)
  "Get the type of a Sokol event."
  (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::type))

(defun event-key-code (event)
  "Get the key code from a keyboard event."
  (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::key-code))

(defun event-char-code (event)
  "Get the character code from a keyboard event."
  (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::char-code))

(defun event-key-repeat (event)
  "Check if a key event is a repeat (key held down).
  Returns T if this is a repeat event, NIL otherwise."
  (not (zerop (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::key-repeat))))

(defun event-frame-count (event)
  "Get the frame count when the event occurred."
  (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::frame-count))

(defun event-window-width (event)
  "Get the window width from a resize event."
  (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::window-width))

(defun event-window-height (event)
  "Get the window height from a resize event."
  (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::window-height))

(defun event-framebuffer-width (event)
  "Get the framebuffer width from a resize event."
  (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::framebuffer-width))

(defun event-framebuffer-height (event)
  "Get the framebuffer height from a resize event."
  (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::framebuffer-height))

(defun event-modifiers (event)
  "Get the modifier flags from an event."
  (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::modifiers))

(defun event-mouse-button (event)
  "Get the mouse button from a mouse event."
  (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::mouse-button))

(defun event-mouse-x (event)
  "Get the mouse X position from an event."
  (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::mouse-x))

(defun event-mouse-y (event)
  "Get the mouse Y position from an event."
  (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::mouse-y))

(defun event-mouse-dx (event)
  "Get the mouse delta X from an event."
  (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::mouse-dx))

(defun event-mouse-dy (event)
  "Get the mouse delta Y from an event."
  (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::mouse-dy))

(defun event-scroll-x (event)
  "Get the scroll X from an event."
  (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::scroll-x))

(defun event-scroll-y (event)
  "Get the scroll Y from an event."
  (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::scroll-y))

(defun event-num-touches (event)
  "Get the number of touches from an event."
  (cffi:foreign-slot-value event '(:struct sapp:sapp-event) 'sapp::num-touches))

(defun event-touches (event)
  "Get the touches array from an event."
  (cffi:foreign-slot-pointer event '(:struct sapp:sapp-event) 'sapp::touches))

(defun event-touch-identifier (event index)
  "Get the touch identifier at INDEX from an event."
  (let ((touches (event-touches event)))
    (cffi:foreign-slot-value
     (cffi:mem-aptr touches '(:struct sapp:sapp-touchpoint) index)
     '(:struct sapp:sapp-touchpoint)
     'sapp::identifier)))

(defun event-touch-pos-x (event index)
  "Get the touch X position at INDEX from an event."
  (let ((touches (event-touches event)))
    (cffi:foreign-slot-value
     (cffi:mem-aptr touches '(:struct sapp:sapp-touchpoint) index)
     '(:struct sapp:sapp-touchpoint)
     'sapp::pos-x)))

(defun event-touch-pos-y (event index)
  "Get the touch Y position at INDEX from an event."
  (let ((touches (event-touches event)))
    (cffi:foreign-slot-value
     (cffi:mem-aptr touches '(:struct sapp:sapp-touchpoint) index)
     '(:struct sapp:sapp-touchpoint)
     'sapp::pos-y)))

;;;; Time Functions

(defun setup-time ()
  "Initialize the time subsystem.
  Call this once before using any time functions."
  (sokol-time:stm-setup))

(defun now ()
  "Get the current time tick.
  Returns a uint64 tick value."
  (sokol-time:stm-now))

(defun time-diff (new-ticks old-ticks)
  "Calculate the difference between two time ticks.
  Returns the number of ticks elapsed."
  (sokol-time:stm-diff new-ticks old-ticks))

(defun time-since (start-ticks)
  "Calculate time elapsed since START-TICKS.
  Returns the number of ticks elapsed."
  (sokol-time:stm-since start-ticks))

(defun laptime (last-time-ptr)
  "Get lap time and update the last time pointer.
  LAST-TIME-PTR should be a foreign pointer to uint64.
  Returns ticks since last call."
  (sokol-time:stm-laptime last-time-ptr))

(defun ticks-to-seconds (ticks)
  "Convert ticks to seconds (as double float)."
  (sokol-time:stm-sec ticks))

(defun ticks-to-ms (ticks)
  "Convert ticks to milliseconds (as double float)."
  (sokol-time:stm-ms ticks))

(defun ticks-to-us (ticks)
  "Convert ticks to microseconds (as double float)."
  (sokol-time:stm-us ticks))

(defun ticks-to-ns (ticks)
  "Convert ticks to nanoseconds (as double float)."
  (sokol-time:stm-ns ticks))

;;;; Audio Functions

(defun setup-audio (&optional (desc (make-instance 'audio-desc)))
  "Initialize the audio subsystem.
  DESC is an AUDIO-DESC object (optional).
  Returns T if successful, NIL otherwise."
  ;; Disable floating point traps for audio (required on SBCL)
  #+sbcl
  (sb-int:set-floating-point-modes :traps nil)
  (with-foreign-struct (desc-ptr desc)
    (sokol-audio:saudio-setup desc-ptr))
  (audio-valid-p))

(defun shutdown-audio ()
  "Shut down the audio subsystem."
  (sokol-audio:saudio-shutdown))

(defun audio-valid-p ()
  "Check if audio subsystem is valid and initialized.
  Returns T if valid, NIL otherwise."
  (sokol-audio:saudio-isvalid))

(defun audio-sample-rate ()
  "Get the audio sample rate (e.g., 44100).
  Returns sample rate as integer."
  (sokol-audio:saudio-sample-rate))

(defun audio-buffer-frames ()
  "Get the number of frames in the audio buffer.
  Returns frame count as integer."
  (sokol-audio:saudio-buffer-frames))

(defun audio-channels ()
  "Get the number of audio channels (1=mono, 2=stereo).
  Returns channel count as integer."
  (sokol-audio:saudio-channels))

(defun audio-suspended-p ()
  "Check if audio playback is suspended.
  Returns T if suspended, NIL otherwise."
  (sokol-audio:saudio-suspended))

(defun audio-expect ()
  "Get the number of sample frames expected to be pushed.
  Use this to determine how many frames to push.
  Returns expected frame count."
  (sokol-audio:saudio-expect))

(defun push-audio-samples (samples num-frames)
  "Push audio samples to the playback buffer.
  SAMPLES: foreign pointer to float array of samples
  NUM-FRAMES: number of frames to push
  Returns number of frames actually pushed."
  (sokol-audio:saudio-push samples num-frames))
