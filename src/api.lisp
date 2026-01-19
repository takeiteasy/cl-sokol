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

;;;; Utilities

(defun environment ()
  "Get the current graphics environment from sokol_glue."
  (sokol-glue:sglue-environment))

(defun swapchain ()
  "Get the current swapchain from sokol_glue."
  (sokol-glue:sglue-swapchain))

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

(defclass event ()
  ((type :initarg :type :accessor event-type
         :documentation "Event type (keyword, e.g., :SAPP-EVENTTYPE-KEY-DOWN)")
   (frame-count :initarg :frame-count :accessor event-frame-count
                :documentation "Frame counter")
   (key-code :initarg :key-code :accessor event-key-code
             :documentation "Key code (keyword, e.g., :SAPP-KEYCODE-SPACE)")
   (char-code :initarg :char-code :accessor event-char-code
              :documentation "Unicode character code")
   (key-repeat :initarg :key-repeat :accessor event-key-repeat
               :documentation "True if this is a key repeat")
   (modifiers :initarg :modifiers :accessor event-modifiers
              :documentation "Modifier keys bitmask")
   (mouse-button :initarg :mouse-button :accessor event-mouse-button
                 :documentation "Mouse button (keyword, e.g., :SAPP-MOUSEBUTTON-LEFT)")
   (mouse-x :initarg :mouse-x :accessor event-mouse-x
            :documentation "Mouse X position")
   (mouse-y :initarg :mouse-y :accessor event-mouse-y
            :documentation "Mouse Y position")
   (mouse-dx :initarg :mouse-dx :accessor event-mouse-dx
             :documentation "Mouse X delta")
   (mouse-dy :initarg :mouse-dy :accessor event-mouse-dy
             :documentation "Mouse Y delta")
   (scroll-x :initarg :scroll-x :accessor event-scroll-x
             :documentation "Scroll X delta")
   (scroll-y :initarg :scroll-y :accessor event-scroll-y
             :documentation "Scroll Y delta")
   (num-touches :initarg :num-touches :accessor event-num-touches
                :documentation "Number of active touches")
   (window-width :initarg :window-width :accessor event-window-width
                 :documentation "Window width")
   (window-height :initarg :window-height :accessor event-window-height
                  :documentation "Window height")
   (framebuffer-width :initarg :framebuffer-width :accessor event-framebuffer-width
                      :documentation "Framebuffer width")
   (framebuffer-height :initarg :framebuffer-height :accessor event-framebuffer-height
                       :documentation "Framebuffer height"))
  (:documentation "Wrapper for sokol sapp_event struct"))

(defun wrap-event (event-ptr)
  "Convert a foreign sapp_event pointer to a CLOS event object."
  (make-instance 'event
    :type (foreign-enum-keyword 'sokol-app:sapp-event-type
                                (foreign-slot-value event-ptr '(:struct sokol-app:sapp-event) 'sokol-app::type))
    :frame-count (foreign-slot-value event-ptr '(:struct sokol-app:sapp-event) 'sokol-app::frame-count)
    :key-code (foreign-enum-keyword 'sokol-app:sapp-keycode
                                    (foreign-slot-value event-ptr '(:struct sokol-app:sapp-event) 'sokol-app::key-code))
    :char-code (foreign-slot-value event-ptr '(:struct sokol-app:sapp-event) 'sokol-app::char-code)
    :key-repeat (foreign-slot-value event-ptr '(:struct sokol-app:sapp-event) 'sokol-app::key-repeat)
    :modifiers (foreign-slot-value event-ptr '(:struct sokol-app:sapp-event) 'sokol-app::modifiers)
    :mouse-button (foreign-enum-keyword 'sokol-app:sapp-mousebutton
                                        (foreign-slot-value event-ptr '(:struct sokol-app:sapp-event) 'sokol-app::mouse-button))
    :mouse-x (foreign-slot-value event-ptr '(:struct sokol-app:sapp-event) 'sokol-app::mouse-x)
    :mouse-y (foreign-slot-value event-ptr '(:struct sokol-app:sapp-event) 'sokol-app::mouse-y)
    :mouse-dx (foreign-slot-value event-ptr '(:struct sokol-app:sapp-event) 'sokol-app::mouse-dx)
    :mouse-dy (foreign-slot-value event-ptr '(:struct sokol-app:sapp-event) 'sokol-app::mouse-dy)
    :scroll-x (foreign-slot-value event-ptr '(:struct sokol-app:sapp-event) 'sokol-app::scroll-x)
    :scroll-y (foreign-slot-value event-ptr '(:struct sokol-app:sapp-event) 'sokol-app::scroll-y)
    :num-touches (foreign-slot-value event-ptr '(:struct sokol-app:sapp-event) 'sokol-app::num-touches)
    :window-width (foreign-slot-value event-ptr '(:struct sokol-app:sapp-event) 'sokol-app::window-width)
    :window-height (foreign-slot-value event-ptr '(:struct sokol-app:sapp-event) 'sokol-app::window-height)
    :framebuffer-width (foreign-slot-value event-ptr '(:struct sokol-app:sapp-event) 'sokol-app::framebuffer-width)
    :framebuffer-height (foreign-slot-value event-ptr '(:struct sokol-app:sapp-event) 'sokol-app::framebuffer-height)))

;;; Event type predicates

(defun key-down-p (event)
  "Return T if EVENT is a key-down event."
  (eq (event-type event) :SAPP-EVENTTYPE-KEY-DOWN))

(defun key-up-p (event)
  "Return T if EVENT is a key-up event."
  (eq (event-type event) :SAPP-EVENTTYPE-KEY-UP))

(defun char-input-p (event)
  "Return T if EVENT is a character input event."
  (eq (event-type event) :SAPP-EVENTTYPE-CHAR))

(defun mouse-down-p (event)
  "Return T if EVENT is a mouse button down event."
  (eq (event-type event) :SAPP-EVENTTYPE-MOUSE-DOWN))

(defun mouse-up-p (event)
  "Return T if EVENT is a mouse button up event."
  (eq (event-type event) :SAPP-EVENTTYPE-MOUSE-UP))

(defun mouse-move-p (event)
  "Return T if EVENT is a mouse move event."
  (eq (event-type event) :SAPP-EVENTTYPE-MOUSE-MOVE))

(defun mouse-scroll-p (event)
  "Return T if EVENT is a mouse scroll event."
  (eq (event-type event) :SAPP-EVENTTYPE-MOUSE-SCROLL))

(defun mouse-enter-p (event)
  "Return T if EVENT is a mouse enter event."
  (eq (event-type event) :SAPP-EVENTTYPE-MOUSE-ENTER))

(defun mouse-leave-p (event)
  "Return T if EVENT is a mouse leave event."
  (eq (event-type event) :SAPP-EVENTTYPE-MOUSE-LEAVE))

(defun window-resized-p (event)
  "Return T if EVENT is a window resize event."
  (eq (event-type event) :SAPP-EVENTTYPE-RESIZED))

(defun window-iconified-p (event)
  "Return T if EVENT is a window iconified event."
  (eq (event-type event) :SAPP-EVENTTYPE-ICONIFIED))

(defun window-restored-p (event)
  "Return T if EVENT is a window restored event."
  (eq (event-type event) :SAPP-EVENTTYPE-RESTORED))

(defun window-focused-p (event)
  "Return T if EVENT is a window focused event."
  (eq (event-type event) :SAPP-EVENTTYPE-FOCUSED))

(defun window-unfocused-p (event)
  "Return T if EVENT is a window unfocused event."
  (eq (event-type event) :SAPP-EVENTTYPE-UNFOCUSED))

(defun quit-requested-p (event)
  "Return T if EVENT is a quit requested event."
  (eq (event-type event) :SAPP-EVENTTYPE-QUIT-REQUESTED))

;;; Modifier key helpers

(defun shift-pressed-p (event)
  "Return T if shift modifier is pressed."
  (logtest (event-modifiers event) #x1)) ; SAPP_MODIFIER_SHIFT = 1

(defun ctrl-pressed-p (event)
  "Return T if ctrl modifier is pressed."
  (logtest (event-modifiers event) #x2)) ; SAPP_MODIFIER_CTRL = 2

(defun alt-pressed-p (event)
  "Return T if alt modifier is pressed."
  (logtest (event-modifiers event) #x4)) ; SAPP_MODIFIER_ALT = 4

(defun super-pressed-p (event)
  "Return T if super/command modifier is pressed."
  (logtest (event-modifiers event) #x8)) ; SAPP_MODIFIER_SUPER = 8

;;; Mouse button helpers

(defun left-button-p (event)
  "Return T if event is for left mouse button."
  (eq (event-mouse-button event) :SAPP-MOUSEBUTTON-LEFT))

(defun right-button-p (event)
  "Return T if event is for right mouse button."
  (eq (event-mouse-button event) :SAPP-MOUSEBUTTON-RIGHT))

(defun middle-button-p (event)
  "Return T if event is for middle mouse button."
  (eq (event-mouse-button event) :SAPP-MOUSEBUTTON-MIDDLE))

;;; Convenience functions for common checks

(defun key-pressed-p (event keycode)
  "Return T if EVENT is a key-down for KEYCODE (keyword, e.g., :SAPP-KEYCODE-SPACE)."
  (and (key-down-p event)
       (eq (event-key-code event) keycode)))

(defun mouse-clicked-p (event button)
  "Return T if EVENT is a mouse-down for BUTTON (keyword, e.g., :SAPP-MOUSEBUTTON-LEFT)."
  (and (mouse-down-p event)
       (eq (event-mouse-button event) button)))

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
