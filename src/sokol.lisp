;;;; sokol.lisp

(in-package #:sokol)

(defgeneric init-cb ())
(defgeneric frame-cb ())
(defgeneric event-cb (event))
(defgeneric cleanup-cb ())

(defmethod init-cb () '())
(defmethod frame-cb () '())
(defmethod event-cb (event)
  (declare (ignore event)))
(defmethod cleanup-cb () '())

(declaim (inline memcpy))
(defcfun "memcpy" :void
  (dest :pointer)
  (src :pointer)
  (n :size))

(defcallback init-cb-wrapper :void ()
  (let ((desc (%sokol:sokol-default-sgdesc)))
    (unwind-protect
         (sg-setup desc)
      (foreign-free desc)))
  (init-cb))

(defcallback frame-cb-wrapper :void ()
  (frame-cb))

(defcallback event-cb-wrapper :void ((event :pointer))
  (event-cb event))

(defcallback cleanup-cb-wrapper :void ()
  (cleanup-cb)
  (sg-shutdown))

(defun run (win-width win-height &optional win-title)
  (let ((desc (foreign-alloc '(:struct %sapp-desc))))
    (unwind-protect
         (with-foreign-slots (((w %sokol::width)
                               (h %sokol::height)
                               (title %sokol::window-title)
                               (init %sokol::init-cb)
                               (frame %sokol::frame-cb)
                               (event %sokol::event-cb)
                               (cleanup %sokol::cleanup-cb))
                              desc (:struct %sapp-desc))
           (with-foreign-string (foreign-win-title (if win-title win-title "sokol"))
             (setf
              w win-width
              h win-height
              title foreign-win-title
              init (callback init-cb-wrapper)
              frame (callback frame-cb-wrapper)
              event (callback event-cb-wrapper)
              cleanup (callback cleanup-cb-wrapper))
             (%sokol:sapp-run desc)))
      (foreign-free desc))))
