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

(defcallback init-cb-wrapper :void ()
  (let ((desc (%sokol:sokol-default-sgdesc)))
    (unwind-protect
        (%sokol:sg-setup desc)
        
      (cffi:foreign-free desc)))
  (init-cb))

(defcallback frame-cb-wrapper :void ()
  (let ((pass-action (foreign-alloc '(:struct %sokol:sg-pass-action))))
    (unwind-protect
        (progn
         (%sokol:sg-begin-default-pass pass-action (%sokol:sapp-width) (%sokol:sapp-height))
         (%sokol:sg-end-pass)
         (%sokol:sg-commit))
      (cffi:foreign-free pass-action)))
  (frame-cb))

(defcallback event-cb-wrapper :void ((event :pointer))
  (event-cb event))

(defcallback cleanup-cb-wrapper :void ()
  (cleanup-cb)
  (%sokol:sg-shutdown))

(defun run (win-width win-height &optional win-title)
  (let ((desc (foreign-alloc '(:struct %sokol:sapp-desc))))
    (unwind-protect
        (with-foreign-slots (((w %sokol::width)
                              (h %sokol::height)
                              (title %sokol::window-title)
                              (init %sokol::init-cb)
                              (frame %sokol::frame-cb)
                              (event %sokol::event-cb)
                              (cleanup %sokol::cleanup-cb)) desc (:struct %sokol:sapp-desc))
          (with-foreign-string (foreign-win-title (if win-title win-title "sokol"))
            (setf
              w win-width
              h win-height
              title foreign-win-title
              init (callback init-cb-wrapper)
              frame (callback frame-cb-wrapper)
              event (callback event-cb-wrapper)
              cleanup (callback cleanup-cb-wrapper))
            (sapp-run desc)))
      (foreign-free desc))))
