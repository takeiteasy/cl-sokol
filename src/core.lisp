;;;; core.lisp
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

;;;; Generic functions for struct conversion

(defgeneric to-foreign (obj &optional ptr)
  (:documentation "Convert a CLOS object to a foreign struct.
If PTR is provided, fills that foreign object, otherwise allocates a new one.
Returns the foreign pointer."))

(defgeneric from-foreign (ptr type)
  (:documentation "Convert a foreign struct to a CLOS object of TYPE."))

(defgeneric foreign-type-of (obj)
  (:documentation "Return the CFFI type symbol for this CLOS object."))

;;;; Macro for defining struct wrappers

(defmacro define-struct-wrapper (name foreign-type slots &key documentation)
  "Define a CLOS wrapper for a Sokol struct.

  SLOTS is a list of (slot-name &key type initform accessor).
  Automatically generates to-foreign and from-foreign methods."
  (let ((slot-defs (loop for slot-spec in slots
                        for slot-name = (if (listp slot-spec) (first slot-spec) slot-spec)
                        for type = (getf (cdr slot-spec) :type t)
                        for initform = (getf (cdr slot-spec) :initform nil)
                        for accessor = (getf (cdr slot-spec) :accessor
                                            (intern (format nil "~A-~A" name slot-name)))
                        collect `(,slot-name
                                  :initarg ,(intern (symbol-name slot-name) :keyword)
                                  :initform ,initform
                                  :accessor ,accessor
                                  :type ,type))))
    `(progn
       (defclass ,name ()
         ,slot-defs
         ,@(when documentation `((:documentation ,documentation))))

       (defmethod foreign-type-of ((obj ,name))
         ',foreign-type))))

;;;; Helper macro for working with foreign structs

(defmacro with-foreign-struct ((var obj) &body body)
  "Temporarily convert CLOS object to foreign struct for use in BODY.
  The foreign struct is automatically freed after BODY completes."
  (let ((ptr-var (gensym "PTR")))
    `(let ((,ptr-var (to-foreign ,obj)))
       (unwind-protect
            (let ((,var ,ptr-var))
              ,@body)
         (foreign-free ,ptr-var)))))

;;;; Base method for allocating foreign structs

(defmethod to-foreign :around (obj &optional ptr)
  "Default behavior: allocate if no pointer provided, then fill it."
  (let ((foreign-ptr (or ptr (foreign-alloc (foreign-type-of obj)))))
    (call-next-method obj foreign-ptr)))

;;;; Application lifecycle generic functions

(defgeneric init ()
  (:documentation "Called once when the application starts.
  Override this method to initialize your application."))

(defgeneric frame ()
  (:documentation "Called each frame to render and update.
  Override this method to implement your frame logic."))

(defgeneric cleanup ()
  (:documentation "Called once when the application shuts down.
  Override this method to clean up resources."))

(defgeneric event (event-data)
  (:documentation "Called for each input event (keyboard, mouse, etc.).
  EVENT-DATA is a foreign pointer to sapp_event.
  Override this method to handle events."))

;;;; Default implementations (do nothing)

(defmethod init ()
  ;; Default: do nothing
  nil)

(defmethod frame ()
  ;; Default: do nothing
  nil)

(defmethod cleanup ()
  ;; Default: do nothing
  nil)

(defmethod event (event-data)
  ;; Default: ignore events
  (declare (ignore event-data))
  nil)

;;;; Utility functions

(defun zero-memory (ptr type)
  "Zero out a foreign struct."
  (loop for i from 0 below (foreign-type-size type)
        do (setf (mem-aref ptr :uint8 i) 0)))

(defun make-foreign-struct (type &key (zero t))
  "Allocate a foreign struct, optionally zeroing it."
  (let ((ptr (foreign-alloc type)))
    (when zero
      (zero-memory ptr type))
    ptr))
