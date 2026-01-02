;;;; shape-wrapper.lisp
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

;;;; CLOS wrappers for sokol-shape
;;;; Primitive shape generation (plane, box, sphere, cylinder, torus)

;;; ============================================================================
;;; CLOS Class Definitions
;;; ============================================================================

(define-struct-wrapper shape-buffer (:struct sokol-shape:sshape-buffer)
  ((valid :type boolean :initform nil :accessor shape-buffer-valid))
  :documentation "Shape buffer containing vertex and index data.")

(define-struct-wrapper shape-plane (:struct sokol-shape:sshape-plane)
  ((width :type float :initform 1.0 :accessor shape-plane-width)
   (depth :type float :initform 1.0 :accessor shape-plane-depth)
   (tiles :type integer :initform 1 :accessor shape-plane-tiles)
   (color :type (unsigned-byte 32) :initform #xFFFFFFFF :accessor shape-plane-color)
   (random-colors :type boolean :initform nil :accessor shape-plane-random-colors)
   (merge :type boolean :initform nil :accessor shape-plane-merge))
  :documentation "Plane generation parameters.")

(define-struct-wrapper shape-box (:struct sokol-shape:sshape-box)
  ((width :type float :initform 1.0 :accessor shape-box-width)
   (height :type float :initform 1.0 :accessor shape-box-height)
   (depth :type float :initform 1.0 :accessor shape-box-depth)
   (tiles :type integer :initform 1 :accessor shape-box-tiles)
   (color :type (unsigned-byte 32) :initform #xFFFFFFFF :accessor shape-box-color)
   (random-colors :type boolean :initform nil :accessor shape-box-random-colors)
   (merge :type boolean :initform nil :accessor shape-box-merge))
  :documentation "Box generation parameters.")

(define-struct-wrapper shape-sphere (:struct sokol-shape:sshape-sphere)
  ((radius :type float :initform 0.5 :accessor shape-sphere-radius)
   (slices :type integer :initform 16 :accessor shape-sphere-slices)
   (stacks :type integer :initform 16 :accessor shape-sphere-stacks)
   (color :type (unsigned-byte 32) :initform #xFFFFFFFF :accessor shape-sphere-color)
   (random-colors :type boolean :initform nil :accessor shape-sphere-random-colors)
   (merge :type boolean :initform nil :accessor shape-sphere-merge))
  :documentation "Sphere generation parameters.")

(define-struct-wrapper shape-cylinder (:struct sokol-shape:sshape-cylinder)
  ((radius :type float :initform 0.5 :accessor shape-cylinder-radius)
   (height :type float :initform 1.0 :accessor shape-cylinder-height)
   (slices :type integer :initform 16 :accessor shape-cylinder-slices)
   (stacks :type integer :initform 1 :accessor shape-cylinder-stacks)
   (color :type (unsigned-byte 32) :initform #xFFFFFFFF :accessor shape-cylinder-color)
   (random-colors :type boolean :initform nil :accessor shape-cylinder-random-colors)
   (merge :type boolean :initform nil :accessor shape-cylinder-merge))
  :documentation "Cylinder generation parameters.")

(define-struct-wrapper shape-torus (:struct sokol-shape:sshape-torus)
  ((radius :type float :initform 0.5 :accessor shape-torus-radius)
   (ring-radius :type float :initform 0.2 :accessor shape-torus-ring-radius)
   (sides :type integer :initform 16 :accessor shape-torus-sides)
   (rings :type integer :initform 16 :accessor shape-torus-rings)
   (color :type (unsigned-byte 32) :initform #xFFFFFFFF :accessor shape-torus-color)
   (random-colors :type boolean :initform nil :accessor shape-torus-random-colors)
   (merge :type boolean :initform nil :accessor shape-torus-merge))
  :documentation "Torus generation parameters.")

;;; ============================================================================
;;; to-foreign Methods
;;; ============================================================================

(defmethod to-foreign ((params shape-plane) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of params)))))
    (zero-memory ptr '(:struct sokol-shape:sshape-plane))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-plane) 'sokol-shape::width)
          (coerce (shape-plane-width params) 'single-float))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-plane) 'sokol-shape::depth)
          (coerce (shape-plane-depth params) 'single-float))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-plane) 'sokol-shape::tiles)
          (shape-plane-tiles params))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-plane) 'sokol-shape::color)
          (shape-plane-color params))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-plane) 'sokol-shape::random-colors)
          (shape-plane-random-colors params))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-plane) 'sokol-shape::merge)
          (shape-plane-merge params))
    ptr))

(defmethod to-foreign ((params shape-box) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of params)))))
    (zero-memory ptr '(:struct sokol-shape:sshape-box))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-box) 'sokol-shape::width)
          (coerce (shape-box-width params) 'single-float))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-box) 'sokol-shape::height)
          (coerce (shape-box-height params) 'single-float))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-box) 'sokol-shape::depth)
          (coerce (shape-box-depth params) 'single-float))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-box) 'sokol-shape::tiles)
          (shape-box-tiles params))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-box) 'sokol-shape::color)
          (shape-box-color params))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-box) 'sokol-shape::random-colors)
          (shape-box-random-colors params))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-box) 'sokol-shape::merge)
          (shape-box-merge params))
    ptr))

(defmethod to-foreign ((params shape-sphere) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of params)))))
    (zero-memory ptr '(:struct sokol-shape:sshape-sphere))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-sphere) 'sokol-shape::radius)
          (coerce (shape-sphere-radius params) 'single-float))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-sphere) 'sokol-shape::slices)
          (shape-sphere-slices params))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-sphere) 'sokol-shape::stacks)
          (shape-sphere-stacks params))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-sphere) 'sokol-shape::color)
          (shape-sphere-color params))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-sphere) 'sokol-shape::random-colors)
          (shape-sphere-random-colors params))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-sphere) 'sokol-shape::merge)
          (shape-sphere-merge params))
    ptr))

(defmethod to-foreign ((params shape-cylinder) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of params)))))
    (zero-memory ptr '(:struct sokol-shape:sshape-cylinder))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-cylinder) 'sokol-shape::radius)
          (coerce (shape-cylinder-radius params) 'single-float))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-cylinder) 'sokol-shape::height)
          (coerce (shape-cylinder-height params) 'single-float))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-cylinder) 'sokol-shape::slices)
          (shape-cylinder-slices params))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-cylinder) 'sokol-shape::stacks)
          (shape-cylinder-stacks params))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-cylinder) 'sokol-shape::color)
          (shape-cylinder-color params))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-cylinder) 'sokol-shape::random-colors)
          (shape-cylinder-random-colors params))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-cylinder) 'sokol-shape::merge)
          (shape-cylinder-merge params))
    ptr))

(defmethod to-foreign ((params shape-torus) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of params)))))
    (zero-memory ptr '(:struct sokol-shape:sshape-torus))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-torus) 'sokol-shape::radius)
          (coerce (shape-torus-radius params) 'single-float))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-torus) 'sokol-shape::ring-radius)
          (coerce (shape-torus-ring-radius params) 'single-float))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-torus) 'sokol-shape::sides)
          (shape-torus-sides params))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-torus) 'sokol-shape::rings)
          (shape-torus-rings params))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-torus) 'sokol-shape::color)
          (shape-torus-color params))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-torus) 'sokol-shape::random-colors)
          (shape-torus-random-colors params))
    (setf (foreign-slot-value ptr '(:struct sokol-shape:sshape-torus) 'sokol-shape::merge)
          (shape-torus-merge params))
    ptr))

;;; ============================================================================
;;; Buffer Management
;;; ============================================================================

(defun make-shape-buffer (vertex-count index-count)
  "Allocate a shape buffer for VERTEX-COUNT vertices and INDEX-COUNT indices."
  (with-foreign-object (buf-ptr '(:struct sokol-shape:sshape-buffer))
    (zero-memory buf-ptr '(:struct sokol-shape:sshape-buffer))

    ;; Allocate vertex buffer
    (let* ((vertex-size (foreign-type-size '(:struct sokol-shape:sshape-vertex)))
           (vertex-data (foreign-alloc :uint8 :count (* vertex-count vertex-size)))
           (vertices-ptr (foreign-slot-pointer buf-ptr '(:struct sokol-shape:sshape-buffer) 'sokol-shape::vertices))
           (vbuf-ptr (foreign-slot-pointer vertices-ptr '(:struct sokol-shape:sshape-buffer-item) 'sokol-shape::buffer)))
      (setf (foreign-slot-value vbuf-ptr '(:struct sokol-shape:sshape-range) 'sokol-shape::ptr) vertex-data)
      (setf (foreign-slot-value vbuf-ptr '(:struct sokol-shape:sshape-range) 'sokol-shape::size)
            (* vertex-count vertex-size)))

    ;; Allocate index buffer
    (let* ((index-size (foreign-type-size :uint16))
           (index-data (foreign-alloc :uint16 :count index-count))
           (indices-ptr (foreign-slot-pointer buf-ptr '(:struct sokol-shape:sshape-buffer) 'sokol-shape::indices))
           (ibuf-ptr (foreign-slot-pointer indices-ptr '(:struct sokol-shape:sshape-buffer-item) 'sokol-shape::buffer)))
      (setf (foreign-slot-value ibuf-ptr '(:struct sokol-shape:sshape-range) 'sokol-shape::ptr) index-data)
      (setf (foreign-slot-value ibuf-ptr '(:struct sokol-shape:sshape-range) 'sokol-shape::size)
            (* index-count index-size)))

    buf-ptr))

(defun free-shape-buffer (buf-ptr)
  "Free the memory associated with a shape buffer."
  (let ((vertices-ptr (foreign-slot-pointer buf-ptr '(:struct sokol-shape:sshape-buffer) 'sokol-shape::vertices))
        (indices-ptr (foreign-slot-pointer buf-ptr '(:struct sokol-shape:sshape-buffer) 'sokol-shape::indices)))
    (let ((vbuf-ptr (foreign-slot-pointer vertices-ptr '(:struct sokol-shape:sshape-buffer-item) 'sokol-shape::buffer))
          (ibuf-ptr (foreign-slot-pointer indices-ptr '(:struct sokol-shape:sshape-buffer-item) 'sokol-shape::buffer)))
      (foreign-free (foreign-slot-value vbuf-ptr '(:struct sokol-shape:sshape-range) 'sokol-shape::ptr))
      (foreign-free (foreign-slot-value ibuf-ptr '(:struct sokol-shape:sshape-range) 'sokol-shape::ptr)))
    (foreign-free buf-ptr)))

;;; ============================================================================
;;; Shape Building Functions
;;; ============================================================================

(defun build-plane (buf-ptr params)
  "Build a plane shape into BUF-PTR using PARAMS."
  (with-foreign-struct (params-ptr params)
    (sokol-shape:sshape-build-plane buf-ptr params-ptr)))

(defun build-box (buf-ptr params)
  "Build a box shape into BUF-PTR using PARAMS."
  (with-foreign-struct (params-ptr params)
    (sokol-shape:sshape-build-box buf-ptr params-ptr)))

(defun build-sphere (buf-ptr params)
  "Build a sphere shape into BUF-PTR using PARAMS."
  (with-foreign-struct (params-ptr params)
    (sokol-shape:sshape-build-sphere buf-ptr params-ptr)))

(defun build-cylinder (buf-ptr params)
  "Build a cylinder shape into BUF-PTR using PARAMS."
  (with-foreign-struct (params-ptr params)
    (sokol-shape:sshape-build-cylinder buf-ptr params-ptr)))

(defun build-torus (buf-ptr params)
  "Build a torus shape into BUF-PTR using PARAMS."
  (with-foreign-struct (params-ptr params)
    (sokol-shape:sshape-build-torus buf-ptr params-ptr)))

;;; ============================================================================
;;; Color Utilities
;;; ============================================================================

(defun shape-color-4f (r g b a)
  "Create a packed color from RGBA floats (0.0-1.0)."
  (sokol-shape:sshape-color-4f r g b a))

(defun shape-color-3f (r g b)
  "Create a packed color from RGB floats (0.0-1.0), alpha=1.0."
  (sokol-shape:sshape-color-3f r g b))

(defun shape-color-4b (r g b a)
  "Create a packed color from RGBA bytes (0-255)."
  (sokol-shape:sshape-color-4b r g b a))

(defun shape-color-3b (r g b)
  "Create a packed color from RGB bytes (0-255), alpha=255."
  (sokol-shape:sshape-color-3b r g b))
