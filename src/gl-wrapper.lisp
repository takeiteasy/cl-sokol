;;;; gl-wrapper.lisp
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

;;;; CLOS wrappers for sokol-gl
;;;; OpenGL 1.x style immediate-mode rendering on top of sokol-gfx

;;; ============================================================================
;;; CLOS Class Definitions
;;; ============================================================================

(define-struct-wrapper gl-context (:struct sokol-gl:sgl-context)
  ((id :type (unsigned-byte 32) :initform 0 :accessor gl-context-id))
  :documentation "Sokol-GL rendering context handle.")

(define-struct-wrapper gl-pipeline (:struct sokol-gl:sgl-pipeline)
  ((id :type (unsigned-byte 32) :initform 0 :accessor gl-pipeline-id))
  :documentation "Sokol-GL pipeline state object handle.")

(define-struct-wrapper gl-error (:struct sokol-gl:sgl-error)
  ((any :type boolean :initform nil :accessor gl-error-any)
   (vertices-full :type boolean :initform nil :accessor gl-error-vertices-full)
   (uniforms-full :type boolean :initform nil :accessor gl-error-uniforms-full)
   (commands-full :type boolean :initform nil :accessor gl-error-commands-full)
   (stack-overflow :type boolean :initform nil :accessor gl-error-stack-overflow)
   (stack-underflow :type boolean :initform nil :accessor gl-error-stack-underflow)
   (no-context :type boolean :initform nil :accessor gl-error-no-context))
  :documentation "Sokol-GL error state.")

(define-struct-wrapper gl-desc (:struct sokol-gl:sgl-desc)
  ((max-vertices :type integer :initform 65536 :accessor gl-desc-max-vertices)
   (max-commands :type integer :initform 16384 :accessor gl-desc-max-commands)
   (context-pool-size :type integer :initform 4 :accessor gl-desc-context-pool-size)
   (pipeline-pool-size :type integer :initform 64 :accessor gl-desc-pipeline-pool-size)
   (color-format :type keyword :initform :default :accessor gl-desc-color-format)
   (depth-format :type keyword :initform :default :accessor gl-desc-depth-format)
   (sample-count :type integer :initform 1 :accessor gl-desc-sample-count)
   (face-winding :type keyword :initform :ccw :accessor gl-desc-face-winding))
  :documentation "Sokol-GL setup configuration.")

(define-struct-wrapper gl-context-desc (:struct sokol-gl:sgl-context-desc)
  ((max-vertices :type integer :initform 65536 :accessor gl-context-desc-max-vertices)
   (max-commands :type integer :initform 16384 :accessor gl-context-desc-max-commands)
   (color-format :type keyword :initform :default :accessor gl-context-desc-color-format)
   (depth-format :type keyword :initform :default :accessor gl-context-desc-depth-format)
   (sample-count :type integer :initform 1 :accessor gl-context-desc-sample-count))
  :documentation "Context creation configuration.")

;;; ============================================================================
;;; to-foreign Methods
;;; ============================================================================

(defmethod to-foreign ((ctx gl-context) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of ctx)))))
    (setf (foreign-slot-value ptr '(:struct sokol-gl:sgl-context) 'sokol-gl::id)
          (gl-context-id ctx))
    ptr))

(defmethod to-foreign ((pip gl-pipeline) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of pip)))))
    (setf (foreign-slot-value ptr '(:struct sokol-gl:sgl-pipeline) 'sokol-gl::id)
          (gl-pipeline-id pip))
    ptr))

(defmethod to-foreign ((desc gl-desc) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of desc)))))
    (zero-memory ptr '(:struct sokol-gl:sgl-desc))
    (setf (foreign-slot-value ptr '(:struct sokol-gl:sgl-desc) 'sokol-gl::max-vertices)
          (gl-desc-max-vertices desc))
    (setf (foreign-slot-value ptr '(:struct sokol-gl:sgl-desc) 'sokol-gl::max-commands)
          (gl-desc-max-commands desc))
    (setf (foreign-slot-value ptr '(:struct sokol-gl:sgl-desc) 'sokol-gl::context-pool-size)
          (gl-desc-context-pool-size desc))
    (setf (foreign-slot-value ptr '(:struct sokol-gl:sgl-desc) 'sokol-gl::pipeline-pool-size)
          (gl-desc-pipeline-pool-size desc))

    ;; Color format (convert keyword to sokol-gfx pixel format)
    (unless (eq (gl-desc-color-format desc) :default)
      (let ((color-fmt-ptr (foreign-slot-pointer ptr '(:struct sokol-gl:sgl-desc) 'sokol-gl::color-format)))
        (setf (mem-ref color-fmt-ptr :int)
              (foreign-enum-value 'sokol-gfx:sg-pixel-format
                                (intern (format nil "SG-PIXELFORMAT-~A"
                                              (string-upcase (gl-desc-color-format desc)))
                                       :keyword)))))

    ;; Depth format
    (unless (eq (gl-desc-depth-format desc) :default)
      (let ((depth-fmt-ptr (foreign-slot-pointer ptr '(:struct sokol-gl:sgl-desc) 'sokol-gl::depth-format)))
        (setf (mem-ref depth-fmt-ptr :int)
              (foreign-enum-value 'sokol-gfx:sg-pixel-format
                                (intern (format nil "SG-PIXELFORMAT-~A"
                                              (string-upcase (gl-desc-depth-format desc)))
                                       :keyword)))))

    (setf (foreign-slot-value ptr '(:struct sokol-gl:sgl-desc) 'sokol-gl::sample-count)
          (gl-desc-sample-count desc))

    ;; Face winding
    (let ((winding-ptr (foreign-slot-pointer ptr '(:struct sokol-gl:sgl-desc) 'sokol-gl::face-winding)))
      (setf (mem-ref winding-ptr :int)
            (foreign-enum-value 'sokol-gfx:sg-face-winding
                              (intern (format nil "SG-FACEWINDING-~A"
                                            (string-upcase (gl-desc-face-winding desc)))
                                     :keyword))))
    ptr))

(defmethod to-foreign ((desc gl-context-desc) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of desc)))))
    (zero-memory ptr '(:struct sokol-gl:sgl-context-desc))
    (setf (foreign-slot-value ptr '(:struct sokol-gl:sgl-context-desc) 'sokol-gl::max-vertices)
          (gl-context-desc-max-vertices desc))
    (setf (foreign-slot-value ptr '(:struct sokol-gl:sgl-context-desc) 'sokol-gl::max-commands)
          (gl-context-desc-max-commands desc))

    ;; Color format
    (unless (eq (gl-context-desc-color-format desc) :default)
      (let ((color-fmt-ptr (foreign-slot-pointer ptr '(:struct sokol-gl:sgl-context-desc) 'sokol-gl::color-format)))
        (setf (mem-ref color-fmt-ptr :int)
              (foreign-enum-value 'sokol-gfx:sg-pixel-format
                                (intern (format nil "SG-PIXELFORMAT-~A"
                                              (string-upcase (gl-context-desc-color-format desc)))
                                       :keyword)))))

    ;; Depth format
    (unless (eq (gl-context-desc-depth-format desc) :default)
      (let ((depth-fmt-ptr (foreign-slot-pointer ptr '(:struct sokol-gl:sgl-context-desc) 'sokol-gl::depth-format)))
        (setf (mem-ref depth-fmt-ptr :int)
              (foreign-enum-value 'sokol-gfx:sg-pixel-format
                                (intern (format nil "SG-PIXELFORMAT-~A"
                                              (string-upcase (gl-context-desc-depth-format desc)))
                                       :keyword)))))

    (setf (foreign-slot-value ptr '(:struct sokol-gl:sgl-context-desc) 'sokol-gl::sample-count)
          (gl-context-desc-sample-count desc))
    ptr))

;;; ============================================================================
;;; from-foreign Methods
;;; ============================================================================

(defmethod from-foreign (ptr (type (eql 'gl-context)))
  (make-instance 'gl-context
                 :id (foreign-slot-value ptr '(:struct sokol-gl:sgl-context) 'sokol-gl::id)))

(defmethod from-foreign (ptr (type (eql 'gl-pipeline)))
  (make-instance 'gl-pipeline
                 :id (foreign-slot-value ptr '(:struct sokol-gl:sgl-pipeline) 'sokol-gl::id)))

(defmethod from-foreign (ptr (type (eql 'gl-error)))
  (make-instance 'gl-error
                 :any (foreign-slot-value ptr '(:struct sokol-gl:sgl-error) 'sokol-gl::any)
                 :vertices-full (foreign-slot-value ptr '(:struct sokol-gl:sgl-error) 'sokol-gl::vertices-full)
                 :uniforms-full (foreign-slot-value ptr '(:struct sokol-gl:sgl-error) 'sokol-gl::uniforms-full)
                 :commands-full (foreign-slot-value ptr '(:struct sokol-gl:sgl-error) 'sokol-gl::commands-full)
                 :stack-overflow (foreign-slot-value ptr '(:struct sokol-gl:sgl-error) 'sokol-gl::stack-overflow)
                 :stack-underflow (foreign-slot-value ptr '(:struct sokol-gl:sgl-error) 'sokol-gl::stack-underflow)
                 :no-context (foreign-slot-value ptr '(:struct sokol-gl:sgl-error) 'sokol-gl::no-context)))

;;; ============================================================================
;;; Setup/Teardown
;;; ============================================================================

(defun setup-gl (&optional (desc (make-instance 'gl-desc)))
  "Initialize sokol-gl. Must be called after setup-gfx."
  (with-foreign-struct (desc-ptr desc)
    (sokol-gl:sgl-setup desc-ptr)))

(defun shutdown-gl ()
  "Shutdown sokol-gl."
  (sokol-gl:sgl-shutdown))

;;; ============================================================================
;;; Context Management
;;; ============================================================================

(defun make-gl-context (&optional (desc (make-instance 'gl-context-desc)))
  "Create a new GL context. Returns a gl-context handle."
  (with-foreign-struct (desc-ptr desc)
    (let ((ctx-struct (sokol-gl:sgl-make-context desc-ptr)))
      (with-foreign-object (ctx-ptr '(:struct sokol-gl:sgl-context))
        (setf (mem-ref ctx-ptr '(:struct sokol-gl:sgl-context)) ctx-struct)
        (from-foreign ctx-ptr 'gl-context)))))

(defun destroy-gl-context (ctx)
  "Destroy a GL context."
  (with-foreign-struct (ctx-ptr ctx)
    (sokol-gl:sgl-destroy-context (mem-ref ctx-ptr '(:struct sokol-gl:sgl-context)))))

(defun set-gl-context (ctx)
  "Set the active GL context."
  (with-foreign-struct (ctx-ptr ctx)
    (sokol-gl:sgl-set-context (mem-ref ctx-ptr '(:struct sokol-gl:sgl-context)))))

(defun get-gl-context ()
  "Get the currently active GL context."
  (let ((ctx-struct (sokol-gl:sgl-get-context)))
    (with-foreign-object (ctx-ptr '(:struct sokol-gl:sgl-context))
      (setf (mem-ref ctx-ptr '(:struct sokol-gl:sgl-context)) ctx-struct)
      (from-foreign ctx-ptr 'gl-context))))

(defun default-gl-context ()
  "Get the default GL context."
  (let ((ctx-struct (sokol-gl:sgl-default-context)))
    (with-foreign-object (ctx-ptr '(:struct sokol-gl:sgl-context))
      (setf (mem-ref ctx-ptr '(:struct sokol-gl:sgl-context)) ctx-struct)
      (from-foreign ctx-ptr 'gl-context))))

(defmacro with-gl-context (ctx &body body)
  "Execute BODY with CTX as the active GL context, then restore the previous context."
  (let ((old-ctx (gensym "OLD-CTX")))
    `(let ((,old-ctx (get-gl-context)))
       (unwind-protect
            (progn
              (set-gl-context ,ctx)
              ,@body)
         (set-gl-context ,old-ctx)))))

;;; ============================================================================
;;; Error Handling
;;; ============================================================================

(defun gl-error ()
  "Get error state for the current context."
  (let ((err-struct (sokol-gl:sgl-error)))
    (with-foreign-object (err-ptr '(:struct sokol-gl:sgl-error))
      (setf (mem-ref err-ptr '(:struct sokol-gl:sgl-error)) err-struct)
      (from-foreign err-ptr 'gl-error))))

(defun gl-context-error (ctx)
  "Get error state for a specific context."
  (with-foreign-struct (ctx-ptr ctx)
    (let ((err-struct (sokol-gl:sgl-context-error (mem-ref ctx-ptr '(:struct sokol-gl:sgl-context)))))
      (with-foreign-object (err-ptr '(:struct sokol-gl:sgl-error))
        (setf (mem-ref err-ptr '(:struct sokol-gl:sgl-error)) err-struct)
        (from-foreign err-ptr 'gl-error)))))

;;; ============================================================================
;;; Rendering
;;; ============================================================================

(defun gl-draw ()
  "Draw the current context's accumulated geometry."
  (sokol-gl:sgl-draw))

(defun gl-draw-context (ctx)
  "Draw a specific context's accumulated geometry."
  (with-foreign-struct (ctx-ptr ctx)
    (sokol-gl:sgl-context-draw (mem-ref ctx-ptr '(:struct sokol-gl:sgl-context)))))

(defun gl-draw-layer (layer-id)
  "Draw a specific layer of the current context."
  (sokol-gl:sgl-draw-layer layer-id))

(defun gl-draw-context-layer (ctx layer-id)
  "Draw a specific layer of a specific context."
  (with-foreign-struct (ctx-ptr ctx)
    (sokol-gl:sgl-context-draw-layer (mem-ref ctx-ptr '(:struct sokol-gl:sgl-context)) layer-id)))

;;; ============================================================================
;;; State Management
;;; ============================================================================

(defun gl-defaults ()
  "Reset all GL state to defaults."
  (sokol-gl:sgl-defaults))

(defun gl-viewport (x y w h &optional (origin-top-left t))
  "Set the viewport rectangle."
  (sokol-gl:sgl-viewport x y w h origin-top-left))

(defun gl-viewportf (x y w h &optional (origin-top-left t))
  "Set the viewport rectangle with float coordinates."
  (sokol-gl:sgl-viewportf x y w h origin-top-left))

(defun gl-scissor (x y w h &optional (origin-top-left t))
  "Set the scissor rectangle."
  (sokol-gl:sgl-scissor-rect x y w h origin-top-left))

(defun gl-scissorf (x y w h &optional (origin-top-left t))
  "Set the scissor rectangle with float coordinates."
  (sokol-gl:sgl-scissor-rectf x y w h origin-top-left))

(defmacro with-gl-scissor ((x y w h &optional (origin-top-left t)) &body body)
  "Execute BODY with scissor rectangle set, then restore defaults."
  `(progn
     (gl-scissor ,x ,y ,w ,h ,origin-top-left)
     (unwind-protect
          (progn ,@body)
       (gl-defaults))))

(defun gl-enable-texture ()
  "Enable texturing."
  (sokol-gl:sgl-enable-texture))

(defun gl-disable-texture ()
  "Disable texturing."
  (sokol-gl:sgl-disable-texture))

(defun gl-layer (layer-id)
  "Set the current layer ID for subsequent draw commands."
  (sokol-gl:sgl-layer layer-id))

;;; ============================================================================
;;; Matrix Operations
;;; ============================================================================

(defun gl-matrix-mode-modelview ()
  "Set matrix mode to modelview."
  (sokol-gl:sgl-matrix-mode-modelview))

(defun gl-matrix-mode-projection ()
  "Set matrix mode to projection."
  (sokol-gl:sgl-matrix-mode-projection))

(defun gl-matrix-mode-texture ()
  "Set matrix mode to texture."
  (sokol-gl:sgl-matrix-mode-texture))

(defun gl-load-identity ()
  "Load identity matrix."
  (sokol-gl:sgl-load-identity))

(defun gl-load-matrix (matrix)
  "Load a 4x4 matrix (16-element float array)."
  (with-foreign-object (m-ptr :float 16)
    (loop for i from 0 below 16
          do (setf (mem-aref m-ptr :float i) (aref matrix i)))
    (sokol-gl:sgl-load-matrix m-ptr)))

(defun gl-load-transpose-matrix (matrix)
  "Load a 4x4 transposed matrix (16-element float array)."
  (with-foreign-object (m-ptr :float 16)
    (loop for i from 0 below 16
          do (setf (mem-aref m-ptr :float i) (aref matrix i)))
    (sokol-gl:sgl-load-transpose-matrix m-ptr)))

(defun gl-mult-matrix (matrix)
  "Multiply current matrix with a 4x4 matrix."
  (with-foreign-object (m-ptr :float 16)
    (loop for i from 0 below 16
          do (setf (mem-aref m-ptr :float i) (aref matrix i)))
    (sokol-gl:sgl-mult-matrix m-ptr)))

(defun gl-mult-transpose-matrix (matrix)
  "Multiply current matrix with a 4x4 transposed matrix."
  (with-foreign-object (m-ptr :float 16)
    (loop for i from 0 below 16
          do (setf (mem-aref m-ptr :float i) (aref matrix i)))
    (sokol-gl:sgl-mult-transpose-matrix m-ptr)))

(defun gl-rotate (angle-rad x y z)
  "Rotate by ANGLE-RAD radians around axis (X, Y, Z)."
  (sokol-gl:sgl-rotate angle-rad x y z))

(defun gl-scale (x y z)
  "Scale by factors (X, Y, Z)."
  (sokol-gl:sgl-scale x y z))

(defun gl-translate (x y z)
  "Translate by vector (X, Y, Z)."
  (sokol-gl:sgl-translate x y z))

(defun gl-frustum (left right bottom top near far)
  "Set frustum projection."
  (sokol-gl:sgl-frustum left right bottom top near far))

(defun gl-ortho (left right bottom top near far)
  "Set orthographic projection."
  (sokol-gl:sgl-ortho left right bottom top near far))

(defun gl-perspective (fov-y aspect z-near z-far)
  "Set perspective projection (gluPerspective-style).
   FOV-Y is field of view in radians."
  (sokol-gl:sgl-perspective fov-y aspect z-near z-far))

(defun gl-lookat (eye-x eye-y eye-z center-x center-y center-z up-x up-y up-z)
  "Set camera transformation (gluLookAt-style)."
  (sokol-gl:sgl-lookat eye-x eye-y eye-z center-x center-y center-z up-x up-y up-z))

(defun gl-push-matrix ()
  "Push current matrix onto the stack."
  (sokol-gl:sgl-push-matrix))

(defun gl-pop-matrix ()
  "Pop matrix from the stack."
  (sokol-gl:sgl-pop-matrix))

(defmacro with-gl-matrix-push (&body body)
  "Execute BODY with matrix pushed, then pop it."
  `(progn
     (gl-push-matrix)
     (unwind-protect
          (progn ,@body)
       (gl-pop-matrix))))

;;; ============================================================================
;;; Primitive Rendering
;;; ============================================================================

(defun gl-begin-points ()
  "Begin drawing points."
  (sokol-gl:sgl-begin-points))

(defun gl-begin-lines ()
  "Begin drawing lines."
  (sokol-gl:sgl-begin-lines))

(defun gl-begin-line-strip ()
  "Begin drawing a line strip."
  (sokol-gl:sgl-begin-line-strip))

(defun gl-begin-triangles ()
  "Begin drawing triangles."
  (sokol-gl:sgl-begin-triangles))

(defun gl-begin-triangle-strip ()
  "Begin drawing a triangle strip."
  (sokol-gl:sgl-begin-triangle-strip))

(defun gl-begin-quads ()
  "Begin drawing quads."
  (sokol-gl:sgl-begin-quads))

(defun gl-end ()
  "End the current primitive."
  (sokol-gl:sgl-end))

(defmacro with-gl-primitive (type &body body)
  "Execute BODY within a begin/end pair for primitive TYPE.
   TYPE should be :points, :lines, :line-strip, :triangles, :triangle-strip, or :quads."
  (let ((begin-fn (ecase type
                    (:points 'gl-begin-points)
                    (:lines 'gl-begin-lines)
                    (:line-strip 'gl-begin-line-strip)
                    (:triangles 'gl-begin-triangles)
                    (:triangle-strip 'gl-begin-triangle-strip)
                    (:quads 'gl-begin-quads))))
    `(progn
       (,begin-fn)
       (unwind-protect
            (progn ,@body)
         (gl-end)))))

;;; ============================================================================
;;; Vertex Attributes
;;; ============================================================================

;; Texture coordinates
(defun gl-texcoord (u v)
  "Set texture coordinates for the next vertex."
  (sokol-gl:sgl-t2f u v))

;; Colors
(defun gl-color3f (r g b)
  "Set color (RGB floats 0.0-1.0) for the next vertex."
  (sokol-gl:sgl-c3f r g b))

(defun gl-color4f (r g b a)
  "Set color (RGBA floats 0.0-1.0) for the next vertex."
  (sokol-gl:sgl-c4f r g b a))

(defun gl-color3b (r g b)
  "Set color (RGB bytes 0-255) for the next vertex."
  (sokol-gl:sgl-c3b r g b))

(defun gl-color4b (r g b a)
  "Set color (RGBA bytes 0-255) for the next vertex."
  (sokol-gl:sgl-c4b r g b a))

(defun gl-color1i (rgba)
  "Set color (packed RGBA32 integer) for the next vertex."
  (sokol-gl:sgl-c1i rgba))

;; Generic color function
(defun gl-color (r g b &optional (a 1.0))
  "Set color for the next vertex. Values are floats 0.0-1.0."
  (gl-color4f r g b a))

;; Point size
(defun gl-point-size (size)
  "Set point size for subsequent points."
  (sokol-gl:sgl-point-size size))

;; Vertices (2D)
(defun gl-vertex2f (x y)
  "Add a 2D vertex at (X, Y)."
  (sokol-gl:sgl-v2f x y))

(defun gl-vertex2f-texcoord (x y u v)
  "Add a 2D vertex with texture coordinates."
  (sokol-gl:sgl-v2f-t2f x y u v))

(defun gl-vertex2f-color3f (x y r g b)
  "Add a 2D vertex with RGB color (floats)."
  (sokol-gl:sgl-v2f-c3f x y r g b))

(defun gl-vertex2f-color4f (x y r g b a)
  "Add a 2D vertex with RGBA color (floats)."
  (sokol-gl:sgl-v2f-c4f x y r g b a))

(defun gl-vertex2f-color3b (x y r g b)
  "Add a 2D vertex with RGB color (bytes)."
  (sokol-gl:sgl-v2f-c3b x y r g b))

(defun gl-vertex2f-color4b (x y r g b a)
  "Add a 2D vertex with RGBA color (bytes)."
  (sokol-gl:sgl-v2f-c4b x y r g b a))

(defun gl-vertex2f-texcoord-color3f (x y u v r g b)
  "Add a 2D vertex with texture coordinates and RGB color."
  (sokol-gl:sgl-v2f-t2f-c3f x y u v r g b))

(defun gl-vertex2f-texcoord-color4f (x y u v r g b a)
  "Add a 2D vertex with texture coordinates and RGBA color."
  (sokol-gl:sgl-v2f-t2f-c4f x y u v r g b a))

;; Vertices (3D)
(defun gl-vertex3f (x y z)
  "Add a 3D vertex at (X, Y, Z)."
  (sokol-gl:sgl-v3f x y z))

(defun gl-vertex3f-texcoord (x y z u v)
  "Add a 3D vertex with texture coordinates."
  (sokol-gl:sgl-v3f-t2f x y z u v))

(defun gl-vertex3f-color3f (x y z r g b)
  "Add a 3D vertex with RGB color (floats)."
  (sokol-gl:sgl-v3f-c3f x y z r g b))

(defun gl-vertex3f-color4f (x y z r g b a)
  "Add a 3D vertex with RGBA color (floats)."
  (sokol-gl:sgl-v3f-c4f x y z r g b a))

(defun gl-vertex3f-color3b (x y z r g b)
  "Add a 3D vertex with RGB color (bytes)."
  (sokol-gl:sgl-v3f-c3b x y z r g b))

(defun gl-vertex3f-color4b (x y z r g b a)
  "Add a 3D vertex with RGBA color (bytes)."
  (sokol-gl:sgl-v3f-c4b x y z r g b a))

(defun gl-vertex3f-texcoord-color3f (x y z u v r g b)
  "Add a 3D vertex with texture coordinates and RGB color."
  (sokol-gl:sgl-v3f-t2f-c3f x y z u v r g b))

(defun gl-vertex3f-texcoord-color4f (x y z u v r g b a)
  "Add a 3D vertex with texture coordinates and RGBA color."
  (sokol-gl:sgl-v3f-t2f-c4f x y z u v r g b a))

;; Generic vertex function (convenience)
(defun gl-vertex (x y &optional z)
  "Add a vertex at (X, Y) or (X, Y, Z)."
  (if z
      (gl-vertex3f x y z)
      (gl-vertex2f x y)))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defun gl-rad (degrees)
  "Convert degrees to radians."
  (sokol-gl:sgl-rad degrees))

(defun gl-deg (radians)
  "Convert radians to degrees."
  (sokol-gl:sgl-deg radians))

;;; ============================================================================
;;; Convenience Macros
;;; ============================================================================

(defmacro with-gl-2d-setup ((width height) &body body)
  "Set up 2D orthographic projection and execute BODY.
   Coordinate system: (0,0) is top-left, (width,height) is bottom-right."
  `(progn
     (gl-defaults)
     (gl-matrix-mode-projection)
     (gl-ortho 0.0 ,width ,height 0.0 -1.0 1.0)
     (gl-matrix-mode-modelview)
     (gl-load-identity)
     ,@body))
