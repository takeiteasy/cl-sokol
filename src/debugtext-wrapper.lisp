;;;; debugtext-wrapper.lisp
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

;;;; CLOS wrappers for sokol-debugtext
;;;; ASCII debug text rendering with retro computer fonts

;;; ============================================================================
;;; CLOS Class Definitions
;;; ============================================================================

(define-struct-wrapper debugtext-context (:struct sokol-debugtext:sdtx-context)
  ((id :type (unsigned-byte 32) :initform 0 :accessor debugtext-context-id))
  :documentation "Sokol-debugtext rendering context handle.")

(define-struct-wrapper debugtext-desc (:struct sokol-debugtext:sdtx-desc)
  ((context-pool-size :type integer :initform 8 :accessor debugtext-desc-context-pool-size)
   (printf-buf-size :type integer :initform 4096 :accessor debugtext-desc-printf-buf-size))
  :documentation "Sokol-debugtext setup configuration.")

(define-struct-wrapper debugtext-context-desc (:struct sokol-debugtext:sdtx-context-desc)
  ((max-commands :type integer :initform 4096 :accessor debugtext-context-desc-max-commands)
   (char-buf-size :type integer :initform 4096 :accessor debugtext-context-desc-char-buf-size)
   (canvas-width :type float :initform 640.0 :accessor debugtext-context-desc-canvas-width)
   (canvas-height :type float :initform 480.0 :accessor debugtext-context-desc-canvas-height)
   (tab-width :type integer :initform 4 :accessor debugtext-context-desc-tab-width)
   (color-format :type keyword :initform :default :accessor debugtext-context-desc-color-format)
   (depth-format :type keyword :initform :default :accessor debugtext-context-desc-depth-format)
   (sample-count :type integer :initform 1 :accessor debugtext-context-desc-sample-count))
  :documentation "Context creation configuration.")

;;; ============================================================================
;;; to-foreign Methods
;;; ============================================================================

(defmethod to-foreign ((ctx debugtext-context) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of ctx)))))
    (setf (foreign-slot-value ptr '(:struct sokol-debugtext:sdtx-context) 'sokol-debugtext::id)
          (debugtext-context-id ctx))
    ptr))

(defmethod to-foreign ((desc debugtext-desc) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of desc)))))
    (zero-memory ptr '(:struct sokol-debugtext:sdtx-desc))
    (setf (foreign-slot-value ptr '(:struct sokol-debugtext:sdtx-desc) 'sokol-debugtext::context-pool-size)
          (debugtext-desc-context-pool-size desc))
    (setf (foreign-slot-value ptr '(:struct sokol-debugtext:sdtx-desc) 'sokol-debugtext::printf-buf-size)
          (debugtext-desc-printf-buf-size desc))
    ptr))

(defmethod to-foreign ((desc debugtext-context-desc) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of desc)))))
    (zero-memory ptr '(:struct sokol-debugtext:sdtx-context-desc))
    (setf (foreign-slot-value ptr '(:struct sokol-debugtext:sdtx-context-desc) 'sokol-debugtext::max-commands)
          (debugtext-context-desc-max-commands desc))
    (setf (foreign-slot-value ptr '(:struct sokol-debugtext:sdtx-context-desc) 'sokol-debugtext::char-buf-size)
          (debugtext-context-desc-char-buf-size desc))
    (setf (foreign-slot-value ptr '(:struct sokol-debugtext:sdtx-context-desc) 'sokol-debugtext::canvas-width)
          (debugtext-context-desc-canvas-width desc))
    (setf (foreign-slot-value ptr '(:struct sokol-debugtext:sdtx-context-desc) 'sokol-debugtext::canvas-height)
          (debugtext-context-desc-canvas-height desc))
    (setf (foreign-slot-value ptr '(:struct sokol-debugtext:sdtx-context-desc) 'sokol-debugtext::tab-width)
          (debugtext-context-desc-tab-width desc))

    ;; Color format
    (unless (eq (debugtext-context-desc-color-format desc) :default)
      (let ((color-fmt-ptr (foreign-slot-pointer ptr '(:struct sokol-debugtext:sdtx-context-desc) 'sokol-debugtext::color-format)))
        (setf (mem-ref color-fmt-ptr :int)
              (foreign-enum-value 'sokol-gfx:sg-pixel-format
                                (intern (format nil "SG-PIXELFORMAT-~A"
                                              (string-upcase (debugtext-context-desc-color-format desc)))
                                       :keyword)))))

    ;; Depth format
    (unless (eq (debugtext-context-desc-depth-format desc) :default)
      (let ((depth-fmt-ptr (foreign-slot-pointer ptr '(:struct sokol-debugtext:sdtx-context-desc) 'sokol-debugtext::depth-format)))
        (setf (mem-ref depth-fmt-ptr :int)
              (foreign-enum-value 'sokol-gfx:sg-pixel-format
                                (intern (format nil "SG-PIXELFORMAT-~A"
                                              (string-upcase (debugtext-context-desc-depth-format desc)))
                                       :keyword)))))

    (setf (foreign-slot-value ptr '(:struct sokol-debugtext:sdtx-context-desc) 'sokol-debugtext::sample-count)
          (debugtext-context-desc-sample-count desc))
    ptr))

;;; ============================================================================
;;; from-foreign Methods
;;; ============================================================================

(defmethod from-foreign (ptr (type (eql 'debugtext-context)))
  (make-instance 'debugtext-context
                 :id (foreign-slot-value ptr '(:struct sokol-debugtext:sdtx-context) 'sokol-debugtext::id)))

;;; ============================================================================
;;; Setup/Teardown
;;; ============================================================================

(defun setup-debugtext (&optional (desc (make-instance 'debugtext-desc)))
  "Initialize sokol-debugtext. Must be called after setup-gfx."
  (with-foreign-struct (desc-ptr desc)
    (sokol-debugtext:sdtx-setup desc-ptr)))

(defun shutdown-debugtext ()
  "Shutdown sokol-debugtext."
  (sokol-debugtext:sdtx-shutdown))

;;; ============================================================================
;;; Context Management
;;; ============================================================================

(defun make-debugtext-context (&optional (desc (make-instance 'debugtext-context-desc)))
  "Create a new debugtext context. Returns a debugtext-context handle."
  (with-foreign-struct (desc-ptr desc)
    (let ((ctx-struct (sokol-debugtext:sdtx-make-context desc-ptr)))
      (with-foreign-object (ctx-ptr '(:struct sokol-debugtext:sdtx-context))
        (setf (mem-ref ctx-ptr '(:struct sokol-debugtext:sdtx-context)) ctx-struct)
        (from-foreign ctx-ptr 'debugtext-context)))))

(defun destroy-debugtext-context (ctx)
  "Destroy a debugtext context."
  (with-foreign-struct (ctx-ptr ctx)
    (sokol-debugtext:sdtx-destroy-context (mem-ref ctx-ptr '(:struct sokol-debugtext:sdtx-context)))))

(defun set-debugtext-context (ctx)
  "Set the active debugtext context."
  (with-foreign-struct (ctx-ptr ctx)
    (sokol-debugtext:sdtx-set-context (mem-ref ctx-ptr '(:struct sokol-debugtext:sdtx-context)))))

(defun get-debugtext-context ()
  "Get the currently active debugtext context."
  (let ((ctx-struct (sokol-debugtext:sdtx-get-context)))
    (with-foreign-object (ctx-ptr '(:struct sokol-debugtext:sdtx-context))
      (setf (mem-ref ctx-ptr '(:struct sokol-debugtext:sdtx-context)) ctx-struct)
      (from-foreign ctx-ptr 'debugtext-context))))

(defun default-debugtext-context ()
  "Get the default debugtext context."
  (let ((ctx-struct (sokol-debugtext:sdtx-default-context)))
    (with-foreign-object (ctx-ptr '(:struct sokol-debugtext:sdtx-context))
      (setf (mem-ref ctx-ptr '(:struct sokol-debugtext:sdtx-context)) ctx-struct)
      (from-foreign ctx-ptr 'debugtext-context))))

(defmacro with-debugtext-context (ctx &body body)
  "Execute BODY with CTX as the active debugtext context, then restore the previous context."
  (let ((old-ctx (gensym "OLD-CTX")))
    `(let ((,old-ctx (get-debugtext-context)))
       (unwind-protect
            (progn
              (set-debugtext-context ,ctx)
              ,@body)
         (set-debugtext-context ,old-ctx)))))

;;; ============================================================================
;;; Rendering
;;; ============================================================================

(defun debugtext-draw ()
  "Draw the current context's accumulated text."
  (sokol-debugtext:sdtx-draw))

(defun debugtext-draw-context (ctx)
  "Draw a specific context's accumulated text."
  (with-foreign-struct (ctx-ptr ctx)
    (sokol-debugtext:sdtx-context-draw (mem-ref ctx-ptr '(:struct sokol-debugtext:sdtx-context)))))

(defun debugtext-draw-layer (layer-id)
  "Draw a specific layer of the current context."
  (sokol-debugtext:sdtx-draw-layer layer-id))

(defun debugtext-draw-context-layer (ctx layer-id)
  "Draw a specific layer of a specific context."
  (with-foreign-struct (ctx-ptr ctx)
    (sokol-debugtext:sdtx-context-draw-layer (mem-ref ctx-ptr '(:struct sokol-debugtext:sdtx-context)) layer-id)))

;;; ============================================================================
;;; Canvas and Font Setup
;;; ============================================================================

(defun debugtext-canvas (width height)
  "Set the virtual canvas size in character grid cells."
  (sokol-debugtext:sdtx-canvas width height))

(defun debugtext-origin (x y)
  "Set the origin position in pixels."
  (sokol-debugtext:sdtx-origin x y))

(defun debugtext-home ()
  "Move cursor to home position (0, 0)."
  (sokol-debugtext:sdtx-home))

(defun debugtext-font (font-index)
  "Select a font by index (0-7).
   Use font-index :kc853, :kc854, :z1013, :cpc, :c64, or :oric for built-in fonts."
  (let ((index (if (keywordp font-index)
                   (ecase font-index
                     (:kc853 0)
                     (:kc854 1)
                     (:z1013 2)
                     (:cpc 3)
                     (:c64 4)
                     (:oric 5))
                   font-index)))
    (sokol-debugtext:sdtx-font index)))

(defun debugtext-layer (layer-id)
  "Set the current layer for subsequent draw commands."
  (sokol-debugtext:sdtx-layer layer-id))

;;; ============================================================================
;;; Cursor Management
;;; ============================================================================

(defun debugtext-pos (x y)
  "Set cursor position (in character cells)."
  (sokol-debugtext:sdtx-pos x y))

(defun debugtext-pos-x (x)
  "Set cursor X position (in character cells)."
  (sokol-debugtext:sdtx-pos-x x))

(defun debugtext-pos-y (y)
  "Set cursor Y position (in character cells)."
  (sokol-debugtext:sdtx-pos-y y))

(defun debugtext-move (dx dy)
  "Move cursor by delta (in character cells)."
  (sokol-debugtext:sdtx-move dx dy))

(defun debugtext-move-x (dx)
  "Move cursor in X direction (in character cells)."
  (sokol-debugtext:sdtx-move-x dx))

(defun debugtext-move-y (dy)
  "Move cursor in Y direction (in character cells)."
  (sokol-debugtext:sdtx-move-y dy))

(defun debugtext-crlf ()
  "Move cursor to next line (carriage return + line feed)."
  (sokol-debugtext:sdtx-crlf))

(defun debugtext-newline ()
  "Alias for debugtext-crlf."
  (debugtext-crlf))

;;; ============================================================================
;;; Color Management
;;; ============================================================================

(defun debugtext-color3b (r g b)
  "Set text color (RGB bytes 0-255)."
  (sokol-debugtext:sdtx-color3b r g b))

(defun debugtext-color3f (r g b)
  "Set text color (RGB floats 0.0-1.0)."
  (sokol-debugtext:sdtx-color3f r g b))

(defun debugtext-color4b (r g b a)
  "Set text color (RGBA bytes 0-255)."
  (sokol-debugtext:sdtx-color4b r g b a))

(defun debugtext-color4f (r g b a)
  "Set text color (RGBA floats 0.0-1.0)."
  (sokol-debugtext:sdtx-color4f r g b a))

(defun debugtext-color1i (rgba)
  "Set text color (packed RGBA32 integer)."
  (sokol-debugtext:sdtx-color1i rgba))

(defun debugtext-color (r g b &optional (a 1.0))
  "Set text color. Values are floats 0.0-1.0."
  (debugtext-color4f r g b a))

;;; ============================================================================
;;; Text Output
;;; ============================================================================

(defun debugtext-putc (char)
  "Output a single character."
  (sokol-debugtext:sdtx-putc (char-code char)))

(defun debugtext-puts (string)
  "Output a string."
  (sokol-debugtext:sdtx-puts string))

(defun debugtext-putr (string length)
  "Output a string with explicit length."
  (sokol-debugtext:sdtx-putr string length))

(defun debugtext-write (string)
  "Alias for debugtext-puts."
  (debugtext-puts string))

(defun debugtext-write-char (char)
  "Alias for debugtext-putc."
  (debugtext-putc char))

(defun debugtext-print (string)
  "Output a string followed by newline."
  (debugtext-puts string)
  (debugtext-newline))

(defun debugtext-printf (format-string &rest args)
  "Output formatted text (uses Common Lisp FORMAT)."
  (debugtext-puts (apply #'format nil format-string args)))

;;; ============================================================================
;;; Convenience Macros
;;; ============================================================================

(defmacro with-debugtext-positioned ((x y) &body body)
  "Execute BODY with cursor positioned at (X, Y), then output."
  `(progn
     (debugtext-pos ,x ,y)
     ,@body))

(defmacro with-debugtext-color ((r g b &optional (a 1.0)) &body body)
  "Execute BODY with text color set."
  `(progn
     (debugtext-color ,r ,g ,b ,a)
     ,@body))

(defun debugtext-printf-at (x y format-string &rest args)
  "Output formatted text at specific position."
  (debugtext-pos x y)
  (apply #'debugtext-printf format-string args))
