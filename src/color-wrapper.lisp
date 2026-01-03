;;;; color-wrapper.lisp
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

;;;; Convenience wrappers for sokol-color
;;;; Provides easy access to X11 color constants and color utilities

;;; ============================================================================
;;; Color Constants (re-exported from sokol-color)
;;; ============================================================================

;; All 147 X11 color constants are available in the sokol-color package.
;; Each color has two forms:
;;   - Float list: sokol-color:+red+ => '(1.0 0.0 0.0 1.0)
;;   - RGBA32 integer: sokol-color:+red-rgba32+ => 0xFF0000FF
;;
;; Common colors are re-exported here for convenience.

(defparameter *red* sokol-color:+red+
  "Red color (1.0 0.0 0.0 1.0)")
(defparameter *green* sokol-color:+green+
  "Green color (0.0 1.0 0.0 1.0)")
(defparameter *blue* sokol-color:+blue+
  "Blue color (0.0 0.0 1.0 1.0)")
(defparameter *white* sokol-color:+white+
  "White color (1.0 1.0 1.0 1.0)")
(defparameter *black* sokol-color:+black+
  "Black color (0.0 0.0 0.0 1.0)")
(defparameter *yellow* sokol-color:+yellow+
  "Yellow color (1.0 1.0 0.0 1.0)")
(defparameter *cyan* sokol-color:+cyan+
  "Cyan color (0.0 1.0 1.0 1.0)")
(defparameter *magenta* sokol-color:+magenta+
  "Magenta color (1.0 0.0 1.0 1.0)")
(defparameter *gray* sokol-color:+gray+
  "Gray color (0.5 0.5 0.5 1.0)")
(defparameter *orange* sokol-color:+orange+
  "Orange color")
(defparameter *purple* sokol-color:+purple+
  "Purple color")
(defparameter *pink* sokol-color:+pink+
  "Pink color")
(defparameter *brown* sokol-color:+brown+
  "Brown color")

;;; ============================================================================
;;; Color Creation Functions
;;; ============================================================================

(defun make-color-4b (r g b a)
  "Create an sg_color from RGBA bytes (0-255).
   Returns an sg_color struct (returned by value from C)."
  (sokol-color:make-color-4b r g b a))

(defun make-color-3b (r g b)
  "Create an sg_color from RGB bytes (0-255), alpha=255."
  (sokol-color:make-color-4b r g b 255))

(defun make-color-4f (r g b a)
  "Create an sg_color from RGBA floats (0.0-1.0)."
  (sokol-color:make-color-4b
    (round (* r 255))
    (round (* g 255))
    (round (* b 255))
    (round (* a 255))))

(defun make-color-3f (r g b)
  "Create an sg_color from RGB floats (0.0-1.0), alpha=1.0."
  (sokol-color:make-color-4b
    (round (* r 255))
    (round (* g 255))
    (round (* b 255))
    255))

(defun make-color-1i (rgba)
  "Create an sg_color from a packed RGBA32 integer (0xRRGGBBAA)."
  (sokol-color:make-color-1i rgba))

;;; ============================================================================
;;; Color Manipulation Functions
;;; ============================================================================

(defun color-lerp (color-a color-b amount)
  "Linearly interpolate between COLOR-A and COLOR-B.
   AMOUNT is a float from 0.0 to 1.0."
  (with-foreign-objects ((a-ptr '(:struct sokol-gfx:sg-color))
                         (b-ptr '(:struct sokol-gfx:sg-color)))
    ;; Copy color-a struct
    (setf (mem-ref a-ptr '(:struct sokol-gfx:sg-color)) color-a)
    ;; Copy color-b struct
    (setf (mem-ref b-ptr '(:struct sokol-gfx:sg-color)) color-b)
    ;; Call lerp
    (sokol-color:color-lerp a-ptr b-ptr (coerce amount 'single-float))))

(defun color-lerp-precise (color-a color-b amount)
  "Linearly interpolate between COLOR-A and COLOR-B (more precise but slower).
   AMOUNT is a float from 0.0 to 1.0."
  (with-foreign-objects ((a-ptr '(:struct sokol-gfx:sg-color))
                         (b-ptr '(:struct sokol-gfx:sg-color)))
    (setf (mem-ref a-ptr '(:struct sokol-gfx:sg-color)) color-a)
    (setf (mem-ref b-ptr '(:struct sokol-gfx:sg-color)) color-b)
    (sokol-color:color-lerp-precise a-ptr b-ptr (coerce amount 'single-float))))

(defun color-multiply (color scale)
  "Multiply each component of COLOR by SCALE."
  (with-foreign-object (c-ptr '(:struct sokol-gfx:sg-color))
    (setf (mem-ref c-ptr '(:struct sokol-gfx:sg-color)) color)
    (sokol-color:color-multiply c-ptr (coerce scale 'single-float))))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defun get-color-by-name (name)
  "Get a color constant by name (keyword).
   Example: (get-color-by-name :red) => '(1.0 0.0 0.0 1.0)
   Returns NIL if color not found."
  (let ((const-name (intern (format nil "+~A+" (string-upcase name)) :sokol-color)))
    (when (boundp const-name)
      (symbol-value const-name))))

(defun get-color-rgba32-by-name (name)
  "Get a packed RGBA32 color constant by name (keyword).
   Example: (get-color-rgba32-by-name :red) => 0xFF0000FF
   Returns NIL if color not found."
  (let ((const-name (intern (format nil "+~A-RGBA32+" (string-upcase name)) :sokol-color)))
    (when (boundp const-name)
      (symbol-value const-name))))

(defun list-all-colors ()
  "Return a list of all available color names as keywords."
  (let (colors)
    (do-external-symbols (sym :sokol-color)
      (let ((name (symbol-name sym)))
        (when (and (> (length name) 2)
                   (char= (char name 0) #\+)
                   (char= (char name (1- (length name))) #\+)
                   (not (search "-RGBA32" name)))
          (push (intern (subseq name 1 (1- (length name))) :keyword) colors))))
    (sort colors #'string< :key #'symbol-name)))
