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
;;;; Provides easy access to common colors and color creation

;;; Re-export common colors from sokol-color package
(defun sg-red () sokol-color:+red+)
(defun sg-green () sokol-color:+green+)
(defun sg-blue () sokol-color:+blue+)
(defun sg-white () sokol-color:+white+)
(defun sg-black () sokol-color:+black+)
(defun sg-yellow () sokol-color:+yellow+)
(defun sg-cyan () sokol-color:+cyan+)
(defun sg-magenta () sokol-color:+magenta+)

;;; Convenience color creation from floats
(defun make-sg-color-3f (r g b)
  "Create sg_color from RGB floats (0.0-1.0), alpha=1.0"
  (sokol-color:make-color-4b
    (round (* r 255))
    (round (* g 255))
    (round (* b 255))
    255))

(defun make-sg-color-4f (r g b a)
  "Create sg_color from RGBA floats (0.0-1.0)"
  (sokol-color:make-color-4b
    (round (* r 255))
    (round (* g 255))
    (round (* b 255))
    (round (* a 255))))
