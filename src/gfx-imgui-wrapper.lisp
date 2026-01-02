;;;; gfx-imgui-wrapper.lisp
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

;;;; Convenience wrappers for sokol-gfx-imgui
;;;; Dear ImGui-based inspector for sokol-gfx resources

(defun setup-gfx-imgui ()
  "Initialize sokol-gfx-imgui debugger"
  (with-foreign-object (desc '(:struct sokol-gfx-imgui:sgimgui-desc))
    (zero-memory desc '(:struct sokol-gfx-imgui:sgimgui-desc))
    (sokol-gfx-imgui:sgimgui-setup desc)))

(defun shutdown-gfx-imgui ()
  "Shutdown sokol-gfx-imgui debugger"
  (sokol-gfx-imgui:sgimgui-shutdown))

(defun gfx-imgui-draw ()
  "Draw all sokol-gfx debugger windows"
  (sokol-gfx-imgui:sgimgui-draw))

(defun gfx-imgui-draw-menu (title)
  "Draw sokol-gfx debugger menu with given TITLE"
  (sokol-gfx-imgui:sgimgui-draw-menu title))

;;; Individual window draw functions
(defun gfx-imgui-draw-buffers ()
  "Draw buffer inspector window"
  (sokol-gfx-imgui:sgimgui-draw-buffer-window))

(defun gfx-imgui-draw-images ()
  "Draw image/texture inspector window"
  (sokol-gfx-imgui:sgimgui-draw-image-window))

(defun gfx-imgui-draw-samplers ()
  "Draw sampler inspector window"
  (sokol-gfx-imgui:sgimgui-draw-sampler-window))

(defun gfx-imgui-draw-shaders ()
  "Draw shader inspector window"
  (sokol-gfx-imgui:sgimgui-draw-shader-window))

(defun gfx-imgui-draw-pipelines ()
  "Draw pipeline inspector window"
  (sokol-gfx-imgui:sgimgui-draw-pipeline-window))

(defun gfx-imgui-draw-capabilities ()
  "Draw capabilities window showing limits and features"
  (sokol-gfx-imgui:sgimgui-draw-capabilities-window))

(defun gfx-imgui-draw-frame-stats ()
  "Draw frame statistics window"
  (sokol-gfx-imgui:sgimgui-draw-frame-stats-window))
