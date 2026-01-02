;;;; imgui-wrapper.lisp
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

;;;; Convenience wrappers for sokol-imgui
;;;; Dear ImGui integration for sokol-gfx

(defun setup-imgui (&key (max-vertices 65536)
                         color-format
                         depth-format
                         sample-count
                         ini-filename
                         no-default-font
                         write-alpha-channel)
  "Initialize sokol-imgui with convenient keyword arguments"
  (with-foreign-object (desc '(:struct sokol-imgui:simgui-desc))
    (zero-memory desc '(:struct sokol-imgui:simgui-desc))
    (setf (foreign-slot-value desc '(:struct sokol-imgui:simgui-desc) 'sokol-imgui::max-vertices)
          max-vertices)
    (when color-format
      (setf (foreign-slot-value desc '(:struct sokol-imgui:simgui-desc) 'sokol-imgui::color-format)
            (foreign-enum-value 'sokol-gfx:sg-pixel-format color-format)))
    (when depth-format
      (setf (foreign-slot-value desc '(:struct sokol-imgui:simgui-desc) 'sokol-imgui::depth-format)
            (foreign-enum-value 'sokol-gfx:sg-pixel-format depth-format)))
    (when sample-count
      (setf (foreign-slot-value desc '(:struct sokol-imgui:simgui-desc) 'sokol-imgui::sample-count)
            sample-count))
    (sokol-imgui:simgui-setup desc)))

(defun shutdown-imgui ()
  "Shutdown sokol-imgui"
  (sokol-imgui:simgui-shutdown))

(defun imgui-new-frame (&key width height delta-time (dpi-scale 1.0))
  "Start a new ImGui frame with convenient keyword arguments"
  (with-foreign-object (desc '(:struct sokol-imgui:simgui-frame-desc))
    (zero-memory desc '(:struct sokol-imgui:simgui-frame-desc))
    (when width
      (setf (foreign-slot-value desc '(:struct sokol-imgui:simgui-frame-desc) 'sokol-imgui::width) width))
    (when height
      (setf (foreign-slot-value desc '(:struct sokol-imgui:simgui-frame-desc) 'sokol-imgui::height) height))
    (when delta-time
      (setf (foreign-slot-value desc '(:struct sokol-imgui:simgui-frame-desc) 'sokol-imgui::delta-time) delta-time))
    (setf (foreign-slot-value desc '(:struct sokol-imgui:simgui-frame-desc) 'sokol-imgui::dpi-scale) dpi-scale)
    (sokol-imgui:simgui-new-frame desc)))

(defun imgui-render ()
  "Render ImGui draw data"
  (sokol-imgui:simgui-render))

(defun imgui-handle-event (event-ptr)
  "Handle sokol-app event for ImGui.
   EVENT-PTR should be a pointer to sapp_event struct.
   Returns T if event was handled by ImGui, NIL otherwise."
  (sokol-imgui:simgui-handle-event event-ptr))
