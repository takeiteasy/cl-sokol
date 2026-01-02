;;;; memtrack-wrapper.lisp
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

;;;; Convenience wrappers for sokol-memtrack
;;;; Simple memory allocation tracking utilities

(defun memtrack-info ()
  "Get memory tracking info. Returns plist with :num-allocs and :num-bytes"
  (with-foreign-object (info '(:struct sokol-memtrack:smemtrack-info-t))
    (setf (mem-ref info '(:struct sokol-memtrack:smemtrack-info-t))
          (sokol-memtrack:smemtrack-info))
    (list :num-allocs (foreign-slot-value info '(:struct sokol-memtrack:smemtrack-info-t) 'sokol-memtrack::num-allocs)
          :num-bytes (foreign-slot-value info '(:struct sokol-memtrack:smemtrack-info-t) 'sokol-memtrack::num-bytes))))

(defun memtrack-allocator ()
  "Get sokol-memtrack allocator functions for use in desc structs.
   Returns plist with :alloc-fn and :free-fn suitable for setting allocator fields."
  (list :alloc-fn (callback sokol-memtrack:smemtrack-alloc)
        :free-fn (callback sokol-memtrack:smemtrack-free)))
