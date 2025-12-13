;;;; packages.lisp
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

(defpackage :sokol-ffi
  (:documentation "Raw CFFI bindings for sokol (not recommended for direct use)")
  (:use :cl :cffi)
  ;; Re-export all sokol packages
  (:use :sokol-log :sokol-gfx :sokol-app :sokol-time
        :sokol-audio :sokol-glue))

(defpackage :sokol
  (:documentation "High-level CLOS wrapper for sokol")
  (:use :cl)
  (:import-from :cffi
                #:foreign-alloc
                #:foreign-free
                #:with-foreign-object
                #:foreign-slot-value
                #:foreign-slot-pointer
                #:mem-ref
                #:mem-aref
                #:foreign-type-size
                #:foreign-enum-value
                #:foreign-string-alloc
                #:callback
                #:defcallback)
  ;; Export main API
  (:export
   ;; Application lifecycle generics
   #:init
   #:frame
   #:cleanup
   #:event
   ;; Main functions
   #:run-app
   ;; CLOS wrappers
   #:gfx-desc
   #:buffer-desc
   #:shader-desc
   #:pipeline-desc
   #:pass-desc
   #:pass-action
   #:bindings
   #:color
   #:range
   #:image-desc
   #:audio-desc
   #:sampler-desc
   #:view-desc
   ;; Struct conversion
   #:to-foreign
   #:from-foreign
   #:with-foreign-struct
   ;; Resource handles (opaque types)
   #:buffer
   #:shader
   #:pipeline
   ;; Setup/teardown
   #:setup-gfx
   #:shutdown-gfx
   ;; Graphics operations
   #:make-buffer
   #:make-shader
   #:make-image
   #:make-sampler
   #:make-view
   #:make-pipeline
   #:begin-pass
   #:end-pass
   #:commit
   #:apply-pipeline
   #:apply-bindings
   #:draw
   ;; Utilities
   #:environment
   #:swapchain
   #:query-backend
   #:make-range-from-array
   #:make-color
   ;; Accessors for common types
   #:color-r
   #:color-g
   #:color-b
   #:color-a
   #:range-ptr
   #:range-size
   #:buffer-desc-size
   #:buffer-desc-type
   #:buffer-desc-usage
   #:buffer-desc-data
   #:buffer-desc-label
   #:pipeline-desc-shader
   #:pipeline-desc-vertex-layouts
   #:pipeline-desc-label
   #:pass-desc-action
   #:pass-action-clear-color
   #:pass-action-load-action
   #:pass-action-store-action
   #:bindings-vertex-buffers
   #:bindings-index-buffer
   #:bindings-views
   #:bindings-samplers
   ;; Time functions
   #:setup-time
   #:now
   #:time-diff
   #:time-since
   #:laptime
   #:ticks-to-seconds
   #:ticks-to-ms
   #:ticks-to-us
   #:ticks-to-ns
   ;; Audio functions
   #:setup-audio
   #:shutdown-audio
   #:audio-valid-p
   #:audio-sample-rate
   #:audio-buffer-frames
   #:audio-channels
   #:audio-suspended-p
   #:audio-expect
   #:push-audio-samples
   ;; Audio accessors
   #:audio-desc-sample-rate
   #:audio-desc-num-channels
   #:audio-desc-buffer-frames
   #:audio-desc-packet-frames
   #:audio-desc-num-packets
   ;; Image accessors
   #:image-desc-width
   #:image-desc-height
   #:image-desc-depth
   #:image-desc-num-mipmaps
   #:image-desc-pixel-format
   #:image-desc-sample-count
   #:image-desc-data
   #:image-desc-label
   ;; Sampler accessors
   #:sampler-desc-min-filter
   #:sampler-desc-mag-filter
   #:sampler-desc-mipmap-filter
   #:sampler-desc-wrap-u
   #:sampler-desc-wrap-v
   #:sampler-desc-wrap-w
   #:sampler-desc-min-lod
   #:sampler-desc-max-lod
   #:sampler-desc-max-anisotropy
   #:sampler-desc-label
   ;; View accessors
   #:view-desc-image
   #:view-desc-mipmap-level
   #:view-desc-first-layer
   #:view-desc-num-layers
   #:view-desc-label))
