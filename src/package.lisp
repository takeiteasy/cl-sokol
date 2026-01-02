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
                #:foreign-enum-keyword
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
   ;; Event wrapper API
   #:wrap-event
   ;; Event type predicates
   #:key-down-p
   #:key-up-p
   #:char-input-p
   #:mouse-down-p
   #:mouse-up-p
   #:mouse-move-p
   #:mouse-scroll-p
   #:mouse-enter-p
   #:mouse-leave-p
   #:window-resized-p
   #:window-iconified-p
   #:window-restored-p
   #:window-focused-p
   #:window-unfocused-p
   #:quit-requested-p
   ;; Modifier key helpers
   #:shift-pressed-p
   #:ctrl-pressed-p
   #:alt-pressed-p
   #:super-pressed-p
   ;; Mouse button helpers
   #:left-button-p
   #:right-button-p
   #:middle-button-p
   ;; Convenience functions
   #:key-pressed-p
   #:mouse-clicked-p
   ;; Event accessors
   #:event-type
   #:event-frame-count
   #:event-key-code
   #:event-char-code
   #:event-key-repeat
   #:event-modifiers
   #:event-mouse-button
   #:event-mouse-x
   #:event-mouse-y
   #:event-mouse-dx
   #:event-mouse-dy
   #:event-scroll-x
   #:event-scroll-y
   #:event-num-touches
   #:event-window-width
   #:event-window-height
   #:event-framebuffer-width
   #:event-framebuffer-height
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
   #:view-desc-label

   ;;; Memtrack wrapper exports
   #:memtrack-info
   #:memtrack-allocator

   ;;; Color wrapper exports
   #:sg-red #:sg-green #:sg-blue #:sg-white #:sg-black
   #:sg-yellow #:sg-cyan #:sg-magenta
   #:make-sg-color-3f
   #:make-sg-color-4f

   ;;; ImGui wrapper exports
   #:setup-imgui
   #:shutdown-imgui
   #:imgui-new-frame
   #:imgui-render
   #:imgui-handle-event

   ;;; GFX-ImGui wrapper exports
   #:setup-gfx-imgui
   #:shutdown-gfx-imgui
   #:gfx-imgui-draw
   #:gfx-imgui-draw-menu
   #:gfx-imgui-draw-buffers
   #:gfx-imgui-draw-images
   #:gfx-imgui-draw-samplers
   #:gfx-imgui-draw-shaders
   #:gfx-imgui-draw-pipelines
   #:gfx-imgui-draw-capabilities
   #:gfx-imgui-draw-frame-stats

   ;;; GL wrapper exports
   ;; CLOS classes
   #:gl-context #:gl-pipeline #:gl-desc #:gl-context-desc #:gl-error
   ;; Setup/teardown
   #:setup-gl #:shutdown-gl
   ;; Context management
   #:make-gl-context #:destroy-gl-context #:set-gl-context #:get-gl-context
   #:default-gl-context #:with-gl-context
   ;; Error handling
   #:gl-error #:gl-context-error
   ;; Rendering
   #:gl-draw #:gl-draw-context #:gl-draw-layer #:gl-draw-context-layer
   ;; State management
   #:gl-defaults #:gl-viewport #:gl-viewportf #:gl-scissor #:gl-scissorf
   #:with-gl-scissor #:gl-enable-texture #:gl-disable-texture #:gl-layer
   ;; Matrix operations
   #:gl-matrix-mode-modelview #:gl-matrix-mode-projection #:gl-matrix-mode-texture
   #:gl-load-identity #:gl-load-matrix #:gl-load-transpose-matrix
   #:gl-mult-matrix #:gl-mult-transpose-matrix
   #:gl-rotate #:gl-scale #:gl-translate
   #:gl-frustum #:gl-ortho #:gl-perspective #:gl-lookat
   #:gl-push-matrix #:gl-pop-matrix #:with-gl-matrix-push
   ;; Primitive rendering
   #:gl-begin-points #:gl-begin-lines #:gl-begin-line-strip
   #:gl-begin-triangles #:gl-begin-triangle-strip #:gl-begin-quads
   #:gl-end #:with-gl-primitive
   ;; Vertex attributes
   #:gl-texcoord #:gl-color3f #:gl-color4f #:gl-color3b #:gl-color4b #:gl-color1i
   #:gl-color #:gl-point-size
   #:gl-vertex2f #:gl-vertex2f-texcoord #:gl-vertex2f-color3f #:gl-vertex2f-color4f
   #:gl-vertex2f-color3b #:gl-vertex2f-color4b
   #:gl-vertex2f-texcoord-color3f #:gl-vertex2f-texcoord-color4f
   #:gl-vertex3f #:gl-vertex3f-texcoord #:gl-vertex3f-color3f #:gl-vertex3f-color4f
   #:gl-vertex3f-color3b #:gl-vertex3f-color4b
   #:gl-vertex3f-texcoord-color3f #:gl-vertex3f-texcoord-color4f
   #:gl-vertex
   ;; Utilities
   #:gl-rad #:gl-deg
   ;; Convenience macros
   #:with-gl-2d-setup

   ;;; DebugText wrapper exports
   ;; CLOS classes
   #:debugtext-context #:debugtext-desc #:debugtext-context-desc
   ;; Setup/teardown
   #:setup-debugtext #:shutdown-debugtext
   ;; Context management
   #:make-debugtext-context #:destroy-debugtext-context #:set-debugtext-context
   #:get-debugtext-context #:default-debugtext-context #:with-debugtext-context
   ;; Rendering
   #:debugtext-draw #:debugtext-draw-context #:debugtext-draw-layer #:debugtext-draw-context-layer
   ;; Canvas and font setup
   #:debugtext-canvas #:debugtext-origin #:debugtext-home #:debugtext-font #:debugtext-layer
   ;; Cursor management
   #:debugtext-pos #:debugtext-pos-x #:debugtext-pos-y
   #:debugtext-move #:debugtext-move-x #:debugtext-move-y
   #:debugtext-crlf #:debugtext-newline
   ;; Color management
   #:debugtext-color3b #:debugtext-color3f #:debugtext-color4b #:debugtext-color4f
   #:debugtext-color1i #:debugtext-color
   ;; Text output
   #:debugtext-putc #:debugtext-puts #:debugtext-putr
   #:debugtext-write #:debugtext-write-char #:debugtext-print #:debugtext-printf
   ;; Convenience macros
   #:with-debugtext-positioned #:with-debugtext-color #:debugtext-printf-at

   ;;; Shape wrapper exports
   ;; CLOS classes
   #:shape-buffer #:shape-plane #:shape-box #:shape-sphere #:shape-cylinder #:shape-torus
   ;; Buffer management
   #:make-shape-buffer #:free-shape-buffer
   ;; Shape building functions
   #:build-plane #:build-box #:build-sphere #:build-cylinder #:build-torus
   ;; Color utilities
   #:shape-color-4f #:shape-color-3f #:shape-color-4b #:shape-color-3b
   ))
