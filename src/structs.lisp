;;;; structs.lisp
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

;;;; Color

(define-struct-wrapper color (:struct sokol-gfx:sg-color)
  ((r :type float :initform 0.0 :accessor color-r)
   (g :type float :initform 0.0 :accessor color-g)
   (b :type float :initform 0.0 :accessor color-b)
   (a :type float :initform 1.0 :accessor color-a))
  :documentation "RGBA color value.")

(defmethod to-foreign ((c color) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of c)))))
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-color) 'sokol-gfx::r) (color-r c))
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-color) 'sokol-gfx::g) (color-g c))
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-color) 'sokol-gfx::b) (color-b c))
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-color) 'sokol-gfx::a) (color-a c))
    ptr))

(defmethod from-foreign (ptr (type (eql 'color)))
  (make-instance 'color
                 :r (foreign-slot-value ptr '(:struct sokol-gfx:sg-color) 'sokol-gfx::r)
                 :g (foreign-slot-value ptr '(:struct sokol-gfx:sg-color) 'sokol-gfx::g)
                 :b (foreign-slot-value ptr '(:struct sokol-gfx:sg-color) 'sokol-gfx::b)
                 :a (foreign-slot-value ptr '(:struct sokol-gfx:sg-color) 'sokol-gfx::a)))

(defun make-color (r g b &optional (a 1.0))
  "Convenience constructor for colors."
  (make-instance 'color :r r :g g :b b :a a))

;;;; Range (for buffer data)

(define-struct-wrapper range (:struct sokol-gfx:sg-range)
  ((ptr :type t :initform (cffi:null-pointer) :accessor range-ptr)
   (size :type integer :initform 0 :accessor range-size))
  :documentation "Memory range for buffer data.")

(defmethod to-foreign ((r range) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of r)))))
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-range) 'sokol-gfx::ptr) (range-ptr r))
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-range) 'sokol-gfx::size) (range-size r))
    ptr))

(defun make-range-from-array (array element-type)
  "Create a range from a Lisp array, allocating foreign memory.
  The foreign memory must be freed by the caller."
  (let* ((count (length array))
         (foreign-data (foreign-alloc element-type :count count)))
    (loop for i from 0 below count
          do (setf (mem-aref foreign-data element-type i) (aref array i)))
    (make-instance 'range
                   :ptr foreign-data
                   :size (* count (foreign-type-size element-type)))))

;;;; Pass Action

(define-struct-wrapper pass-action (:struct sokol-gfx:sg-pass-action)
  ((clear-color :type color :initform (make-color 0.0 0.0 0.0 1.0) :accessor pass-action-clear-color)
   (load-action :type keyword :initform :clear :accessor pass-action-load-action)
   (store-action :type keyword :initform :store :accessor pass-action-store-action))
  :documentation "Describes what to do at the start of a rendering pass.")

(defmethod to-foreign ((pa pass-action) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of pa)))))
    (zero-memory ptr '(:struct sokol-gfx:sg-pass-action))

    ;; Set color[0]
    (let* ((colors-ptr (foreign-slot-pointer ptr '(:struct sokol-gfx:sg-pass-action) 'sokol-gfx::colors))
           (color0-ptr colors-ptr)
           (clear-value-ptr (foreign-slot-pointer color0-ptr '(:struct sokol-gfx:sg-color-attachment-action)
                                                   'sokol-gfx::clear-value)))
      ;; Set load action
      (setf (foreign-slot-value color0-ptr '(:struct sokol-gfx:sg-color-attachment-action)
                                'sokol-gfx::load-action)
            (foreign-enum-value 'sokol-gfx:sg-load-action
                               (intern (format nil "SG-LOADACTION-~A"
                                             (string-upcase (pass-action-load-action pa)))
                                      :keyword)))

      ;; Set store action
      (setf (foreign-slot-value color0-ptr '(:struct sokol-gfx:sg-color-attachment-action)
                                'sokol-gfx::store-action)
            (foreign-enum-value 'sokol-gfx:sg-store-action
                               (intern (format nil "SG-STOREACTION-~A"
                                             (string-upcase (pass-action-store-action pa)))
                                      :keyword)))

      ;; Set clear color
      (let ((color (pass-action-clear-color pa)))
        (setf (foreign-slot-value clear-value-ptr '(:struct sokol-gfx:sg-color) 'sokol-gfx::r)
              (color-r color))
        (setf (foreign-slot-value clear-value-ptr '(:struct sokol-gfx:sg-color) 'sokol-gfx::g)
              (color-g color))
        (setf (foreign-slot-value clear-value-ptr '(:struct sokol-gfx:sg-color) 'sokol-gfx::b)
              (color-b color))
        (setf (foreign-slot-value clear-value-ptr '(:struct sokol-gfx:sg-color) 'sokol-gfx::a)
              (color-a color))))

    ptr))

;;;; Graphics Descriptor

(define-struct-wrapper gfx-desc (:struct sokol-gfx:sg-desc)
  ((buffer-pool-size :type integer :initform 128 :accessor gfx-desc-buffer-pool-size)
   (image-pool-size :type integer :initform 128 :accessor gfx-desc-image-pool-size)
   (sampler-pool-size :type integer :initform 64 :accessor gfx-desc-sampler-pool-size)
   (shader-pool-size :type integer :initform 32 :accessor gfx-desc-shader-pool-size)
   (pipeline-pool-size :type integer :initform 64 :accessor gfx-desc-pipeline-pool-size)
   (view-pool-size :type integer :initform 16 :accessor gfx-desc-view-pool-size))
  :documentation "Graphics subsystem initialization parameters.")

(defmethod to-foreign ((desc gfx-desc) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of desc)))))
    (zero-memory ptr '(:struct sokol-gfx:sg-desc))

    ;; Set pool sizes directly (they are fields in sg-desc, not a sub-struct)
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-desc) 'sokol-gfx::buffer-pool-size)
          (gfx-desc-buffer-pool-size desc))
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-desc) 'sokol-gfx::image-pool-size)
          (gfx-desc-image-pool-size desc))
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-desc) 'sokol-gfx::sampler-pool-size)
          (gfx-desc-sampler-pool-size desc))
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-desc) 'sokol-gfx::shader-pool-size)
          (gfx-desc-shader-pool-size desc))
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-desc) 'sokol-gfx::pipeline-pool-size)
          (gfx-desc-pipeline-pool-size desc))
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-desc) 'sokol-gfx::view-pool-size)
          (gfx-desc-view-pool-size desc))

    ;; Set environment from sokol_glue (only if sapp is running)
    ;; sglue-environment will return a valid environment when called from within sapp
    (handler-case
        (let ((env (sokol-glue:sglue-environment))
              (env-ptr (foreign-slot-pointer ptr '(:struct sokol-gfx:sg-desc) 'sokol-gfx::environment)))
          (setf (mem-ref env-ptr '(:struct sokol-gfx:sg-environment)) env))
      (error ()
        ;; If sglue-environment fails, leave environment zeroed
        ;; This happens when setup-gfx is called outside of sapp context
        nil))

    ptr))

;;;; Buffer Descriptor

(define-struct-wrapper buffer-desc (:struct sokol-gfx:sg-buffer-desc)
  ((size :type integer :initform 0 :accessor buffer-desc-size)
   (buffer-type :type keyword :initform :vertex-buffer :accessor buffer-desc-type)
   (usage :type keyword :initform :immutable :accessor buffer-desc-usage)
   (data :type (or null range) :initform nil :accessor buffer-desc-data)
   (label :type (or null string) :initform nil :accessor buffer-desc-label))
  :documentation "Describes a vertex or index buffer.")

(defmethod to-foreign ((desc buffer-desc) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of desc)))))
    (zero-memory ptr '(:struct sokol-gfx:sg-buffer-desc))

    ;; Set size
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-buffer-desc) 'sokol-gfx::size)
          (buffer-desc-size desc))

    ;; Set buffer type and usage flags in sg-buffer-usage struct
    (let ((usage-ptr (foreign-slot-pointer ptr '(:struct sokol-gfx:sg-buffer-desc) 'sokol-gfx::usage)))
      ;; Set buffer type (vertex-buffer or index-buffer)
      (setf (foreign-slot-value usage-ptr '(:struct sokol-gfx:sg-buffer-usage) 'sokol-gfx::vertex-buffer)
            (eq (buffer-desc-type desc) :vertex-buffer))
      (setf (foreign-slot-value usage-ptr '(:struct sokol-gfx:sg-buffer-usage) 'sokol-gfx::index-buffer)
            (eq (buffer-desc-type desc) :index-buffer))
      ;; Set usage flags (immutable, dynamic-update, or stream-update)
      (setf (foreign-slot-value usage-ptr '(:struct sokol-gfx:sg-buffer-usage) 'sokol-gfx::immutable)
            (eq (buffer-desc-usage desc) :immutable))
      (setf (foreign-slot-value usage-ptr '(:struct sokol-gfx:sg-buffer-usage) 'sokol-gfx::dynamic-update)
            (eq (buffer-desc-usage desc) :dynamic))
      (setf (foreign-slot-value usage-ptr '(:struct sokol-gfx:sg-buffer-usage) 'sokol-gfx::stream-update)
            (eq (buffer-desc-usage desc) :stream)))

    ;; Set data if provided
    (when (buffer-desc-data desc)
      (let ((data-ptr (foreign-slot-pointer ptr '(:struct sokol-gfx:sg-buffer-desc) 'sokol-gfx::data))
            (range-data (buffer-desc-data desc)))
        (setf (foreign-slot-value data-ptr '(:struct sokol-gfx:sg-range) 'sokol-gfx::ptr)
              (range-ptr range-data))
        (setf (foreign-slot-value data-ptr '(:struct sokol-gfx:sg-range) 'sokol-gfx::size)
              (range-size range-data))))

    ;; Set label if provided
    ;; Note: Label strings must remain allocated for the lifetime of the buffer
    ;; For now, we skip label to avoid memory management issues
    ;; (when (buffer-desc-label desc)
    ;;   (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-buffer-desc) 'sokol-gfx::label)
    ;;         (foreign-string-alloc (buffer-desc-label desc))))

    ptr))

;;;; Pipeline Descriptor

(define-struct-wrapper pipeline-desc (:struct sokol-gfx:sg-pipeline-desc)
  ((shader :type t :initform nil :accessor pipeline-desc-shader)
   (vertex-layouts :type list :initform nil :accessor pipeline-desc-vertex-layouts)
   (vertex-stride :type integer :initform 0 :accessor pipeline-desc-vertex-stride)
   (index-type :type keyword :initform :none :accessor pipeline-desc-index-type)
   (cull-mode :type keyword :initform :none :accessor pipeline-desc-cull-mode)
   ;; Depth state
   (depth-compare :type keyword :initform :always :accessor pipeline-desc-depth-compare)
   (depth-write-enabled :type boolean :initform nil :accessor pipeline-desc-depth-write-enabled)
   ;; Blend state (for color attachment 0)
   (blend-enabled :type boolean :initform nil :accessor pipeline-desc-blend-enabled)
   (blend-src-factor-rgb :type keyword :initform :one :accessor pipeline-desc-blend-src-factor-rgb)
   (blend-dst-factor-rgb :type keyword :initform :zero :accessor pipeline-desc-blend-dst-factor-rgb)
   (blend-src-factor-alpha :type keyword :initform :one :accessor pipeline-desc-blend-src-factor-alpha)
   (blend-dst-factor-alpha :type keyword :initform :zero :accessor pipeline-desc-blend-dst-factor-alpha)
   (label :type (or null string) :initform nil :accessor pipeline-desc-label))
  :documentation "Describes a graphics pipeline (shader + state).")

(defun %keyword-to-index-type (kw)
  "Convert keyword to sokol index type enum value."
  (cffi:foreign-enum-value 'sokol-gfx:sg-index-type
                           (ecase kw
                             (:none :sg-indextype-none)
                             (:uint16 :sg-indextype-uint16)
                             (:uint32 :sg-indextype-uint32))))

(defun %keyword-to-cull-mode (kw)
  "Convert keyword to sokol cull mode enum value."
  (cffi:foreign-enum-value 'sokol-gfx:sg-cull-mode
                           (ecase kw
                             (:none :sg-cullmode-none)
                             (:front :sg-cullmode-front)
                             (:back :sg-cullmode-back))))

(defun %keyword-to-compare-func (kw)
  "Convert keyword to sokol compare function enum value."
  (cffi:foreign-enum-value 'sokol-gfx:sg-compare-func
                           (ecase kw
                             (:never :sg-comparefunc-never)
                             (:less :sg-comparefunc-less)
                             (:equal :sg-comparefunc-equal)
                             (:less-equal :sg-comparefunc-less-equal)
                             (:greater :sg-comparefunc-greater)
                             (:not-equal :sg-comparefunc-not-equal)
                             (:greater-equal :sg-comparefunc-greater-equal)
                             (:always :sg-comparefunc-always))))

(defun %keyword-to-blend-factor (kw)
  "Convert keyword to sokol blend factor enum value."
  (cffi:foreign-enum-value 'sokol-gfx:sg-blend-factor
                           (ecase kw
                             (:zero :sg-blendfactor-zero)
                             (:one :sg-blendfactor-one)
                             (:src-color :sg-blendfactor-src-color)
                             (:one-minus-src-color :sg-blendfactor-one-minus-src-color)
                             (:src-alpha :sg-blendfactor-src-alpha)
                             (:one-minus-src-alpha :sg-blendfactor-one-minus-src-alpha)
                             (:dst-color :sg-blendfactor-dst-color)
                             (:one-minus-dst-color :sg-blendfactor-one-minus-dst-color)
                             (:dst-alpha :sg-blendfactor-dst-alpha)
                             (:one-minus-dst-alpha :sg-blendfactor-one-minus-dst-alpha)
                             (:src-alpha-saturated :sg-blendfactor-src-alpha-saturated)
                             (:blend-color :sg-blendfactor-blend-color)
                             (:one-minus-blend-color :sg-blendfactor-one-minus-blend-color)
                             (:blend-alpha :sg-blendfactor-blend-alpha)
                             (:one-minus-blend-alpha :sg-blendfactor-one-minus-blend-alpha))))

(defmethod to-foreign ((desc pipeline-desc) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of desc)))))
    (zero-memory ptr '(:struct sokol-gfx:sg-pipeline-desc))

    ;; Set shader (struct by value)
    (when (pipeline-desc-shader desc)
      (let ((shader-ptr (foreign-slot-pointer ptr '(:struct sokol-gfx:sg-pipeline-desc) 'sokol-gfx::shader)))
        (setf (mem-ref shader-ptr '(:struct sokol-gfx:sg-shader))
              (pipeline-desc-shader desc))))

    ;; Set vertex layouts
    (when (pipeline-desc-vertex-layouts desc)
      (let* ((layout-ptr (foreign-slot-pointer ptr '(:struct sokol-gfx:sg-pipeline-desc) 'sokol-gfx::layout))
             (attrs-ptr (foreign-slot-pointer layout-ptr '(:struct sokol-gfx:sg-vertex-layout-state) 'sokol-gfx::attrs))
             (buffers-ptr (foreign-slot-pointer layout-ptr '(:struct sokol-gfx:sg-vertex-layout-state) 'sokol-gfx::buffers))
             (offset 0))
        ;; Set vertex attributes with proper offsets
        (loop for format in (pipeline-desc-vertex-layouts desc)
              for i from 0
              do (let ((attr-ptr (cffi:mem-aptr attrs-ptr '(:struct sokol-gfx:sg-vertex-attr-state) i)))
                   ;; Set buffer index (all from buffer 0)
                   (setf (foreign-slot-value attr-ptr '(:struct sokol-gfx:sg-vertex-attr-state)
                                             'sokol-gfx::buffer-index)
                         0)
                   ;; Set offset
                   (setf (foreign-slot-value attr-ptr '(:struct sokol-gfx:sg-vertex-attr-state)
                                             'sokol-gfx::offset)
                         offset)
                   ;; Set format
                   (setf (foreign-slot-value attr-ptr '(:struct sokol-gfx:sg-vertex-attr-state)
                                             'sokol-gfx::format)
                         (foreign-enum-value 'sokol-gfx:sg-vertex-format format))
                   ;; Calculate next offset based on format size
                   ;; float4 = 16 bytes, float3 = 12 bytes, float2 = 8 bytes, float = 4 bytes
                   (incf offset (ecase format
                                  (:sg-vertexformat-float4 16)
                                  (:sg-vertexformat-float3 12)
                                  (:sg-vertexformat-float2 8)
                                  (:sg-vertexformat-float 4)))))
        ;; Set buffer stride if specified
        (when (plusp (pipeline-desc-vertex-stride desc))
          (setf (foreign-slot-value
                 (cffi:mem-aptr buffers-ptr '(:struct sokol-gfx:sg-vertex-buffer-layout-state) 0)
                 '(:struct sokol-gfx:sg-vertex-buffer-layout-state)
                 'sokol-gfx::stride)
                (pipeline-desc-vertex-stride desc)))))

    ;; Set index type
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-pipeline-desc) 'sokol-gfx::index-type)
          (%keyword-to-index-type (pipeline-desc-index-type desc)))

    ;; Set cull mode
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-pipeline-desc) 'sokol-gfx::cull-mode)
          (%keyword-to-cull-mode (pipeline-desc-cull-mode desc)))

    ;; Set depth state
    (let ((depth-ptr (foreign-slot-pointer ptr '(:struct sokol-gfx:sg-pipeline-desc) 'sokol-gfx::depth)))
      (setf (foreign-slot-value depth-ptr '(:struct sokol-gfx:sg-depth-state) 'sokol-gfx::compare)
            (%keyword-to-compare-func (pipeline-desc-depth-compare desc)))
      (setf (foreign-slot-value depth-ptr '(:struct sokol-gfx:sg-depth-state) 'sokol-gfx::write-enabled)
            (pipeline-desc-depth-write-enabled desc)))

    ;; Set blend state for color attachment 0
    (when (pipeline-desc-blend-enabled desc)
      (let* ((colors-ptr (foreign-slot-pointer ptr '(:struct sokol-gfx:sg-pipeline-desc) 'sokol-gfx::colors))
             (color0-ptr colors-ptr)
             (blend-ptr (foreign-slot-pointer color0-ptr '(:struct sokol-gfx:sg-color-target-state) 'sokol-gfx::blend)))
        (setf (foreign-slot-value blend-ptr '(:struct sokol-gfx:sg-blend-state) 'sokol-gfx::enabled) t)
        (setf (foreign-slot-value blend-ptr '(:struct sokol-gfx:sg-blend-state) 'sokol-gfx::src-factor-rgb)
              (%keyword-to-blend-factor (pipeline-desc-blend-src-factor-rgb desc)))
        (setf (foreign-slot-value blend-ptr '(:struct sokol-gfx:sg-blend-state) 'sokol-gfx::dst-factor-rgb)
              (%keyword-to-blend-factor (pipeline-desc-blend-dst-factor-rgb desc)))
        (setf (foreign-slot-value blend-ptr '(:struct sokol-gfx:sg-blend-state) 'sokol-gfx::op-rgb)
              (cffi:foreign-enum-value 'sokol-gfx:sg-blend-op :sg-blendop-add))
        (setf (foreign-slot-value blend-ptr '(:struct sokol-gfx:sg-blend-state) 'sokol-gfx::src-factor-alpha)
              (%keyword-to-blend-factor (pipeline-desc-blend-src-factor-alpha desc)))
        (setf (foreign-slot-value blend-ptr '(:struct sokol-gfx:sg-blend-state) 'sokol-gfx::dst-factor-alpha)
              (%keyword-to-blend-factor (pipeline-desc-blend-dst-factor-alpha desc)))
        (setf (foreign-slot-value blend-ptr '(:struct sokol-gfx:sg-blend-state) 'sokol-gfx::op-alpha)
              (cffi:foreign-enum-value 'sokol-gfx:sg-blend-op :sg-blendop-add))))

    ptr))

;;;; Pass Descriptor

(define-struct-wrapper pass-desc (:struct sokol-gfx:sg-pass)
  ((action :type pass-action :initform (make-instance 'pass-action) :accessor pass-desc-action))
  :documentation "Describes a rendering pass.")

(defmethod to-foreign ((desc pass-desc) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of desc)))))
    (zero-memory ptr '(:struct sokol-gfx:sg-pass))

    ;; Set action (struct by value)
    (let ((action-ptr (foreign-slot-pointer ptr '(:struct sokol-gfx:sg-pass) 'sokol-gfx::action)))
      (to-foreign (pass-desc-action desc) action-ptr))

    ;; Set swapchain (struct by value)
    (let ((swapchain-ptr (foreign-slot-pointer ptr '(:struct sokol-gfx:sg-pass) 'sokol-gfx::swapchain)))
      (setf (mem-ref swapchain-ptr '(:struct sokol-gfx:sg-swapchain))
            (sokol-glue:sglue-swapchain)))

    ptr))

;;;; Bindings

(define-struct-wrapper bindings (:struct sokol-gfx:sg-bindings)
  ((vertex-buffers :type list :initform nil :accessor bindings-vertex-buffers)
   (index-buffer :type t :initform nil :accessor bindings-index-buffer)
   (views :type list :initform nil :accessor bindings-views)
   (samplers :type list :initform nil :accessor bindings-samplers))
  :documentation "Resource bindings for a draw call (buffers, views, samplers).")

(defmethod to-foreign ((b bindings) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of b)))))
    (zero-memory ptr '(:struct sokol-gfx:sg-bindings))

    ;; Set vertex buffers - copy the id field from each buffer
    (let ((vbs-ptr (foreign-slot-pointer ptr '(:struct sokol-gfx:sg-bindings) 'sokol-gfx::vertex-buffers)))
      (loop for buffer in (bindings-vertex-buffers b)
            for i from 0
            for target-ptr = (cffi:mem-aptr vbs-ptr '(:struct sokol-gfx:sg-buffer) i)
            do (setf (foreign-slot-value target-ptr '(:struct sokol-gfx:sg-buffer) 'sokol-gfx::id)
                     (foreign-slot-value buffer '(:struct sokol-gfx:sg-buffer) 'sokol-gfx::id))))

    ;; Set index buffer if provided - copy the id field
    (when (bindings-index-buffer b)
      (let ((ib-ptr (foreign-slot-pointer ptr '(:struct sokol-gfx:sg-bindings) 'sokol-gfx::index-buffer)))
        (setf (foreign-slot-value ib-ptr '(:struct sokol-gfx:sg-buffer) 'sokol-gfx::id)
              (foreign-slot-value (bindings-index-buffer b) '(:struct sokol-gfx:sg-buffer) 'sokol-gfx::id))))

    ;; Set views if provided
    (when (bindings-views b)
      (let ((views-ptr (foreign-slot-pointer ptr '(:struct sokol-gfx:sg-bindings) 'sokol-gfx::views)))
        (loop for view in (bindings-views b)
              for i from 0
              do (setf (mem-ref (cffi:mem-aptr views-ptr '(:struct sokol-gfx:sg-view) i)
                                '(:struct sokol-gfx:sg-view))
                       view))))

    ;; Set samplers if provided
    (when (bindings-samplers b)
      (let ((samplers-ptr (foreign-slot-pointer ptr '(:struct sokol-gfx:sg-bindings) 'sokol-gfx::samplers)))
        (loop for sampler in (bindings-samplers b)
              for i from 0
              do (setf (mem-ref (cffi:mem-aptr samplers-ptr '(:struct sokol-gfx:sg-sampler) i)
                                '(:struct sokol-gfx:sg-sampler))
                       sampler))))

    ptr))

;;;; Audio Descriptor

(define-struct-wrapper audio-desc (:struct sokol-audio:saudio-desc)
  ((sample-rate :type integer :initform 0 :accessor audio-desc-sample-rate)
   (num-channels :type integer :initform 2 :accessor audio-desc-num-channels)
   (buffer-frames :type integer :initform 0 :accessor audio-desc-buffer-frames)
   (packet-frames :type integer :initform 0 :accessor audio-desc-packet-frames)
   (num-packets :type integer :initform 0 :accessor audio-desc-num-packets))
  :documentation "Audio subsystem initialization parameters.")

(defmethod to-foreign ((desc audio-desc) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of desc)))))
    (zero-memory ptr '(:struct sokol-audio:saudio-desc))

    ;; Set audio parameters (0 means use default)
    (setf (foreign-slot-value ptr '(:struct sokol-audio:saudio-desc) 'sokol-audio::sample-rate)
          (audio-desc-sample-rate desc))
    (setf (foreign-slot-value ptr '(:struct sokol-audio:saudio-desc) 'sokol-audio::num-channels)
          (audio-desc-num-channels desc))
    (setf (foreign-slot-value ptr '(:struct sokol-audio:saudio-desc) 'sokol-audio::buffer-frames)
          (audio-desc-buffer-frames desc))
    (setf (foreign-slot-value ptr '(:struct sokol-audio:saudio-desc) 'sokol-audio::packet-frames)
          (audio-desc-packet-frames desc))
    (setf (foreign-slot-value ptr '(:struct sokol-audio:saudio-desc) 'sokol-audio::num-packets)
          (audio-desc-num-packets desc))

    ptr))

;;;; Image Descriptor

(define-struct-wrapper image-desc (:struct sokol-gfx:sg-image-desc)
  ((width :type integer :initform 0 :accessor image-desc-width)
   (height :type integer :initform 0 :accessor image-desc-height)
   (depth :type integer :initform 1 :accessor image-desc-depth)
   (num-mipmaps :type integer :initform 1 :accessor image-desc-num-mipmaps)
   (pixel-format :type keyword :initform :sg-pixelformat-rgba8 :accessor image-desc-pixel-format)
   (sample-count :type integer :initform 1 :accessor image-desc-sample-count)
   (data :type (or null list) :initform nil :accessor image-desc-data)
   (label :type (or null string) :initform nil :accessor image-desc-label))
  :documentation "Describes an image/texture.")

(defmethod to-foreign ((desc image-desc) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of desc)))))
    (zero-memory ptr '(:struct sokol-gfx:sg-image-desc))

    ;; Set dimensions
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-image-desc) 'sokol-gfx::width)
          (image-desc-width desc))
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-image-desc) 'sokol-gfx::height)
          (image-desc-height desc))
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-image-desc) 'sokol-gfx::depth)
          (image-desc-depth desc))
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-image-desc) 'sokol-gfx::num-mipmaps)
          (image-desc-num-mipmaps desc))

    ;; Set pixel format
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-image-desc) 'sokol-gfx::pixel-format)
          (foreign-enum-value 'sokol-gfx:sg-pixel-format (image-desc-pixel-format desc)))

    ;; Set sample count
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-image-desc) 'sokol-gfx::sample-count)
          (image-desc-sample-count desc))

    ;; Set data if provided (list of ranges for each mipmap level)
    (when (image-desc-data desc)
      (let ((data-array-ptr (foreign-slot-pointer ptr '(:struct sokol-gfx:sg-image-desc) 'sokol-gfx::data)))
        (loop for mip-data in (image-desc-data desc)
              for mip-level from 0
              do (let ((subimg-ptr (cffi:mem-aptr data-array-ptr '(:struct sokol-gfx:sg-image-data) mip-level)))
                   (loop for face-data in (if (listp mip-data) mip-data (list mip-data))
                         for face from 0
                         do (let ((face-ptr (cffi:mem-aptr subimg-ptr '(:struct sokol-gfx:sg-range) face)))
                              (setf (foreign-slot-value face-ptr '(:struct sokol-gfx:sg-range) 'sokol-gfx::ptr)
                                    (range-ptr face-data))
                              (setf (foreign-slot-value face-ptr '(:struct sokol-gfx:sg-range) 'sokol-gfx::size)
                                    (range-size face-data))))))))

    ptr))

;;;; Sampler Descriptor

(define-struct-wrapper sampler-desc (:struct sokol-gfx:sg-sampler-desc)
  ((min-filter :type keyword :initform :sg-filter-nearest :accessor sampler-desc-min-filter)
   (mag-filter :type keyword :initform :sg-filter-nearest :accessor sampler-desc-mag-filter)
   (mipmap-filter :type keyword :initform :sg-filter-nearest :accessor sampler-desc-mipmap-filter)
   (wrap-u :type keyword :initform :sg-wrap-repeat :accessor sampler-desc-wrap-u)
   (wrap-v :type keyword :initform :sg-wrap-repeat :accessor sampler-desc-wrap-v)
   (wrap-w :type keyword :initform :sg-wrap-repeat :accessor sampler-desc-wrap-w)
   (min-lod :type float :initform 0.0 :accessor sampler-desc-min-lod)
   (max-lod :type float :initform 1000000.0 :accessor sampler-desc-max-lod)
   (max-anisotropy :type integer :initform 1 :accessor sampler-desc-max-anisotropy)
   (label :type (or null string) :initform nil :accessor sampler-desc-label))
  :documentation "Describes a texture sampler.")

(defmethod to-foreign ((desc sampler-desc) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of desc)))))
    (zero-memory ptr '(:struct sokol-gfx:sg-sampler-desc))

    ;; Set filter modes
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-sampler-desc) 'sokol-gfx::min-filter)
          (foreign-enum-value 'sokol-gfx:sg-filter (sampler-desc-min-filter desc)))
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-sampler-desc) 'sokol-gfx::mag-filter)
          (foreign-enum-value 'sokol-gfx:sg-filter (sampler-desc-mag-filter desc)))
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-sampler-desc) 'sokol-gfx::mipmap-filter)
          (foreign-enum-value 'sokol-gfx:sg-filter (sampler-desc-mipmap-filter desc)))

    ;; Set wrap modes
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-sampler-desc) 'sokol-gfx::wrap-u)
          (foreign-enum-value 'sokol-gfx:sg-wrap (sampler-desc-wrap-u desc)))
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-sampler-desc) 'sokol-gfx::wrap-v)
          (foreign-enum-value 'sokol-gfx:sg-wrap (sampler-desc-wrap-v desc)))
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-sampler-desc) 'sokol-gfx::wrap-w)
          (foreign-enum-value 'sokol-gfx:sg-wrap (sampler-desc-wrap-w desc)))

    ;; Set LOD range
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-sampler-desc) 'sokol-gfx::min-lod)
          (float (sampler-desc-min-lod desc) 1.0))
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-sampler-desc) 'sokol-gfx::max-lod)
          (float (sampler-desc-max-lod desc) 1.0))

    ;; Set anisotropy
    (setf (foreign-slot-value ptr '(:struct sokol-gfx:sg-sampler-desc) 'sokol-gfx::max-anisotropy)
          (sampler-desc-max-anisotropy desc))

    ptr))

;;;; View Descriptor

(define-struct-wrapper view-desc (:struct sokol-gfx:sg-view-desc)
  ((image :type t :initform nil :accessor view-desc-image)
   (mipmap-level :type integer :initform 0 :accessor view-desc-mipmap-level)
   (first-layer :type integer :initform 0 :accessor view-desc-first-layer)
   (num-layers :type integer :initform 1 :accessor view-desc-num-layers)
   (label :type (or null string) :initform nil :accessor view-desc-label))
  :documentation "Describes a texture view for shader access.")

(defmethod to-foreign ((desc view-desc) &optional ptr)
  (let ((ptr (or ptr (foreign-alloc (foreign-type-of desc)))))
    (zero-memory ptr '(:struct sokol-gfx:sg-view-desc))

    ;; Set image/texture reference
    (when (view-desc-image desc)
      (let ((texture-ptr (foreign-slot-pointer ptr '(:struct sokol-gfx:sg-view-desc) 'sokol-gfx::texture)))
        ;; Set the image in the texture view
        (setf (foreign-slot-value texture-ptr '(:struct sokol-gfx:sg-texture-view-desc) 'sokol-gfx::image)
              (view-desc-image desc))
        ;; Set mipmap level
        (setf (foreign-slot-value texture-ptr '(:struct sokol-gfx:sg-texture-view-desc) 'sokol-gfx::mipmap-level)
              (view-desc-mipmap-level desc))
        ;; Set layer range
        (setf (foreign-slot-value texture-ptr '(:struct sokol-gfx:sg-texture-view-desc) 'sokol-gfx::first-layer)
              (view-desc-first-layer desc))
        (setf (foreign-slot-value texture-ptr '(:struct sokol-gfx:sg-texture-view-desc) 'sokol-gfx::num-layers)
              (view-desc-num-layers desc))))

    ptr))
