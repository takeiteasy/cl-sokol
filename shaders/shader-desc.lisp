(in-package #:cl-sokol-shaders)

;;; Build sg_shader_desc structures from shader reflection data

(defvar *shader-desc-cache* (make-hash-table :test 'equal)
  "Cache of compiled shader descriptors, keyed by (name . backend).")

(defun cache-key (name backend)
  "Create a cache key from name (string) and backend."
  (cons name backend))

(defun get-cached-shader-desc (name backend)
  "Get a cached shader descriptor, or NIL if not cached."
  (gethash (cache-key name backend) *shader-desc-cache*))

(defun cache-shader-desc (name backend desc)
  "Cache a shader descriptor."
  (setf (gethash (cache-key name backend) *shader-desc-cache*) desc))

(defun invalidate-shader-cache (&optional name)
  "Invalidate cached shader descriptors. If NAME is provided, only invalidate that shader."
  (if name
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (when (eq (car k) name)
                   (remhash k *shader-desc-cache*)))
               *shader-desc-cache*)
      (clrhash *shader-desc-cache*)))

(defun string-to-foreign-bytes (string)
  "Convert a Lisp string to a foreign byte array (null-terminated).
Uses CFFI's foreign-string-alloc which handles encoding."
  (cffi:foreign-string-alloc string :encoding :utf-8))

(defun set-shader-function (func-ptr source entry-point)
  "Set up a sg_shader_function struct with source code."
  ;; Set source as a string pointer
  (setf (cffi:foreign-slot-value func-ptr '(:struct sg:sg-shader-function) 'sg::source)
        (cffi:foreign-string-alloc source))
  ;; Set entry point
  (setf (cffi:foreign-slot-value func-ptr '(:struct sg:sg-shader-function) 'sg::entry)
        (cffi:foreign-string-alloc entry-point)))

(defun entry-point-for-backend (shader-backend)
  "Return the entry point name for a shader backend."
  (case shader-backend
    (:metal "main0")  ; Metal uses main0
    (:hlsl "main")    ; HLSL uses main
    (:wgsl "main")    ; WGSL uses main (with @vertex/@fragment decorators)
    (t "main")))      ; GLSL uses main

(defun set-shader-attrs (desc attr-base-types)
  "Set up the attrs array in sg_shader_desc.
ATTR-BASE-TYPES is a list of 16 sokol attr base type keywords."
  (let ((attrs-ptr (cffi:foreign-slot-pointer desc '(:struct sg:sg-shader-desc) 'sg::attrs)))
    (loop for i from 0 below 16
          for base-type in attr-base-types
          for attr-ptr = (if (zerop i)
                             attrs-ptr
                             (cffi:mem-aptr attrs-ptr '(:struct sg:sg-shader-vertex-attr) i))
          do (setf (cffi:foreign-slot-value attr-ptr '(:struct sg:sg-shader-vertex-attr) 'sg::base-type)
                   (cffi:foreign-enum-value 'sg:sg-shader-attr-base-type base-type)))))

(defun zero-shader-desc (desc)
  "Zero out a sg_shader_desc struct."
  (loop for i from 0 below (cffi:foreign-type-size '(:struct sg:sg-shader-desc))
        do (setf (cffi:mem-aref desc :uint8 i) 0)))

;;; Uniform Block Descriptor Building

(defun stage-to-shader-stage (stage)
  "Convert a stage keyword to sg-shader-stage enum value."
  (case stage
    (:vertex (cffi:foreign-enum-value 'sg:sg-shader-stage :sg-shaderstage-vertex))
    (:fragment (cffi:foreign-enum-value 'sg:sg-shader-stage :sg-shaderstage-fragment))
    (:both (cffi:foreign-enum-value 'sg:sg-shader-stage :sg-shaderstage-fragment)) ; Use fragment for combined
    (otherwise (cffi:foreign-enum-value 'sg:sg-shader-stage :sg-shaderstage-none))))

(defun set-uniform-block-desc (ub-ptr block shader-backend index)
  "Set up a single sg_shader_uniform_block struct.
UB-PTR is the pointer to the uniform block struct.
BLOCK is a uniform-block object.
SHADER-BACKEND is :glsl, :metal, :hlsl, or :wgsl.
INDEX is the uniform block index (0-7)."
  ;; Set stage
  (setf (cffi:foreign-slot-value ub-ptr '(:struct sg:sg-shader-uniform-block) 'sg::stage)
        (stage-to-shader-stage (block-stage block)))
  ;; Set size (must be aligned to 16 bytes for most backends)
  (let ((size (block-size block)))
    ;; Align size to 16 bytes
    (setf size (* 16 (ceiling size 16)))
    (setf (cffi:foreign-slot-value ub-ptr '(:struct sg:sg-shader-uniform-block) 'sg::size)
          size))
  ;; Set backend-specific binding slots
  (let ((binding (or (block-binding block) index)))
    (case shader-backend
      (:metal
       (setf (cffi:foreign-slot-value ub-ptr '(:struct sg:sg-shader-uniform-block) 'sg::msl-buffer-n)
             binding))
      (:hlsl
       (setf (cffi:foreign-slot-value ub-ptr '(:struct sg:sg-shader-uniform-block) 'sg::hlsl-register-b-n)
             binding))
      (:wgsl
       (setf (cffi:foreign-slot-value ub-ptr '(:struct sg:sg-shader-uniform-block) 'sg::wgsl-group0-binding-n)
             binding))
      ;; GLSL doesn't use uniform blocks directly - it uses the glsl_uniforms array
      ;; But we still set the binding for layout(binding=N)
      (:glsl
       ;; For GLSL 420+ with explicit bindings, we can use binding layout qualifier
       ;; The actual uniform setup is handled differently
       nil))))

(defun set-shader-uniform-blocks (desc reflection shader-backend)
  "Populate the uniform_blocks array in sg_shader_desc."
  (let ((blocks (shader-uniform-blocks reflection))
        (ubs-ptr (cffi:foreign-slot-pointer desc '(:struct sg:sg-shader-desc) 'sg::uniform-blocks)))
    (loop for block in blocks
          for i from 0 below 8  ; sokol supports up to 8 uniform blocks
          for ub-ptr = (cffi:mem-aptr ubs-ptr '(:struct sg:sg-shader-uniform-block) i)
          do (set-uniform-block-desc ub-ptr block shader-backend i))))

;;; Texture/Sampler Descriptor Building

(defun image-type-to-sg-image-type (image-type)
  "Convert image type keyword to sg-image-type enum value."
  (cffi:foreign-enum-value 'sg:sg-image-type
                           (case image-type
                             (:2d :sg-imagetype-2d)
                             (:3d :sg-imagetype-3d)
                             (:cube :sg-imagetype-cube)
                             (:array :sg-imagetype-array)
                             (otherwise :sg-imagetype-2d))))

(defun sample-type-to-sg-image-sample-type (sample-type)
  "Convert sample type keyword to sg-image-sample-type enum value."
  (cffi:foreign-enum-value 'sg:sg-image-sample-type
                           (case sample-type
                             (:float :sg-imagesampletype-float)
                             (:depth :sg-imagesampletype-depth)
                             (:sint :sg-imagesampletype-sint)
                             (:uint :sg-imagesampletype-uint)
                             (otherwise :sg-imagesampletype-float))))

(defun set-shader-image-desc (img-ptr texture shader-backend index)
  "Set up a single sg_shader_image struct (texture view)."
  ;; Set stage
  (setf (cffi:foreign-slot-value img-ptr '(:struct sg:sg-shader-image) 'sg::stage)
        (stage-to-shader-stage (texture-stage texture)))
  ;; Set image type
  (setf (cffi:foreign-slot-value img-ptr '(:struct sg:sg-shader-image) 'sg::image-type)
        (image-type-to-sg-image-type (texture-image-type texture)))
  ;; Set sample type
  (setf (cffi:foreign-slot-value img-ptr '(:struct sg:sg-shader-image) 'sg::sample-type)
        (sample-type-to-sg-image-sample-type (texture-sample-type texture)))
  ;; Set multisampled
  (setf (cffi:foreign-slot-value img-ptr '(:struct sg:sg-shader-image) 'sg::multisampled)
        (if (texture-multisampled texture) 1 0))
  ;; Set backend-specific bindings
  (let ((binding (or (texture-binding texture) index)))
    (case shader-backend
      (:metal
       (setf (cffi:foreign-slot-value img-ptr '(:struct sg:sg-shader-image) 'sg::msl-texture-n)
             binding))
      (:hlsl
       (setf (cffi:foreign-slot-value img-ptr '(:struct sg:sg-shader-image) 'sg::hlsl-register-t-n)
             binding))
      (:wgsl
       (setf (cffi:foreign-slot-value img-ptr '(:struct sg:sg-shader-image) 'sg::wgsl-group1-binding-n)
             binding))
      (:glsl
       ;; GLSL uses combined texture-samplers, handled in image_sampler_pairs
       nil))))

(defun set-shader-sampler-desc (smp-ptr texture shader-backend index)
  "Set up a single sg_shader_sampler struct.
For GLSL combined samplers, the sampler is implicit in the texture."
  ;; Set stage
  (setf (cffi:foreign-slot-value smp-ptr '(:struct sg:sg-shader-sampler) 'sg::stage)
        (stage-to-shader-stage (texture-stage texture)))
  ;; Set sampler type (filtering by default)
  (setf (cffi:foreign-slot-value smp-ptr '(:struct sg:sg-shader-sampler) 'sg::sampler-type)
        (cffi:foreign-enum-value 'sg:sg-sampler-type
                                 (if (eq (texture-sample-type texture) :depth)
                                     :sg-samplertype-comparison
                                     :sg-samplertype-filtering)))
  ;; Set backend-specific bindings
  (let ((binding (or (texture-binding texture) index)))
    (case shader-backend
      (:metal
       (setf (cffi:foreign-slot-value smp-ptr '(:struct sg:sg-shader-sampler) 'sg::msl-sampler-n)
             binding))
      (:hlsl
       (setf (cffi:foreign-slot-value smp-ptr '(:struct sg:sg-shader-sampler) 'sg::hlsl-register-s-n)
             binding))
      (:wgsl
       ;; WGSL samplers are in group 1, after textures
       (setf (cffi:foreign-slot-value smp-ptr '(:struct sg:sg-shader-sampler) 'sg::wgsl-group1-binding-n)
             (+ binding 16)))  ; Offset samplers after textures
      (:glsl
       ;; GLSL uses combined samplers
       nil))))

(defun set-shader-image-sampler-pair (pair-ptr texture index)
  "Set up a single sg_shader_image_sampler_pair struct for GLSL combined samplers."
  ;; Set stage
  (setf (cffi:foreign-slot-value pair-ptr '(:struct sg:sg-shader-image-sampler-pair) 'sg::stage)
        (stage-to-shader-stage (texture-stage texture)))
  ;; Set image slot index
  (setf (cffi:foreign-slot-value pair-ptr '(:struct sg:sg-shader-image-sampler-pair) 'sg::image-slot)
        index)
  ;; Set sampler slot index (same as image for combined samplers)
  (setf (cffi:foreign-slot-value pair-ptr '(:struct sg:sg-shader-image-sampler-pair) 'sg::sampler-slot)
        index)
  ;; Set GLSL name for the combined sampler
  (setf (cffi:foreign-slot-value pair-ptr '(:struct sg:sg-shader-image-sampler-pair) 'sg::glsl-name)
        (cffi:foreign-string-alloc (texture-glsl-name texture))))

(defun set-shader-textures-and-samplers (desc reflection shader-backend)
  "Populate the images, samplers, and image_sampler_pairs arrays in sg_shader_desc."
  (let ((textures (shader-textures reflection)))
    (when textures
      (let ((imgs-ptr (cffi:foreign-slot-pointer desc '(:struct sg:sg-shader-desc) 'sg::images))
            (smps-ptr (cffi:foreign-slot-pointer desc '(:struct sg:sg-shader-desc) 'sg::samplers))
            (pairs-ptr (cffi:foreign-slot-pointer desc '(:struct sg:sg-shader-desc) 'sg::image-sampler-pairs)))
        (loop for texture in textures
              for i from 0 below 16  ; sokol supports up to 16 images/samplers
              for img-ptr = (cffi:mem-aptr imgs-ptr '(:struct sg:sg-shader-image) i)
              for smp-ptr = (cffi:mem-aptr smps-ptr '(:struct sg:sg-shader-sampler) i)
              for pair-ptr = (cffi:mem-aptr pairs-ptr '(:struct sg:sg-shader-image-sampler-pair) i)
              do (set-shader-image-desc img-ptr texture shader-backend i)
                 (set-shader-sampler-desc smp-ptr texture shader-backend i)
                 ;; For GLSL, also set up image-sampler pairs
                 (when (eq shader-backend :glsl)
                   (set-shader-image-sampler-pair pair-ptr texture i)))))))

(defun build-shader-desc (name reflection shader-backend sokol-backend)
  "Build a sg_shader_desc from shader reflection data.
Returns a foreign pointer to the descriptor (caller is responsible for memory)."
  (let ((desc (cffi:foreign-alloc '(:struct sg:sg-shader-desc)))
        (entry-point (entry-point-for-backend shader-backend)))
    ;; Zero out the descriptor
    (zero-shader-desc desc)

    ;; Set label
    (setf (cffi:foreign-slot-value desc '(:struct sg:sg-shader-desc) 'sg::label)
          (cffi:foreign-string-alloc (format nil "~(~A~)_shader" name)))

    ;; Set vertex function
    (let ((vf-ptr (cffi:foreign-slot-pointer desc '(:struct sg:sg-shader-desc) 'sg::vertex-func)))
      (set-shader-function vf-ptr
                           (vertex-source reflection)
                           entry-point))

    ;; Set fragment function
    (let ((ff-ptr (cffi:foreign-slot-pointer desc '(:struct sg:sg-shader-desc) 'sg::fragment-func)))
      (set-shader-function ff-ptr
                           (fragment-source reflection)
                           entry-point))

    ;; Set vertex attributes
    (set-shader-attrs desc (reflection-attr-base-types reflection))

    ;; Set uniform blocks
    (set-shader-uniform-blocks desc reflection shader-backend)

    ;; Set textures and samplers
    (set-shader-textures-and-samplers desc reflection shader-backend)

    ;; Cache and return
    (cache-shader-desc name sokol-backend desc)
    desc))

(defun build-shader-desc-for-backend (name vertex-entry fragment-entry sokol-backend)
  "Build a shader descriptor for a specific sokol backend.
This is the main entry point for creating shader descriptors."
  ;; Check cache first
  (let ((cached (get-cached-shader-desc name sokol-backend)))
    (when cached
      (return-from build-shader-desc-for-backend cached)))

  ;; Determine shader backend
  (let ((shader-backend (sokol-backend-to-shader-backend sokol-backend)))
    ;; Check for supported backends
    (unless (member shader-backend '(:glsl :metal :hlsl :wgsl))
      (error "Backend ~A (shader backend ~A) not yet implemented."
             sokol-backend shader-backend))

    ;; Generate reflection data using the appropriate backend
    (let* ((reflection (extract-reflection name vertex-entry fragment-entry
                                           :backend shader-backend
                                           :version (case shader-backend
                                                      (:glsl (sokol-backend-to-glsl-version sokol-backend))
                                                      (t nil)))))
      ;; Build and return the descriptor
      (build-shader-desc name reflection shader-backend sokol-backend))))
