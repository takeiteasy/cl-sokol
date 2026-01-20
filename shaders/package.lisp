(defpackage #:cl-sokol-shaders
  (:use #:cl)
  (:nicknames #:cl-sokol/shaders)
  (:export
   ;; Main API
   #:define-sokol-shader
   #:shader-desc
   #:make-shader
   #:detect-backend
   #:list-defined-shaders
   #:shader-info
   #:shader-package

   ;; Reflection data - shader reflection class
   #:shader-reflection
   #:shader-name
   #:vertex-source
   #:fragment-source
   #:shader-attributes
   #:shader-uniforms
   #:vertex-uniforms
   #:fragment-uniforms
   #:extract-reflection

   ;; Reflection data - uniform blocks
   #:shader-uniform-blocks
   #:uniform-block
   #:block-name
   #:block-glsl-name
   #:block-stage
   #:block-size
   #:block-binding
   #:block-members
   #:uniform-member
   #:member-name
   #:member-type-name
   #:member-offset
   #:member-array-count

   ;; Reflection data - textures and samplers
   #:shader-textures
   #:shader-samplers
   #:texture-sampler-pairs
   #:shader-texture
   #:texture-name
   #:texture-glsl-name
   #:texture-stage
   #:texture-image-type
   #:texture-sample-type
   #:texture-multisampled
   #:texture-binding
   #:shader-sampler
   #:sampler-name
   #:sampler-glsl-name
   #:sampler-stage
   #:sampler-filter-type
   #:sampler-binding

   ;; Backend support
   #:sokol-backend-to-shader-backend
   #:*supported-backends*
   #:*backend-mapping*

   ;; Low-level
   #:generate-shader-source
   #:build-shader-desc
   #:build-shader-desc-for-backend
   #:invalidate-shader-cache

   ;; Type mapping
   #:type-to-attr-base-type
   #:type-to-uniform-type
   #:type-size-bytes
   #:type-size-std140))
