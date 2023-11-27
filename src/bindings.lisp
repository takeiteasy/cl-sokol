;;;; bindings.lisp

(in-package #:%sokol)

(pushnew (asdf:system-relative-pathname :cl-sokol "bin/") *foreign-library-directories*)
(define-foreign-library libsokol
  (t (:default "libsokol")))

(unless (foreign-library-loaded-p 'libsokol)
  (use-foreign-library libsokol))

(defcstruct (%sg-buffer :class sg-buffer-type)
  (id :unsigned-int))

(defcstruct (%sg-image :class sg-image-type)
  (id :unsigned-int))

(defcstruct (%sg-sampler :class sg-sampler-type)
  (id :unsigned-int))

(defcstruct (%sg-shader :class sg-shader-type)
  (id :unsigned-int))

(defcstruct (%sg-pipeline :class sg-pipeline-type)
  (id :unsigned-int))

(defcstruct (%sg-pass :class sg-pass-type)
  (id :unsigned-int))

(defcstruct (%sg-context :class sg-context-type)
  (id :unsigned-int))

(defcstruct (%sg-range :class sg-range-type)
  (ptr (:pointer :void))
  (size :unsigned-long))

(defconstant +sg-invalid-id+ 0)
(defconstant +sg-num-shader-stages+ 2)
(defconstant +sg-num-inflight-frames+ 2)
(defconstant +sg-max-color-attachments+ 4)
(defconstant +sg-max-vertex-buffers+ 8)
(defconstant +sg-max-shaderstage-images+ 12)
(defconstant +sg-max-shaderstage-samplers+ 8)
(defconstant +sg-max-shaderstage-imagesamplerpairs+ 12)
(defconstant +sg-max-shaderstage-ubs+ 4)
(defconstant +sg-max-ub-members+ 16)
(defconstant +sg-max-vertex-attributes+ 16)
(defconstant +sg-max-mipmaps+ 16)
(defconstant +sg-max-texturearray-layers+ 128)

(defcstruct (%sg-color :class sg-color-type)
  (r :float)
  (g :float)
  (b :float)
  (a :float))

(defcenum sg-backend
  (:sg-backend-glcore33 0)
  (:sg-backend-gles3 1)
  (:sg-backend-d3d11 2)
  (:sg-backend-metal-ios 3)
  (:sg-backend-metal-macos 4)
  (:sg-backend-metal-simulator 5)
  (:sg-backend-wgpu 6)
  (:sg-backend-dummy 7))

(defcenum sg-pixel-format
  (:-sg-pixelformat-default 0)
  (:sg-pixelformat-none 1)
  (:sg-pixelformat-r8 2)
  (:sg-pixelformat-r8sn 3)
  (:sg-pixelformat-r8ui 4)
  (:sg-pixelformat-r8si 5)
  (:sg-pixelformat-r16 6)
  (:sg-pixelformat-r16sn 7)
  (:sg-pixelformat-r16ui 8)
  (:sg-pixelformat-r16si 9)
  (:sg-pixelformat-r16f 10)
  (:sg-pixelformat-rg8 11)
  (:sg-pixelformat-rg8sn 12)
  (:sg-pixelformat-rg8ui 13)
  (:sg-pixelformat-rg8si 14)
  (:sg-pixelformat-r32ui 15)
  (:sg-pixelformat-r32si 16)
  (:sg-pixelformat-r32f 17)
  (:sg-pixelformat-rg16 18)
  (:sg-pixelformat-rg16sn 19)
  (:sg-pixelformat-rg16ui 20)
  (:sg-pixelformat-rg16si 21)
  (:sg-pixelformat-rg16f 22)
  (:sg-pixelformat-rgba8 23)
  (:sg-pixelformat-srgb8a8 24)
  (:sg-pixelformat-rgba8sn 25)
  (:sg-pixelformat-rgba8ui 26)
  (:sg-pixelformat-rgba8si 27)
  (:sg-pixelformat-bgra8 28)
  (:sg-pixelformat-rgb10a2 29)
  (:sg-pixelformat-rg11b10f 30)
  (:sg-pixelformat-rg32ui 31)
  (:sg-pixelformat-rg32si 32)
  (:sg-pixelformat-rg32f 33)
  (:sg-pixelformat-rgba16 34)
  (:sg-pixelformat-rgba16sn 35)
  (:sg-pixelformat-rgba16ui 36)
  (:sg-pixelformat-rgba16si 37)
  (:sg-pixelformat-rgba16f 38)
  (:sg-pixelformat-rgba32ui 39)
  (:sg-pixelformat-rgba32si 40)
  (:sg-pixelformat-rgba32f 41)
  (:sg-pixelformat-depth 42)
  (:sg-pixelformat-depth-stencil 43)
  (:sg-pixelformat-bc1-rgba 44)
  (:sg-pixelformat-bc2-rgba 45)
  (:sg-pixelformat-bc3-rgba 46)
  (:sg-pixelformat-bc4-r 47)
  (:sg-pixelformat-bc4-rsn 48)
  (:sg-pixelformat-bc5-rg 49)
  (:sg-pixelformat-bc5-rgsn 50)
  (:sg-pixelformat-bc6h-rgbf 51)
  (:sg-pixelformat-bc6h-rgbuf 52)
  (:sg-pixelformat-bc7-rgba 53)
  (:sg-pixelformat-pvrtc-rgb-2bpp 54)
  (:sg-pixelformat-pvrtc-rgb-4bpp 55)
  (:sg-pixelformat-pvrtc-rgba-2bpp 56)
  (:sg-pixelformat-pvrtc-rgba-4bpp 57)
  (:sg-pixelformat-etc2-rgb8 58)
  (:sg-pixelformat-etc2-rgb8a1 59)
  (:sg-pixelformat-etc2-rgba8 60)
  (:sg-pixelformat-etc2-rg11 61)
  (:sg-pixelformat-etc2-rg11sn 62)
  (:sg-pixelformat-rgb9e5 63)
  (:-sg-pixelformat-num 64)
  (:-sg-pixelformat-force-u32 2147483647))

(defcstruct (%sg-pixelformat-info :class sg-pixelformat-info-type)
  (sample :int)
  (filter :int)
  (render :int)
  (blend :int)
  (msaa :int)
  (depth :int))

(defcstruct (%sg-features :class sg-features-type)
  (origin-top-left :int)
  (image-clamp-to-border :int)
  (mrt-independent-blend-state :int)
  (mrt-independent-write-mask :int))

(defcstruct (%sg-limits :class sg-limits-type)
  (max-image-size-2d :int)
  (max-image-size-cube :int)
  (max-image-size-3d :int)
  (max-image-size-array :int)
  (max-image-array-layers :int)
  (max-vertex-attrs :int)
  (gl-max-vertex-uniform-vectors :int)
  (gl-max-combined-texture-image-units :int))

(defcenum sg-resource-state
  (:sg-resourcestate-initial 0)
  (:sg-resourcestate-alloc 1)
  (:sg-resourcestate-valid 2)
  (:sg-resourcestate-failed 3)
  (:sg-resourcestate-invalid 4)
  (:-sg-resourcestate-force-u32 2147483647))

(defcenum sg-usage
  (:-sg-usage-default 0)
  (:sg-usage-immutable 1)
  (:sg-usage-dynamic 2)
  (:sg-usage-stream 3)
  (:-sg-usage-num 4)
  (:-sg-usage-force-u32 2147483647))

(defcenum sg-buffer-type
  (:-sg-buffertype-default 0)
  (:sg-buffertype-vertexbuffer 1)
  (:sg-buffertype-indexbuffer 2)
  (:-sg-buffertype-num 3)
  (:-sg-buffertype-force-u32 2147483647))

(defcenum sg-index-type
  (:-sg-indextype-default 0)
  (:sg-indextype-none 1)
  (:sg-indextype-uint16 2)
  (:sg-indextype-uint32 3)
  (:-sg-indextype-num 4)
  (:-sg-indextype-force-u32 2147483647))

(defcenum sg-image-type
  (:-sg-imagetype-default 0)
  (:sg-imagetype-2d 1)
  (:sg-imagetype-cube 2)
  (:sg-imagetype-3d 3)
  (:sg-imagetype-array 4)
  (:-sg-imagetype-num 5)
  (:-sg-imagetype-force-u32 2147483647))

(defcenum sg-image-sample-type
  (:-sg-imagesampletype-default 0)
  (:sg-imagesampletype-float 1)
  (:sg-imagesampletype-depth 2)
  (:sg-imagesampletype-sint 3)
  (:sg-imagesampletype-uint 4)
  (:sg-imagesampletype-unfilterable-float 5)
  (:-sg-imagesampletype-num 6)
  (:-sg-imagesampletype-force-u32 2147483647))

(defcenum sg-sampler-type
  (:-sg-samplertype-default 0)
  (:sg-samplertype-filtering 1)
  (:sg-samplertype-nonfiltering 2)
  (:sg-samplertype-comparison 3)
  (:-sg-samplertype-num 4)
  (:-sg-samplertype-force-u32 5))

(defcenum sg-cube-face
  (:sg-cubeface-pos-x 0)
  (:sg-cubeface-neg-x 1)
  (:sg-cubeface-pos-y 2)
  (:sg-cubeface-neg-y 3)
  (:sg-cubeface-pos-z 4)
  (:sg-cubeface-neg-z 5)
  (:sg-cubeface-num 6)
  (:-sg-cubeface-force-u32 2147483647))

(defcenum sg-shader-stage
  (:sg-shaderstage-vs 0)
  (:sg-shaderstage-fs 1)
  (:-sg-shaderstage-force-u32 2147483647))

(defcenum sg-primitive-type
  (:-sg-primitivetype-default 0)
  (:sg-primitivetype-points 1)
  (:sg-primitivetype-lines 2)
  (:sg-primitivetype-line-strip 3)
  (:sg-primitivetype-triangles 4)
  (:sg-primitivetype-triangle-strip 5)
  (:-sg-primitivetype-num 6)
  (:-sg-primitivetype-force-u32 2147483647))

(defcenum sg-filter
  (:-sg-filter-default 0)
  (:sg-filter-none 1)
  (:sg-filter-nearest 2)
  (:sg-filter-linear 3)
  (:-sg-filter-num 4)
  (:-sg-filter-force-u32 2147483647))

(defcenum sg-wrap
  (:-sg-wrap-default 0)
  (:sg-wrap-repeat 1)
  (:sg-wrap-clamp-to-edge 2)
  (:sg-wrap-clamp-to-border 3)
  (:sg-wrap-mirrored-repeat 4)
  (:-sg-wrap-num 5)
  (:-sg-wrap-force-u32 2147483647))

(defcenum sg-border-color
  (:-sg-bordercolor-default 0)
  (:sg-bordercolor-transparent-black 1)
  (:sg-bordercolor-opaque-black 2)
  (:sg-bordercolor-opaque-white 3)
  (:-sg-bordercolor-num 4)
  (:-sg-bordercolor-force-u32 2147483647))

(defcenum sg-vertex-format
  (:sg-vertexformat-invalid 0)
  (:sg-vertexformat-float 1)
  (:sg-vertexformat-float2 2)
  (:sg-vertexformat-float3 3)
  (:sg-vertexformat-float4 4)
  (:sg-vertexformat-byte4 5)
  (:sg-vertexformat-byte4n 6)
  (:sg-vertexformat-ubyte4 7)
  (:sg-vertexformat-ubyte4n 8)
  (:sg-vertexformat-short2 9)
  (:sg-vertexformat-short2n 10)
  (:sg-vertexformat-ushort2n 11)
  (:sg-vertexformat-short4 12)
  (:sg-vertexformat-short4n 13)
  (:sg-vertexformat-ushort4n 14)
  (:sg-vertexformat-uint10-n2 15)
  (:sg-vertexformat-half2 16)
  (:sg-vertexformat-half4 17)
  (:-sg-vertexformat-num 18)
  (:-sg-vertexformat-force-u32 2147483647))

(defcenum sg-vertex-step
  (:-sg-vertexstep-default 0)
  (:sg-vertexstep-per-vertex 1)
  (:sg-vertexstep-per-instance 2)
  (:-sg-vertexstep-num 3)
  (:-sg-vertexstep-force-u32 2147483647))

(defcenum sg-uniform-type
  (:sg-uniformtype-invalid 0)
  (:sg-uniformtype-float 1)
  (:sg-uniformtype-float2 2)
  (:sg-uniformtype-float3 3)
  (:sg-uniformtype-float4 4)
  (:sg-uniformtype-int 5)
  (:sg-uniformtype-int2 6)
  (:sg-uniformtype-int3 7)
  (:sg-uniformtype-int4 8)
  (:sg-uniformtype-mat4 9)
  (:-sg-uniformtype-num 10)
  (:-sg-uniformtype-force-u32 2147483647))

(defcenum sg-uniform-layout
  (:-sg-uniformlayout-default 0)
  (:sg-uniformlayout-native 1)
  (:sg-uniformlayout-std140 2)
  (:-sg-uniformlayout-num 3)
  (:-sg-uniformlayout-force-u32 2147483647))

(defcenum sg-cull-mode
  (:-sg-cullmode-default 0)
  (:sg-cullmode-none 1)
  (:sg-cullmode-front 2)
  (:sg-cullmode-back 3)
  (:-sg-cullmode-num 4)
  (:-sg-cullmode-force-u32 2147483647))

(defcenum sg-face-winding
  (:-sg-facewinding-default 0)
  (:sg-facewinding-ccw 1)
  (:sg-facewinding-cw 2)
  (:-sg-facewinding-num 3)
  (:-sg-facewinding-force-u32 2147483647))

(defcenum sg-compare-func
  (:-sg-comparefunc-default 0)
  (:sg-comparefunc-never 1)
  (:sg-comparefunc-less 2)
  (:sg-comparefunc-equal 3)
  (:sg-comparefunc-less-equal 4)
  (:sg-comparefunc-greater 5)
  (:sg-comparefunc-not-equal 6)
  (:sg-comparefunc-greater-equal 7)
  (:sg-comparefunc-always 8)
  (:-sg-comparefunc-num 9)
  (:-sg-comparefunc-force-u32 2147483647))

(defcenum sg-stencil-op
  (:-sg-stencilop-default 0)
  (:sg-stencilop-keep 1)
  (:sg-stencilop-zero 2)
  (:sg-stencilop-replace 3)
  (:sg-stencilop-incr-clamp 4)
  (:sg-stencilop-decr-clamp 5)
  (:sg-stencilop-invert 6)
  (:sg-stencilop-incr-wrap 7)
  (:sg-stencilop-decr-wrap 8)
  (:-sg-stencilop-num 9)
  (:-sg-stencilop-force-u32 2147483647))

(defcenum sg-blend-factor
  (:-sg-blendfactor-default 0)
  (:sg-blendfactor-zero 1)
  (:sg-blendfactor-one 2)
  (:sg-blendfactor-src-color 3)
  (:sg-blendfactor-one-minus-src-color 4)
  (:sg-blendfactor-src-alpha 5)
  (:sg-blendfactor-one-minus-src-alpha 6)
  (:sg-blendfactor-dst-color 7)
  (:sg-blendfactor-one-minus-dst-color 8)
  (:sg-blendfactor-dst-alpha 9)
  (:sg-blendfactor-one-minus-dst-alpha 10)
  (:sg-blendfactor-src-alpha-saturated 11)
  (:sg-blendfactor-blend-color 12)
  (:sg-blendfactor-one-minus-blend-color 13)
  (:sg-blendfactor-blend-alpha 14)
  (:sg-blendfactor-one-minus-blend-alpha 15)
  (:-sg-blendfactor-num 16)
  (:-sg-blendfactor-force-u32 2147483647))

(defcenum sg-blend-op
  (:-sg-blendop-default 0)
  (:sg-blendop-add 1)
  (:sg-blendop-subtract 2)
  (:sg-blendop-reverse-subtract 3)
  (:-sg-blendop-num 4)
  (:-sg-blendop-force-u32 2147483647))

(defcenum sg-color-mask
  (:-sg-colormask-default 0)
  (:sg-colormask-none 16)
  (:sg-colormask-r 1)
  (:sg-colormask-g 2)
  (:sg-colormask-rg 3)
  (:sg-colormask-b 4)
  (:sg-colormask-rb 5)
  (:sg-colormask-gb 6)
  (:sg-colormask-rgb 7)
  (:sg-colormask-a 8)
  (:sg-colormask-ra 9)
  (:sg-colormask-ga 10)
  (:sg-colormask-rga 11)
  (:sg-colormask-ba 12)
  (:sg-colormask-rba 13)
  (:sg-colormask-gba 14)
  (:sg-colormask-rgba 15)
  (:-sg-colormask-force-u32 2147483647))

(defcenum sg-load-action
  (:-sg-loadaction-default 0)
  (:sg-loadaction-clear 1)
  (:sg-loadaction-load 2)
  (:sg-loadaction-dontcare 3)
  (:-sg-loadaction-force-u32 2147483647))

(defcenum sg-store-action
  (:-sg-storeaction-default 0)
  (:sg-storeaction-store 1)
  (:sg-storeaction-dontcare 2)
  (:-sg-storeaction-force-u32 2147483647))

(defcstruct (%sg-color-attachment-action :class sg-color-attachment-action-type)
  (load-action sg-load-action)
  (store-action sg-store-action)
  (clear-value (:struct %sg-color)))

(defcstruct (%sg-depth-attachment-action :class sg-depth-attachment-action-type)
  (load-action sg-load-action)
  (store-action sg-store-action)
  (clear-value :float))

(defcstruct (%sg-stencil-attachment-action :class sg-stencil-attachment-action-type)
  (load-action sg-load-action)
  (store-action sg-store-action)
  (clear-value :unsigned-char))

(defcstruct (%sg-pass-action :class sg-pass-action-type)
  (-start-canary :unsigned-int)
  (colors (:array (:struct %sg-color-attachment-action) 4))
  (depth (:struct %sg-depth-attachment-action))
  (stencil (:struct %sg-stencil-attachment-action))
  (-end-canary :unsigned-int))

(defcstruct (%sg-stage-bindings :class sg-stage-bindings-type)
  (images (:array (:struct %sg-image) 12))
  (samplers (:array (:struct %sg-sampler) 8)))

(defcstruct (%sg-bindings :class sg-bindings-type)
  (-start-canary :unsigned-int)
  (vertex-buffers (:array (:struct %sg-buffer) 8))
  (vertex-buffer-offsets (:array :int 8))
  (index-buffer (:struct %sg-buffer))
  (index-buffer-offset :int)
  (vs (:struct %sg-stage-bindings))
  (fs (:struct %sg-stage-bindings))
  (-end-canary :unsigned-int))

(defcstruct (%sg-buffer-desc :class sg-buffer-desc-type)
  (-start-canary :unsigned-int)
  (size :unsigned-long)
  (type sg-buffer-type)
  (usage sg-usage)
  (data (:struct %sg-range))
  (label (:pointer :char))
  (gl-buffers (:array :unsigned-int 2))
  (mtl-buffers (:array (:pointer :void) 2))
  (d3d11-buffer (:pointer :void))
  (wgpu-buffer (:pointer :void))
  (-end-canary :unsigned-int))

(defcstruct (%sg-image-data :class sg-image-data-type)
  (subimage (:array (:array (:struct %sg-range) 16) 6)))

(defcstruct (%sg-image-desc :class sg-image-desc-type)
  (-start-canary :unsigned-int)
  (type sg-image-type)
  (render-target :int)
  (width :int)
  (height :int)
  (num-slices :int)
  (num-mipmaps :int)
  (usage sg-usage)
  (pixel-format sg-pixel-format)
  (sample-count :int)
  (data (:struct %sg-image-data))
  (label (:pointer :char))
  (gl-textures (:array :unsigned-int 2))
  (gl-texture-target :unsigned-int)
  (mtl-textures (:array (:pointer :void) 2))
  (d3d11-texture (:pointer :void))
  (d3d11-shader-resource-view (:pointer :void))
  (wgpu-texture (:pointer :void))
  (wgpu-texture-view (:pointer :void))
  (-end-canary :unsigned-int))

(defcstruct (%sg-sampler-desc :class sg-sampler-desc-type)
  (-start-canary :unsigned-int)
  (min-filter sg-filter)
  (mag-filter sg-filter)
  (mipmap-filter sg-filter)
  (wrap-u sg-wrap)
  (wrap-v sg-wrap)
  (wrap-w sg-wrap)
  (min-lod :float)
  (max-lod :float)
  (border-color sg-border-color)
  (compare sg-compare-func)
  (max-anisotropy :unsigned-int)
  (label (:pointer :char))
  (gl-sampler :unsigned-int)
  (mtl-sampler (:pointer :void))
  (d3d11-sampler (:pointer :void))
  (wgpu-sampler (:pointer :void))
  (-end-canary :unsigned-int))

(defcstruct (%sg-shader-attr-desc :class sg-shader-attr-desc-type)
  (name (:pointer :char))
  (sem-name (:pointer :char))
  (sem-index :int))

(defcstruct (%sg-shader-uniform-desc :class sg-shader-uniform-desc-type)
  (name (:pointer :char))
  (type sg-uniform-type)
  (array-count :int))

(defcstruct (%sg-shader-uniform-block-desc :class sg-shader-uniform-block-desc-type)
  (size :unsigned-long)
  (layout sg-uniform-layout)
  (uniforms (:array (:struct %sg-shader-uniform-desc) 16)))

(defcstruct (%sg-shader-image-desc :class sg-shader-image-desc-type)
  (used :int)
  (multisampled :int)
  (image-type sg-image-type)
  (sample-type sg-image-sample-type))

(defcstruct (%sg-shader-sampler-desc :class sg-shader-sampler-desc-type)
  (used :int)
  (sampler-type sg-sampler-type))

(defcstruct (%sg-shader-image-sampler-pair-desc :class sg-shader-image-sampler-pair-desc-type)
  (used :int)
  (image-slot :int)
  (sampler-slot :int)
  (glsl-name (:pointer :char)))

(defcstruct (%sg-shader-stage-desc :class sg-shader-stage-desc-type)
  (source (:pointer :char))
  (bytecode (:struct %sg-range))
  (entry (:pointer :char))
  (d3d11-target (:pointer :char))
  (uniform-blocks (:array (:struct %sg-shader-uniform-block-desc) 4))
  (images (:array (:struct %sg-shader-image-desc) 12))
  (samplers (:array (:struct %sg-shader-sampler-desc) 8))
  (image-sampler-pairs (:array (:struct %sg-shader-image-sampler-pair-desc) 12)))

(defcstruct (%sg-shader-desc :class sg-shader-desc-type)
  (-start-canary :unsigned-int)
  (attrs (:array (:struct %sg-shader-attr-desc) 16))
  (vs (:struct %sg-shader-stage-desc))
  (fs (:struct %sg-shader-stage-desc))
  (label (:pointer :char))
  (-end-canary :unsigned-int))

(defcstruct (%sg-vertex-buffer-layout-state :class sg-vertex-buffer-layout-state-type)
  (stride :int)
  (step-func sg-vertex-step)
  (step-rate :int))

(defcstruct (%sg-vertex-attr-state :class sg-vertex-attr-state-type)
  (buffer-index :int)
  (offset :int)
  (format sg-vertex-format))

(defcstruct (%sg-vertex-layout-state :class sg-vertex-layout-state-type)
  (buffers (:array (:struct %sg-vertex-buffer-layout-state) 8))
  (attrs (:array (:struct %sg-vertex-attr-state) 16)))

(defcstruct (%sg-stencil-face-state :class sg-stencil-face-state-type)
  (compare sg-compare-func)
  (fail-op sg-stencil-op)
  (depth-fail-op sg-stencil-op)
  (pass-op sg-stencil-op))

(defcstruct (%sg-stencil-state :class sg-stencil-state-type)
  (enabled :int)
  (front (:struct %sg-stencil-face-state))
  (back (:struct %sg-stencil-face-state))
  (read-mask :unsigned-char)
  (write-mask :unsigned-char)
  (ref :unsigned-char))

(defcstruct (%sg-depth-state :class sg-depth-state-type)
  (pixel-format sg-pixel-format)
  (compare sg-compare-func)
  (write-enabled :int)
  (bias :float)
  (bias-slope-scale :float)
  (bias-clamp :float))

(defcstruct (%sg-blend-state :class sg-blend-state-type)
  (enabled :int)
  (src-factor-rgb sg-blend-factor)
  (dst-factor-rgb sg-blend-factor)
  (op-rgb sg-blend-op)
  (src-factor-alpha sg-blend-factor)
  (dst-factor-alpha sg-blend-factor)
  (op-alpha sg-blend-op))

(defcstruct (%sg-color-target-state :class sg-color-target-state-type)
  (pixel-format sg-pixel-format)
  (write-mask sg-color-mask)
  (blend (:struct %sg-blend-state)))

(defcstruct (%sg-pipeline-desc :class sg-pipeline-desc-type)
  (-start-canary :unsigned-int)
  (shader (:struct %sg-shader))
  (layout (:struct %sg-vertex-layout-state))
  (depth (:struct %sg-depth-state))
  (stencil (:struct %sg-stencil-state))
  (color-count :int)
  (colors (:array (:struct %sg-color-target-state) 4))
  (primitive-type sg-primitive-type)
  (index-type sg-index-type)
  (cull-mode sg-cull-mode)
  (face-winding sg-face-winding)
  (sample-count :int)
  (blend-color (:struct %sg-color))
  (alpha-to-coverage-enabled :int)
  (label (:pointer :char))
  (-end-canary :unsigned-int))

(defcstruct (%sg-pass-attachment-desc :class sg-pass-attachment-desc-type)
  (image (:struct %sg-image))
  (mip-level :int)
  (slice :int))

(defcstruct (%sg-pass-desc :class sg-pass-desc-type)
  (-start-canary :unsigned-int)
  (color-attachments (:array (:struct %sg-pass-attachment-desc) 4))
  (resolve-attachments (:array (:struct %sg-pass-attachment-desc) 4))
  (depth-stencil-attachment (:struct %sg-pass-attachment-desc))
  (label (:pointer :char))
  (-end-canary :unsigned-int))

(defcstruct (%sg-trace-hooks :class sg-trace-hooks-type)
  (user-data (:pointer :void))
  (reset-state-cache :pointer)
  (make-buffer :pointer)
  (make-image :pointer)
  (make-sampler :pointer)
  (make-shader :pointer)
  (make-pipeline :pointer)
  (make-pass :pointer)
  (destroy-buffer :pointer)
  (destroy-image :pointer)
  (destroy-sampler :pointer)
  (destroy-shader :pointer)
  (destroy-pipeline :pointer)
  (destroy-pass :pointer)
  (update-buffer :pointer)
  (update-image :pointer)
  (append-buffer :pointer)
  (begin-default-pass :pointer)
  (begin-pass :pointer)
  (apply-viewport :pointer)
  (apply-scissor-rect :pointer)
  (apply-pipeline :pointer)
  (apply-bindings :pointer)
  (apply-uniforms :pointer)
  (draw :pointer)
  (end-pass :pointer)
  (commit :pointer)
  (alloc-buffer :pointer)
  (alloc-image :pointer)
  (alloc-sampler :pointer)
  (alloc-shader :pointer)
  (alloc-pipeline :pointer)
  (alloc-pass :pointer)
  (dealloc-buffer :pointer)
  (dealloc-image :pointer)
  (dealloc-sampler :pointer)
  (dealloc-shader :pointer)
  (dealloc-pipeline :pointer)
  (dealloc-pass :pointer)
  (init-buffer :pointer)
  (init-image :pointer)
  (init-sampler :pointer)
  (init-shader :pointer)
  (init-pipeline :pointer)
  (init-pass :pointer)
  (uninit-buffer :pointer)
  (uninit-image :pointer)
  (uninit-sampler :pointer)
  (uninit-shader :pointer)
  (uninit-pipeline :pointer)
  (uninit-pass :pointer)
  (fail-buffer :pointer)
  (fail-image :pointer)
  (fail-sampler :pointer)
  (fail-shader :pointer)
  (fail-pipeline :pointer)
  (fail-pass :pointer)
  (push-debug-group :pointer)
  (pop-debug-group :pointer))

(defcstruct (%sg-slot-info :class sg-slot-info-type)
  (state sg-resource-state)
  (res-id :unsigned-int)
  (ctx-id :unsigned-int))

(defcstruct (%sg-buffer-info :class sg-buffer-info-type)
  (slot (:struct %sg-slot-info))
  (update-frame-index :unsigned-int)
  (append-frame-index :unsigned-int)
  (append-pos :int)
  (append-overflow :int)
  (num-slots :int)
  (active-slot :int))

(defcstruct (%sg-image-info :class sg-image-info-type)
  (slot (:struct %sg-slot-info))
  (upd-frame-index :unsigned-int)
  (num-slots :int)
  (active-slot :int))

(defcstruct (%sg-sampler-info :class sg-sampler-info-type)
  (slot (:struct %sg-slot-info)))

(defcstruct (%sg-shader-info :class sg-shader-info-type)
  (slot (:struct %sg-slot-info)))

(defcstruct (%sg-pipeline-info :class sg-pipeline-info-type)
  (slot (:struct %sg-slot-info)))

(defcstruct (%sg-pass-info :class sg-pass-info-type)
  (slot (:struct %sg-slot-info)))

(defcstruct (%sg-frame-stats-gl :class sg-frame-stats-gl-type)
  (num-bind-buffer :unsigned-int)
  (num-active-texture :unsigned-int)
  (num-bind-texture :unsigned-int)
  (num-bind-sampler :unsigned-int)
  (num-use-program :unsigned-int)
  (num-render-state :unsigned-int)
  (num-vertex-attrib-pointer :unsigned-int)
  (num-vertex-attrib-divisor :unsigned-int)
  (num-enable-vertex-attrib-array :unsigned-int)
  (num-disable-vertex-attrib-array :unsigned-int)
  (num-uniform :unsigned-int))

(defcstruct (%sg-frame-stats-d3d11-pass :class sg-frame-stats-d3d11-pass-type)
  (num-om-set-render-targets :unsigned-int)
  (num-clear-render-target-view :unsigned-int)
  (num-clear-depth-stencil-view :unsigned-int)
  (num-resolve-subresource :unsigned-int))

(defcstruct (%sg-frame-stats-d3d11-pipeline :class sg-frame-stats-d3d11-pipeline-type)
  (num-rs-set-state :unsigned-int)
  (num-om-set-depth-stencil-state :unsigned-int)
  (num-om-set-blend-state :unsigned-int)
  (num-ia-set-primitive-topology :unsigned-int)
  (num-ia-set-input-layout :unsigned-int)
  (num-vs-set-shader :unsigned-int)
  (num-vs-set-constant-buffers :unsigned-int)
  (num-ps-set-shader :unsigned-int)
  (num-ps-set-constant-buffers :unsigned-int))

(defcstruct (%sg-frame-stats-d3d11-bindings :class sg-frame-stats-d3d11-bindings-type)
  (num-ia-set-vertex-buffers :unsigned-int)
  (num-ia-set-index-buffer :unsigned-int)
  (num-vs-set-shader-resources :unsigned-int)
  (num-ps-set-shader-resources :unsigned-int)
  (num-vs-set-samplers :unsigned-int)
  (num-ps-set-samplers :unsigned-int))

(defcstruct (%sg-frame-stats-d3d11-uniforms :class sg-frame-stats-d3d11-uniforms-type)
  (num-update-subresource :unsigned-int))

(defcstruct (%sg-frame-stats-d3d11-draw :class sg-frame-stats-d3d11-draw-type)
  (num-draw-indexed-instanced :unsigned-int)
  (num-draw-indexed :unsigned-int)
  (num-draw-instanced :unsigned-int)
  (num-draw :unsigned-int))

(defcstruct (%sg-frame-stats-d3d11 :class sg-frame-stats-d3d11-type)
  (pass (:struct %sg-frame-stats-d3d11-pass))
  (pipeline (:struct %sg-frame-stats-d3d11-pipeline))
  (bindings (:struct %sg-frame-stats-d3d11-bindings))
  (uniforms (:struct %sg-frame-stats-d3d11-uniforms))
  (draw (:struct %sg-frame-stats-d3d11-draw))
  (num-map :unsigned-int)
  (num-unmap :unsigned-int))

(defcstruct (%sg-frame-stats-metal-idpool :class sg-frame-stats-metal-idpool-type)
  (num-added :unsigned-int)
  (num-released :unsigned-int)
  (num-garbage-collected :unsigned-int))

(defcstruct (%sg-frame-stats-metal-pipeline :class sg-frame-stats-metal-pipeline-type)
  (num-set-blend-color :unsigned-int)
  (num-set-cull-mode :unsigned-int)
  (num-set-front-facing-winding :unsigned-int)
  (num-set-stencil-reference-value :unsigned-int)
  (num-set-depth-bias :unsigned-int)
  (num-set-render-pipeline-state :unsigned-int)
  (num-set-depth-stencil-state :unsigned-int))

(defcstruct (%sg-frame-stats-metal-bindings :class sg-frame-stats-metal-bindings-type)
  (num-set-vertex-buffer :unsigned-int)
  (num-set-vertex-texture :unsigned-int)
  (num-set-vertex-sampler-state :unsigned-int)
  (num-set-fragment-texture :unsigned-int)
  (num-set-fragment-sampler-state :unsigned-int))

(defcstruct (%sg-frame-stats-metal-uniforms :class sg-frame-stats-metal-uniforms-type)
  (num-set-vertex-buffer-offset :unsigned-int)
  (num-set-fragment-buffer-offset :unsigned-int))

(defcstruct (%sg-frame-stats-metal :class sg-frame-stats-metal-type)
  (idpool (:struct %sg-frame-stats-metal-idpool))
  (pipeline (:struct %sg-frame-stats-metal-pipeline))
  (bindings (:struct %sg-frame-stats-metal-bindings))
  (uniforms (:struct %sg-frame-stats-metal-uniforms)))

(defcstruct (%sg-frame-stats-wgpu-uniforms :class sg-frame-stats-wgpu-uniforms-type)
  (num-set-bindgroup :unsigned-int)
  (size-write-buffer :unsigned-int))

(defcstruct (%sg-frame-stats-wgpu-bindings :class sg-frame-stats-wgpu-bindings-type)
  (num-set-vertex-buffer :unsigned-int)
  (num-skip-redundant-vertex-buffer :unsigned-int)
  (num-set-index-buffer :unsigned-int)
  (num-skip-redundant-index-buffer :unsigned-int)
  (num-create-bindgroup :unsigned-int)
  (num-discard-bindgroup :unsigned-int)
  (num-set-bindgroup :unsigned-int)
  (num-skip-redundant-bindgroup :unsigned-int)
  (num-bindgroup-cache-hits :unsigned-int)
  (num-bindgroup-cache-misses :unsigned-int)
  (num-bindgroup-cache-collisions :unsigned-int)
  (num-bindgroup-cache-hash-vs-key-mismatch :unsigned-int))

(defcstruct (%sg-frame-stats-wgpu :class sg-frame-stats-wgpu-type)
  (uniforms (:struct %sg-frame-stats-wgpu-uniforms))
  (bindings (:struct %sg-frame-stats-wgpu-bindings)))

(defcstruct (%sg-frame-stats :class sg-frame-stats-type)
  (frame-index :unsigned-int)
  (num-passes :unsigned-int)
  (num-apply-viewport :unsigned-int)
  (num-apply-scissor-rect :unsigned-int)
  (num-apply-pipeline :unsigned-int)
  (num-apply-bindings :unsigned-int)
  (num-apply-uniforms :unsigned-int)
  (num-draw :unsigned-int)
  (num-update-buffer :unsigned-int)
  (num-append-buffer :unsigned-int)
  (num-update-image :unsigned-int)
  (size-apply-uniforms :unsigned-int)
  (size-update-buffer :unsigned-int)
  (size-append-buffer :unsigned-int)
  (size-update-image :unsigned-int)
  (gl (:struct %sg-frame-stats-gl))
  (d3d11 (:struct %sg-frame-stats-d3d11))
  (metal (:struct %sg-frame-stats-metal))
  (wgpu (:struct %sg-frame-stats-wgpu)))

(defcenum sg-log-item
  (:sg-logitem-ok 0)
  (:sg-logitem-malloc-failed 1)
  (:sg-logitem-gl-texture-format-not-supported 2)
  (:sg-logitem-gl-3d-textures-not-supported 3)
  (:sg-logitem-gl-array-textures-not-supported 4)
  (:sg-logitem-gl-shader-compilation-failed 5)
  (:sg-logitem-gl-shader-linking-failed 6)
  (:sg-logitem-gl-vertex-attribute-not-found-in-shader 7)
  (:sg-logitem-gl-texture-name-not-found-in-shader 8)
  (:sg-logitem-gl-framebuffer-status-undefined 9)
  (:sg-logitem-gl-framebuffer-status-incomplete-attachment 10)
  (:sg-logitem-gl-framebuffer-status-incomplete-missing-attachment 11)
  (:sg-logitem-gl-framebuffer-status-unsupported 12)
  (:sg-logitem-gl-framebuffer-status-incomplete-multisample 13)
  (:sg-logitem-gl-framebuffer-status-unknown 14)
  (:sg-logitem-d3d11-create-buffer-failed 15)
  (:sg-logitem-d3d11-create-depth-texture-unsupported-pixel-format 16)
  (:sg-logitem-d3d11-create-depth-texture-failed 17)
  (:sg-logitem-d3d11-create-2d-texture-unsupported-pixel-format 18)
  (:sg-logitem-d3d11-create-2d-texture-failed 19)
  (:sg-logitem-d3d11-create-2d-srv-failed 20)
  (:sg-logitem-d3d11-create-3d-texture-unsupported-pixel-format 21)
  (:sg-logitem-d3d11-create-3d-texture-failed 22)
  (:sg-logitem-d3d11-create-3d-srv-failed 23)
  (:sg-logitem-d3d11-create-msaa-texture-failed 24)
  (:sg-logitem-d3d11-create-sampler-state-failed 25)
  (:sg-logitem-d3d11-load-d3dcompiler-47-dll-failed 26)
  (:sg-logitem-d3d11-shader-compilation-failed 27)
  (:sg-logitem-d3d11-shader-compilation-output 28)
  (:sg-logitem-d3d11-create-constant-buffer-failed 29)
  (:sg-logitem-d3d11-create-input-layout-failed 30)
  (:sg-logitem-d3d11-create-rasterizer-state-failed 31)
  (:sg-logitem-d3d11-create-depth-stencil-state-failed 32)
  (:sg-logitem-d3d11-create-blend-state-failed 33)
  (:sg-logitem-d3d11-create-rtv-failed 34)
  (:sg-logitem-d3d11-create-dsv-failed 35)
  (:sg-logitem-d3d11-map-for-update-buffer-failed 36)
  (:sg-logitem-d3d11-map-for-append-buffer-failed 37)
  (:sg-logitem-d3d11-map-for-update-image-failed 38)
  (:sg-logitem-metal-create-buffer-failed 39)
  (:sg-logitem-metal-texture-format-not-supported 40)
  (:sg-logitem-metal-create-texture-failed 41)
  (:sg-logitem-metal-create-sampler-failed 42)
  (:sg-logitem-metal-shader-compilation-failed 43)
  (:sg-logitem-metal-shader-creation-failed 44)
  (:sg-logitem-metal-shader-compilation-output 45)
  (:sg-logitem-metal-vertex-shader-entry-not-found 46)
  (:sg-logitem-metal-fragment-shader-entry-not-found 47)
  (:sg-logitem-metal-create-rps-failed 48)
  (:sg-logitem-metal-create-rps-output 49)
  (:sg-logitem-metal-create-dss-failed 50)
  (:sg-logitem-wgpu-bindgroups-pool-exhausted 51)
  (:sg-logitem-wgpu-bindgroupscache-size-greater-one 52)
  (:sg-logitem-wgpu-bindgroupscache-size-pow2 53)
  (:sg-logitem-wgpu-createbindgroup-failed 54)
  (:sg-logitem-wgpu-create-buffer-failed 55)
  (:sg-logitem-wgpu-create-texture-failed 56)
  (:sg-logitem-wgpu-create-texture-view-failed 57)
  (:sg-logitem-wgpu-create-sampler-failed 58)
  (:sg-logitem-wgpu-create-shader-module-failed 59)
  (:sg-logitem-wgpu-shader-too-many-images 60)
  (:sg-logitem-wgpu-shader-too-many-samplers 61)
  (:sg-logitem-wgpu-shader-create-bindgroup-layout-failed 62)
  (:sg-logitem-wgpu-create-pipeline-layout-failed 63)
  (:sg-logitem-wgpu-create-render-pipeline-failed 64)
  (:sg-logitem-wgpu-pass-create-texture-view-failed 65)
  (:sg-logitem-uninit-buffer-active-context-mismatch 66)
  (:sg-logitem-uninit-image-active-context-mismatch 67)
  (:sg-logitem-uninit-sampler-active-context-mismatch 68)
  (:sg-logitem-uninit-shader-active-context-mismatch 69)
  (:sg-logitem-uninit-pipeline-active-context-mismatch 70)
  (:sg-logitem-uninit-pass-active-context-mismatch 71)
  (:sg-logitem-identical-commit-listener 72)
  (:sg-logitem-commit-listener-array-full 73)
  (:sg-logitem-trace-hooks-not-enabled 74)
  (:sg-logitem-dealloc-buffer-invalid-state 75)
  (:sg-logitem-dealloc-image-invalid-state 76)
  (:sg-logitem-dealloc-sampler-invalid-state 77)
  (:sg-logitem-dealloc-shader-invalid-state 78)
  (:sg-logitem-dealloc-pipeline-invalid-state 79)
  (:sg-logitem-dealloc-pass-invalid-state 80)
  (:sg-logitem-init-buffer-invalid-state 81)
  (:sg-logitem-init-image-invalid-state 82)
  (:sg-logitem-init-sampler-invalid-state 83)
  (:sg-logitem-init-shader-invalid-state 84)
  (:sg-logitem-init-pipeline-invalid-state 85)
  (:sg-logitem-init-pass-invalid-state 86)
  (:sg-logitem-uninit-buffer-invalid-state 87)
  (:sg-logitem-uninit-image-invalid-state 88)
  (:sg-logitem-uninit-sampler-invalid-state 89)
  (:sg-logitem-uninit-shader-invalid-state 90)
  (:sg-logitem-uninit-pipeline-invalid-state 91)
  (:sg-logitem-uninit-pass-invalid-state 92)
  (:sg-logitem-fail-buffer-invalid-state 93)
  (:sg-logitem-fail-image-invalid-state 94)
  (:sg-logitem-fail-sampler-invalid-state 95)
  (:sg-logitem-fail-shader-invalid-state 96)
  (:sg-logitem-fail-pipeline-invalid-state 97)
  (:sg-logitem-fail-pass-invalid-state 98)
  (:sg-logitem-buffer-pool-exhausted 99)
  (:sg-logitem-image-pool-exhausted 100)
  (:sg-logitem-sampler-pool-exhausted 101)
  (:sg-logitem-shader-pool-exhausted 102)
  (:sg-logitem-pipeline-pool-exhausted 103)
  (:sg-logitem-pass-pool-exhausted 104)
  (:sg-logitem-draw-without-bindings 105)
  (:sg-logitem-validate-bufferdesc-canary 106)
  (:sg-logitem-validate-bufferdesc-size 107)
  (:sg-logitem-validate-bufferdesc-data 108)
  (:sg-logitem-validate-bufferdesc-data-size 109)
  (:sg-logitem-validate-bufferdesc-no-data 110)
  (:sg-logitem-validate-imagedata-nodata 111)
  (:sg-logitem-validate-imagedata-data-size 112)
  (:sg-logitem-validate-imagedesc-canary 113)
  (:sg-logitem-validate-imagedesc-width 114)
  (:sg-logitem-validate-imagedesc-height 115)
  (:sg-logitem-validate-imagedesc-rt-pixelformat 116)
  (:sg-logitem-validate-imagedesc-nonrt-pixelformat 117)
  (:sg-logitem-validate-imagedesc-msaa-but-no-rt 118)
  (:sg-logitem-validate-imagedesc-no-msaa-rt-support 119)
  (:sg-logitem-validate-imagedesc-msaa-num-mipmaps 120)
  (:sg-logitem-validate-imagedesc-msaa-3d-image 121)
  (:sg-logitem-validate-imagedesc-depth-3d-image 122)
  (:sg-logitem-validate-imagedesc-rt-immutable 123)
  (:sg-logitem-validate-imagedesc-rt-no-data 124)
  (:sg-logitem-validate-imagedesc-injected-no-data 125)
  (:sg-logitem-validate-imagedesc-dynamic-no-data 126)
  (:sg-logitem-validate-imagedesc-compressed-immutable 127)
  (:sg-logitem-validate-samplerdesc-canary 128)
  (:sg-logitem-validate-samplerdesc-minfilter-none 129)
  (:sg-logitem-validate-samplerdesc-magfilter-none 130)
  (:sg-logitem-validate-samplerdesc-anistropic-requires-linear-filtering 131)
  (:sg-logitem-validate-shaderdesc-canary 132)
  (:sg-logitem-validate-shaderdesc-source 133)
  (:sg-logitem-validate-shaderdesc-bytecode 134)
  (:sg-logitem-validate-shaderdesc-source-or-bytecode 135)
  (:sg-logitem-validate-shaderdesc-no-bytecode-size 136)
  (:sg-logitem-validate-shaderdesc-no-cont-ubs 137)
  (:sg-logitem-validate-shaderdesc-no-cont-ub-members 138)
  (:sg-logitem-validate-shaderdesc-no-ub-members 139)
  (:sg-logitem-validate-shaderdesc-ub-member-name 140)
  (:sg-logitem-validate-shaderdesc-ub-size-mismatch 141)
  (:sg-logitem-validate-shaderdesc-ub-array-count 142)
  (:sg-logitem-validate-shaderdesc-ub-std140-array-type 143)
  (:sg-logitem-validate-shaderdesc-no-cont-images 144)
  (:sg-logitem-validate-shaderdesc-no-cont-samplers 145)
  (:sg-logitem-validate-shaderdesc-image-sampler-pair-image-slot-out-of-range 146)
  (:sg-logitem-validate-shaderdesc-image-sampler-pair-sampler-slot-out-of-range 147)
  (:sg-logitem-validate-shaderdesc-image-sampler-pair-name-required-for-gl 148)
  (:sg-logitem-validate-shaderdesc-image-sampler-pair-has-name-but-not-used 149)
  (:sg-logitem-validate-shaderdesc-image-sampler-pair-has-image-but-not-used 150)
  (:sg-logitem-validate-shaderdesc-image-sampler-pair-has-sampler-but-not-used 151)
  (:sg-logitem-validate-shaderdesc-nonfiltering-sampler-required 152)
  (:sg-logitem-validate-shaderdesc-comparison-sampler-required 153)
  (:sg-logitem-validate-shaderdesc-image-not-referenced-by-image-sampler-pairs 154)
  (:sg-logitem-validate-shaderdesc-sampler-not-referenced-by-image-sampler-pairs 155)
  (:sg-logitem-validate-shaderdesc-no-cont-image-sampler-pairs 156)
  (:sg-logitem-validate-shaderdesc-attr-semantics 157)
  (:sg-logitem-validate-shaderdesc-attr-string-too-long 158)
  (:sg-logitem-validate-pipelinedesc-canary 159)
  (:sg-logitem-validate-pipelinedesc-shader 160)
  (:sg-logitem-validate-pipelinedesc-no-attrs 161)
  (:sg-logitem-validate-pipelinedesc-layout-stride4 162)
  (:sg-logitem-validate-pipelinedesc-attr-semantics 163)
  (:sg-logitem-validate-passdesc-canary 164)
  (:sg-logitem-validate-passdesc-no-attachments 165)
  (:sg-logitem-validate-passdesc-no-cont-color-atts 166)
  (:sg-logitem-validate-passdesc-image 167)
  (:sg-logitem-validate-passdesc-miplevel 168)
  (:sg-logitem-validate-passdesc-face 169)
  (:sg-logitem-validate-passdesc-layer 170)
  (:sg-logitem-validate-passdesc-slice 171)
  (:sg-logitem-validate-passdesc-image-no-rt 172)
  (:sg-logitem-validate-passdesc-color-inv-pixelformat 173)
  (:sg-logitem-validate-passdesc-depth-inv-pixelformat 174)
  (:sg-logitem-validate-passdesc-image-sizes 175)
  (:sg-logitem-validate-passdesc-image-sample-counts 176)
  (:sg-logitem-validate-passdesc-resolve-color-image-msaa 177)
  (:sg-logitem-validate-passdesc-resolve-image 178)
  (:sg-logitem-validate-passdesc-resolve-sample-count 179)
  (:sg-logitem-validate-passdesc-resolve-miplevel 180)
  (:sg-logitem-validate-passdesc-resolve-face 181)
  (:sg-logitem-validate-passdesc-resolve-layer 182)
  (:sg-logitem-validate-passdesc-resolve-slice 183)
  (:sg-logitem-validate-passdesc-resolve-image-no-rt 184)
  (:sg-logitem-validate-passdesc-resolve-image-sizes 185)
  (:sg-logitem-validate-passdesc-resolve-image-format 186)
  (:sg-logitem-validate-passdesc-depth-image 187)
  (:sg-logitem-validate-passdesc-depth-miplevel 188)
  (:sg-logitem-validate-passdesc-depth-face 189)
  (:sg-logitem-validate-passdesc-depth-layer 190)
  (:sg-logitem-validate-passdesc-depth-slice 191)
  (:sg-logitem-validate-passdesc-depth-image-no-rt 192)
  (:sg-logitem-validate-passdesc-depth-image-sizes 193)
  (:sg-logitem-validate-passdesc-depth-image-sample-count 194)
  (:sg-logitem-validate-beginpass-pass 195)
  (:sg-logitem-validate-beginpass-color-attachment-image 196)
  (:sg-logitem-validate-beginpass-resolve-attachment-image 197)
  (:sg-logitem-validate-beginpass-depthstencil-attachment-image 198)
  (:sg-logitem-validate-apip-pipeline-valid-id 199)
  (:sg-logitem-validate-apip-pipeline-exists 200)
  (:sg-logitem-validate-apip-pipeline-valid 201)
  (:sg-logitem-validate-apip-shader-exists 202)
  (:sg-logitem-validate-apip-shader-valid 203)
  (:sg-logitem-validate-apip-att-count 204)
  (:sg-logitem-validate-apip-color-format 205)
  (:sg-logitem-validate-apip-depth-format 206)
  (:sg-logitem-validate-apip-sample-count 207)
  (:sg-logitem-validate-abnd-pipeline 208)
  (:sg-logitem-validate-abnd-pipeline-exists 209)
  (:sg-logitem-validate-abnd-pipeline-valid 210)
  (:sg-logitem-validate-abnd-vbs 211)
  (:sg-logitem-validate-abnd-vb-exists 212)
  (:sg-logitem-validate-abnd-vb-type 213)
  (:sg-logitem-validate-abnd-vb-overflow 214)
  (:sg-logitem-validate-abnd-no-ib 215)
  (:sg-logitem-validate-abnd-ib 216)
  (:sg-logitem-validate-abnd-ib-exists 217)
  (:sg-logitem-validate-abnd-ib-type 218)
  (:sg-logitem-validate-abnd-ib-overflow 219)
  (:sg-logitem-validate-abnd-vs-expected-image-binding 220)
  (:sg-logitem-validate-abnd-vs-img-exists 221)
  (:sg-logitem-validate-abnd-vs-image-type-mismatch 222)
  (:sg-logitem-validate-abnd-vs-image-msaa 223)
  (:sg-logitem-validate-abnd-vs-expected-filterable-image 224)
  (:sg-logitem-validate-abnd-vs-expected-depth-image 225)
  (:sg-logitem-validate-abnd-vs-unexpected-image-binding 226)
  (:sg-logitem-validate-abnd-vs-expected-sampler-binding 227)
  (:sg-logitem-validate-abnd-vs-unexpected-sampler-compare-never 228)
  (:sg-logitem-validate-abnd-vs-expected-sampler-compare-never 229)
  (:sg-logitem-validate-abnd-vs-expected-nonfiltering-sampler 230)
  (:sg-logitem-validate-abnd-vs-unexpected-sampler-binding 231)
  (:sg-logitem-validate-abnd-vs-smp-exists 232)
  (:sg-logitem-validate-abnd-fs-expected-image-binding 233)
  (:sg-logitem-validate-abnd-fs-img-exists 234)
  (:sg-logitem-validate-abnd-fs-image-type-mismatch 235)
  (:sg-logitem-validate-abnd-fs-image-msaa 236)
  (:sg-logitem-validate-abnd-fs-expected-filterable-image 237)
  (:sg-logitem-validate-abnd-fs-expected-depth-image 238)
  (:sg-logitem-validate-abnd-fs-unexpected-image-binding 239)
  (:sg-logitem-validate-abnd-fs-expected-sampler-binding 240)
  (:sg-logitem-validate-abnd-fs-unexpected-sampler-compare-never 241)
  (:sg-logitem-validate-abnd-fs-expected-sampler-compare-never 242)
  (:sg-logitem-validate-abnd-fs-expected-nonfiltering-sampler 243)
  (:sg-logitem-validate-abnd-fs-unexpected-sampler-binding 244)
  (:sg-logitem-validate-abnd-fs-smp-exists 245)
  (:sg-logitem-validate-aub-no-pipeline 246)
  (:sg-logitem-validate-aub-no-ub-at-slot 247)
  (:sg-logitem-validate-aub-size 248)
  (:sg-logitem-validate-updatebuf-usage 249)
  (:sg-logitem-validate-updatebuf-size 250)
  (:sg-logitem-validate-updatebuf-once 251)
  (:sg-logitem-validate-updatebuf-append 252)
  (:sg-logitem-validate-appendbuf-usage 253)
  (:sg-logitem-validate-appendbuf-size 254)
  (:sg-logitem-validate-appendbuf-update 255)
  (:sg-logitem-validate-updimg-usage 256)
  (:sg-logitem-validate-updimg-once 257)
  (:sg-logitem-validation-failed 258))

(defcstruct (%sg-metal-context-desc :class sg-metal-context-desc-type)
  (device (:pointer :void))
  (renderpass-descriptor-cb :pointer)
  (renderpass-descriptor-userdata-cb :pointer)
  (drawable-cb :pointer)
  (drawable-userdata-cb :pointer)
  (user-data (:pointer :void)))

(defcstruct (%sg-d3d11-context-desc :class sg-d3d11-context-desc-type)
  (device (:pointer :void))
  (device-context (:pointer :void))
  (render-target-view-cb :pointer)
  (render-target-view-userdata-cb :pointer)
  (depth-stencil-view-cb :pointer)
  (depth-stencil-view-userdata-cb :pointer)
  (user-data (:pointer :void)))

(defcstruct (%sg-wgpu-context-desc :class sg-wgpu-context-desc-type)
  (device (:pointer :void))
  (render-view-cb :pointer)
  (render-view-userdata-cb :pointer)
  (resolve-view-cb :pointer)
  (resolve-view-userdata-cb :pointer)
  (depth-stencil-view-cb :pointer)
  (depth-stencil-view-userdata-cb :pointer)
  (user-data (:pointer :void)))

(defcstruct (%sg-gl-context-desc :class sg-gl-context-desc-type)
  (default-framebuffer-cb :pointer)
  (default-framebuffer-userdata-cb :pointer)
  (user-data (:pointer :void)))

(defcstruct (%sg-context-desc :class sg-context-desc-type)
  (color-format sg-pixel-format)
  (depth-format sg-pixel-format)
  (sample-count :int)
  (metal (:struct %sg-metal-context-desc))
  (d3d11 (:struct %sg-d3d11-context-desc))
  (wgpu (:struct %sg-wgpu-context-desc))
  (gl (:struct %sg-gl-context-desc)))

(defcstruct (%sg-commit-listener :class sg-commit-listener-type)
  (func :pointer)
  (user-data (:pointer :void)))

(defcstruct (%sg-allocator :class sg-allocator-type)
  (alloc-fn :pointer)
  (free-fn :pointer)
  (user-data (:pointer :void)))

(defcstruct (%sg-logger :class sg-logger-type)
  (func :pointer)
  (user-data (:pointer :void)))

(defcstruct (%sg-desc :class sg-desc-type)
  (-start-canary :unsigned-int)
  (buffer-pool-size :int)
  (image-pool-size :int)
  (sampler-pool-size :int)
  (shader-pool-size :int)
  (pipeline-pool-size :int)
  (pass-pool-size :int)
  (context-pool-size :int)
  (uniform-buffer-size :int)
  (max-commit-listeners :int)
  (disable-validation :int)
  (mtl-force-managed-storage-mode :int)
  (wgpu-disable-bindgroups-cache :int)
  (wgpu-bindgroups-cache-size :int)
  (allocator (:struct %sg-allocator))
  (logger (:struct %sg-logger))
  (context (:struct %sg-context-desc))
  (-end-canary :unsigned-int))

(defcfun (sg-setup "sg_setup") :void
  (desc (:pointer (:struct %sg-desc))))

(defcfun (sg-shutdown "sg_shutdown") :void)

(defcfun (sg-isvalid "sg_isvalid") :int)

(defcfun (sg-reset-state-cache "sg_reset_state_cache") :void)

(defcfun (sg-install-trace-hooks "sg_install_trace_hooks") (:struct %sg-trace-hooks)
  (trace_hooks (:pointer (:struct %sg-trace-hooks))))

(defcfun (sg-push-debug-group "sg_push_debug_group") :void
  (name (:pointer :char)))

(defcfun (sg-pop-debug-group "sg_pop_debug_group") :void)

(defcfun (sg-add-commit-listener "sg_add_commit_listener") :int
  (listener (:struct %sg-commit-listener)))

(defcfun (sg-remove-commit-listener "sg_remove_commit_listener") :int
  (listener (:struct %sg-commit-listener)))

(defcfun (sg-make-buffer "sg_make_buffer") (:struct %sg-buffer)
  (desc (:pointer (:struct %sg-buffer-desc))))

(defcfun (sg-make-image "sg_make_image") (:struct %sg-image)
  (desc (:pointer (:struct %sg-image-desc))))

(defcfun (sg-make-sampler "sg_make_sampler") (:struct %sg-sampler)
  (desc (:pointer (:struct %sg-sampler-desc))))

(defcfun (sg-make-shader "sg_make_shader") (:struct %sg-shader)
  (desc (:pointer (:struct %sg-shader-desc))))

(defcfun (sg-make-pipeline "sg_make_pipeline") (:struct %sg-pipeline)
  (desc (:pointer (:struct %sg-pipeline-desc))))

(defcfun (sg-make-pass "sg_make_pass") (:struct %sg-pass)
  (desc (:pointer (:struct %sg-pass-desc))))

(defcfun (sg-destroy-buffer "sg_destroy_buffer") :void
  (buf (:struct %sg-buffer)))

(defcfun (sg-destroy-image "sg_destroy_image") :void
  (img (:struct %sg-image)))

(defcfun (sg-destroy-sampler "sg_destroy_sampler") :void
  (smp (:struct %sg-sampler)))

(defcfun (sg-destroy-shader "sg_destroy_shader") :void
  (shd (:struct %sg-shader)))

(defcfun (sg-destroy-pipeline "sg_destroy_pipeline") :void
  (pip (:struct %sg-pipeline)))

(defcfun (sg-destroy-pass "sg_destroy_pass") :void
  (pass (:struct %sg-pass)))

(defcfun (sg-update-buffer "sg_update_buffer") :void
  (buf (:struct %sg-buffer))
  (data (:pointer (:struct %sg-range))))

(defcfun (sg-update-image "sg_update_image") :void
  (img (:struct %sg-image))
  (data (:pointer (:struct %sg-image-data))))

(defcfun (sg-append-buffer "sg_append_buffer") :int
  (buf (:struct %sg-buffer))
  (data (:pointer (:struct %sg-range))))

(defcfun (sg-query-buffer-overflow "sg_query_buffer_overflow") :int
  (buf (:struct %sg-buffer)))

(defcfun (sg-query-buffer-will-overflow "sg_query_buffer_will_overflow") :int
  (buf (:struct %sg-buffer))
  (size :unsigned-long))

(defcfun (sg-begin-default-pass "sg_begin_default_pass") :void
  (pass_action (:pointer (:struct %sg-pass-action)))
  (width :int)
  (height :int))

(defcfun (sg-begin-default-passf "sg_begin_default_passf") :void
  (pass_action (:pointer (:struct %sg-pass-action)))
  (width :float)
  (height :float))

(defcfun (sg-begin-pass "sg_begin_pass") :void
  (pass (:struct %sg-pass))
  (pass_action (:pointer (:struct %sg-pass-action))))

(defcfun (sg-apply-viewport "sg_apply_viewport") :void
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (origin_top_left :int))

(defcfun (sg-apply-viewportf "sg_apply_viewportf") :void
  (x :float)
  (y :float)
  (width :float)
  (height :float)
  (origin_top_left :int))

(defcfun (sg-apply-scissor-rect "sg_apply_scissor_rect") :void
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (origin_top_left :int))

(defcfun (sg-apply-scissor-rectf "sg_apply_scissor_rectf") :void
  (x :float)
  (y :float)
  (width :float)
  (height :float)
  (origin_top_left :int))

(defcfun (sg-apply-pipeline "sg_apply_pipeline") :void
  (pip (:struct %sg-pipeline)))

(defcfun (sg-apply-bindings "sg_apply_bindings") :void
  (bindings (:pointer (:struct %sg-bindings))))

(defcfun (sg-apply-uniforms "sg_apply_uniforms") :void
  (stage sg-shader-stage)
  (ub_index :int)
  (data (:pointer (:struct %sg-range))))

(defcfun (sg-draw "sg_draw") :void
  (base_element :int)
  (num_elements :int)
  (num_instances :int))

(defcfun (sg-end-pass "sg_end_pass") :void)

(defcfun (sg-commit "sg_commit") :void)

(defcfun (sg-query-desc "sg_query_desc") (:struct %sg-desc))

(defcfun (sg-query-backend "sg_query_backend") sg-backend)

(defcfun (sg-query-features "sg_query_features") (:struct %sg-features))

(defcfun (sg-query-limits "sg_query_limits") (:struct %sg-limits))

(defcfun (sg-query-pixelformat "sg_query_pixelformat") (:struct %sg-pixelformat-info)
  (fmt sg-pixel-format))

(defcfun (sg-query-buffer-state "sg_query_buffer_state") sg-resource-state
  (buf (:struct %sg-buffer)))

(defcfun (sg-query-image-state "sg_query_image_state") sg-resource-state
  (img (:struct %sg-image)))

(defcfun (sg-query-sampler-state "sg_query_sampler_state") sg-resource-state
  (smp (:struct %sg-sampler)))

(defcfun (sg-query-shader-state "sg_query_shader_state") sg-resource-state
  (shd (:struct %sg-shader)))

(defcfun (sg-query-pipeline-state "sg_query_pipeline_state") sg-resource-state
  (pip (:struct %sg-pipeline)))

(defcfun (sg-query-pass-state "sg_query_pass_state") sg-resource-state
  (pass (:struct %sg-pass)))

(defcfun (sg-query-buffer-info "sg_query_buffer_info") (:struct %sg-buffer-info)
  (buf (:struct %sg-buffer)))

(defcfun (sg-query-image-info "sg_query_image_info") (:struct %sg-image-info)
  (img (:struct %sg-image)))

(defcfun (sg-query-sampler-info "sg_query_sampler_info") (:struct %sg-sampler-info)
  (smp (:struct %sg-sampler)))

(defcfun (sg-query-shader-info "sg_query_shader_info") (:struct %sg-shader-info)
  (shd (:struct %sg-shader)))

(defcfun (sg-query-pipeline-info "sg_query_pipeline_info") (:struct %sg-pipeline-info)
  (pip (:struct %sg-pipeline)))

(defcfun (sg-query-pass-info "sg_query_pass_info") (:struct %sg-pass-info)
  (pass (:struct %sg-pass)))

(defcfun (sg-query-buffer-desc "sg_query_buffer_desc") (:struct %sg-buffer-desc)
  (buf (:struct %sg-buffer)))

(defcfun (sg-query-image-desc "sg_query_image_desc") (:struct %sg-image-desc)
  (img (:struct %sg-image)))

(defcfun (sg-query-sampler-desc "sg_query_sampler_desc") (:struct %sg-sampler-desc)
  (smp (:struct %sg-sampler)))

(defcfun (sg-query-shader-desc "sg_query_shader_desc") (:struct %sg-shader-desc)
  (shd (:struct %sg-shader)))

(defcfun (sg-query-pipeline-desc "sg_query_pipeline_desc") (:struct %sg-pipeline-desc)
  (pip (:struct %sg-pipeline)))

(defcfun (sg-query-pass-desc "sg_query_pass_desc") (:struct %sg-pass-desc)
  (pass (:struct %sg-pass)))

(defcfun (sg-query-buffer-defaults "sg_query_buffer_defaults") (:struct %sg-buffer-desc)
  (desc (:pointer (:struct %sg-buffer-desc))))

(defcfun (sg-query-image-defaults "sg_query_image_defaults") (:struct %sg-image-desc)
  (desc (:pointer (:struct %sg-image-desc))))

(defcfun (sg-query-sampler-defaults "sg_query_sampler_defaults") (:struct %sg-sampler-desc)
  (desc (:pointer (:struct %sg-sampler-desc))))

(defcfun (sg-query-shader-defaults "sg_query_shader_defaults") (:struct %sg-shader-desc)
  (desc (:pointer (:struct %sg-shader-desc))))

(defcfun (sg-query-pipeline-defaults "sg_query_pipeline_defaults") (:struct %sg-pipeline-desc)
  (desc (:pointer (:struct %sg-pipeline-desc))))

(defcfun (sg-query-pass-defaults "sg_query_pass_defaults") (:struct %sg-pass-desc)
  (desc (:pointer (:struct %sg-pass-desc))))

(defcfun (sg-alloc-buffer "sg_alloc_buffer") (:struct %sg-buffer))

(defcfun (sg-alloc-image "sg_alloc_image") (:struct %sg-image))

(defcfun (sg-alloc-sampler "sg_alloc_sampler") (:struct %sg-sampler))

(defcfun (sg-alloc-shader "sg_alloc_shader") (:struct %sg-shader))

(defcfun (sg-alloc-pipeline "sg_alloc_pipeline") (:struct %sg-pipeline))

(defcfun (sg-alloc-pass "sg_alloc_pass") (:struct %sg-pass))

(defcfun (sg-dealloc-buffer "sg_dealloc_buffer") :void
  (buf (:struct %sg-buffer)))

(defcfun (sg-dealloc-image "sg_dealloc_image") :void
  (img (:struct %sg-image)))

(defcfun (sg-dealloc-sampler "sg_dealloc_sampler") :void
  (smp (:struct %sg-sampler)))

(defcfun (sg-dealloc-shader "sg_dealloc_shader") :void
  (shd (:struct %sg-shader)))

(defcfun (sg-dealloc-pipeline "sg_dealloc_pipeline") :void
  (pip (:struct %sg-pipeline)))

(defcfun (sg-dealloc-pass "sg_dealloc_pass") :void
  (pass (:struct %sg-pass)))

(defcfun (sg-init-buffer "sg_init_buffer") :void
  (buf (:struct %sg-buffer))
  (desc (:pointer (:struct %sg-buffer-desc))))

(defcfun (sg-init-image "sg_init_image") :void
  (img (:struct %sg-image))
  (desc (:pointer (:struct %sg-image-desc))))

(defcfun (sg-init-sampler "sg_init_sampler") :void
  (smg (:struct %sg-sampler))
  (desc (:pointer (:struct %sg-sampler-desc))))

(defcfun (sg-init-shader "sg_init_shader") :void
  (shd (:struct %sg-shader))
  (desc (:pointer (:struct %sg-shader-desc))))

(defcfun (sg-init-pipeline "sg_init_pipeline") :void
  (pip (:struct %sg-pipeline))
  (desc (:pointer (:struct %sg-pipeline-desc))))

(defcfun (sg-init-pass "sg_init_pass") :void
  (pass (:struct %sg-pass))
  (desc (:pointer (:struct %sg-pass-desc))))

(defcfun (sg-uninit-buffer "sg_uninit_buffer") :void
  (buf (:struct %sg-buffer)))

(defcfun (sg-uninit-image "sg_uninit_image") :void
  (img (:struct %sg-image)))

(defcfun (sg-uninit-sampler "sg_uninit_sampler") :void
  (smp (:struct %sg-sampler)))

(defcfun (sg-uninit-shader "sg_uninit_shader") :void
  (shd (:struct %sg-shader)))

(defcfun (sg-uninit-pipeline "sg_uninit_pipeline") :void
  (pip (:struct %sg-pipeline)))

(defcfun (sg-uninit-pass "sg_uninit_pass") :void
  (pass (:struct %sg-pass)))

(defcfun (sg-fail-buffer "sg_fail_buffer") :void
  (buf (:struct %sg-buffer)))

(defcfun (sg-fail-image "sg_fail_image") :void
  (img (:struct %sg-image)))

(defcfun (sg-fail-sampler "sg_fail_sampler") :void
  (smp (:struct %sg-sampler)))

(defcfun (sg-fail-shader "sg_fail_shader") :void
  (shd (:struct %sg-shader)))

(defcfun (sg-fail-pipeline "sg_fail_pipeline") :void
  (pip (:struct %sg-pipeline)))

(defcfun (sg-fail-pass "sg_fail_pass") :void
  (pass (:struct %sg-pass)))

(defcfun (sg-enable-frame-stats "sg_enable_frame_stats") :void)

(defcfun (sg-disable-frame-stats "sg_disable_frame_stats") :void)

(defcfun (sg-frame-stats-enabled "sg_frame_stats_enabled") :int)

(defcfun (sg-query-frame-stats "sg_query_frame_stats") (:struct %sg-frame-stats))

(defcfun (sg-setup-context "sg_setup_context") (:struct %sg-context))

(defcfun (sg-activate-context "sg_activate_context") :void
  (ctx_id (:struct %sg-context)))

(defcfun (sg-discard-context "sg_discard_context") :void
  (ctx_id (:struct %sg-context)))

(defcstruct (%sg-d3d11-buffer-info :class sg-d3d11-buffer-info-type)
  (buf (:pointer :void)))

(defcstruct (%sg-d3d11-image-info :class sg-d3d11-image-info-type)
  (tex2d (:pointer :void))
  (tex3d (:pointer :void))
  (res (:pointer :void))
  (srv (:pointer :void)))

(defcstruct (%sg-d3d11-sampler-info :class sg-d3d11-sampler-info-type)
  (smp (:pointer :void)))

(defcstruct (%sg-d3d11-shader-info :class sg-d3d11-shader-info-type)
  (vs-cbufs (:array (:pointer :void) 4))
  (fs-cbufs (:array (:pointer :void) 4))
  (vs (:pointer :void))
  (fs (:pointer :void)))

(defcstruct (%sg-d3d11-pipeline-info :class sg-d3d11-pipeline-info-type)
  (il (:pointer :void))
  (rs (:pointer :void))
  (dss (:pointer :void))
  (bs (:pointer :void)))

(defcstruct (%sg-d3d11-pass-info :class sg-d3d11-pass-info-type)
  (color-rtv (:array (:pointer :void) 4))
  (resolve-rtv (:array (:pointer :void) 4))
  (dsv (:pointer :void)))

(defcstruct (%sg-mtl-buffer-info :class sg-mtl-buffer-info-type)
  (buf (:array (:pointer :void) 2))
  (active-slot :int))

(defcstruct (%sg-mtl-image-info :class sg-mtl-image-info-type)
  (tex (:array (:pointer :void) 2))
  (active-slot :int))

(defcstruct (%sg-mtl-sampler-info :class sg-mtl-sampler-info-type)
  (smp (:pointer :void)))

(defcstruct (%sg-mtl-shader-info :class sg-mtl-shader-info-type)
  (vs-lib (:pointer :void))
  (fs-lib (:pointer :void))
  (vs-func (:pointer :void))
  (fs-func (:pointer :void)))

(defcstruct (%sg-mtl-pipeline-info :class sg-mtl-pipeline-info-type)
  (rps (:pointer :void))
  (dss (:pointer :void)))

(defcstruct (%sg-wgpu-buffer-info :class sg-wgpu-buffer-info-type)
  (buf (:pointer :void)))

(defcstruct (%sg-wgpu-image-info :class sg-wgpu-image-info-type)
  (tex (:pointer :void))
  (view (:pointer :void)))

(defcstruct (%sg-wgpu-sampler-info :class sg-wgpu-sampler-info-type)
  (smp (:pointer :void)))

(defcstruct (%sg-wgpu-shader-info :class sg-wgpu-shader-info-type)
  (vs-mod (:pointer :void))
  (fs-mod (:pointer :void))
  (bgl (:pointer :void)))

(defcstruct (%sg-wgpu-pipeline-info :class sg-wgpu-pipeline-info-type)
  (pip (:pointer :void)))

(defcstruct (%sg-wgpu-pass-info :class sg-wgpu-pass-info-type)
  (color-view (:array (:pointer :void) 4))
  (resolve-view (:array (:pointer :void) 4))
  (ds-view (:pointer :void)))

(defcstruct (%sg-gl-buffer-info :class sg-gl-buffer-info-type)
  (buf (:array :unsigned-int 2))
  (active-slot :int))

(defcstruct (%sg-gl-image-info :class sg-gl-image-info-type)
  (tex (:array :unsigned-int 2))
  (tex-target :unsigned-int)
  (msaa-render-buffer :unsigned-int)
  (active-slot :int))

(defcstruct (%sg-gl-sampler-info :class sg-gl-sampler-info-type)
  (smp :unsigned-int))

(defcstruct (%sg-gl-shader-info :class sg-gl-shader-info-type)
  (prog :unsigned-int))

(defcstruct (%sg-gl-pass-info :class sg-gl-pass-info-type)
  (frame-buffer :unsigned-int)
  (msaa-resolve-framebuffer (:array :unsigned-int 4)))

(defcfun (sg-d3d11-device "sg_d3d11_device") (:pointer :void))

(defcfun (sg-d3d11-device-context "sg_d3d11_device_context") (:pointer :void))

(defcfun (sg-d3d11-query-buffer-info "sg_d3d11_query_buffer_info") (:struct %sg-d3d11-buffer-info)
  (buf (:struct %sg-buffer)))

(defcfun (sg-d3d11-query-image-info "sg_d3d11_query_image_info") (:struct %sg-d3d11-image-info)
  (img (:struct %sg-image)))

(defcfun (sg-d3d11-query-sampler-info "sg_d3d11_query_sampler_info") (:struct %sg-d3d11-sampler-info)
  (smp (:struct %sg-sampler)))

(defcfun (sg-d3d11-query-shader-info "sg_d3d11_query_shader_info") (:struct %sg-d3d11-shader-info)
  (shd (:struct %sg-shader)))

(defcfun (sg-d3d11-query-pipeline-info "sg_d3d11_query_pipeline_info") (:struct %sg-d3d11-pipeline-info)
  (pip (:struct %sg-pipeline)))

(defcfun (sg-d3d11-query-pass-info "sg_d3d11_query_pass_info") (:struct %sg-d3d11-pass-info)
  (pass (:struct %sg-pass)))

(defcfun (sg-mtl-device "sg_mtl_device") (:pointer :void))

(defcfun (sg-mtl-render-command-encoder "sg_mtl_render_command_encoder") (:pointer :void))

(defcfun (sg-mtl-query-buffer-info "sg_mtl_query_buffer_info") (:struct %sg-mtl-buffer-info)
  (buf (:struct %sg-buffer)))

(defcfun (sg-mtl-query-image-info "sg_mtl_query_image_info") (:struct %sg-mtl-image-info)
  (img (:struct %sg-image)))

(defcfun (sg-mtl-query-sampler-info "sg_mtl_query_sampler_info") (:struct %sg-mtl-sampler-info)
  (smp (:struct %sg-sampler)))

(defcfun (sg-mtl-query-shader-info "sg_mtl_query_shader_info") (:struct %sg-mtl-shader-info)
  (shd (:struct %sg-shader)))

(defcfun (sg-mtl-query-pipeline-info "sg_mtl_query_pipeline_info") (:struct %sg-mtl-pipeline-info)
  (pip (:struct %sg-pipeline)))

(defcfun (sg-wgpu-device "sg_wgpu_device") (:pointer :void))

(defcfun (sg-wgpu-queue "sg_wgpu_queue") (:pointer :void))

(defcfun (sg-wgpu-command-encoder "sg_wgpu_command_encoder") (:pointer :void))

(defcfun (sg-wgpu-render-pass-encoder "sg_wgpu_render_pass_encoder") (:pointer :void))

(defcfun (sg-wgpu-query-buffer-info "sg_wgpu_query_buffer_info") (:struct %sg-wgpu-buffer-info)
  (buf (:struct %sg-buffer)))

(defcfun (sg-wgpu-query-image-info "sg_wgpu_query_image_info") (:struct %sg-wgpu-image-info)
  (img (:struct %sg-image)))

(defcfun (sg-wgpu-query-sampler-info "sg_wgpu_query_sampler_info") (:struct %sg-wgpu-sampler-info)
  (smp (:struct %sg-sampler)))

(defcfun (sg-wgpu-query-shader-info "sg_wgpu_query_shader_info") (:struct %sg-wgpu-shader-info)
  (shd (:struct %sg-shader)))

(defcfun (sg-wgpu-query-pipeline-info "sg_wgpu_query_pipeline_info") (:struct %sg-wgpu-pipeline-info)
  (pip (:struct %sg-pipeline)))

(defcfun (sg-wgpu-query-pass-info "sg_wgpu_query_pass_info") (:struct %sg-wgpu-pass-info)
  (pass (:struct %sg-pass)))

(defcfun (sg-gl-query-buffer-info "sg_gl_query_buffer_info") (:struct %sg-gl-buffer-info)
  (buf (:struct %sg-buffer)))

(defcfun (sg-gl-query-image-info "sg_gl_query_image_info") (:struct %sg-gl-image-info)
  (img (:struct %sg-image)))

(defcfun (sg-gl-query-sampler-info "sg_gl_query_sampler_info") (:struct %sg-gl-sampler-info)
  (smp (:struct %sg-sampler)))

(defcfun (sg-gl-query-shader-info "sg_gl_query_shader_info") (:struct %sg-gl-shader-info)
  (shd (:struct %sg-shader)))

(defcfun (sg-gl-query-pass-info "sg_gl_query_pass_info") (:struct %sg-gl-pass-info)
  (pass (:struct %sg-pass)))

(defconstant +sapp-max-touchpoints+ 8)
(defconstant +sapp-max-mousebuttons+ 3)
(defconstant +sapp-max-keycodes+ 512)
(defconstant +sapp-max-iconimages+ 8)

(defcenum sapp-event-type
  (:sapp-eventtype-invalid 0)
  (:sapp-eventtype-key-down 1)
  (:sapp-eventtype-key-up 2)
  (:sapp-eventtype-char 3)
  (:sapp-eventtype-mouse-down 4)
  (:sapp-eventtype-mouse-up 5)
  (:sapp-eventtype-mouse-scroll 6)
  (:sapp-eventtype-mouse-move 7)
  (:sapp-eventtype-mouse-enter 8)
  (:sapp-eventtype-mouse-leave 9)
  (:sapp-eventtype-touches-began 10)
  (:sapp-eventtype-touches-moved 11)
  (:sapp-eventtype-touches-ended 12)
  (:sapp-eventtype-touches-cancelled 13)
  (:sapp-eventtype-resized 14)
  (:sapp-eventtype-iconified 15)
  (:sapp-eventtype-restored 16)
  (:sapp-eventtype-focused 17)
  (:sapp-eventtype-unfocused 18)
  (:sapp-eventtype-suspended 19)
  (:sapp-eventtype-resumed 20)
  (:sapp-eventtype-quit-requested 21)
  (:sapp-eventtype-clipboard-pasted 22)
  (:sapp-eventtype-files-dropped 23)
  (:-sapp-eventtype-num 24)
  (:-sapp-eventtype-force-u32 2147483647))

(defcenum sapp-keycode
  (:sapp-keycode-invalid 0)
  (:sapp-keycode-space 32)
  (:sapp-keycode-apostrophe 39)
  (:sapp-keycode-comma 44)
  (:sapp-keycode-minus 45)
  (:sapp-keycode-period 46)
  (:sapp-keycode-slash 47)
  (:sapp-keycode-0 48)
  (:sapp-keycode-1 49)
  (:sapp-keycode-2 50)
  (:sapp-keycode-3 51)
  (:sapp-keycode-4 52)
  (:sapp-keycode-5 53)
  (:sapp-keycode-6 54)
  (:sapp-keycode-7 55)
  (:sapp-keycode-8 56)
  (:sapp-keycode-9 57)
  (:sapp-keycode-semicolon 59)
  (:sapp-keycode-equal 61)
  (:sapp-keycode-a 65)
  (:sapp-keycode-b 66)
  (:sapp-keycode-c 67)
  (:sapp-keycode-d 68)
  (:sapp-keycode-e 69)
  (:sapp-keycode-f 70)
  (:sapp-keycode-g 71)
  (:sapp-keycode-h 72)
  (:sapp-keycode-i 73)
  (:sapp-keycode-j 74)
  (:sapp-keycode-k 75)
  (:sapp-keycode-l 76)
  (:sapp-keycode-m 77)
  (:sapp-keycode-n 78)
  (:sapp-keycode-o 79)
  (:sapp-keycode-p 80)
  (:sapp-keycode-q 81)
  (:sapp-keycode-r 82)
  (:sapp-keycode-s 83)
  (:sapp-keycode-t 84)
  (:sapp-keycode-u 85)
  (:sapp-keycode-v 86)
  (:sapp-keycode-w 87)
  (:sapp-keycode-x 88)
  (:sapp-keycode-y 89)
  (:sapp-keycode-z 90)
  (:sapp-keycode-left-bracket 91)
  (:sapp-keycode-backslash 92)
  (:sapp-keycode-right-bracket 93)
  (:sapp-keycode-grave-accent 96)
  (:sapp-keycode-world-1 161)
  (:sapp-keycode-world-2 162)
  (:sapp-keycode-escape 256)
  (:sapp-keycode-enter 257)
  (:sapp-keycode-tab 258)
  (:sapp-keycode-backspace 259)
  (:sapp-keycode-insert 260)
  (:sapp-keycode-delete 261)
  (:sapp-keycode-right 262)
  (:sapp-keycode-left 263)
  (:sapp-keycode-down 264)
  (:sapp-keycode-up 265)
  (:sapp-keycode-page-up 266)
  (:sapp-keycode-page-down 267)
  (:sapp-keycode-home 268)
  (:sapp-keycode-end 269)
  (:sapp-keycode-caps-lock 280)
  (:sapp-keycode-scroll-lock 281)
  (:sapp-keycode-num-lock 282)
  (:sapp-keycode-print-screen 283)
  (:sapp-keycode-pause 284)
  (:sapp-keycode-f1 290)
  (:sapp-keycode-f2 291)
  (:sapp-keycode-f3 292)
  (:sapp-keycode-f4 293)
  (:sapp-keycode-f5 294)
  (:sapp-keycode-f6 295)
  (:sapp-keycode-f7 296)
  (:sapp-keycode-f8 297)
  (:sapp-keycode-f9 298)
  (:sapp-keycode-f10 299)
  (:sapp-keycode-f11 300)
  (:sapp-keycode-f12 301)
  (:sapp-keycode-f13 302)
  (:sapp-keycode-f14 303)
  (:sapp-keycode-f15 304)
  (:sapp-keycode-f16 305)
  (:sapp-keycode-f17 306)
  (:sapp-keycode-f18 307)
  (:sapp-keycode-f19 308)
  (:sapp-keycode-f20 309)
  (:sapp-keycode-f21 310)
  (:sapp-keycode-f22 311)
  (:sapp-keycode-f23 312)
  (:sapp-keycode-f24 313)
  (:sapp-keycode-f25 314)
  (:sapp-keycode-kp-0 320)
  (:sapp-keycode-kp-1 321)
  (:sapp-keycode-kp-2 322)
  (:sapp-keycode-kp-3 323)
  (:sapp-keycode-kp-4 324)
  (:sapp-keycode-kp-5 325)
  (:sapp-keycode-kp-6 326)
  (:sapp-keycode-kp-7 327)
  (:sapp-keycode-kp-8 328)
  (:sapp-keycode-kp-9 329)
  (:sapp-keycode-kp-decimal 330)
  (:sapp-keycode-kp-divide 331)
  (:sapp-keycode-kp-multiply 332)
  (:sapp-keycode-kp-subtract 333)
  (:sapp-keycode-kp-add 334)
  (:sapp-keycode-kp-enter 335)
  (:sapp-keycode-kp-equal 336)
  (:sapp-keycode-left-shift 340)
  (:sapp-keycode-left-control 341)
  (:sapp-keycode-left-alt 342)
  (:sapp-keycode-left-super 343)
  (:sapp-keycode-right-shift 344)
  (:sapp-keycode-right-control 345)
  (:sapp-keycode-right-alt 346)
  (:sapp-keycode-right-super 347)
  (:sapp-keycode-menu 348))

(defcenum sapp-android-tooltype
  (:sapp-androidtooltype-unknown 0)
  (:sapp-androidtooltype-finger 1)
  (:sapp-androidtooltype-stylus 2)
  (:sapp-androidtooltype-mouse 3))

(defcstruct (%sapp-touchpoint :class sapp-touchpoint-type)
  (identifier :unsigned-long)
  (pos-x :float)
  (pos-y :float)
  (android-tooltype sapp-android-tooltype)
  (changed :int))

(defcenum sapp-mousebutton
  (:sapp-mousebutton-left 0)
  (:sapp-mousebutton-right 1)
  (:sapp-mousebutton-middle 2)
  (:sapp-mousebutton-invalid 256))

(defconstant +sapp-modifier-shift+ 1)
(defconstant +sapp-modifier-ctrl+ 2)
(defconstant +sapp-modifier-alt+ 4)
(defconstant +sapp-modifier-super+ 8)
(defconstant +sapp-modifier-lmb+ 256)
(defconstant +sapp-modifier-rmb+ 512)
(defconstant +sapp-modifier-mmb+ 1024)

(defcstruct (%sapp-event :class sapp-event-type)
  (frame-count :unsigned-long-long)
  (type sapp-event-type)
  (key-code sapp-keycode)
  (char-code :unsigned-int)
  (key-repeat :int)
  (modifiers :unsigned-int)
  (mouse-button sapp-mousebutton)
  (mouse-x :float)
  (mouse-y :float)
  (mouse-dx :float)
  (mouse-dy :float)
  (scroll-x :float)
  (scroll-y :float)
  (num-touches :int)
  (touches (:array (:struct %sapp-touchpoint) 8))
  (window-width :int)
  (window-height :int)
  (framebuffer-width :int)
  (framebuffer-height :int))

(defcstruct (%sapp-range :class sapp-range-type)
  (ptr (:pointer :void))
  (size :unsigned-long))

(defcstruct (%sapp-image-desc :class sapp-image-desc-type)
  (width :int)
  (height :int)
  (pixels (:struct %sapp-range)))

(defcstruct (%sapp-icon-desc :class sapp-icon-desc-type)
  (sokol-default :int)
  (images (:array (:struct %sapp-image-desc) 8)))

(defcstruct (%sapp-allocator :class sapp-allocator-type)
  (alloc-fn :pointer)
  (free-fn :pointer)
  (user-data (:pointer :void)))

(defcenum sapp-log-item
  (:sapp-logitem-ok 0)
  (:sapp-logitem-malloc-failed 1)
  (:sapp-logitem-macos-invalid-nsopengl-profile 2)
  (:sapp-logitem-win32-load-opengl32-dll-failed 3)
  (:sapp-logitem-win32-create-helper-window-failed 4)
  (:sapp-logitem-win32-helper-window-getdc-failed 5)
  (:sapp-logitem-win32-dummy-context-set-pixelformat-failed 6)
  (:sapp-logitem-win32-create-dummy-context-failed 7)
  (:sapp-logitem-win32-dummy-context-make-current-failed 8)
  (:sapp-logitem-win32-get-pixelformat-attrib-failed 9)
  (:sapp-logitem-win32-wgl-find-pixelformat-failed 10)
  (:sapp-logitem-win32-wgl-describe-pixelformat-failed 11)
  (:sapp-logitem-win32-wgl-set-pixelformat-failed 12)
  (:sapp-logitem-win32-wgl-arb-create-context-required 13)
  (:sapp-logitem-win32-wgl-arb-create-context-profile-required 14)
  (:sapp-logitem-win32-wgl-opengl-3-2-not-supported 15)
  (:sapp-logitem-win32-wgl-opengl-profile-not-supported 16)
  (:sapp-logitem-win32-wgl-incompatible-device-context 17)
  (:sapp-logitem-win32-wgl-create-context-attribs-failed-other 18)
  (:sapp-logitem-win32-d3d11-create-device-and-swapchain-with-debug-failed 19)
  (:sapp-logitem-win32-d3d11-get-idxgifactory-failed 20)
  (:sapp-logitem-win32-d3d11-get-idxgiadapter-failed 21)
  (:sapp-logitem-win32-d3d11-query-interface-idxgidevice1-failed 22)
  (:sapp-logitem-win32-register-raw-input-devices-failed-mouse-lock 23)
  (:sapp-logitem-win32-register-raw-input-devices-failed-mouse-unlock 24)
  (:sapp-logitem-win32-get-raw-input-data-failed 25)
  (:sapp-logitem-linux-glx-load-libgl-failed 26)
  (:sapp-logitem-linux-glx-load-entry-points-failed 27)
  (:sapp-logitem-linux-glx-extension-not-found 28)
  (:sapp-logitem-linux-glx-query-version-failed 29)
  (:sapp-logitem-linux-glx-version-too-low 30)
  (:sapp-logitem-linux-glx-no-glxfbconfigs 31)
  (:sapp-logitem-linux-glx-no-suitable-glxfbconfig 32)
  (:sapp-logitem-linux-glx-get-visual-from-fbconfig-failed 33)
  (:sapp-logitem-linux-glx-required-extensions-missing 34)
  (:sapp-logitem-linux-glx-create-context-failed 35)
  (:sapp-logitem-linux-glx-create-window-failed 36)
  (:sapp-logitem-linux-x11-create-window-failed 37)
  (:sapp-logitem-linux-egl-bind-opengl-api-failed 38)
  (:sapp-logitem-linux-egl-bind-opengl-es-api-failed 39)
  (:sapp-logitem-linux-egl-get-display-failed 40)
  (:sapp-logitem-linux-egl-initialize-failed 41)
  (:sapp-logitem-linux-egl-no-configs 42)
  (:sapp-logitem-linux-egl-no-native-visual 43)
  (:sapp-logitem-linux-egl-get-visual-info-failed 44)
  (:sapp-logitem-linux-egl-create-window-surface-failed 45)
  (:sapp-logitem-linux-egl-create-context-failed 46)
  (:sapp-logitem-linux-egl-make-current-failed 47)
  (:sapp-logitem-linux-x11-open-display-failed 48)
  (:sapp-logitem-linux-x11-query-system-dpi-failed 49)
  (:sapp-logitem-linux-x11-dropped-file-uri-wrong-scheme 50)
  (:sapp-logitem-android-unsupported-input-event-input-cb 51)
  (:sapp-logitem-android-unsupported-input-event-main-cb 52)
  (:sapp-logitem-android-read-msg-failed 53)
  (:sapp-logitem-android-write-msg-failed 54)
  (:sapp-logitem-android-msg-create 55)
  (:sapp-logitem-android-msg-resume 56)
  (:sapp-logitem-android-msg-pause 57)
  (:sapp-logitem-android-msg-focus 58)
  (:sapp-logitem-android-msg-no-focus 59)
  (:sapp-logitem-android-msg-set-native-window 60)
  (:sapp-logitem-android-msg-set-input-queue 61)
  (:sapp-logitem-android-msg-destroy 62)
  (:sapp-logitem-android-unknown-msg 63)
  (:sapp-logitem-android-loop-thread-started 64)
  (:sapp-logitem-android-loop-thread-done 65)
  (:sapp-logitem-android-native-activity-onstart 66)
  (:sapp-logitem-android-native-activity-onresume 67)
  (:sapp-logitem-android-native-activity-onsaveinstancestate 68)
  (:sapp-logitem-android-native-activity-onwindowfocuschanged 69)
  (:sapp-logitem-android-native-activity-onpause 70)
  (:sapp-logitem-android-native-activity-onstop 71)
  (:sapp-logitem-android-native-activity-onnativewindowcreated 72)
  (:sapp-logitem-android-native-activity-onnativewindowdestroyed 73)
  (:sapp-logitem-android-native-activity-oninputqueuecreated 74)
  (:sapp-logitem-android-native-activity-oninputqueuedestroyed 75)
  (:sapp-logitem-android-native-activity-onconfigurationchanged 76)
  (:sapp-logitem-android-native-activity-onlowmemory 77)
  (:sapp-logitem-android-native-activity-ondestroy 78)
  (:sapp-logitem-android-native-activity-done 79)
  (:sapp-logitem-android-native-activity-oncreate 80)
  (:sapp-logitem-android-create-thread-pipe-failed 81)
  (:sapp-logitem-android-native-activity-create-success 82)
  (:sapp-logitem-wgpu-swapchain-create-surface-failed 83)
  (:sapp-logitem-wgpu-swapchain-create-swapchain-failed 84)
  (:sapp-logitem-wgpu-swapchain-create-depth-stencil-texture-failed 85)
  (:sapp-logitem-wgpu-swapchain-create-depth-stencil-view-failed 86)
  (:sapp-logitem-wgpu-swapchain-create-msaa-texture-failed 87)
  (:sapp-logitem-wgpu-swapchain-create-msaa-view-failed 88)
  (:sapp-logitem-wgpu-request-device-status-error 89)
  (:sapp-logitem-wgpu-request-device-status-unknown 90)
  (:sapp-logitem-wgpu-request-adapter-status-unavailable 91)
  (:sapp-logitem-wgpu-request-adapter-status-error 92)
  (:sapp-logitem-wgpu-request-adapter-status-unknown 93)
  (:sapp-logitem-wgpu-create-instance-failed 94)
  (:sapp-logitem-image-data-size-mismatch 95)
  (:sapp-logitem-dropped-file-path-too-long 96)
  (:sapp-logitem-clipboard-string-too-big 97))

(defcstruct (%sapp-logger :class sapp-logger-type)
  (func :pointer)
  (user-data (:pointer :void)))

(defcstruct (%sapp-desc :class sapp-desc-type)
  (init-cb :pointer)
  (frame-cb :pointer)
  (cleanup-cb :pointer)
  (event-cb :pointer)
  (user-data (:pointer :void))
  (init-userdata-cb :pointer)
  (frame-userdata-cb :pointer)
  (cleanup-userdata-cb :pointer)
  (event-userdata-cb :pointer)
  (width :int)
  (height :int)
  (sample-count :int)
  (swap-interval :int)
  (high-dpi :int)
  (fullscreen :int)
  (alpha :int)
  (window-title (:pointer :char))
  (enable-clipboard :int)
  (clipboard-size :int)
  (enable-dragndrop :int)
  (max-dropped-files :int)
  (max-dropped-file-path-length :int)
  (icon (:struct %sapp-icon-desc))
  (allocator (:struct %sapp-allocator))
  (logger (:struct %sapp-logger))
  (gl-major-version :int)
  (gl-minor-version :int)
  (win32-console-utf8 :int)
  (win32-console-create :int)
  (win32-console-attach :int)
  (html5-canvas-name (:pointer :char))
  (html5-canvas-resize :int)
  (html5-preserve-drawing-buffer :int)
  (html5-premultiplied-alpha :int)
  (html5-ask-leave-site :int)
  (ios-keyboard-resizes-canvas :int))

(defcenum sapp-html5-fetch-error
  (:sapp-html5-fetch-error-no-error 0)
  (:sapp-html5-fetch-error-buffer-too-small 1)
  (:sapp-html5-fetch-error-other 2))

(defcstruct (%sapp-html5-fetch-response :class sapp-html5-fetch-response-type)
  (succeeded :int)
  (error-code sapp-html5-fetch-error)
  (file-index :int)
  (data (:struct %sapp-range))
  (buffer (:struct %sapp-range))
  (user-data (:pointer :void)))

(defcstruct (%sapp-html5-fetch-request :class sapp-html5-fetch-request-type)
  (dropped-file-index :int)
  (callback :pointer)
  (buffer (:struct %sapp-range))
  (user-data (:pointer :void)))

(defcenum sapp-mouse-cursor
  (:sapp-mousecursor-default 0)
  (:sapp-mousecursor-arrow 1)
  (:sapp-mousecursor-ibeam 2)
  (:sapp-mousecursor-crosshair 3)
  (:sapp-mousecursor-pointing-hand 4)
  (:sapp-mousecursor-resize-ew 5)
  (:sapp-mousecursor-resize-ns 6)
  (:sapp-mousecursor-resize-nwse 7)
  (:sapp-mousecursor-resize-nesw 8)
  (:sapp-mousecursor-resize-all 9)
  (:sapp-mousecursor-not-allowed 10)
  (:-sapp-mousecursor-num 11))

(defcfun (sapp-isvalid "sapp_isvalid") :int)

(defcfun (sapp-width "sapp_width") :int)

(defcfun (sapp-widthf "sapp_widthf") :float)

(defcfun (sapp-height "sapp_height") :int)

(defcfun (sapp-heightf "sapp_heightf") :float)

(defcfun (sapp-color-format "sapp_color_format") :int)

(defcfun (sapp-depth-format "sapp_depth_format") :int)

(defcfun (sapp-sample-count "sapp_sample_count") :int)

(defcfun (sapp-high-dpi "sapp_high_dpi") :int)

(defcfun (sapp-dpi-scale "sapp_dpi_scale") :float)

(defcfun (sapp-show-keyboard "sapp_show_keyboard") :void
  (show :int))

(defcfun (sapp-keyboard-shown "sapp_keyboard_shown") :int)

(defcfun (sapp-is-fullscreen "sapp_is_fullscreen") :int)

(defcfun (sapp-toggle-fullscreen "sapp_toggle_fullscreen") :void)

(defcfun (sapp-show-mouse "sapp_show_mouse") :void
  (show :int))

(defcfun (sapp-mouse-shown "sapp_mouse_shown") :int)

(defcfun (sapp-lock-mouse "sapp_lock_mouse") :void
  (lock :int))

(defcfun (sapp-mouse-locked "sapp_mouse_locked") :int)

(defcfun (sapp-set-mouse-cursor "sapp_set_mouse_cursor") :void
  (cursor sapp-mouse-cursor))

(defcfun (sapp-get-mouse-cursor "sapp_get_mouse_cursor") sapp-mouse-cursor)

(defcfun (sapp-userdata "sapp_userdata") (:pointer :void))

(defcfun (sapp-query-desc "sapp_query_desc") (:struct %sapp-desc))

(defcfun (sapp-request-quit "sapp_request_quit") :void)

(defcfun (sapp-cancel-quit "sapp_cancel_quit") :void)

(defcfun (sapp-quit "sapp_quit") :void)

(defcfun (sapp-consume-event "sapp_consume_event") :void)

(defcfun (sapp-frame-count "sapp_frame_count") :unsigned-long-long)

(defcfun (sapp-frame-duration "sapp_frame_duration") :double)

(defcfun (sapp-set-clipboard-string "sapp_set_clipboard_string") :void
  (str (:pointer :char)))

(defcfun (sapp-get-clipboard-string "sapp_get_clipboard_string") (:pointer :char))

(defcfun (sapp-set-window-title "sapp_set_window_title") :void
  (str (:pointer :char)))

(defcfun (sapp-set-icon "sapp_set_icon") :void
  (icon_desc (:pointer (:struct %sapp-icon-desc))))

(defcfun (sapp-get-num-dropped-files "sapp_get_num_dropped_files") :int)

(defcfun (sapp-get-dropped-file-path "sapp_get_dropped_file_path") (:pointer :char)
  (index :int))

(defcfun (sapp-run "sapp_run") :void
  (desc (:pointer (:struct %sapp-desc))))

(defcfun (sapp-egl-get-display "sapp_egl_get_display") (:pointer :void))

(defcfun (sapp-egl-get-context "sapp_egl_get_context") (:pointer :void))

(defcfun (sapp-html5-ask-leave-site "sapp_html5_ask_leave_site") :void
  (ask :int))

(defcfun (sapp-html5-get-dropped-file-size "sapp_html5_get_dropped_file_size") :unsigned-int
  (index :int))

(defcfun (sapp-html5-fetch-dropped-file "sapp_html5_fetch_dropped_file") :void
  (request (:pointer (:struct %sapp-html5-fetch-request))))

(defcfun (sapp-metal-get-device "sapp_metal_get_device") (:pointer :void))

(defcfun (sapp-metal-get-renderpass-descriptor "sapp_metal_get_renderpass_descriptor") (:pointer :void))

(defcfun (sapp-metal-get-drawable "sapp_metal_get_drawable") (:pointer :void))

(defcfun (sapp-macos-get-window "sapp_macos_get_window") (:pointer :void))

(defcfun (sapp-ios-get-window "sapp_ios_get_window") (:pointer :void))

(defcfun (sapp-d3d11-get-device "sapp_d3d11_get_device") (:pointer :void))

(defcfun (sapp-d3d11-get-device-context "sapp_d3d11_get_device_context") (:pointer :void))

(defcfun (sapp-d3d11-get-swap-chain "sapp_d3d11_get_swap_chain") (:pointer :void))

(defcfun (sapp-d3d11-get-render-target-view "sapp_d3d11_get_render_target_view") (:pointer :void))

(defcfun (sapp-d3d11-get-depth-stencil-view "sapp_d3d11_get_depth_stencil_view") (:pointer :void))

(defcfun (sapp-win32-get-hwnd "sapp_win32_get_hwnd") (:pointer :void))

(defcfun (sapp-wgpu-get-device "sapp_wgpu_get_device") (:pointer :void))

(defcfun (sapp-wgpu-get-render-view "sapp_wgpu_get_render_view") (:pointer :void))

(defcfun (sapp-wgpu-get-resolve-view "sapp_wgpu_get_resolve_view") (:pointer :void))

(defcfun (sapp-wgpu-get-depth-stencil-view "sapp_wgpu_get_depth_stencil_view") (:pointer :void))

(defcfun (sapp-android-get-native-activity "sapp_android_get_native_activity") (:pointer :void))

(defcenum saudio-log-item
  (:saudio-logitem-ok 0)
  (:saudio-logitem-malloc-failed 1)
  (:saudio-logitem-alsa-snd-pcm-open-failed 2)
  (:saudio-logitem-alsa-float-samples-not-supported 3)
  (:saudio-logitem-alsa-requested-buffer-size-not-supported 4)
  (:saudio-logitem-alsa-requested-channel-count-not-supported 5)
  (:saudio-logitem-alsa-snd-pcm-hw-params-set-rate-near-failed 6)
  (:saudio-logitem-alsa-snd-pcm-hw-params-failed 7)
  (:saudio-logitem-alsa-pthread-create-failed 8)
  (:saudio-logitem-wasapi-create-event-failed 9)
  (:saudio-logitem-wasapi-create-device-enumerator-failed 10)
  (:saudio-logitem-wasapi-get-default-audio-endpoint-failed 11)
  (:saudio-logitem-wasapi-device-activate-failed 12)
  (:saudio-logitem-wasapi-audio-client-initialize-failed 13)
  (:saudio-logitem-wasapi-audio-client-get-buffer-size-failed 14)
  (:saudio-logitem-wasapi-audio-client-get-service-failed 15)
  (:saudio-logitem-wasapi-audio-client-set-event-handle-failed 16)
  (:saudio-logitem-wasapi-create-thread-failed 17)
  (:saudio-logitem-aaudio-streambuilder-open-stream-failed 18)
  (:saudio-logitem-aaudio-pthread-create-failed 19)
  (:saudio-logitem-aaudio-restarting-stream-after-error 20)
  (:saudio-logitem-using-aaudio-backend 21)
  (:saudio-logitem-aaudio-create-streambuilder-failed 22)
  (:saudio-logitem-using-sles-backend 23)
  (:saudio-logitem-sles-create-engine-failed 24)
  (:saudio-logitem-sles-engine-get-engine-interface-failed 25)
  (:saudio-logitem-sles-create-output-mix-failed 26)
  (:saudio-logitem-sles-mixer-get-volume-interface-failed 27)
  (:saudio-logitem-sles-engine-create-audio-player-failed 28)
  (:saudio-logitem-sles-player-get-play-interface-failed 29)
  (:saudio-logitem-sles-player-get-volume-interface-failed 30)
  (:saudio-logitem-sles-player-get-bufferqueue-interface-failed 31)
  (:saudio-logitem-coreaudio-new-output-failed 32)
  (:saudio-logitem-coreaudio-allocate-buffer-failed 33)
  (:saudio-logitem-coreaudio-start-failed 34)
  (:saudio-logitem-backend-buffer-size-isnt-multiple-of-packet-size 35))

(defcstruct (%saudio-logger :class saudio-logger-type)
  (func :pointer)
  (user-data (:pointer :void)))

(defcstruct (%saudio-allocator :class saudio-allocator-type)
  (alloc-fn :pointer)
  (free-fn :pointer)
  (user-data (:pointer :void)))

(defcstruct (%saudio-desc :class saudio-desc-type)
  (sample-rate :int)
  (num-channels :int)
  (buffer-frames :int)
  (packet-frames :int)
  (num-packets :int)
  (stream-cb :pointer)
  (stream-userdata-cb :pointer)
  (user-data (:pointer :void))
  (allocator (:struct %saudio-allocator))
  (logger (:struct %saudio-logger)))

(defcfun (saudio-setup "saudio_setup") :void
  (desc (:pointer (:struct %saudio-desc))))

(defcfun (saudio-shutdown "saudio_shutdown") :void)

(defcfun (saudio-isvalid "saudio_isvalid") :int)

(defcfun (saudio-userdata "saudio_userdata") (:pointer :void))

(defcfun (saudio-query-desc "saudio_query_desc") (:struct %saudio-desc))

(defcfun (saudio-sample-rate "saudio_sample_rate") :int)

(defcfun (saudio-buffer-frames "saudio_buffer_frames") :int)

(defcfun (saudio-channels "saudio_channels") :int)

(defcfun (saudio-suspended "saudio_suspended") :int)

(defcfun (saudio-expect "saudio_expect") :int)

(defcfun (saudio-push "saudio_push") :int
  (frames (:pointer :float))
  (num_frames :int))

(defcfun (stm-setup "stm_setup") :void)

(defcfun (stm-now "stm_now") :unsigned-long-long)

(defcfun (stm-diff "stm_diff") :unsigned-long-long
  (new_ticks :unsigned-long-long)
  (old_ticks :unsigned-long-long))

(defcfun (stm-since "stm_since") :unsigned-long-long
  (start_ticks :unsigned-long-long))

(defcfun (stm-laptime "stm_laptime") :unsigned-long-long
  (last_time (:pointer :unsigned-long-long)))

(defcfun (stm-round-to-common-refresh-rate "stm_round_to_common_refresh_rate") :unsigned-long-long
  (frame_ticks :unsigned-long-long))

(defcfun (stm-sec "stm_sec") :double
  (ticks :unsigned-long-long))

(defcfun (stm-ms "stm_ms") :double
  (ticks :unsigned-long-long))

(defcfun (stm-us "stm_us") :double
  (ticks :unsigned-long-long))

(defcfun (stm-ns "stm_ns") :double
  (ticks :unsigned-long-long))

(defcstruct (%sargs-allocator :class sargs-allocator-type)
  (alloc-fn :pointer)
  (free-fn :pointer)
  (user-data (:pointer :void)))

(defcstruct (%sargs-desc :class sargs-desc-type)
  (argc :int)
  (argv (:pointer (:pointer :char)))
  (max-args :int)
  (buf-size :int)
  (allocator (:struct %sargs-allocator)))

(defcfun (sargs-setup "sargs_setup") :void
  (desc (:pointer (:struct %sargs-desc))))

(defcfun (sargs-shutdown "sargs_shutdown") :void)

(defcfun (sargs-isvalid "sargs_isvalid") :int)

(defcfun (sargs-exists "sargs_exists") :int
  (key (:pointer :char)))

(defcfun (sargs-value "sargs_value") (:pointer :char)
  (key (:pointer :char)))

(defcfun (sargs-value-def "sargs_value_def") (:pointer :char)
  (key (:pointer :char))
  (def (:pointer :char)))

(defcfun (sargs-equals "sargs_equals") :int
  (key (:pointer :char))
  (val (:pointer :char)))

(defcfun (sargs-boolean "sargs_boolean") :int
  (key (:pointer :char)))

(defcfun (sargs-find "sargs_find") :int
  (key (:pointer :char)))

(defcfun (sargs-num-args "sargs_num_args") :int)

(defcfun (sargs-key-at "sargs_key_at") (:pointer :char)
  (index :int))

(defcfun (sargs-value-at "sargs_value_at") (:pointer :char)
  (index :int))

(defcenum sfetch-log-item-t
  (:sfetch-logitem-ok 0)
  (:sfetch-logitem-malloc-failed 1)
  (:sfetch-logitem-file-path-utf8-decoding-failed 2)
  (:sfetch-logitem-send-queue-full 3)
  (:sfetch-logitem-request-channel-index-too-big 4)
  (:sfetch-logitem-request-path-is-null 5)
  (:sfetch-logitem-request-path-too-long 6)
  (:sfetch-logitem-request-callback-missing 7)
  (:sfetch-logitem-request-chunk-size-greater-buffer-size 8)
  (:sfetch-logitem-request-userdata-ptr-is-set-but-userdata-size-is-null 9)
  (:sfetch-logitem-request-userdata-ptr-is-null-but-userdata-size-is-not 10)
  (:sfetch-logitem-request-userdata-size-too-big 11)
  (:sfetch-logitem-clamping-num-channels-to-max-channels 12)
  (:sfetch-logitem-request-pool-exhausted 13))

(defcstruct (%sfetch-logger-t :class sfetch-logger-t-type)
  (func :pointer)
  (user-data (:pointer :void)))

(defcstruct (%sfetch-range-t :class sfetch-range-t-type)
  (ptr (:pointer :void))
  (size :unsigned-long))

(defcstruct (%sfetch-allocator-t :class sfetch-allocator-t-type)
  (alloc-fn :pointer)
  (free-fn :pointer)
  (user-data (:pointer :void)))

(defcstruct (%sfetch-desc-t :class sfetch-desc-t-type)
  (max-requests :unsigned-int)
  (num-channels :unsigned-int)
  (num-lanes :unsigned-int)
  (allocator (:struct %sfetch-allocator-t))
  (logger (:struct %sfetch-logger-t)))

(defcstruct (%sfetch-handle-t :class sfetch-handle-t-type)
  (id :unsigned-int))

(defcenum sfetch-error-t
  (:sfetch-error-no-error 0)
  (:sfetch-error-file-not-found 1)
  (:sfetch-error-no-buffer 2)
  (:sfetch-error-buffer-too-small 3)
  (:sfetch-error-unexpected-eof 4)
  (:sfetch-error-invalid-http-status 5)
  (:sfetch-error-cancelled 6))

(defcstruct (%sfetch-response-t :class sfetch-response-t-type)
  (handle (:struct %sfetch-handle-t))
  (dispatched :int)
  (fetched :int)
  (paused :int)
  (finished :int)
  (failed :int)
  (cancelled :int)
  (error-code sfetch-error-t)
  (channel :unsigned-int)
  (lane :unsigned-int)
  (path (:pointer :char))
  (user-data (:pointer :void))
  (data-offset :unsigned-int)
  (data (:struct %sfetch-range-t))
  (buffer (:struct %sfetch-range-t)))

(defcstruct (%sfetch-request-t :class sfetch-request-t-type)
  (channel :unsigned-int)
  (path (:pointer :char))
  (callback :pointer)
  (chunk-size :unsigned-int)
  (buffer (:struct %sfetch-range-t))
  (user-data (:struct %sfetch-range-t)))

(defcfun (sfetch-setup "sfetch_setup") :void
  (desc (:pointer (:struct %sfetch-desc-t))))

(defcfun (sfetch-shutdown "sfetch_shutdown") :void)

(defcfun (sfetch-valid "sfetch_valid") :int)

(defcfun (sfetch-desc "sfetch_desc") (:struct %sfetch-desc-t))

(defcfun (sfetch-max-userdata-bytes "sfetch_max_userdata_bytes") :int)

(defcfun (sfetch-max-path "sfetch_max_path") :int)

(defcfun (sfetch-send "sfetch_send") (:struct %sfetch-handle-t)
  (request (:pointer (:struct %sfetch-request-t))))

(defcfun (sfetch-handle-valid "sfetch_handle_valid") :int
  (h (:struct %sfetch-handle-t)))

(defcfun (sfetch-dowork "sfetch_dowork") :void)

(defcfun (sfetch-bind-buffer "sfetch_bind_buffer") :void
  (h (:struct %sfetch-handle-t))
  (buffer (:struct %sfetch-range-t)))

(defcfun (sfetch-unbind-buffer "sfetch_unbind_buffer") (:pointer :void)
  (h (:struct %sfetch-handle-t)))

(defcfun (sfetch-cancel "sfetch_cancel") :void
  (h (:struct %sfetch-handle-t)))

(defcfun (sfetch-pause "sfetch_pause") :void
  (h (:struct %sfetch-handle-t)))

(defcfun (sfetch-continue "sfetch_continue") :void
  (h (:struct %sfetch-handle-t)))

(defcfun (sokol-default-sgdesc "sokol_default_sgdesc") (:pointer (:struct %sg-desc)))
(defcfun (sg-install-trace-hooks-ptr "sg_install_trace_hooks_ptr") (:struct %sg-trace-hooks)
  (trace_hooks (:pointer (:struct %sg-trace-hooks))))
(defcfun (sg-make-buffer-ptr "sg_make_buffer_ptr") (:struct %sg-buffer)
  (desc (:pointer (:struct %sg-buffer-desc))))
(defcfun (sg-make-image-ptr "sg_make_image_ptr") (:struct %sg-image)
  (desc (:pointer (:struct %sg-image-desc))))
(defcfun (sg-make-sampler-ptr "sg_make_sampler_ptr") (:struct %sg-sampler)
  (desc (:pointer (:struct %sg-sampler-desc))))
(defcfun (sg-make-shader-ptr "sg_make_shader_ptr") (:struct %sg-shader)
  (desc (:pointer (:struct %sg-shader-desc))))
(defcfun (sg-make-pipeline-ptr "sg_make_pipeline_ptr") (:struct %sg-pipeline)
  (desc (:pointer (:struct %sg-pipeline-desc))))
(defcfun (sg-make-pass-ptr "sg_make_pass_ptr") (:struct %sg-pass)
  (desc (:pointer (:struct %sg-pass-desc))))
(defcfun (sg-query-desc-ptr "sg_query_desc_ptr") (:struct %sg-desc))
(defcfun (sg-query-features-ptr "sg_query_features_ptr") (:struct %sg-features))
(defcfun (sg-query-limits-ptr "sg_query_limits_ptr") (:struct %sg-limits))
(defcfun (sg-query-pixelformat-ptr "sg_query_pixelformat_ptr") (:struct %sg-pixelformat-info)
  (fmt sg-pixel-format))
(defcfun (sg-query-buffer-info-ptr "sg_query_buffer_info_ptr") (:struct %sg-buffer-info)
  (buf (:struct %sg-buffer)))
(defcfun (sg-query-image-info-ptr "sg_query_image_info_ptr") (:struct %sg-image-info)
  (img (:struct %sg-image)))
(defcfun (sg-query-sampler-info-ptr "sg_query_sampler_info_ptr") (:struct %sg-sampler-info)
  (smp (:struct %sg-sampler)))
(defcfun (sg-query-shader-info-ptr "sg_query_shader_info_ptr") (:struct %sg-shader-info)
  (shd (:struct %sg-shader)))
(defcfun (sg-query-pipeline-info-ptr "sg_query_pipeline_info_ptr") (:struct %sg-pipeline-info)
  (pip (:struct %sg-pipeline)))
(defcfun (sg-query-pass-info-ptr "sg_query_pass_info_ptr") (:struct %sg-pass-info)
  (pass (:struct %sg-pass)))
(defcfun (sg-query-buffer-desc-ptr "sg_query_buffer_desc_ptr") (:struct %sg-buffer-desc)
  (buf (:struct %sg-buffer)))
(defcfun (sg-query-image-desc-ptr "sg_query_image_desc_ptr") (:struct %sg-image-desc)
  (img (:struct %sg-image)))
(defcfun (sg-query-sampler-desc-ptr "sg_query_sampler_desc_ptr") (:struct %sg-sampler-desc)
  (smp (:struct %sg-sampler)))
(defcfun (sg-query-shader-desc-ptr "sg_query_shader_desc_ptr") (:struct %sg-shader-desc)
  (shd (:struct %sg-shader)))
(defcfun (sg-query-pipeline-desc-ptr "sg_query_pipeline_desc_ptr") (:struct %sg-pipeline-desc)
  (pip (:struct %sg-pipeline)))
(defcfun (sg-query-pass-desc-ptr "sg_query_pass_desc_ptr") (:struct %sg-pass-desc)
  (pass (:struct %sg-pass)))
(defcfun (sg-query-buffer-defaults-ptr "sg_query_buffer_defaults_ptr") (:struct %sg-buffer-desc)
  (desc (:pointer (:struct %sg-buffer-desc))))
(defcfun (sg-query-image-defaults-ptr "sg_query_image_defaults_ptr") (:struct %sg-image-desc)
  (desc (:pointer (:struct %sg-image-desc))))
(defcfun (sg-query-sampler-defaults-ptr "sg_query_sampler_defaults_ptr") (:struct %sg-sampler-desc)
  (desc (:pointer (:struct %sg-sampler-desc))))
(defcfun (sg-query-shader-defaults-ptr "sg_query_shader_defaults_ptr") (:struct %sg-shader-desc)
  (desc (:pointer (:struct %sg-shader-desc))))
(defcfun (sg-query-pipeline-defaults-ptr "sg_query_pipeline_defaults_ptr") (:struct %sg-pipeline-desc)
  (desc (:pointer (:struct %sg-pipeline-desc))))
(defcfun (sg-query-pass-defaults-ptr "sg_query_pass_defaults_ptr") (:struct %sg-pass-desc)
  (desc (:pointer (:struct %sg-pass-desc))))
(defcfun (sg-alloc-buffer-ptr "sg_alloc_buffer_ptr") (:struct %sg-buffer))
(defcfun (sg-alloc-image-ptr "sg_alloc_image_ptr") (:struct %sg-image))
(defcfun (sg-alloc-sampler-ptr "sg_alloc_sampler_ptr") (:struct %sg-sampler))
(defcfun (sg-alloc-shader-ptr "sg_alloc_shader_ptr") (:struct %sg-shader))
(defcfun (sg-alloc-pipeline-ptr "sg_alloc_pipeline_ptr") (:struct %sg-pipeline))
(defcfun (sg-alloc-pass-ptr "sg_alloc_pass_ptr") (:struct %sg-pass))
(defcfun (sg-query-frame-stats-ptr "sg_query_frame_stats_ptr") (:struct %sg-frame-stats))
(defcfun (sg-setup-context-ptr "sg_setup_context_ptr") (:struct %sg-context))
(defcfun (sg-d3d11-query-buffer-info-ptr "sg_d3d11_query_buffer_info_ptr") (:struct %sg-d3d11-buffer-info)
  (buf (:struct %sg-buffer)))
(defcfun (sg-d3d11-query-image-info-ptr "sg_d3d11_query_image_info_ptr") (:struct %sg-d3d11-image-info)
  (img (:struct %sg-image)))
(defcfun (sg-d3d11-query-sampler-info-ptr "sg_d3d11_query_sampler_info_ptr") (:struct %sg-d3d11-sampler-info)
  (smp (:struct %sg-sampler)))
(defcfun (sg-d3d11-query-shader-info-ptr "sg_d3d11_query_shader_info_ptr") (:struct %sg-d3d11-shader-info)
  (shd (:struct %sg-shader)))
(defcfun (sg-d3d11-query-pipeline-info-ptr "sg_d3d11_query_pipeline_info_ptr") (:struct %sg-d3d11-pipeline-info)
  (pip (:struct %sg-pipeline)))
(defcfun (sg-d3d11-query-pass-info-ptr "sg_d3d11_query_pass_info_ptr") (:struct %sg-d3d11-pass-info)
  (pass (:struct %sg-pass)))
(defcfun (sg-mtl-query-buffer-info-ptr "sg_mtl_query_buffer_info_ptr") (:struct %sg-mtl-buffer-info)
  (buf (:struct %sg-buffer)))
(defcfun (sg-mtl-query-image-info-ptr "sg_mtl_query_image_info_ptr") (:struct %sg-mtl-image-info)
  (img (:struct %sg-image)))
(defcfun (sg-mtl-query-sampler-info-ptr "sg_mtl_query_sampler_info_ptr") (:struct %sg-mtl-sampler-info)
  (smp (:struct %sg-sampler)))
(defcfun (sg-mtl-query-shader-info-ptr "sg_mtl_query_shader_info_ptr") (:struct %sg-mtl-shader-info)
  (shd (:struct %sg-shader)))
(defcfun (sg-mtl-query-pipeline-info-ptr "sg_mtl_query_pipeline_info_ptr") (:struct %sg-mtl-pipeline-info)
  (pip (:struct %sg-pipeline)))
(defcfun (sg-wgpu-query-buffer-info-ptr "sg_wgpu_query_buffer_info_ptr") (:struct %sg-wgpu-buffer-info)
  (buf (:struct %sg-buffer)))
(defcfun (sg-wgpu-query-image-info-ptr "sg_wgpu_query_image_info_ptr") (:struct %sg-wgpu-image-info)
  (img (:struct %sg-image)))
(defcfun (sg-wgpu-query-sampler-info-ptr "sg_wgpu_query_sampler_info_ptr") (:struct %sg-wgpu-sampler-info)
  (smp (:struct %sg-sampler)))
(defcfun (sg-wgpu-query-shader-info-ptr "sg_wgpu_query_shader_info_ptr") (:struct %sg-wgpu-shader-info)
  (shd (:struct %sg-shader)))
(defcfun (sg-wgpu-query-pipeline-info-ptr "sg_wgpu_query_pipeline_info_ptr") (:struct %sg-wgpu-pipeline-info)
  (pip (:struct %sg-pipeline)))
(defcfun (sg-wgpu-query-pass-info-ptr "sg_wgpu_query_pass_info_ptr") (:struct %sg-wgpu-pass-info)
  (pass (:struct %sg-pass)))
(defcfun (sg-gl-query-buffer-info-ptr "sg_gl_query_buffer_info_ptr") (:struct %sg-gl-buffer-info)
  (buf (:struct %sg-buffer)))
(defcfun (sg-gl-query-image-info-ptr "sg_gl_query_image_info_ptr") (:struct %sg-gl-image-info)
  (img (:struct %sg-image)))
(defcfun (sg-gl-query-sampler-info-ptr "sg_gl_query_sampler_info_ptr") (:struct %sg-gl-sampler-info)
  (smp (:struct %sg-sampler)))
(defcfun (sg-gl-query-shader-info-ptr "sg_gl_query_shader_info_ptr") (:struct %sg-gl-shader-info)
  (shd (:struct %sg-shader)))
(defcfun (sg-gl-query-pass-info-ptr "sg_gl_query_pass_info_ptr") (:struct %sg-gl-pass-info)
  (pass (:struct %sg-pass)))
(defcfun (sapp-query-desc-ptr "sapp_query_desc_ptr") (:struct %sapp-desc))
(defcfun (saudio-query-desc-ptr "saudio_query_desc_ptr") (:struct %saudio-desc))
(defcfun (sfetch-desc-ptr "sfetch_desc_ptr") (:struct %sfetch-desc-t))
(defcfun (sfetch-send-ptr "sfetch_send_ptr") (:struct %sfetch-handle-t)
  (request (:pointer (:struct %sfetch-request-t))))