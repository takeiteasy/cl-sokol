(in-package #:cl-sokol-shaders)

;;; Type mappings between clsl types and sokol types

;; Map clsl type names to sokol vertex attribute base types
(defun type-to-attr-base-type (type-name)
  "Convert a clsl type name to sg-shader-attr-base-type keyword.
TYPE-NAME should be a keyword like :float, :vec4, :int, :ivec3, :uint, etc."
  (case type-name
    ;; Float types
    ((:float :vec2 :vec3 :vec4
      :mat2 :mat3 :mat4
      :mat2x2 :mat2x3 :mat2x4
      :mat3x2 :mat3x3 :mat3x4
      :mat4x2 :mat4x3 :mat4x4
      :double :dvec2 :dvec3 :dvec4
      :dmat2 :dmat3 :dmat4)
     :sg-shaderattrbasetype-float)
    ;; Signed integer types
    ((:int :ivec2 :ivec3 :ivec4
      :int8 :i8vec2 :i8vec3 :i8vec4
      :int16 :i16vec2 :i16vec3 :i16vec4
      :int64 :i64vec2 :i64vec3 :i64vec4)
     :sg-shaderattrbasetype-sint)
    ;; Unsigned integer types
    ((:uint :uvec2 :uvec3 :uvec4
      :uint8 :u8vec2 :u8vec3 :u8vec4
      :uint16 :u16vec2 :u16vec3 :u16vec4
      :uint64 :u64vec2 :u64vec3 :u64vec4
      :bool :bvec2 :bvec3 :bvec4)  ; bools map to uint
     :sg-shaderattrbasetype-uint)
    (otherwise
     :sg-shaderattrbasetype-undefined)))

;; Map clsl type names to sokol uniform types
(defun type-to-uniform-type (type-name)
  "Convert a clsl type name to sg-uniform-type keyword."
  (case type-name
    (:float :sg-uniformtype-float)
    (:vec2 :sg-uniformtype-float2)
    (:vec3 :sg-uniformtype-float3)
    (:vec4 :sg-uniformtype-float4)
    (:int :sg-uniformtype-int)
    (:ivec2 :sg-uniformtype-int2)
    (:ivec3 :sg-uniformtype-int3)
    (:ivec4 :sg-uniformtype-int4)
    (:mat4 :sg-uniformtype-mat4)
    (otherwise :sg-uniformtype-invalid)))

;; Get the size in bytes of a shader type
(defun type-size-bytes (type-name)
  "Return the size in bytes of a shader type."
  (case type-name
    (:float 4)
    (:vec2 8)
    (:vec3 12)
    (:vec4 16)
    (:int 4)
    (:ivec2 8)
    (:ivec3 12)
    (:ivec4 16)
    (:uint 4)
    (:uvec2 8)
    (:uvec3 12)
    (:uvec4 16)
    (:mat2 (* 4 4))      ; 2x2 floats
    (:mat3 (* 9 4))      ; 3x3 floats
    (:mat4 (* 16 4))     ; 4x4 floats
    (:double 8)
    (:dvec2 16)
    (:dvec3 24)
    (:dvec4 32)
    (otherwise 0)))

;; Get the size in bytes of a shader type using std140 layout rules
;; std140 rules: vec3 takes same space as vec4, matrices are arrays of column vectors
(defun type-size-std140 (type-name)
  "Return the size in bytes of a shader type using std140 layout rules.
std140 is the default layout for uniform blocks in GLSL/Metal/HLSL."
  (case type-name
    (:float 4)
    (:vec2 8)
    (:vec3 16)     ; vec3 aligned to vec4 in std140
    (:vec4 16)
    (:int 4)
    (:ivec2 8)
    (:ivec3 16)    ; ivec3 aligned to ivec4 in std140
    (:ivec4 16)
    (:uint 4)
    (:uvec2 8)
    (:uvec3 16)    ; uvec3 aligned to uvec4 in std140
    (:uvec4 16)
    (:bool 4)      ; bool is 4 bytes in std140
    (:bvec2 8)
    (:bvec3 16)
    (:bvec4 16)
    ;; Matrices in std140: array of column vectors, each vec aligned to vec4
    (:mat2 (* 2 16))     ; 2 columns of vec4-aligned vec2 = 32 bytes
    (:mat3 (* 3 16))     ; 3 columns of vec4-aligned vec3 = 48 bytes
    (:mat4 (* 4 16))     ; 4 columns of vec4 = 64 bytes
    (:mat2x2 (* 2 16))
    (:mat2x3 (* 2 16))
    (:mat2x4 (* 2 16))
    (:mat3x2 (* 3 16))
    (:mat3x3 (* 3 16))
    (:mat3x4 (* 3 16))
    (:mat4x2 (* 4 16))
    (:mat4x3 (* 4 16))
    (:mat4x4 (* 4 16))
    (:double 8)
    (:dvec2 16)
    (:dvec3 32)    ; dvec3 aligned to dvec4
    (:dvec4 32)
    (otherwise 16)))  ; Default to 16 for unknown types (conservative)

;; Map sokol backend to shader language backend
(defparameter *backend-mapping*
  '((:glcore33 . :glsl)
    (:gles3 . :glsl)
    (:metal-macos . :metal)
    (:metal-ios . :metal)
    (:metal-simulator . :metal)
    (:d3d11 . :hlsl)
    (:wgpu . :wgsl))
  "Mapping from sokol backends to shader language backends.")

(defun sokol-backend-to-shader-backend (sokol-backend)
  "Convert a sokol backend keyword to a shader language backend keyword."
  (or (cdr (assoc sokol-backend *backend-mapping*))
      (error "Unsupported sokol backend: ~A" sokol-backend)))

(defparameter *supported-backends*
  '(:glsl :metal :hlsl :wgsl)
  "List of supported shader language backends.")

;; GLSL version mapping
(defun sokol-backend-to-glsl-version (sokol-backend)
  "Return the GLSL version number for a sokol backend."
  (case sokol-backend
    (:glcore33 330)
    (:gles3 300)  ; Actually GLSL ES 300
    (otherwise 450)))

;; Metal version for different platforms
(defun sokol-backend-to-metal-version (sokol-backend)
  "Return the Metal version string for a sokol backend."
  (case sokol-backend
    (:metal-macos "2.0")
    (:metal-ios "2.0")
    (:metal-simulator "2.0")
    (otherwise "2.0")))
