(in-package #:3bgl-shaders)

;;; HLSL (High Level Shading Language) backend for 3bgl-shader
;;; This file provides HLSL output generation for Direct3D 11 support.

;; HLSL type name translations
(defparameter *hlsl-type-names*
  (alexandria:plist-hash-table
   '(:float "float"
     :vec2 "float2"
     :vec3 "float3"
     :vec4 "float4"
     :int "int"
     :ivec2 "int2"
     :ivec3 "int3"
     :ivec4 "int4"
     :uint "uint"
     :uvec2 "uint2"
     :uvec3 "uint3"
     :uvec4 "uint4"
     :bool "bool"
     :bvec2 "bool2"
     :bvec3 "bool3"
     :bvec4 "bool4"
     :mat2 "float2x2"
     :mat3 "float3x3"
     :mat4 "float4x4"
     :mat2x2 "float2x2"
     :mat2x3 "float2x3"
     :mat2x4 "float2x4"
     :mat3x2 "float3x2"
     :mat3x3 "float3x3"
     :mat3x4 "float3x4"
     :mat4x2 "float4x2"
     :mat4x3 "float4x3"
     :mat4x4 "float4x4"
     :double "double"
     :dvec2 "double2"
     :dvec3 "double3"
     :dvec4 "double4"
     :sampler-2d "Texture2D"
     :sampler-3d "Texture3D"
     :sampler-cube "TextureCube"
     :sampler-2d-array "Texture2DArray"
     :isampler-2d "Texture2D<int4>"
     :usampler-2d "Texture2D<uint4>"
     :sampler-2d-shadow "Texture2D"
     :void "void")))

(defun hlsl-type-name (type-name)
  "Convert a GLSL type name to HLSL type name."
  (or (gethash type-name *hlsl-type-names*)
      (string-downcase (symbol-name type-name))))

;; HLSL-specific translate-type method
(defmethod translate-type-for-backend ((type concrete-type) (backend (eql :hlsl)))
  (hlsl-type-name (name type)))

;;; HLSL Semantic Mapping

(defun hlsl-semantic-for-input (name index stage)
  "Generate HLSL semantic for an input binding."
  (let ((name-str (string-upcase (symbol-name name))))
    (cond
      ;; Position input
      ((or (string= name-str "POSITION")
           (string= name-str "GL-POSITION"))
       "POSITION")
      ;; Color inputs
      ((or (string-search "COLOR" name-str)
           (string= name-str "COLOR0"))
       (format nil "COLOR~d" (or (extract-index name-str) 0)))
      ;; Texture coordinate inputs
      ((or (string-search "TEXCOORD" name-str)
           (string-search "UV" name-str)
           (string-search "TEX-COORD" name-str))
       (format nil "TEXCOORD~d" (or (extract-index name-str) index)))
      ;; Normal
      ((string-search "NORMAL" name-str)
       "NORMAL")
      ;; Tangent
      ((string-search "TANGENT" name-str)
       "TANGENT")
      ;; Default to TEXCOORD with index
      (t (format nil "TEXCOORD~d" index)))))

(defun hlsl-semantic-for-output (name stage)
  "Generate HLSL semantic for an output binding."
  (let ((name-str (string-upcase (symbol-name name))))
    (cond
      ;; Position output (vertex shader)
      ((or (string= name-str "GL-POSITION")
           (string= name-str "POSITION"))
       "SV_Position")
      ;; Fragment depth
      ((or (string= name-str "GL-FRAG-DEPTH")
           (string= name-str "FRAG-DEPTH"))
       "SV_Depth")
      ;; Fragment color output
      ((eq stage :fragment)
       (if (string-search "COLOR" name-str)
           (format nil "SV_Target~d" (or (extract-index name-str) 0))
           "SV_Target0"))
      ;; Varyings use TEXCOORD semantics
      (t "TEXCOORD0"))))

(defun string-search (needle haystack)
  "Return T if NEEDLE is found in HAYSTACK."
  (search needle haystack))

(defun extract-index (name-str)
  "Extract numeric index from end of name string."
  (let ((len (length name-str)))
    (when (and (> len 0) (digit-char-p (char name-str (1- len))))
      (parse-integer (subseq name-str (1- len)) :junk-allowed t))))

;;; HLSL Context for code generation

(defclass hlsl-shader-context ()
  ((stage :initarg :stage :accessor hlsl-stage)
   (inputs :initform nil :accessor hlsl-inputs)
   (outputs :initform nil :accessor hlsl-outputs)
   (uniforms :initform nil :accessor hlsl-uniforms)
   (cbuffer-index :initform 0 :accessor hlsl-cbuffer-index)
   (texture-index :initform 0 :accessor hlsl-texture-index)
   (sampler-index :initform 0 :accessor hlsl-sampler-index))
  (:documentation "Context for HLSL shader generation."))

(defvar *hlsl-context* nil)

(defun collect-hlsl-bindings (objects stage)
  "Collect input/output/uniform bindings for HLSL struct generation."
  (let ((inputs nil)
        (outputs nil)
        (uniforms nil))
    (loop for obj in objects
          when (typep obj 'interface-binding)
            do (let* ((sb (stage-binding obj))
                      (iq (interface-qualifier sb))
                      (iq-key (if (consp iq) (car iq) iq)))
                 (case iq-key
                   (:in (push obj inputs))
                   (:out (push obj outputs))
                   (:uniform (push obj uniforms)))))
    (values (nreverse inputs)
            (nreverse outputs)
            (nreverse uniforms))))

;;; HLSL Input/Output Struct Generation

(defun print-hlsl-input-struct (inputs stage stream)
  "Print the HLSL input struct definition."
  (format stream "struct VSInput {~%")
  (let ((index 0))
    (loop for input in inputs
          for sb = (stage-binding input)
          for type = (value-type sb)
          for lq = (layout-qualifier sb)
          for location = (or (getf lq :location) index)
          do (format stream "    ~a ~a : ~a;~%"
                     (hlsl-type-name (name type))
                     (translate-name input)
                     (hlsl-semantic-for-input (name input) location stage))
             (incf index)))
  (format stream "};~%~%"))

(defun print-hlsl-output-struct (outputs stage stream)
  "Print the HLSL output struct definition."
  (format stream "struct ~a {~%"
          (if (eq stage :vertex) "VSOutput" "PSOutput"))
  ;; For vertex shader, always include position with SV_Position
  (when (eq stage :vertex)
    (format stream "    float4 position : SV_Position;~%"))
  ;; Add other outputs
  (let ((locn-index 0))
    (loop for output in outputs
          for name = (name output)
          for sb = (stage-binding output)
          unless (or (eq name 'gl-position)
                     (string-equal (string name) "GL-POSITION"))
            do (cond
                 ;; Interface block with bindings - expand to individual fields
                 ((or (interface-block sb) (typep (binding sb) 'bindings))
                  (let ((bindings (bindings (or (interface-block sb) (binding sb)))))
                    (loop for b in bindings
                          do (format stream "    ~a ~a : TEXCOORD~d;~%"
                                     (hlsl-type-name (name (value-type b)))
                                     (translate-name b)
                                     locn-index)
                             (incf locn-index))))
                 ;; Fragment output
                 ((eq stage :fragment)
                  (format stream "    ~a ~a : ~a;~%"
                          (hlsl-type-name (name (value-type sb)))
                          (translate-name output)
                          (hlsl-semantic-for-output name stage)))
                 ;; Varying
                 (t
                  (format stream "    ~a ~a : TEXCOORD~d;~%"
                          (hlsl-type-name (name (value-type sb)))
                          (translate-name output)
                          locn-index)
                  (incf locn-index)))))
  (format stream "};~%~%"))

(defun print-hlsl-fragment-input-struct (inputs stream)
  "Print the HLSL input struct for pixel shader (from vertex outputs)."
  (format stream "struct PSInput {~%")
  (format stream "    float4 position : SV_Position;~%")
  (let ((locn-index 0))
    (loop for input in inputs
          for name = (name input)
          for sb = (stage-binding input)
          unless (or (eq name 'gl-position)
                     (string-equal (string name) "GL-POSITION"))
            do (cond
                 ;; Interface block with bindings - expand
                 ((or (interface-block sb) (typep (binding sb) 'bindings))
                  (let ((bindings (bindings (or (interface-block sb) (binding sb)))))
                    (loop for b in bindings
                          do (format stream "    ~a ~a : TEXCOORD~d;~%"
                                     (hlsl-type-name (name (value-type b)))
                                     (translate-name b)
                                     locn-index)
                             (incf locn-index))))
                 ;; Simple input
                 (t
                  (format stream "    ~a ~a : TEXCOORD~d;~%"
                          (hlsl-type-name (name (value-type sb)))
                          (translate-name input)
                          locn-index)
                  (incf locn-index)))))
  (format stream "};~%~%"))

;;; HLSL Constant Buffer and Resource Generation

(defun print-hlsl-cbuffer (uniforms stream)
  "Print constant buffer declarations for uniforms."
  ;; Group non-sampler uniforms into a cbuffer
  (let ((scalar-uniforms (loop for u in uniforms
                               for sb = (stage-binding u)
                               for type = (value-type sb)
                               unless (is-sampler-type-name (name type))
                                 collect u)))
    (when scalar-uniforms
      (format stream "cbuffer params : register(b0) {~%")
      (loop for u in scalar-uniforms
            for sb = (stage-binding u)
            for type = (value-type sb)
            do (format stream "    ~a ~a;~%"
                       (hlsl-type-name (name type))
                       (translate-name u)))
      (format stream "};~%~%"))))

(defun is-sampler-type-name (type-name)
  "Return T if TYPE-NAME is a sampler type."
  (member type-name '(:sampler-2d :sampler-3d :sampler-cube :sampler-2d-array
                      :isampler-2d :isampler-3d :isampler-cube :isampler-2d-array
                      :usampler-2d :usampler-3d :usampler-cube :usampler-2d-array
                      :sampler-2d-shadow :sampler-cube-shadow :sampler-2d-array-shadow)))

(defun print-hlsl-textures-and-samplers (uniforms stream)
  "Print texture and sampler declarations."
  (let ((tex-index 0)
        (smp-index 0))
    (loop for u in uniforms
          for sb = (stage-binding u)
          for type = (value-type sb)
          for type-name = (name type)
          when (is-sampler-type-name type-name)
            do (let ((tex-type (case type-name
                                 ((:sampler-2d :isampler-2d :usampler-2d :sampler-2d-shadow) "Texture2D")
                                 ((:sampler-3d :isampler-3d :usampler-3d) "Texture3D")
                                 ((:sampler-cube :isampler-cube :usampler-cube) "TextureCube")
                                 ((:sampler-2d-array :isampler-2d-array :usampler-2d-array) "Texture2DArray")
                                 (t "Texture2D"))))
                 ;; Texture declaration
                 (format stream "~a ~a_texture : register(t~d);~%"
                         tex-type
                         (translate-name u)
                         tex-index)
                 ;; Sampler declaration
                 (format stream "SamplerState ~a_sampler : register(s~d);~%"
                         (translate-name u)
                         smp-index)
                 (incf tex-index)
                 (incf smp-index)))
    (when (> tex-index 0)
      (format stream "~%"))))

;;; Main HLSL Output Generation

(defmethod generate-output (objects inferred-types (backend (eql :hlsl))
                            &key version extensions &allow-other-keys)
  "Generate HLSL shader source from compiled shader objects."
  (declare (ignore version extensions))
  (let* ((stage *current-shader-stage*))
    (multiple-value-bind (inputs outputs uniforms)
        (collect-hlsl-bindings objects stage)
      (let ((*hlsl-context* (make-instance 'hlsl-shader-context
                                           :stage stage)))
        (setf (hlsl-inputs *hlsl-context*) inputs
              (hlsl-outputs *hlsl-context*) outputs
              (hlsl-uniforms *hlsl-context*) uniforms)

        (with-output-to-string (*standard-output*)
          ;; Constant buffer for uniforms
          (print-hlsl-cbuffer uniforms *standard-output*)

          ;; Texture and sampler declarations
          (print-hlsl-textures-and-samplers uniforms *standard-output*)

          ;; Input struct
          (case stage
            (:vertex
             (when inputs
               (print-hlsl-input-struct inputs stage *standard-output*)))
            (:fragment
             (let ((frag-inputs (loop for obj in objects
                                      when (typep obj 'interface-binding)
                                        when (let* ((sb (stage-binding obj))
                                                    (iq (interface-qualifier sb)))
                                               (eq (if (consp iq) (car iq) iq) :in))
                                          collect obj)))
               (when frag-inputs
                 (print-hlsl-fragment-input-struct frag-inputs *standard-output*)))))

          ;; Output struct
          (print-hlsl-output-struct outputs stage *standard-output*)

          ;; Generate function
          (loop for object in objects
                when (typep object 'global-function)
                  do (let ((overloads (gethash object inferred-types)))
                       (assert overloads)
                       (loop for overload in overloads
                             for *binding-types* = (gethash overload
                                                           (final-binding-type-cache object))
                             do (assert *binding-types*)
                                (print-hlsl-function object stage *standard-output*)))))))))

(defun print-hlsl-function (func stage stream)
  "Print an HLSL function definition."
  (let* ((name (translate-name func))
         (is-main (string= name "main"))
         (entry-name (if is-main "main" name)))
    (if is-main
        ;; Main function with HLSL entry point signature
        (progn
          (format stream "~a ~a(~a input) {~%"
                  (if (eq stage :vertex) "VSOutput" "PSOutput")
                  entry-name
                  (if (eq stage :vertex) "VSInput" "PSInput"))
          (format stream "    ~a output = (~a)0;~%"
                  (if (eq stage :vertex) "VSOutput" "PSOutput")
                  (if (eq stage :vertex) "VSOutput" "PSOutput"))
          ;; Print body with HLSL-specific translations
          (print-hlsl-function-body func stage stream)
          (format stream "    return output;~%")
          (format stream "}~%"))
        ;; Regular helper function
        (progn
          (format stream "~a ~a("
                  (translate-type-for-backend (value-type func) :hlsl)
                  entry-name)
          (format stream "~{~a~^, ~}" (bindings func))
          (format stream ") {~%")
          (print-hlsl-function-body func stage stream)
          (format stream "}~%")))))

(defun print-hlsl-function-body (func stage stream)
  "Print the body of an HLSL function with appropriate translations."
  (loop for form in (body func)
        do (print-hlsl-statement form stage stream 4)))

(defun is-hlsl-output-binding-p (binding)
  "Check if a binding is an output binding."
  (or (member binding (hlsl-outputs *hlsl-context*))
      (and (typep binding 'interface-binding)
           (let* ((sb (stage-binding binding))
                  (iq (interface-qualifier sb))
                  (iq-key (if (consp iq) (car iq) iq)))
             (eq iq-key :out)))
      (loop for out in (hlsl-outputs *hlsl-context*)
            thereis (eq (name binding) (name out)))))

(defun print-hlsl-statement (form stage stream indent)
  "Print a single statement in HLSL syntax."
  (let ((indent-str (make-string indent :initial-element #\Space)))
    (typecase form
      (variable-write
       (let* ((binding (binding form))
              (value (value form)))
         (cond
           ;; Handle slot-access binding
           ((typep binding 'slot-access)
            (let* ((b (binding binding))
                   (field (field binding)))
              (when (typep b 'variable-read)
                (let ((vr-binding (binding b)))
                  (when (typep vr-binding 'interface-binding)
                    (let* ((sb (stage-binding vr-binding))
                           (iq (interface-qualifier sb))
                           (iq-key (if (consp iq) (car iq) iq)))
                      (format stream "~aoutput.~a = ~a;~%"
                              indent-str
                              (translate-name field)
                              (print-hlsl-expression value stage))
                      (return-from print-hlsl-statement)))))
              (format stream "~a~a.~a = ~a;~%"
                      indent-str
                      (print-hlsl-expression b stage)
                      (translate-name field)
                      (print-hlsl-expression value stage))))
           ;; Handle gl_Position -> output.position
           ((let ((name (name binding)))
              (or (eq name 'gl-position)
                  (string-equal (string name) "GL-POSITION")))
            (format stream "~aoutput.position = ~a;~%"
                    indent-str
                    (print-hlsl-expression value stage)))
           ;; Handle any output binding -> output.name
           ((is-hlsl-output-binding-p binding)
            (format stream "~aoutput.~a = ~a;~%"
                    indent-str
                    (translate-name binding)
                    (print-hlsl-expression value stage)))
           (t
            (format stream "~a~a = ~a;~%"
                    indent-str
                    (translate-name binding)
                    (print-hlsl-expression value stage))))))
      (t
       (format stream "~a// TODO: ~a~%" indent-str (type-of form))))))

(defun print-hlsl-expression (expr stage)
  "Convert an expression to HLSL syntax string."
  (typecase expr
    (variable-read
     (let* ((binding (binding expr))
            (name (name binding)))
       (cond
         ((member binding (hlsl-inputs *hlsl-context*))
          (format nil "input.~a" (translate-name binding)))
         ((typep binding 'interface-binding)
          (let* ((sb (stage-binding binding))
                 (iq (interface-qualifier sb))
                 (iq-key (if (consp iq) (car iq) iq)))
            (case iq-key
              (:in (format nil "input.~a" (translate-name binding)))
              (:out (format nil "output.~a" (translate-name binding)))
              (t (translate-name binding)))))
         (t (translate-name binding)))))
    (slot-access
     (let* ((b (binding expr))
            (field (field expr)))
       (when (typep b 'variable-read)
         (let ((vr-binding (binding b)))
           (when (typep vr-binding 'interface-binding)
             (let* ((sb (stage-binding vr-binding))
                    (iq (interface-qualifier sb))
                    (iq-key (if (consp iq) (car iq) iq)))
               (return-from print-hlsl-expression
                 (case iq-key
                   (:out (format nil "output.~a" (translate-name field)))
                   (:in (format nil "input.~a" (translate-name field)))
                   (t (format nil "~a.~a"
                              (print-hlsl-expression b stage)
                              (translate-name field)))))))))
       (format nil "~a.~a"
               (print-hlsl-expression b stage)
               (translate-name field))))
    (function-call
     (let ((f (called-function expr))
           (args (arguments expr)))
       ;; Handle texture sampling - HLSL uses .Sample() method
       (if (and (typep f 'function-binding)
                (member (name f) '(texture texture-lod)))
           (format nil "~a_texture.Sample(~a_sampler, ~a)"
                   (translate-name (first args))
                   (translate-name (first args))
                   (print-hlsl-expression (second args) stage))
           (format nil "~a(~{~a~^, ~})"
                   (translate-name f)
                   (mapcar (lambda (a) (print-hlsl-expression a stage)) args)))))
    (number
     (format nil "~a" expr))
    (t
     (format nil "/* unknown: ~a */" (type-of expr)))))
