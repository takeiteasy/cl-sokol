(in-package #:3bgl-shaders)

;;; WGSL (WebGPU Shading Language) backend for 3bgl-shader
;;; This file provides WGSL output generation for WebGPU support.

;; WGSL type name translations
(defparameter *wgsl-type-names*
  (alexandria:plist-hash-table
   '(:float "f32"
     :vec2 "vec2<f32>"
     :vec3 "vec3<f32>"
     :vec4 "vec4<f32>"
     :int "i32"
     :ivec2 "vec2<i32>"
     :ivec3 "vec3<i32>"
     :ivec4 "vec4<i32>"
     :uint "u32"
     :uvec2 "vec2<u32>"
     :uvec3 "vec3<u32>"
     :uvec4 "vec4<u32>"
     :bool "bool"
     :bvec2 "vec2<bool>"
     :bvec3 "vec3<bool>"
     :bvec4 "vec4<bool>"
     :mat2 "mat2x2<f32>"
     :mat3 "mat3x3<f32>"
     :mat4 "mat4x4<f32>"
     :mat2x2 "mat2x2<f32>"
     :mat2x3 "mat2x3<f32>"
     :mat2x4 "mat2x4<f32>"
     :mat3x2 "mat3x2<f32>"
     :mat3x3 "mat3x3<f32>"
     :mat3x4 "mat3x4<f32>"
     :mat4x2 "mat4x2<f32>"
     :mat4x3 "mat4x3<f32>"
     :mat4x4 "mat4x4<f32>"
     :sampler-2d "texture_2d<f32>"
     :sampler-3d "texture_3d<f32>"
     :sampler-cube "texture_cube<f32>"
     :sampler-2d-array "texture_2d_array<f32>"
     :isampler-2d "texture_2d<i32>"
     :usampler-2d "texture_2d<u32>"
     :sampler-2d-shadow "texture_depth_2d"
     :void "void")))

(defun wgsl-type-name (type-name)
  "Convert a GLSL type name to WGSL type name."
  (or (gethash type-name *wgsl-type-names*)
      (string-downcase (symbol-name type-name))))

;; WGSL-specific translate-type method
(defmethod translate-type-for-backend ((type concrete-type) (backend (eql :wgsl)))
  (wgsl-type-name (name type)))

;;; WGSL Context for code generation

(defclass wgsl-shader-context ()
  ((stage :initarg :stage :accessor wgsl-stage)
   (inputs :initform nil :accessor wgsl-inputs)
   (outputs :initform nil :accessor wgsl-outputs)
   (uniforms :initform nil :accessor wgsl-uniforms)
   (binding-index :initform 0 :accessor wgsl-binding-index))
  (:documentation "Context for WGSL shader generation."))

(defvar *wgsl-context* nil)

(defun collect-wgsl-bindings (objects stage)
  "Collect input/output/uniform bindings for WGSL struct generation."
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

;;; WGSL Input/Output Struct Generation

(defun print-wgsl-vertex-input-struct (inputs stream)
  "Print the WGSL input struct for vertex shader."
  (format stream "struct VertexInput {~%")
  (let ((index 0))
    (loop for input in inputs
          for sb = (stage-binding input)
          for type = (value-type sb)
          for lq = (layout-qualifier sb)
          for location = (or (getf lq :location) index)
          do (format stream "    @location(~d) ~a: ~a,~%"
                     location
                     (translate-name input)
                     (wgsl-type-name (name type)))
             (incf index)))
  (format stream "};~%~%"))

(defun print-wgsl-vertex-output-struct (outputs stream)
  "Print the WGSL output struct for vertex shader."
  (format stream "struct VertexOutput {~%")
  ;; Position is always @builtin(position)
  (format stream "    @builtin(position) position: vec4<f32>,~%")
  ;; Other outputs use @location
  (let ((locn-index 0))
    (loop for output in outputs
          for name = (name output)
          for sb = (stage-binding output)
          unless (or (eq name 'gl-position)
                     (string-equal (string name) "GL-POSITION"))
            do (cond
                 ;; Interface block - expand fields
                 ((or (interface-block sb) (typep (binding sb) 'bindings))
                  (let ((bindings (bindings (or (interface-block sb) (binding sb)))))
                    (loop for b in bindings
                          do (format stream "    @location(~d) ~a: ~a,~%"
                                     locn-index
                                     (translate-name b)
                                     (wgsl-type-name (name (value-type b))))
                             (incf locn-index))))
                 ;; Simple output
                 (t
                  (format stream "    @location(~d) ~a: ~a,~%"
                          locn-index
                          (translate-name output)
                          (wgsl-type-name (name (value-type sb))))
                  (incf locn-index)))))
  (format stream "};~%~%"))

(defun print-wgsl-fragment-input-struct (inputs stream)
  "Print the WGSL input struct for fragment shader."
  (format stream "struct FragmentInput {~%")
  ;; Position from vertex shader
  (format stream "    @builtin(position) position: vec4<f32>,~%")
  (let ((locn-index 0))
    (loop for input in inputs
          for name = (name input)
          for sb = (stage-binding input)
          unless (or (eq name 'gl-position)
                     (string-equal (string name) "GL-POSITION"))
            do (cond
                 ;; Interface block - expand fields
                 ((or (interface-block sb) (typep (binding sb) 'bindings))
                  (let ((bindings (bindings (or (interface-block sb) (binding sb)))))
                    (loop for b in bindings
                          do (format stream "    @location(~d) ~a: ~a,~%"
                                     locn-index
                                     (translate-name b)
                                     (wgsl-type-name (name (value-type b))))
                             (incf locn-index))))
                 ;; Simple input
                 (t
                  (format stream "    @location(~d) ~a: ~a,~%"
                          locn-index
                          (translate-name input)
                          (wgsl-type-name (name (value-type sb))))
                  (incf locn-index)))))
  (format stream "};~%~%"))

(defun print-wgsl-fragment-output-struct (outputs stream)
  "Print the WGSL output struct for fragment shader."
  (format stream "struct FragmentOutput {~%")
  (let ((color-index 0))
    (loop for output in outputs
          for sb = (stage-binding output)
          for type = (value-type sb)
          do (format stream "    @location(~d) ~a: ~a,~%"
                     color-index
                     (translate-name output)
                     (wgsl-type-name (name type)))
             (incf color-index)))
  (format stream "};~%~%"))

;;; WGSL Uniform and Resource Generation

(defun is-wgsl-sampler-type-name (type-name)
  "Return T if TYPE-NAME is a sampler type."
  (member type-name '(:sampler-2d :sampler-3d :sampler-cube :sampler-2d-array
                      :isampler-2d :isampler-3d :isampler-cube :isampler-2d-array
                      :usampler-2d :usampler-3d :usampler-cube :usampler-2d-array
                      :sampler-2d-shadow :sampler-cube-shadow :sampler-2d-array-shadow)))

(defun print-wgsl-uniforms (uniforms stream)
  "Print uniform buffer and resource bindings."
  ;; Group 0: Uniform buffers
  (let ((scalar-uniforms (loop for u in uniforms
                               for sb = (stage-binding u)
                               for type = (value-type sb)
                               unless (is-wgsl-sampler-type-name (name type))
                                 collect u)))
    (when scalar-uniforms
      (format stream "struct Uniforms {~%")
      (loop for u in scalar-uniforms
            for sb = (stage-binding u)
            for type = (value-type sb)
            do (format stream "    ~a: ~a,~%"
                       (translate-name u)
                       (wgsl-type-name (name type))))
      (format stream "};~%~%")
      (format stream "@group(0) @binding(0) var<uniform> uniforms: Uniforms;~%~%")))

  ;; Group 1: Textures and samplers
  (let ((tex-binding 0)
        (smp-binding 16))  ; Samplers start at binding 16 in group 1
    (loop for u in uniforms
          for sb = (stage-binding u)
          for type = (value-type sb)
          for type-name = (name type)
          when (is-wgsl-sampler-type-name type-name)
            do (let ((tex-type (case type-name
                                 ((:sampler-2d :isampler-2d :usampler-2d) "texture_2d<f32>")
                                 ((:sampler-3d :isampler-3d :usampler-3d) "texture_3d<f32>")
                                 ((:sampler-cube :isampler-cube :usampler-cube) "texture_cube<f32>")
                                 ((:sampler-2d-array :isampler-2d-array :usampler-2d-array) "texture_2d_array<f32>")
                                 (:sampler-2d-shadow "texture_depth_2d")
                                 (t "texture_2d<f32>"))))
                 ;; Texture
                 (format stream "@group(1) @binding(~d) var ~a_texture: ~a;~%"
                         tex-binding
                         (translate-name u)
                         tex-type)
                 ;; Sampler
                 (format stream "@group(1) @binding(~d) var ~a_sampler: sampler;~%"
                         smp-binding
                         (translate-name u))
                 (incf tex-binding)
                 (incf smp-binding)))
    (when (> tex-binding 0)
      (format stream "~%"))))

;;; Main WGSL Output Generation

(defmethod generate-output (objects inferred-types (backend (eql :wgsl))
                            &key version extensions &allow-other-keys)
  "Generate WGSL shader source from compiled shader objects."
  (declare (ignore version extensions))
  (let* ((stage *current-shader-stage*))
    (multiple-value-bind (inputs outputs uniforms)
        (collect-wgsl-bindings objects stage)
      (let ((*wgsl-context* (make-instance 'wgsl-shader-context
                                           :stage stage)))
        (setf (wgsl-inputs *wgsl-context*) inputs
              (wgsl-outputs *wgsl-context*) outputs
              (wgsl-uniforms *wgsl-context*) uniforms)

        (with-output-to-string (*standard-output*)
          ;; Uniform buffer and resources
          (print-wgsl-uniforms uniforms *standard-output*)

          ;; Input/output structs based on stage
          (case stage
            (:vertex
             (when inputs
               (print-wgsl-vertex-input-struct inputs *standard-output*))
             (print-wgsl-vertex-output-struct outputs *standard-output*))
            (:fragment
             (let ((frag-inputs (loop for obj in objects
                                      when (typep obj 'interface-binding)
                                        when (let* ((sb (stage-binding obj))
                                                    (iq (interface-qualifier sb)))
                                               (eq (if (consp iq) (car iq) iq) :in))
                                          collect obj)))
               (when frag-inputs
                 (print-wgsl-fragment-input-struct frag-inputs *standard-output*)))
             (when outputs
               (print-wgsl-fragment-output-struct outputs *standard-output*))))

          ;; Generate function
          (loop for object in objects
                when (typep object 'global-function)
                  do (let ((overloads (gethash object inferred-types)))
                       (assert overloads)
                       (loop for overload in overloads
                             for *binding-types* = (gethash overload
                                                           (final-binding-type-cache object))
                             do (assert *binding-types*)
                                (print-wgsl-function object stage *standard-output*)))))))))

(defun print-wgsl-function (func stage stream)
  "Print a WGSL function definition."
  (let* ((name (translate-name func))
         (is-main (string= name "main"))
         (entry-name (if is-main "main" name)))
    (if is-main
        ;; Main function with WGSL entry point decorators
        (progn
          (format stream "@~a~%"
                  (case stage
                    (:vertex "vertex")
                    (:fragment "fragment")
                    (:compute "compute")
                    (t "vertex")))
          (format stream "fn ~a(input: ~a) -> ~a {~%"
                  entry-name
                  (case stage
                    (:vertex "VertexInput")
                    (:fragment "FragmentInput")
                    (t "VertexInput"))
                  (case stage
                    (:vertex "VertexOutput")
                    (:fragment "FragmentOutput")
                    (t "VertexOutput")))
          (format stream "    var output: ~a;~%"
                  (case stage
                    (:vertex "VertexOutput")
                    (:fragment "FragmentOutput")
                    (t "VertexOutput")))
          ;; Print body
          (print-wgsl-function-body func stage stream)
          (format stream "    return output;~%")
          (format stream "}~%"))
        ;; Regular helper function
        (progn
          (format stream "fn ~a("
                  entry-name)
          (format stream "~{~a~^, ~}"
                  (loop for b in (bindings func)
                        collect (format nil "~a: ~a"
                                        (translate-name b)
                                        (wgsl-type-name (name (value-type b))))))
          (format stream ") -> ~a {~%"
                  (wgsl-type-name (name (value-type func))))
          (print-wgsl-function-body func stage stream)
          (format stream "}~%")))))

(defun print-wgsl-function-body (func stage stream)
  "Print the body of a WGSL function."
  (loop for form in (body func)
        do (print-wgsl-statement form stage stream 4)))

(defun is-wgsl-output-binding-p (binding)
  "Check if a binding is an output binding."
  (or (member binding (wgsl-outputs *wgsl-context*))
      (and (typep binding 'interface-binding)
           (let* ((sb (stage-binding binding))
                  (iq (interface-qualifier sb))
                  (iq-key (if (consp iq) (car iq) iq)))
             (eq iq-key :out)))
      (loop for out in (wgsl-outputs *wgsl-context*)
            thereis (eq (name binding) (name out)))))

(defun print-wgsl-statement (form stage stream indent)
  "Print a single statement in WGSL syntax."
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
                              (print-wgsl-expression value stage))
                      (return-from print-wgsl-statement)))))
              (format stream "~a~a.~a = ~a;~%"
                      indent-str
                      (print-wgsl-expression b stage)
                      (translate-name field)
                      (print-wgsl-expression value stage))))
           ;; Handle gl_Position -> output.position
           ((let ((name (name binding)))
              (or (eq name 'gl-position)
                  (string-equal (string name) "GL-POSITION")))
            (format stream "~aoutput.position = ~a;~%"
                    indent-str
                    (print-wgsl-expression value stage)))
           ;; Handle any output binding -> output.name
           ((is-wgsl-output-binding-p binding)
            (format stream "~aoutput.~a = ~a;~%"
                    indent-str
                    (translate-name binding)
                    (print-wgsl-expression value stage)))
           (t
            (format stream "~a~a = ~a;~%"
                    indent-str
                    (translate-name binding)
                    (print-wgsl-expression value stage))))))
      (t
       (format stream "~a// TODO: ~a~%" indent-str (type-of form))))))

(defun print-wgsl-expression (expr stage)
  "Convert an expression to WGSL syntax string."
  (typecase expr
    (variable-read
     (let* ((binding (binding expr))
            (name (name binding)))
       (cond
         ((member binding (wgsl-inputs *wgsl-context*))
          (format nil "input.~a" (translate-name binding)))
         ((typep binding 'interface-binding)
          (let* ((sb (stage-binding binding))
                 (iq (interface-qualifier sb))
                 (iq-key (if (consp iq) (car iq) iq)))
            (case iq-key
              (:in (format nil "input.~a" (translate-name binding)))
              (:out (format nil "output.~a" (translate-name binding)))
              (:uniform (format nil "uniforms.~a" (translate-name binding)))
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
               (return-from print-wgsl-expression
                 (case iq-key
                   (:out (format nil "output.~a" (translate-name field)))
                   (:in (format nil "input.~a" (translate-name field)))
                   (:uniform (format nil "uniforms.~a" (translate-name field)))
                   (t (format nil "~a.~a"
                              (print-wgsl-expression b stage)
                              (translate-name field)))))))))
       (format nil "~a.~a"
               (print-wgsl-expression b stage)
               (translate-name field))))
    (function-call
     (let ((f (called-function expr))
           (args (arguments expr)))
       ;; Handle texture sampling - WGSL uses textureSample()
       (if (and (typep f 'function-binding)
                (member (name f) '(texture texture-lod)))
           (format nil "textureSample(~a_texture, ~a_sampler, ~a)"
                   (translate-name (first args))
                   (translate-name (first args))
                   (print-wgsl-expression (second args) stage))
           ;; Handle some GLSL->WGSL function name translations
           (let ((func-name (case (name f)
                              (mix "mix")
                              (clamp "clamp")
                              (normalize "normalize")
                              (dot "dot")
                              (cross "cross")
                              (length "length")
                              (abs "abs")
                              (min "min")
                              (max "max")
                              (pow "pow")
                              (sqrt "sqrt")
                              (sin "sin")
                              (cos "cos")
                              (tan "tan")
                              (t (translate-name f)))))
             (format nil "~a(~{~a~^, ~})"
                     func-name
                     (mapcar (lambda (a) (print-wgsl-expression a stage)) args))))))
    (number
     ;; WGSL requires type suffixes for literals
     (if (floatp expr)
         (format nil "~f" expr)
         (format nil "~d" expr)))
    (t
     (format nil "/* unknown: ~a */" (type-of expr)))))
