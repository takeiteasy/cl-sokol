(in-package #:3bgl-shaders)

;;; Metal Shading Language (MSL) backend for 3bgl-shader
;;; This file provides Metal output generation alongside the existing GLSL backend.

;; Backend selection
(defparameter *current-backend* :glsl
  "Current shader output backend. :glsl or :metal")

;; Metal type name translations
(defparameter *metal-type-names*
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
     :double "float"  ; Metal doesn't have double, use float
     :dvec2 "float2"
     :dvec3 "float3"
     :dvec4 "float4"
     :sampler-2d "texture2d<float>"
     :sampler-cube "texturecube<float>"
     :void "void")))

(defun metal-type-name (type-name)
  "Convert a GLSL type name to Metal type name."
  (or (gethash type-name *metal-type-names*)
      (string-downcase (symbol-name type-name))))

;; Metal-specific translate-type method
(defmethod translate-type-for-backend ((type concrete-type) (backend (eql :metal)))
  (metal-type-name (name type)))

(defmethod translate-type-for-backend ((type concrete-type) (backend (eql :glsl)))
  (glsl-name type))

(defmethod translate-type-for-backend (type backend)
  ;; Fallback to existing translate-type
  (translate-type type))

;; Metal struct generation for inputs/outputs
(defclass metal-shader-context ()
  ((stage :initarg :stage :accessor metal-stage)
   (inputs :initform nil :accessor metal-inputs)
   (outputs :initform nil :accessor metal-outputs)
   (uniforms :initform nil :accessor metal-uniforms)
   (input-struct-name :accessor metal-input-struct-name)
   (output-struct-name :accessor metal-output-struct-name))
  (:documentation "Context for Metal shader generation."))

(defvar *metal-context* nil)

(defun collect-metal-bindings (objects stage)
  "Collect input/output/uniform bindings for Metal struct generation."
  (let ((inputs nil)
        (outputs nil)
        (uniforms nil))
    (loop for obj in objects
          when (typep obj 'interface-binding)
            do (let* ((sb (stage-binding obj))
                      (iq (interface-qualifier sb))
                      (iq-key (if (consp iq) (car iq) iq)))
                 (case iq-key
                   (:in (when (eq stage :vertex)
                          (push obj inputs)))
                   (:out (push obj outputs))
                   (:uniform (push obj uniforms)))))
    (values (nreverse inputs)
            (nreverse outputs)
            (nreverse uniforms))))

(defun metal-attribute-for-input (binding index)
  "Generate Metal attribute annotation for an input binding."
  (let* ((sb (stage-binding binding))
         (lq (layout-qualifier sb))
         (location (or (getf lq :location) index)))
    (format nil "[[attribute(~d)]]" location)))

(defun metal-attribute-for-output (binding stage)
  "Generate Metal attribute annotation for an output binding."
  (let* ((name (name binding))
         (name-str (string-downcase (symbol-name name))))
    (cond
      ;; Special built-in outputs
      ((or (eq name 'gl-position)
           (string-equal name-str "gl-position")
           (string-equal name-str "position"))
       "[[position]]")
      ((or (string-equal name-str "gl-frag-depth")
           (string-equal name-str "frag-depth"))
       "[[depth(any)]]")
      ;; Fragment color outputs
      ((eq stage :fragment)
       "[[color(0)]]")
      ;; Varyings (vertex->fragment)
      (t
       (let ((index (or (position binding (metal-outputs *metal-context*)) 0)))
         (format nil "[[user(locn~d)]]" index))))))

(defun print-metal-input-struct (inputs stage stream)
  "Print the Metal input struct definition."
  (when (and inputs (eq stage :vertex))
    (format stream "struct VertexIn {~%")
    (loop for input in inputs
          for i from 0
          for sb = (stage-binding input)
          for type = (value-type sb)
          do (format stream "    ~a ~a ~a;~%"
                     (metal-type-name (name type))
                     (translate-name input)
                     (metal-attribute-for-input input i)))
    (format stream "};~%~%")))

(defun print-metal-output-struct (outputs stage stream)
  "Print the Metal output struct definition."
  (format stream "struct ~a {~%"
          (if (eq stage :vertex) "VertexOut" "FragmentOut"))
  ;; For vertex shader, always include position
  (when (eq stage :vertex)
    (format stream "    float4 position [[position]];~%"))
  ;; Add other outputs - expand interface blocks
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
                          do (format stream "    ~a ~a [[user(locn~d)]];~%"
                                     (metal-type-name (name (value-type b)))
                                     (translate-name b)
                                     locn-index)
                             (incf locn-index))))
                 ;; Simple output
                 (t
                  (let ((type (value-type sb)))
                    (format stream "    ~a ~a ~a;~%"
                            (metal-type-name (name type))
                            (translate-name output)
                            (metal-attribute-for-output output stage)))
                  (incf locn-index)))))
  (format stream "};~%~%"))

(defun print-metal-varying-input-struct (inputs stream)
  "Print the Metal input struct for fragment shader (from vertex outputs/varyings)."
  (format stream "struct FragmentIn {~%")
  (format stream "    float4 position [[position]];~%")
  (let ((locn-index 0))
    (loop for input in inputs
          for name = (name input)
          for sb = (stage-binding input)
          unless (or (eq name 'gl-position)
                     (string-equal (string name) "GL-POSITION"))
            do (cond
                 ;; Interface block with bindings - expand to individual fields
                 ((or (interface-block sb) (typep (binding sb) 'bindings))
                  (let ((bindings (bindings (or (interface-block sb) (binding sb)))))
                    (loop for b in bindings
                          do (format stream "    ~a ~a [[user(locn~d)]];~%"
                                     (metal-type-name (name (value-type b)))
                                     (translate-name b)
                                     locn-index)
                             (incf locn-index))))
                 ;; Simple input
                 (t
                  (let ((type (value-type sb)))
                    (format stream "    ~a ~a [[user(locn~d)]];~%"
                            (metal-type-name (name type))
                            (translate-name input)
                            locn-index))
                  (incf locn-index)))))
  (format stream "};~%~%"))

(defun generate-metal-preamble (stream)
  "Generate Metal shader preamble."
  (format stream "#include <metal_stdlib>~%")
  (format stream "#include <simd/simd.h>~%~%")
  (format stream "using namespace metal;~%~%"))

(defmethod generate-output (objects inferred-types (backend (eql :metal))
                            &key version extensions &allow-other-keys)
  "Generate Metal shader source from compiled shader objects."
  (declare (ignore version extensions))
  (let* ((*current-backend* :metal)
         (stage *current-shader-stage*))
    (multiple-value-bind (inputs outputs uniforms)
        (collect-metal-bindings objects stage)
      (let ((*metal-context* (make-instance 'metal-shader-context
                                            :stage stage)))
        (setf (metal-inputs *metal-context*) inputs
              (metal-outputs *metal-context*) outputs
              (metal-uniforms *metal-context*) uniforms)

        (with-output-to-string (*standard-output*)
          ;; Preamble
          (generate-metal-preamble *standard-output*)

          ;; Input struct (for vertex shader)
          (print-metal-input-struct inputs stage *standard-output*)

          ;; Output struct
          (print-metal-output-struct outputs stage *standard-output*)

          ;; For fragment shader, print input struct from vertex outputs
          (when (eq stage :fragment)
            ;; Fragment inputs come from vertex outputs, but we need to generate
            ;; a struct for them. For now, use the interface bindings marked as :in
            (let ((frag-inputs (loop for obj in objects
                                     when (typep obj 'interface-binding)
                                       when (let* ((sb (stage-binding obj))
                                                   (iq (interface-qualifier sb)))
                                              (eq (if (consp iq) (car iq) iq) :in))
                                         collect obj)))
              (when frag-inputs
                (print-metal-varying-input-struct frag-inputs *standard-output*))))

          ;; Generate function
          (loop for object in objects
                when (typep object 'global-function)
                  do (let ((overloads (gethash object inferred-types)))
                       (assert overloads)
                       (loop for overload in overloads
                             for *binding-types* = (gethash overload
                                                           (final-binding-type-cache object))
                             do (assert *binding-types*)
                                (print-metal-function object stage *standard-output*)))))))))

(defun print-metal-function (func stage stream)
  "Print a Metal function definition."
  (let* ((name (translate-name func))
         (is-main (string= name "main"))
         (entry-name (if is-main "main0" name))
         (stage-keyword (case stage
                          (:vertex "vertex")
                          (:fragment "fragment")
                          (:compute "kernel")
                          (t ""))))
    (if is-main
        ;; Main function with Metal entry point signature
        (progn
          (format stream "~a ~a ~a("
                  stage-keyword
                  (if (eq stage :vertex) "VertexOut" "FragmentOut")
                  entry-name)
          ;; Parameters
          (case stage
            (:vertex
             (format stream "VertexIn in [[stage_in]]"))
            (:fragment
             (format stream "FragmentIn in [[stage_in]]")))
          (format stream ") {~%")
          (format stream "    ~a out = {};~%"
                  (if (eq stage :vertex) "VertexOut" "FragmentOut"))
          ;; Print body with Metal-specific translations
          (print-metal-function-body func stage stream)
          (format stream "    return out;~%")
          (format stream "}~%"))
        ;; Regular helper function
        (progn
          (format stream "~a ~a("
                  (translate-type-for-backend (value-type func) :metal)
                  entry-name)
          (format stream "~{~a~^, ~}" (bindings func))
          (format stream ") {~%")
          ;; Print body
          (print-metal-function-body func stage stream)
          (format stream "}~%")))))

(defun print-metal-function-body (func stage stream)
  "Print the body of a Metal function with appropriate translations."
  ;; For now, a simplified version that handles basic cases
  ;; A full implementation would need to walk the AST and translate each node
  (loop for form in (body func)
        do (print-metal-statement form stage stream 4)))

(defun is-output-binding-p (binding)
  "Check if a binding is an output binding."
  (or (member binding (metal-outputs *metal-context*))
      (and (typep binding 'interface-binding)
           (let* ((sb (stage-binding binding))
                  (iq (interface-qualifier sb))
                  (iq-key (if (consp iq) (car iq) iq)))
             (eq iq-key :out)))
      ;; Check by name if it matches any output
      (loop for out in (metal-outputs *metal-context*)
            thereis (eq (name binding) (name out)))))

(defun print-metal-statement (form stage stream indent)
  "Print a single statement in Metal syntax."
  (let ((indent-str (make-string indent :initial-element #\Space)))
    (typecase form
      (variable-write
       (let* ((binding (binding form))
              (value (value form)))
         (cond
           ;; Handle slot-access binding (e.g., (@ outs color) = ...)
           ((typep binding 'slot-access)
            (let* ((b (binding binding))
                   (field (field binding)))
              (when (typep b 'variable-read)
                (let ((vr-binding (binding b)))
                  (when (typep vr-binding 'interface-binding)
                    (let* ((sb (stage-binding vr-binding))
                           (iq (interface-qualifier sb))
                           (iq-key (if (consp iq) (car iq) iq)))
                      (format stream "~a~a.~a = ~a;~%"
                              indent-str
                              (case iq-key
                                (:out "out")
                                (:in "in")
                                (t (translate-name vr-binding)))
                              (translate-name field)
                              (print-metal-expression value stage))
                      (return-from print-metal-statement)))))
              ;; Default slot access assignment
              (format stream "~a~a.~a = ~a;~%"
                      indent-str
                      (print-metal-expression b stage)
                      (translate-name field)
                      (print-metal-expression value stage))))
           ;; Handle gl_Position -> out.position
           ((let ((name (name binding)))
              (or (eq name 'gl-position)
                  (string-equal (string name) "GL-POSITION")))
            (format stream "~aout.position = ~a;~%"
                    indent-str
                    (print-metal-expression value stage)))
           ;; Handle any output binding -> out.name
           ((is-output-binding-p binding)
            (format stream "~aout.~a = ~a;~%"
                    indent-str
                    (translate-name binding)
                    (print-metal-expression value stage)))
           (t
            (format stream "~a~a = ~a;~%"
                    indent-str
                    (translate-name binding)
                    (print-metal-expression value stage))))))
      (t
       ;; Default: just print the form (this is incomplete)
       (format stream "~a// TODO: ~a~%" indent-str (type-of form))))))

(defun print-metal-expression (expr stage)
  "Convert an expression to Metal syntax string."
  (typecase expr
    (variable-read
     (let* ((binding (binding expr))
            (name (name binding)))
       ;; Handle input variables -> in.name
       (cond
         ((member binding (metal-inputs *metal-context*))
          (format nil "in.~a" (translate-name binding)))
         ;; Handle interface block access
         ((typep binding 'interface-binding)
          (let* ((sb (stage-binding binding))
                 (iq (interface-qualifier sb))
                 (iq-key (if (consp iq) (car iq) iq)))
            (case iq-key
              (:in (format nil "in.~a" (translate-name binding)))
              (:out (format nil "out.~a" (translate-name binding)))
              (t (translate-name binding)))))
         (t
          (translate-name binding)))))
    (slot-access
     ;; Handle interface.field access (e.g., outs.color)
     (let* ((b (binding expr))
            (field (field expr)))
       ;; Check if this is an interface block access
       (when (typep b 'variable-read)
         (let ((vr-binding (binding b)))
           (when (typep vr-binding 'interface-binding)
             (let* ((sb (stage-binding vr-binding))
                    (iq (interface-qualifier sb))
                    (iq-key (if (consp iq) (car iq) iq)))
               (return-from print-metal-expression
                 (case iq-key
                   (:out (format nil "out.~a" (translate-name field)))
                   (:in (format nil "in.~a" (translate-name field)))
                   (t (format nil "~a.~a"
                              (print-metal-expression b stage)
                              (translate-name field)))))))))
       ;; Default slot access
       (format nil "~a.~a"
               (print-metal-expression b stage)
               (translate-name field))))
    (function-call
     (let ((f (called-function expr))
           (args (arguments expr)))
       (format nil "~a(~{~a~^, ~})"
               (translate-name f)
               (mapcar (lambda (a) (print-metal-expression a stage)) args))))
    (number
     (format nil "~a" expr))
    (t
     (format nil "/* unknown: ~a */" (type-of expr)))))
