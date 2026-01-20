(in-package #:cl-sokol/shaders)

;;; User-facing API for defining sokol shaders

(defvar *defined-shaders* (make-hash-table :test 'equal)
  "Registry of defined shaders, mapping name (string) to (vertex-entry . fragment-entry).")

(defmacro define-sokol-shader (name &body stages)
  "Define a sokol shader from clsl entry points.

Usage:
  (define-sokol-shader my-shader
    (:vertex my-package::vertex-main)
    (:fragment my-package::fragment-main))

This creates:
  - A function (my-shader-desc backend) that returns a sg_shader_desc pointer
  - The shader is compiled lazily on first use for each backend

STAGES should be a list of (:vertex entry-point) and (:fragment entry-point) forms,
where entry-point is a symbol naming a function defined using clsl-glsl:defun."
  (let ((vertex-entry (second (assoc :vertex stages)))
        (fragment-entry (second (assoc :fragment stages)))
        (compute-entry (second (assoc :compute stages))))
    (when (and compute-entry (or vertex-entry fragment-entry))
      (error "Cannot mix compute shader with vertex/fragment shaders"))
    (unless (or compute-entry (and vertex-entry fragment-entry))
      (error "Must specify both :vertex and :fragment stages (or just :compute)"))
    (when compute-entry
      (error "Compute shaders not yet supported"))

    (let ((desc-fn-name (intern (format nil "~A-SHADER-DESC" name)))
          (shader-name-str (string name)))
      `(progn
         ;; Register the shader using string key for cross-package compatibility
         (setf (gethash ,shader-name-str *defined-shaders*)
               (cons ',vertex-entry ',fragment-entry))

         ;; Define the descriptor function
         (defun ,desc-fn-name (backend)
           ,(format nil "Get shader descriptor for ~A shader.~%~
                        Returns a pointer to sg_shader_desc configured for BACKEND.~%~
                        The descriptor is cached after first creation." name)
           (build-shader-desc-for-backend ,shader-name-str ',vertex-entry ',fragment-entry backend))

         ;; Return the shader name
         ',name))))

(defun normalize-shader-name (name)
  "Convert a shader name to canonical string form."
  (etypecase name
    (string name)
    (symbol (string name))))

(defun shader-desc (name backend)
  "Get a shader descriptor by name and backend.
This is an alternative to calling the generated NAME-shader-desc function directly."
  (let* ((name-str (normalize-shader-name name))
         (entry (gethash name-str *defined-shaders*)))
    (unless entry
      (error "Shader ~A not defined. Use DEFINE-SOKOL-SHADER first." name))
    (build-shader-desc-for-backend name-str (car entry) (cdr entry) backend)))

(defun list-defined-shaders ()
  "Return a list of all defined shader names (as strings)."
  (loop for name being the hash-keys of *defined-shaders*
        collect name))

(defun shader-info (name)
  "Return information about a defined shader."
  (let* ((name-str (normalize-shader-name name))
         (entry (gethash name-str *defined-shaders*)))
    (if entry
        (list :name name-str
              :vertex-entry (car entry)
              :fragment-entry (cdr entry))
        (error "Shader ~A not defined." name))))

;; Export the clsl-glsl/cl package name for users
(defun shader-package ()
  "Return the package to USE for writing shaders.
Users should (use-package :clsl-glsl/cl) in their shader package."
  (find-package :clsl-glsl/cl))

;;; Convenience functions

(defun detect-backend ()
  "Detect appropriate shader backend for current platform.
Returns a sokol backend keyword."
  #+darwin :metal-macos
  #+windows :d3d11
  #-(or darwin windows) :glcore33)

(defun make-shader (name &optional (backend (detect-backend)))
  "Create a sokol shader from a defined Lisp shader.
Returns an sg_shader handle ready for use in pipelines.

NAME should be the name used with DEFINE-SOKOL-SHADER.
BACKEND defaults to the detected platform backend."
  (let ((desc (shader-desc name backend)))
    (sg:sg-make-shader desc)))
