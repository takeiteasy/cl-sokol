(in-package #:cl-sokol-shaders)

;;; Extract shader reflection data from clsl's generate-stage output

(defclass shader-reflection ()
  ((name :initarg :name :accessor shader-name)
   (vertex-source :initarg :vertex-source :accessor vertex-source :initform nil)
   (fragment-source :initarg :fragment-source :accessor fragment-source :initform nil)
   (attributes :initarg :attributes :accessor shader-attributes :initform nil)
   (vertex-uniforms :initarg :vertex-uniforms :accessor vertex-uniforms :initform nil)
   (fragment-uniforms :initarg :fragment-uniforms :accessor fragment-uniforms :initform nil)
   (vertex-buffers :initarg :vertex-buffers :accessor vertex-buffers :initform nil)
   (fragment-buffers :initarg :fragment-buffers :accessor fragment-buffers :initform nil)
   ;; Uniform blocks (UBOs) - combined from vertex and fragment stages
   (uniform-blocks :initarg :uniform-blocks :accessor shader-uniform-blocks :initform nil)
   ;; Textures (sampled images)
   (textures :initarg :textures :accessor shader-textures :initform nil)
   ;; Samplers
   (samplers :initarg :samplers :accessor shader-samplers :initform nil)
   ;; Texture-sampler pairs for combined sampler backends (GLSL)
   (texture-sampler-pairs :initarg :texture-sampler-pairs :accessor texture-sampler-pairs :initform nil))
  (:documentation "Holds reflection data for a complete shader program."))

(defclass shader-attribute ()
  ((name :initarg :name :accessor attr-name)
   (glsl-name :initarg :glsl-name :accessor attr-glsl-name)
   (type-name :initarg :type-name :accessor attr-type-name)
   (location :initarg :location :accessor attr-location :initform 0))
  (:documentation "A vertex shader input attribute."))

(defclass shader-uniform ()
  ((name :initarg :name :accessor uniform-name)
   (glsl-name :initarg :glsl-name :accessor uniform-glsl-name)
   (type-name :initarg :type-name :accessor uniform-type-name)
   (components :initarg :components :accessor uniform-components :initform nil))
  (:documentation "A shader uniform variable."))

;;; Uniform Block Support

(defclass uniform-block ()
  ((name :initarg :name :accessor block-name)
   (glsl-name :initarg :glsl-name :accessor block-glsl-name)
   (stage :initarg :stage :accessor block-stage)  ; :vertex, :fragment, or :both
   (size :initarg :size :accessor block-size)     ; Total size in bytes
   (binding :initarg :binding :accessor block-binding :initform nil) ; layout(binding=N)
   (members :initarg :members :accessor block-members :initform nil))
  (:documentation "A uniform block (UBO) definition."))

(defclass uniform-member ()
  ((name :initarg :name :accessor member-name)
   (type-name :initarg :type-name :accessor member-type-name)
   (offset :initarg :offset :accessor member-offset :initform 0)
   (array-count :initarg :array-count :accessor member-array-count :initform 0))
  (:documentation "A member of a uniform block."))

;;; Texture and Sampler Support

(defclass shader-texture ()
  ((name :initarg :name :accessor texture-name)
   (glsl-name :initarg :glsl-name :accessor texture-glsl-name)
   (stage :initarg :stage :accessor texture-stage)  ; :vertex, :fragment, or :both
   (image-type :initarg :image-type :accessor texture-image-type :initform :2d) ; :2d, :3d, :cube, :array
   (sample-type :initarg :sample-type :accessor texture-sample-type :initform :float) ; :float, :depth, :sint, :uint
   (multisampled :initarg :multisampled :accessor texture-multisampled :initform nil)
   (binding :initarg :binding :accessor texture-binding :initform nil))
  (:documentation "A shader texture (sampled image)."))

(defclass shader-sampler ()
  ((name :initarg :name :accessor sampler-name)
   (glsl-name :initarg :glsl-name :accessor sampler-glsl-name)
   (stage :initarg :stage :accessor sampler-stage)  ; :vertex, :fragment, or :both
   (sampler-type :initarg :sampler-type :accessor sampler-filter-type :initform :filtering) ; :filtering, :comparison, :nonfiltering
   (binding :initarg :binding :accessor sampler-binding :initform nil))
  (:documentation "A shader sampler."))

(defun parse-attribute (attr-list)
  "Parse an attribute list from generate-stage into a shader-attribute object.
ATTR-LIST is (LISP-NAME \"glslName\" type-name)"
  (destructuring-bind (lisp-name glsl-name type-name) attr-list
    (make-instance 'shader-attribute
                   :name lisp-name
                   :glsl-name glsl-name
                   :type-name type-name
                   ;; Location is typically derived from the order or explicit :location
                   :location 0)))

(defun parse-uniform (uniform-list)
  "Parse a uniform list from generate-stage into a shader-uniform object.
UNIFORM-LIST is (LISP-NAME \"glslName\" type-name . properties)"
  (destructuring-bind (lisp-name glsl-name type-name &rest properties) uniform-list
    (make-instance 'shader-uniform
                   :name lisp-name
                   :glsl-name glsl-name
                   :type-name type-name
                   :components (getf properties :components))))

;;; Uniform Block Parsing

(defun parse-uniform-block (buffer-list stage)
  "Parse a buffer list from generate-stage into a uniform-block object.
BUFFER-LIST is (LISP-NAME \"glslName\" :layout (:binding N ...) :components ((name type) ...))
STAGE is :vertex or :fragment."
  (destructuring-bind (lisp-name glsl-name &rest properties) buffer-list
    (let* ((layout (getf properties :layout))
           (binding (getf layout :binding))
           (components (getf properties :components))
           (members (loop for (member-name member-type) in components
                          for offset = 0 then (+ offset (type-size-std140 prev-type))
                          for prev-type = member-type
                          collect (make-instance 'uniform-member
                                                 :name member-name
                                                 :type-name member-type
                                                 :offset offset)))
           (total-size (if components
                           (loop for (nil member-type) in components
                                 sum (type-size-std140 member-type))
                           0)))
      (make-instance 'uniform-block
                     :name lisp-name
                     :glsl-name glsl-name
                     :stage stage
                     :binding binding
                     :size total-size
                     :members members))))

(defun merge-uniform-blocks (vs-blocks fs-blocks)
  "Merge vertex and fragment uniform blocks, marking shared blocks as :both stage.
Blocks with the same name are assumed to be the same block used in both stages."
  (let ((result nil)
        (vs-names (mapcar #'block-name vs-blocks)))
    ;; Add all vertex blocks
    (dolist (block vs-blocks)
      (push block result))
    ;; Add fragment blocks, or update stage to :both if already present
    (dolist (block fs-blocks)
      (let ((existing (find (block-name block) result :key #'block-name)))
        (if existing
            ;; Block exists in both stages
            (setf (block-stage existing) :both)
            ;; New block only in fragment stage
            (push block result))))
    (nreverse result)))

;;; Texture/Sampler Parsing

(defun sampler-type-to-image-type (type-name)
  "Convert a sampler type name to image type keyword."
  (case type-name
    ((:sampler-2d :isampler-2d :usampler-2d :sampler-2d-shadow) :2d)
    ((:sampler-3d :isampler-3d :usampler-3d) :3d)
    ((:sampler-cube :isampler-cube :usampler-cube :sampler-cube-shadow) :cube)
    ((:sampler-2d-array :isampler-2d-array :usampler-2d-array :sampler-2d-array-shadow) :array)
    (otherwise :2d)))

(defun sampler-type-to-sample-type (type-name)
  "Convert a sampler type name to sample type keyword."
  (case type-name
    ((:sampler-2d-shadow :sampler-cube-shadow :sampler-2d-array-shadow) :depth)
    ((:isampler-2d :isampler-3d :isampler-cube :isampler-2d-array) :sint)
    ((:usampler-2d :usampler-3d :usampler-cube :usampler-2d-array) :uint)
    (otherwise :float)))

(defun is-sampler-type-p (type-name)
  "Return T if TYPE-NAME is a sampler type."
  (member type-name '(:sampler-2d :sampler-3d :sampler-cube :sampler-2d-array
                      :isampler-2d :isampler-3d :isampler-cube :isampler-2d-array
                      :usampler-2d :usampler-3d :usampler-cube :usampler-2d-array
                      :sampler-2d-shadow :sampler-cube-shadow :sampler-2d-array-shadow)))

(defun extract-textures-from-uniforms (uniforms stage)
  "Extract texture declarations from uniform list.
In GLSL, textures are combined with samplers (sampler2D etc)."
  (loop for uniform in uniforms
        for type-name = (uniform-type-name uniform)
        when (is-sampler-type-p type-name)
          collect (make-instance 'shader-texture
                                 :name (uniform-name uniform)
                                 :glsl-name (uniform-glsl-name uniform)
                                 :stage stage
                                 :image-type (sampler-type-to-image-type type-name)
                                 :sample-type (sampler-type-to-sample-type type-name))))

(defun merge-textures (vs-textures fs-textures)
  "Merge vertex and fragment textures, marking shared ones as :both stage."
  (let ((result nil))
    ;; Add all vertex textures
    (dolist (tex vs-textures)
      (push tex result))
    ;; Add fragment textures, or update stage to :both if already present
    (dolist (tex fs-textures)
      (let ((existing (find (texture-name tex) result :key #'texture-name)))
        (if existing
            (setf (texture-stage existing) :both)
            (push tex result))))
    (nreverse result)))

(defun generate-shader-source (stage entry-point &key (backend :glsl) (version 450))
  "Generate shader source for a single stage.
Returns (values source uniforms attributes buffers structs)."
  (clsl:generate-stage stage entry-point
                               :backend backend
                               :version version
                               :expand-uniforms t))

(defun extract-reflection (name vertex-entry fragment-entry &key (backend :glsl) (version 450))
  "Extract complete reflection data for a shader program.
Returns a shader-reflection object with all metadata."
  ;; Generate vertex shader
  (multiple-value-bind (vs-source vs-uniforms vs-attrs vs-buffers vs-structs)
      (generate-shader-source :vertex vertex-entry :backend backend :version version)
    (declare (ignore vs-structs))
    ;; Generate fragment shader
    (multiple-value-bind (fs-source fs-uniforms fs-attrs fs-buffers fs-structs)
        (generate-shader-source :fragment fragment-entry :backend backend :version version)
      (declare (ignore fs-attrs fs-structs))  ; Fragment shader doesn't have vertex attrs

      ;; For non-GLSL backends, attributes come back empty from generate-stage
      ;; We need to extract them from the GLSL version for reflection
      (let* ((attrs (if (and (null vs-attrs) (not (eq backend :glsl)))
                       ;; Generate GLSL just for attribute info
                       (multiple-value-bind (glsl-source glsl-uniforms glsl-attrs)
                           (generate-shader-source :vertex vertex-entry :backend :glsl :version 450)
                         (declare (ignore glsl-source glsl-uniforms))
                         (mapcar #'parse-attribute glsl-attrs))
                       (mapcar #'parse-attribute vs-attrs)))
            ;; Parse uniforms
            (vs-parsed-uniforms (mapcar #'parse-uniform vs-uniforms))
            (fs-parsed-uniforms (mapcar #'parse-uniform fs-uniforms))
            ;; Parse uniform blocks from buffers
            (vs-uniform-blocks (mapcar (lambda (b) (parse-uniform-block b :vertex)) vs-buffers))
            (fs-uniform-blocks (mapcar (lambda (b) (parse-uniform-block b :fragment)) fs-buffers))
            ;; Extract textures from uniforms (GLSL combined samplers)
            (vs-textures (extract-textures-from-uniforms vs-parsed-uniforms :vertex))
            (fs-textures (extract-textures-from-uniforms fs-parsed-uniforms :fragment)))
        (make-instance 'shader-reflection
                       :name name
                       :vertex-source vs-source
                       :fragment-source fs-source
                       :attributes attrs
                       :vertex-uniforms vs-parsed-uniforms
                       :fragment-uniforms fs-parsed-uniforms
                       :vertex-buffers vs-buffers
                       :fragment-buffers fs-buffers
                       ;; Merged uniform blocks
                       :uniform-blocks (merge-uniform-blocks vs-uniform-blocks fs-uniform-blocks)
                       ;; Merged textures
                       :textures (merge-textures vs-textures fs-textures))))))

(defun sort-attributes-by-location (attributes)
  "Sort attributes by their location, filling gaps with nil."
  (let ((max-loc (reduce #'max attributes :key #'attr-location :initial-value -1)))
    (when (>= max-loc 0)
      (let ((result (make-array (1+ max-loc) :initial-element nil)))
        (dolist (attr attributes)
          (setf (aref result (attr-location attr)) attr))
        (coerce result 'list)))))

(defun reflection-attr-base-types (reflection)
  "Return a list of sokol attr base types for all 16 attribute slots."
  (let ((attrs (shader-attributes reflection))
        (result (make-list 16 :initial-element :sg-shaderattrbasetype-float)))
    (dolist (attr attrs)
      (when (and attr (< (attr-location attr) 16))
        (setf (nth (attr-location attr) result)
              (type-to-attr-base-type (attr-type-name attr)))))
    result))
