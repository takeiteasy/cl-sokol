;;;; test.lisp

(defpackage :cl-sokol/app
  (:use #:cl)
  (:nicknames :%sapp))

(in-package :cl-sokol/app)

(pushnew (asdf:system-relative-pathname :cl-sokol #p"build/")
         cffi:*foreign-library-directories*
         :test #'equal)

(cffi:define-foreign-library sokol-sapp
  (:darwin "libsokol_app.dylib")
  (:unix "libsokol_app.so")
  (:windows "libsokol_app.dll")
  (t (:default "libsokol_app")))

(unless (cffi:foreign-library-loaded-p 'sokol-sapp)
  (cffi:use-foreign-library sokol-sapp))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cl-sokol/app::translate-to-lisp (name)
    (autowrap:default-c-to-lisp
     (if (or (< (length name) 5)
             (not (string-equal "sapp_"
                                (subseq (string-downcase name) 0 5))))
         name
         (subseq name 5)))))

(autowrap:c-include #P"sokol_app.h"
                    :spec-path (asdf:system-relative-pathname :cl-sokol #p"spec/")
                    :c-to-lisp-function #'translate-to-lisp
                    ;; TODO: Handle exclude/include symbols for Windows + Linux
                    :exclude-definitions ("^_(?!SAPP)")
                    :include-definitions ("__darwin_intptr_t"))