;;;; test.lisp

(defpackage :cl-sokol
  (:use #:cl #:autowrap))

(in-package :cl-sokol)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cl-sokol::translate-to-lisp (name)
    (autowrap:default-c-to-lisp
     (if (or (< (length name) 5)
             (not (string-equal "sapp_"
                                (subseq (string-downcase name) 0 5))))
         name
         (subseq name 5)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi-sys:%load-foreign-library
   :libtest (merge-pathnames "libsokol_app.dylib" (asdf-path 'cl-sokol))))

(c-include '(cl-sokol "sokol/sokol_app.h")
           :spec-path '(cl-sokol)
           :exclude-definitions ("^_(?!SAPP)")
           :include-definitions ("__darwin_intptr_t")
           :c-to-lisp-function #'translate-to-lisp)
