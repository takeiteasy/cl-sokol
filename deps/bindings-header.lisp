;;;; bindings.lisp

(in-package #:%sokol)

(pushnew (asdf:system-relative-pathname :cl-sokol "bin/") *foreign-library-directories*)
(define-foreign-library libsokol
  (t (:default "libsokol")))

(unless (foreign-library-loaded-p 'libsokol)
  (use-foreign-library libsokol))
