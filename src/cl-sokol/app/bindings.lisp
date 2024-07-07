;;;; cl-sokol/app/bindings.lisp

;; The MIT License (MIT)

;; Copyright (c) 2024 George Watson

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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

(autowrap:c-include (asdf:system-relative-pathname :cl-sokol #p"src/sokol/sokol_app.h")
                    :spec-path (asdf:system-relative-pathname :cl-sokol #p"spec/")
                    :c-to-lisp-function #'translate-to-lisp)
