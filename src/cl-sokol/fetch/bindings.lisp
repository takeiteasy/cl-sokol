;;;; cl-sokol/fetch/bindings.lisp

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

(defpackage :cl-sokol/fetch
  (:use #:cl)
  (:nicknames :%sfetch))

(in-package :cl-sokol/fetch)

(pushnew (asdf:system-relative-pathname :cl-sokol #p"build/")
         cffi:*foreign-library-directories*
         :test #'equal)

(cffi:define-foreign-library sokol-sfetch
  (:darwin "libsokol_fetch.dylib")
  (:unix "libsokol_fetch.so")
  (:windows "libsokol_fetch.dll")
  (t (:default "libsokol_fetch")))

(unless (cffi:foreign-library-loaded-p 'sokol-sfetch)
  (cffi:use-foreign-library sokol-sfetch))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cl-sokol/fetch::translate-to-lisp (name)
    (autowrap:default-c-to-lisp
     (if (or (< (length name) 7)
             (not (string-equal "sfetch_"
                                (subseq (string-downcase name) 0 7))))
         name
         (subseq name 7)))))

(autowrap:c-include (asdf:system-relative-pathname :cl-sokol #p"src/sokol/sokol_fetch.h")
                    :spec-path (asdf:system-relative-pathname :cl-sokol #p"spec/")
                    :symbol-exceptions (("sfetch_continue" . "RESUME"))
                    :c-to-lisp-function #'translate-to-lisp
                    :exclude-definitions ("^(?!sfetch)"
                                          "^_(?!SFETCH)")
                    :include-definitions ("^uint32_t$"
                                          "^size_t$"
                                          "^_SFETCH"))
