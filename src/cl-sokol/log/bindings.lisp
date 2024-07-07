;;;; cl-sokol/log/bindings.lisp

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

(defpackage :cl-sokol/log
  (:use #:cl)
  (:nicknames :%slog))

(in-package :cl-sokol/log)

(pushnew (asdf:system-relative-pathname :cl-sokol #p"build/")
         cffi:*foreign-library-directories*
         :test #'equal)

(cffi:define-foreign-library sokol-slog
  (:darwin "libsokol_log.dylib")
  (:unix "libsokol_log.so")
  (:windows "libsokol_log.dll")
  (t (:default "libsokol_log")))

(unless (cffi:foreign-library-loaded-p 'sokol-slog)
  (cffi:use-foreign-library sokol-slog))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cl-sokol/log::translate-to-lisp (name)
    (autowrap:default-c-to-lisp
     (if (or (< (length name) 5)
             (not (string-equal "slog_"
                                (subseq (string-downcase name) 0 5))))
         name
         (subseq name 5)))))

(autowrap:c-include (asdf:system-relative-pathname :cl-sokol #p"src/sokol/sokol_log.h")
                    :spec-path (asdf:system-relative-pathname :cl-sokol #p"spec/")
                    :c-to-lisp-function #'translate-to-lisp
                    :exclude-definitions ("^(?!slog)")
                    :include-definitions ("^uint32_t$"))
