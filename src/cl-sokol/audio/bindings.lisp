;;;; cl-sokol/audio/bindings.lisp

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

(defpackage :cl-sokol/audio
  (:use #:cl)
  (:nicknames :%saudio))

(in-package :cl-sokol/audio)

(pushnew (asdf:system-relative-pathname :cl-sokol #p"build/")
         cffi:*foreign-library-directories*
         :test #'equal)

(cffi:define-foreign-library sokol-saudio
  (:darwin "libsokol_audio.dylib")
  (:unix "libsokol_audio.so")
  (:windows "libsokol_audio.dll")
  (t (:default "libsokol_audio")))

(unless (cffi:foreign-library-loaded-p 'sokol-saudio)
  (cffi:use-foreign-library sokol-saudio))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cl-sokol/audio::translate-to-lisp (name)
    (autowrap:default-c-to-lisp
     (if (or (< (length name) 7)
             (not (string-equal "saudio_"
                                (subseq (string-downcase name) 0 7))))
         name
         (subseq name 7)))))

(autowrap:c-include (asdf:system-relative-pathname :cl-sokol #p"src/sokol/sokol_audio.h")
                    :spec-path (asdf:system-relative-pathname :cl-sokol #p"spec/")
                    :symbol-exceptions (("saudio_push" . "PUSH-SAMPLES"))
                    :c-to-lisp-function #'translate-to-lisp
                    :exclude-definitions ("^(?!saudio)")
                    :include-definitions ("^_SAUDIO"
                                          "^uint32_t$"))
