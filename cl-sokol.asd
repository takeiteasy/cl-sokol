;;;; cl-sokol.asd
;;;; Copyright (C) 2025 George Watson
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(asdf:defsystem #:cl-sokol
  :description "Common Lisp bindings and wrapper for sokol"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPL-3.0"
  :version "0.1.0"
  :depends-on (#:cffi
               #:cffi-libffi)
  :serial t
  :components ((:file "src/sokol-log")
               (:file "src/sokol-gfx")
               (:file "src/sokol-app")
               (:file "src/sokol-time")
               (:file "src/sokol-audio")
               (:file "src/sokol-glue")
               (:file "src/package")
               (:file "src/core")
               (:file "src/structs")
               (:file "src/api"))
  :in-order-to ((test-op (test-op #:cl-sokol/tests))))

(asdf:defsystem #:cl-sokol/tests
  :description "Tests for cl-sokol"
  :depends-on (#:cl-sokol)
  :components ((:module "tests"
                :components ((:file "wrapper-tests"))))
  :perform (test-op (o c) (symbol-call :cl-sokol/tests :run-tests)))

;;;; Optional Util Module Subsystems

(asdf:defsystem #:cl-sokol/gl
  :description "CLOS wrappers for sokol-gl (OpenGL 1.x compatibility layer)"
  :depends-on (#:cl-sokol)
  :serial t
  :components ((:file "src/sokol-gl")
               (:file "src/gl-wrapper")))

(asdf:defsystem #:cl-sokol/debugtext
  :description "CLOS wrappers for sokol-debugtext (ASCII debug text rendering)"
  :depends-on (#:cl-sokol)
  :serial t
  :components ((:file "src/sokol-debugtext")
               (:file "src/debugtext-wrapper")))

(asdf:defsystem #:cl-sokol/shape
  :description "CLOS wrappers for sokol-shape (primitive shape generation)"
  :depends-on (#:cl-sokol)
  :serial t
  :components ((:file "src/sokol-shape")
               (:file "src/shape-wrapper")))

(asdf:defsystem #:cl-sokol/memtrack
  :description "Convenience wrappers for sokol-memtrack (memory tracking)"
  :depends-on (#:cl-sokol)
  :serial t
  :components ((:file "src/sokol-memtrack")
               (:file "src/memtrack-wrapper")))

(asdf:defsystem #:cl-sokol/color
  :description "Convenience wrappers for sokol-color (X11 color utilities)"
  :depends-on (#:cl-sokol)
  :serial t
  :components ((:file "src/sokol-color")
               (:file "src/color-wrapper")))

(asdf:defsystem #:cl-sokol/imgui
  :description "Convenience wrappers for sokol-imgui (Dear ImGui integration)"
  :depends-on (#:cl-sokol)
  :serial t
  :components ((:file "src/sokol-imgui")
               (:file "src/imgui-wrapper")))

(asdf:defsystem #:cl-sokol/gfx-imgui
  :description "Convenience wrappers for sokol-gfx-imgui (sokol-gfx debug inspector)"
  :depends-on (#:cl-sokol #:cl-sokol/imgui)
  :serial t
  :components ((:file "src/sokol-gfx-imgui")
               (:file "src/gfx-imgui-wrapper")))

(asdf:defsystem #:cl-sokol/all
  :description "All cl-sokol subsystems (convenience system)"
  :depends-on (#:cl-sokol
               #:cl-sokol/gl
               #:cl-sokol/debugtext
               #:cl-sokol/shape
               #:cl-sokol/memtrack
               #:cl-sokol/color
               #:cl-sokol/imgui
               #:cl-sokol/gfx-imgui))
