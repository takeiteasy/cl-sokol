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
