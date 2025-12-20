# cl-sokol

> **WARNING** This is a work in progress

Common Lisp bindings + wrapper for [sokol](https://github.com/floooh/sokol). Uses CFFI for foreign function interface.

## Quick Example

```lisp
(ql:quickload :cl-sokol)
(in-package :sokol)

(defvar *pipeline* nil)
(defvar *vertex-buffer* nil)

(defmethod init ()
  (setup-gfx)
  
  ;; Create vertex buffer with position (xyz) + color (rgba)
  (let* ((vertices (vector
                     0.0  0.5 0.5   1.0 0.0 0.0 1.0    ; top (red)
                     0.5 -0.5 0.5   0.0 1.0 0.0 1.0    ; right (green)
                    -0.5 -0.5 0.5   0.0 0.0 1.0 1.0))  ; left (blue)
         (vertex-range (make-range-from-array vertices :float))
         (buffer-desc (make-instance 'buffer-desc
                                     :buffer-type :vertex-buffer
                                     :usage :immutable
                                     :data vertex-range)))
    (setf *vertex-buffer* (make-buffer buffer-desc)))
  
  ;; Create shader and pipeline
  (let* ((shader-desc (triangle-shader-desc :metal-macos))
         (shader (make-shader shader-desc))
         (pipeline-desc (make-instance 'pipeline-desc
                                       :shader shader
                                       :vertex-layouts '(:sg-vertexformat-float3
                                                        :sg-vertexformat-float4))))
    (setf *pipeline* (make-pipeline pipeline-desc))))

(defmethod frame ()
  (let ((pass (make-instance 'pass-desc
                            :action (make-instance 'pass-action
                                                  :clear-color (make-color 0.1 0.1 0.1 1.0)
                                                  :load-action :clear))))
    (begin-pass pass))
  
  (apply-pipeline *pipeline*)
  (apply-bindings (make-instance 'bindings :vertex-buffers (list *vertex-buffer*)))
  (draw 0 3 1)
  
  (end-pass)
  (commit))

(defmethod cleanup ()
  (shutdown-gfx))

(run-app :title "Triangle" :width 640 :height 480)
```

## Quick Start

```bash
# Clone the repository to Quicklisp local-projects
cd ~/quicklisp/local-projects # or wherever you keep your Quicklisp projects
git clone https://github.com/takeiteasy/cl-sokol.git
cd cl-sokol
git clone https://github.com/floooh/sokol.git
# Build native library
make native
# Load and run in REPL
sbcl --load run-example.lisp
```

```lisp
(ql:quickload :cl-sokol)
(ql:quickload :example-wrapper-triangle)
(example-wrapper-triangle:run-triangle-example)
```

### Clear Screen

```lisp
(defmethod init ()
  (setup-gfx))

(defmethod frame ()
  (begin-pass (make-instance 'pass-desc
                :action (make-instance 'pass-action
                          :clear-color (make-color 0.2 0.3 0.4 1.0)
                          :load-action :clear)))
  (end-pass)
  (commit))

(defmethod cleanup ()
  (shutdown-gfx))

(run-app :title "Clear" :width 800 :height 600)
```

### Timing

```lisp
(setup-time)
(let ((start (now)))
  ;; ... do work ...
  (format t "Elapsed: ~,2f ms~%" (ticks-to-ms (time-since start))))
```

### Audio

```lisp
(setup-audio (make-instance 'audio-desc
               :sample-rate 44100
               :num-channels 2))

(loop while running do
  (let ((frames (audio-expect)))
    (when (> frames 0)
      (let ((samples (generate-samples frames)))
        (push-audio-samples samples frames)))))

(shutdown-audio)
```

## Generating Bindings + Shader tools

These are unofficial bindings, you will have to build a copy of `sokol-shdc` with lisp support.

```bash
git clone https://github.com/takeiteasy/sokol.git
cd sokol
git checkout lisp-bindings
cd ..
git clone https://github.com/takeiteasy/sokol-tools.git
cd sokol-tools
git checkout lisp-bindings
cd ..
make sokol-tools
make bindings
```

## License

```
Copyright (C) 2025 George Watson

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
```
