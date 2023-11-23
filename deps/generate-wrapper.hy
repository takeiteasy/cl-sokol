#!/usr/bin/env hy
(import sys)
(import json)
(import os)

(defmacro read-file [path]
  `(.strip (.read (open ~path "r"))))

(defmacro unless [expr #* body]
  `(when (not ~expr)
     (do
       ~@body)))

(assert (= (len sys.argv) 2))
(assert (os.path.isfile (get sys.argv 1)))

(defn translate-symbol [tree]
  (print tree)
  "")

(defmacro loop-tree-do [#* body]
  `(for [entry tree]
     (when (= "function" (get entry "tag"))
       (let [fname (get entry "name")
             return-type (get entry "return-type")
             rtype (get return-type "tag")
             func-params (get entry "parameters")
             params (if func-params
                        (.join ","
                               (lfor p func-params
                                     (translate-symbol p)))
                        "void")]
         (when (.startswith rtype "s")
           ~@body)))))

(let [tree (json.loads (read-file (get sys.argv 1)))]
  (loop-tree-do
    (unless params
      (print f"{rtype}* {fname}({params});"))))
