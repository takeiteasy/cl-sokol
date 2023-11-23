#!/usr/bin/env hy
(import sys)
(import json)
(import re)
(import os)

(assert (= (len sys.argv) 2))
(assert (os.path.isfile (get sys.argv 1)))

(setv **disable-structs** False)
(setv **disable-enums** False)
(setv **disable-functions** False)
(setv **disable-bindings-output** False)
(setv **disable-package-output** False)
(setv **enable-debug-dump** False)

(setv **struct-symbols** [])
(setv **enum-symbols** [])
(setv **function-symbols** [])
(setv **typedefs** {})

(defn dump-symbols []
  (when **enable-debug-dump**
    (print "TYPEDEFS:")
    (for [[k v] (.items **typedefs**)]
      (print f"{k} => {v}"))
    (print "STRUCTS:")
    (for [s **struct-symbols**]
      (print s))
    (print "ENUMS")
    (for [s **enum-symbols**]
      (print s))
    (print "FUNCTIONS")
    (for [s **function-symbols**]
      (print s))))

(defmacro translate-name [name]
  `(.lower (re.sub "_" "-" ~name)))

(defmacro read-file [path]
  `(.strip (.read (open ~path "r"))))

(defmacro unless [expr #* body]
  `(when (not ~expr)
     (do
       ~@body)))

(defmacro struct-format [name]
  `f"(:struct {~name})")

(defmacro pointer-format [tree]
  `f"(:pointer {(translate-symbol ~tree)})")

(defmacro array-format [tree size]
  `f"(:array {(translate-symbol ~tree)} {~size})")

(defn translate-symbol [tree]
  (setv c-symbol-tag (get tree "tag")
        symbol-tag (translate-name c-symbol-tag))
  (cond
    (.startswith c-symbol-tag ":") (match c-symbol-tag
                                        ":function-pointer" ":pointer"
                                        ":_Bool" ":int"
                                        ":struct" (struct-format (translate-name (get tree "name")))
                                        ":enum" (translate-name (get tree "name"))
                                        ":pointer" (pointer-format (get tree "type"))
                                        ":array" (array-format (get tree "type") (get tree "size"))
                                        t symbol-tag)
    (in symbol-tag **struct-symbols**) (struct-format (if (in "name" tree)
                                                          (get tree "name")
                                                          symbol-tag))
    (in c-symbol-tag (.keys **typedefs**)) (get **typedefs** c-symbol-tag)
    ;; (or (in symbol-tag **enum-symbols**) (in symbol-tag **function-symbols**) symbol-tag
    True symbol-tag))

(defn append-typedef [tree]
  (setv typedef-name (get tree "name")
        typedef-type (get tree "type")
        type-tag (get typedef-type "tag"))
  (cond
    (= type-tag ":struct") (when (= typedef-name (get typedef-type "name"))
                             (return))
    (= type-tag ":union") (return))
  (let [original-symbol (translate-symbol typedef-type)]
    (when (and original-symbol
               (!= original-symbol typedef-name))
      (setv (get **typedefs** typedef-name) original-symbol))))

(defmacro reduce [initializer #* args]
  (setv g (hy.gensym)
        a (hy.gensym))
  `(do
     (setv ~g ~initializer)
     (for [~a ~args]
       (setv ~g (+ ~g ~a)))
     ~g))

(defn translate-struct [tree]
  (setv struct-name (translate-name (get tree "name"))
        fields (lfor field (get tree "fields")
                     (do
                       ;; (print (get field "type"))
                     f"({(translate-name (get field "name"))} {(translate-symbol (get field "type"))})")))
  (unless (in struct-name **struct-symbols**)
    (.append **struct-symbols** struct-name))
  f"(defcstruct {struct-name}\n  {(.join "\n  " fields)})")

(defn translate-enum [tree]
  (setv enum-name (translate-name (get tree "name"))
        fields (lfor field (get tree "fields")
                     #((translate-name (get field "name")) (get field "value"))))
  (if enum-name
      (do
        (unless (in enum-name **enum-symbols**)
          (.append **enum-symbols** enum-name))
        f"(defcenum {enum-name}\n  {(.join "\n  " (lfor field fields f"(:{(get field 0)} {(get field 1)})"))})")
      (do
        (setv result [])
        (for [field fields]
          (let [anon-name f"+{(get field 0)}+"]
            (unless (in anon-name **enum-symbols**)
              (.append **enum-symbols** anon-name))
            (.append result f"(defconstant {anon-name} {(get field 1)})")))
        (.join "\n" result))))

(defn translate-function [tree]
  (setv orig-function-name (get tree "name")
        function-name (translate-name orig-function-name)
        function-ret (translate-symbol (get tree "return-type"))
        function-params (lfor param (get tree "parameters")
                              #((get param "name") (translate-symbol (get param "type")))))
  (unless (in function-name **function-symbols**)
    (.append **function-symbols** function-name))
  f"(defcfun ({function-name} \"{orig-function-name}\") {function-ret}{(if function-params (+ "\n  " (.join "\n  " (lfor p function-params f"({(get p 0)} {(get p 1)})"))) "")})")

(defmacro remove-when [src body]
  (setv g (hy.gensym))
  `(do
     (setv ~g [])
     (for [*test* ~src]
       (when ~body
         (.append ~g *test*)))
     ~g))

(defmacro unless-disabled [test res]
  (setv g (hy.gensym))
  `(let [~g ~res]
     (if ~test
         None
         ~g)))

(let [bindings-out (open "src/bindings.lisp" "w")
      tree (json.loads (read-file (get sys.argv 1)))
      bindings (remove-when
                 (lfor entry tree
                       (match (get entry "tag")
                              "typedef" (append-typedef entry)
                              "struct" (unless-disabled **disable-structs** (translate-struct entry))
                              "enum" (unless-disabled **disable-enums** (translate-enum entry))
                              "function" (unless-disabled **disable-functions** (translate-function entry))))
                 *test*)]
  (unless **disable-bindings-output**
    (do
      (bindings-out.write
        (reduce ""
                (read-file "deps/bindings-header.lisp")
                "\n\n"
                (.join "\n\n" bindings)))
      (bindings-out.close))))

(unless **disable-package-output**
  (let [package-out (open "src/package.lisp" "w")
        export-list (lfor e (reduce [] (lfor s **struct-symbols** f"{s}") **enum-symbols** **function-symbols**) f"#:{e}")]
    (do
      (package-out.write
        (reduce ""
                (read-file "deps/package-header.lisp")
                "\n   "
                (.join "\n   " export-list)
                "))\n\n"
                (read-file "deps/package-footer.lisp")))
      (package-out.close))))

(dump-symbols)
