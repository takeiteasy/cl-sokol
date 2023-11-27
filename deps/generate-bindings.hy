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
  `f"(:struct %{~name})")

(defmacro pointer-format [tree]
  `f"(:pointer {(translate-symbol ~tree)})")

(defmacro array-format [tree size]
  `f"(:array {(translate-symbol ~tree)} {~size})")

(defn translate-symbol [tree]
  (let [c-symbol-tag (get tree "tag")
        symbol-tag (translate-name c-symbol-tag)]
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
      True symbol-tag)))

(defn append-typedef [tree]
  (let [typedef-name (get tree "name")
        typedef-type (get tree "type")
        type-tag (get typedef-type "tag")]
    (cond
      (= type-tag ":struct") (when (= typedef-name (get typedef-type "name"))
                               (return))
      (= type-tag ":union") (return))
    (let [original-symbol (translate-symbol typedef-type)]
      (when (and original-symbol
                 (!= original-symbol typedef-name))
        (setv (get **typedefs** typedef-name) original-symbol)))))

(defmacro reduce [initializer #* args]
  (let [g (hy.gensym)
        a (hy.gensym)]
    `(do
       (setv ~g ~initializer)
       (for [~a ~args]
         (setv ~g (+ ~g ~a)))
       ~g)))

                                ; (defn c-to-lisp-symbol [symbol]
                                ;   (cond
                                ;     (= symbol ":float") ":single-float"
                                ;     (or (.startswith symbol "sg")
                                ;         (in symbol #(":unsigned-int" ":int" ":unsigned-long" ":unsigned-char"))) ":integer"
                                ;     (.starts-with symbol "(:array") ":vector"
                                ;     (.starts-with symbol "(:struct") symbol
                                ;     True (do
                                ;            (print f"Unknown symbol: {symbol}")
                                ;            "UNKNOWN")))

;; (defn generate-translation-wrapper [tree]
;;   (let [struct-name (translate-name (get tree "name"))
;;         fields (lfor field (get tree "fields") f"{(translate-name (get field "name"))}")]
;;     (.join "\n" #(f"(defmethod translate-from-foreign (ptr (type {struct-name}-type))"
;;                   f"  (with-foreign-slots (({(.join " " fields)}) ptr (:struct %{struct-name}))"
;;                   f"    (make-{struct-name} {(.join " " (lfor field fields f":{field} {field}"))})))"
;;                   f"(defmethod expand-from-foreign (ptr (type {struct-name}-type))"
;;                   f"  `(with-foreign-slots (({(.join " " fields)}) ,ptr (:struct %{struct-name}))"
;;                   f"    (make-{struct-name} {(.join " " (lfor field fields f":{field} {field}"))})))"
;;                   f"(defmethod translate-into-foreign-memory (value (type {struct-name}-type) ptr)"
;;                   f"  (with-foreign-slots (({(.join " " fields)}) ptr (:struct %{struct-name}))"
;;                   f"    (setf "
;;                   (reduce
;;                     "      "
;;                     (.join "\n      " (lfor field fields f"{field} ({struct-name}-{field} value)"))
;;                     ")))")
;;                   ))))

;; (defn default-value [tree]
;;   (let [tag (get (get tree "type") "tag")]
;;     (cond
;;       (= tag ":float") "0.0"
;;       (= tag ":array") f"(make-array {(get (get tree "type") "size")})"
;;       (in tag ["uint32_t" "uint64_t" "uint8_t" ":int" "size_t" "uintptr_t"]) "0"
;;       (in tag [":pointer" ":function-pointer" ":_Bool"]) "nil"
;;       ; (in (translate-name tag) **struct-symbols**) f"(make-{(translate-name tag)})"
;;       True "nil")))

;; (defn translate-wrapper-struct [tree]
;;   (let [struct-name (translate-name (get tree "name"))
;;         fields (lfor field (get tree "fields") f"({(translate-name (get field "name"))} {(default-value field)})")]
;;     (if (.startswith struct-name "-")
;;         None
;;         f"(defstruct {struct-name}\n  {(.join "\n  " fields)})")))

(defn translate-struct [tree]
  (let [struct-name (translate-name (get tree "name"))
        fields (lfor field (get tree "fields") f"({(translate-name (get field "name"))} {(translate-symbol (get field "type"))})")
        field-names (lfor field (get tree "fields") f"{(translate-name (get field "name"))}")]
    (when (.startswith struct-name "-")
      (return None))
    (unless (in struct-name **struct-symbols**)
      (.append **struct-symbols** struct-name))
    f"(defcstruct (%{struct-name} :class {struct-name}-type)\n  {(.join "\n  " fields)})"))

(defn translate-enum [tree]
  (let [enum-name (translate-name (get tree "name"))
        fields (lfor field (get tree "fields")
                     #((translate-name (get field "name")) (get field "value")))]
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
          (.join "\n" result)))))

(setv **c-wrap-functions** [])
(setv **ignore-functions** ["sokol_main"])

(defn translate-function [tree [append-func True]]
  (let [orig-function-name (get tree "name")
        function-name (translate-name orig-function-name)
        function-ret (translate-symbol (get tree "return-type"))
        return-type (translate-name (get (get tree "return-type") "tag"))
        function-params (lfor param (get tree "parameters")
                              #((get param "name") (translate-symbol (get param "type"))))]
    (when (in orig-function-name **ignore-functions**)
      (return None))
    (when (not (.startswith return-type ":"))
      (when (and (in return-type **struct-symbols**)
                 append-func)
        (.append **c-wrap-functions** tree)))
    (when (not (in function-name **function-symbols**))
      (.append **function-symbols** function-name))
    f"(defcfun ({function-name} \"{orig-function-name}\") {function-ret}{(if function-params (+ "\n  " (.join "\n  " (lfor p function-params f"({(get p 0)} {(get p 1)})"))) "")})"))

(defmacro remove-when [src condition]
  (let [g (hy.gensym)]
    `(let [~g []]
       (for [*test* ~src]
         (when ~condition
           (.append ~g *test*)))
       ~g)))

(defmacro unless-disabled [test res]
  (let [g (hy.gensym)]
    `(let [~g ~res]
       (if ~test
           None
           ~g))))

(defn reconstruct-param [param var-name]
  (let [tag (get (get param "type") "tag")
        already-ptr False]
    (when (= tag ":pointer")
      (setv tag (get (get (get param "type") "type") "tag")
            already-ptr True))
    f"{tag}* {(if already-ptr "_" "")}{var-name}"))

(defn to-alphabet [idx]
  (chr (+ (ord "a") idx)))

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
    (bindings-out.write
      (reduce ""
              (read-file "deps/bindings-header.lisp")
              "\n\n"
              (.join "\n\n" bindings))))
  (bindings-out.close))

(let [cheader-out (open "deps/sokol_wrapper.h" "w")
      csource-out (open "deps/sokol_wrapper.c" "w")
      bindings-out (open "src/bindings.lisp" "a")
      cheader []
      csource []
      bindings []]
  (for [tree **c-wrap-functions**]
    (let [func-name (get tree "name")
          return-type (get (get tree "return-type") "tag")
          params-raw (get tree "parameters")
          params (if (not params-raw)
                     "void"
                     (.join ", " (lfor [i p] (enumerate params-raw) (reconstruct-param p (to-alphabet i)))))
          out-params (if (not params-raw)
                         ""
                         (.join ", " (lfor [i p] (enumerate params-raw)
                                           (let [c (to-alphabet i)]
                                             (if (= (get (get p "type") "tag") ":pointer")
                                                 f"_{c}"
                                                 f"*{c}")))))]
      (setv (get tree "name") (+ (get tree "name") "_ptr"))
      (.append bindings (translate-function tree False))
      (.append cheader f"{return-type}* {func-name}_ptr({params});")
      (.append csource (.join "\n" [(+ f"{return-type}* {func-name}_ptr({params}) " "{")
                                    f"    {return-type}* result = malloc(sizeof({return-type}));"
                                    f"    {return-type} tmp = {func-name}({out-params});"
                                    "    return result;"
                                    "}\n"]))))
  (cheader-out.write (reduce
                       "#pragma once\n"
                       "#include \"sokol.h\"\n"
                       "\n"
                       (.join "\n" cheader)))
  (csource-out.write (reduce
                       "#include \"sokol.h\"\n"
                       "#include \"sokol_wrapper.h\"\n"
                       "#include <stdlib.h>\n"
                       "\n"
                       (.join "\n" csource)))
  (bindings-out.write (+ "\n"
                         (.join "\n\n" bindings)))
  (cheader-out.close)
  (csource-out.close)
  (bindings-out.close))

(unless **disable-package-output**
  (let [package-out (open "src/package.lisp" "w")
        export-list (lfor e (reduce [] (lfor s **struct-symbols** f"%{s}") **enum-symbols** **function-symbols**) f"#:{e}")]
    (package-out.write
      (reduce ""
              (read-file "deps/package-header.lisp")
              "\n   "
              (.join "\n   " export-list)
              "))\n\n"
              (read-file "deps/package-footer.lisp")))
    (package-out.close)))
