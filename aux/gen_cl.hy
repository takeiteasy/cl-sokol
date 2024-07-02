#!/usr/bin/env hy
(import sys)
(import json)
(import re)
(import os)
(import argparse)
(import subprocess)
(import platform)

(defmacro die [msg]
  `(do
    (print f"ERROR: {~msg}" :file sys.stderr)
    (sys.exit 1)))

(defmacro file-exists? [path]
  `(and (os.path.isfile ~path)
        (not (os.path.isdir ~path))))

(setv **c2ffi-path** (if (= (platform.system) "Windows")
                         ".\\c2ffi\\build\\bin\\c2ffi.exe"
                         "./c2ffi/build/bin/c2ffi"))
(setv **header-path** "aux/sokol_all.h")

(setv **enable-dump-json** False)
(setv **enable-debug-dump** False)

(setv **struct-symbols** [])
(setv **enum-symbols** [])
(setv **function-symbols** [])
(setv **typedefs** {})

(setv **c-wrap-functions** [])
(setv **c-wrap-functions-two** [])
(setv **ignore-functions** ["sokol_main"])

(let [parser (argparse.ArgumentParser :description "sokol wrappings+bindings generator for Common Lisp")]
  (parser.add-argument "-d" "--dump"
                       :action "store_true"
                       :help "Dump parsed objects and bail")
  (parser.add-argument "-j" "--json"
                       :action "store_true"
                       :help "Dump c2ffi JSON to stdout and bail")
  (parser.add-argument "-p" "--path"
                       :help "Specify path to header")
  (let [args (parser.parse-args)]
    (when args.path
      (when (not (file-exists? args.path))
        (die "can't find '{args.path}'"))
      (setv **header-path** args.path))
    (when args.json
      (setv **enable-dump-json** True))
    (when args.dump
      (setv **enable-debug-dump** True))))

;; WARNING!
;; The functions below are for doing various things. Those things may include stuff and occurrences.
;; I wrote them during an acid trip and didn't make any comments or even use descriptive names.
;; Now neither God nor I have any clue what a lot of them do.
;; Don't blame me if you edit any of them and your computer explodes.

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
    (let [valid False]
      (for [prefix ["sg_" "sapp_" "saudio_"]]
        (when (typedef-name.startswith prefix)
          (setv valid True)
          (break)))
      (when (not valid)
        (return)))
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

(defn c-to-lisp-symbol [symbol]
  (cond
    (= symbol ":float") ":single-float"
    (or (.startswith symbol "sg")
        (in symbol #(":unsigned-int" ":int" ":unsigned-long" ":unsigned-char"))) ":integer"
    (.starts-with symbol "(:array") ":vector"
    (.starts-with symbol "(:struct") symbol
    True (do
           (print f"Unknown symbol: {symbol}")
           "UNKNOWN")))

(defn generate-translation-wrapper [tree]
  (let [struct-name (translate-name (get tree "name"))
        fields (lfor field (get tree "fields") f"{(translate-name (get field "name"))}")]
    (.join "\n" #(f"(defmethod translate-from-foreign (ptr (type {struct-name}-type))"
                  f"  (with-foreign-slots (({(.join " " fields)}) ptr (:struct %{struct-name}))"
                  f"    (make-{struct-name} {(.join " " (lfor field fields f":{field} {field}"))})))"
                  f"(defmethod expand-from-foreign (ptr (type {struct-name}-type))"
                  f"  `(with-foreign-slots (({(.join " " fields)}) ,ptr (:struct %{struct-name}))"
                  f"    (make-{struct-name} {(.join " " (lfor field fields f":{field} {field}"))})))"
                  f"(defmethod translate-into-foreign-memory (value (type {struct-name}-type) ptr)"
                  f"  (with-foreign-slots (({(.join " " fields)}) ptr (:struct %{struct-name}))"
                  f"    (setf "
                  (reduce
                    "      "
                    (.join "\n      " (lfor field fields f"{field} ({struct-name}-{field} value)"))
                    ")))")
                  ))))

(defn default-value [tree]
  (let [tag (get (get tree "type") "tag")]
    (cond
      (= tag ":float") "0.0"
      (= tag ":array") f"(make-array {(get (get tree "type") "size")})"
      (in tag ["uint32_t" "uint64_t" "uint8_t" ":int" "size_t" "uintptr_t"]) "0"
      (in tag [":pointer" ":function-pointer" ":_Bool"]) "nil"
      ; (in (translate-name tag) **struct-symbols**) f"(make-{(translate-name tag)})"
      True "nil")))

(defn translate-wrapper-struct [tree]
  (let [struct-name (translate-name (get tree "name"))
        fields (lfor field (get tree "fields") f"({(translate-name (get field "name"))} {(default-value field)})")]
    (if (.startswith struct-name "-")
        None
        f"(defstruct {struct-name}\n  {(.join "\n  " fields)})")))

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

(defn translate-function [tree [append-func True]]
  (let [orig-function-name (get tree "name")
        function-name (translate-name orig-function-name)
        function-ret (translate-symbol (get tree "return-type"))
        return-type (translate-name (get (get tree "return-type") "tag"))
        function-params (lfor param (get tree "parameters")
                              #((get param "name") (translate-symbol (get param "type"))))]
    (when (in orig-function-name **ignore-functions**)
      (return None))
    (if (not (.startswith return-type ":"))
        (when (and (in return-type **struct-symbols**)
                   append-func)
          (.append **c-wrap-functions** tree))
        (let [wrap False]
          (for [p function-params]
            (when (.startswith (get p 1) "(:struct ")
              (setv wrap True)
              (break)))
          (when wrap
            (.append **c-wrap-functions-two** tree))))
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

;; NOTE! This is where the various functions do a myriad of things ends, I know whats going on down here

(defn call-c2ffi [path]
  (let [cmd [**c2ffi-path** "-Isokol" path]
        out (subprocess.run cmd
                            :capture-output True
                            :check True
                            :text True)
        result None]
    (try
      (setv result (json.loads out.stdout))
      (except [e json.JSONDecodeError]
        (die (str e))))
    result))

(setv **tree** (call-c2ffi **header-path**))

(when **enable-dump-json**
  (print (json.dumps **tree**)))

(setv **bindings** (remove-when
                     (lfor entry **tree**
                           (match (get entry "tag")
                                  "typedef" (append-typedef entry)
                                  "struct" (translate-struct entry)
                                  "enum" (translate-enum entry)
                                  "function" (translate-function entry)))
                     *test*))

(for [b **bindings**]
  (print b))
(sys.exit 0)

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
    (print s))
  (sys.exit 0))

(defn reconstruct-param [param var-name]
  (let [tag (get (get param "type") "tag")]
    (if (tag.startswith "sg_")
        f"{tag}* {var-name}"
        (if (= tag ":pointer")
            f"{(get (get (get param "type") "type") "tag")}* {var-name}"
            f"{tag} {var-name}"))))

(defn to-alphabet [idx]
  (chr (+ (ord "a") idx)))

(let [cheader []
      csource []]
  (for [tree **c-wrap-functions-two**]
    ;; (for [p (get tree "parameters")]
    ;;   (let [tag (get (get p "type") "tag")]
    ;;     (when (tag.startswith "sg_")
    ;;       (setv
    ;;         (get (get p "type") "tag") ":pointer"
    ;;         (get (get p "type") "type") {"tag" tag}))))
    (let [func-name (get tree "name")
          return-type-raw (get (get tree "return-type") "tag")
          return-type (if (= return-type-raw ":_Bool")
                          "int"
                          (get return-type-raw (slice 1 None)))
          params-raw (get tree "parameters")
          params (.join ", " (lfor [i p] (enumerate params-raw) (reconstruct-param p (to-alphabet i))))
          out-params (.join ", " (lfor [i p] (enumerate params-raw)
                                           (let [c (to-alphabet i)]
                                             (if (or (= (get (get p "type") "tag") ":pointer")
                                                     (not (.startswith (get (get p "type") "tag") "sg_")))
                                                 f"{c}"
                                                 f"*{c}"))))]
      (setv (get tree "name") (+ (get tree "name") "_cl"))
      (.append cheader f"{return-type} {func-name}_cl({params});")
      (.append csource (.join "\n" [(+ f"{return-type} {func-name}_cl({params}) " "{")
                                    f"     {(if (= return-type "void") "" "return ")}{func-name}({out-params});"
                                    "}\n"]))))
  (for [line cheader]
    (print line))
  (print)
  (for [line csource]
    (print line)))

(defn reconstruct-param-two [param var-name]
  (let [tag (get (get param "type") "tag")
        already-ptr False]
    (when (= tag ":pointer")
      (setv tag (get (get (get param "type") "type") "tag")
            already-ptr True))
    f"{tag}* {(if already-ptr "_" "")}{var-name}"))

(let [cheader []
      csource []
      bindings []]
  (for [tree **c-wrap-functions**]
    (let [func-name (get tree "name")
          return-type (get (get tree "return-type") "tag")
          params-raw (get tree "parameters")
          params (if (not params-raw)
                     "void"
                     (.join ", " (lfor [i p] (enumerate params-raw) (reconstruct-param-two p (to-alphabet i)))))
          out-params (if (not params-raw)
                         ""
                         (.join ", " (lfor [i p] (enumerate params-raw)
                                           (let [c (to-alphabet i)]
                                             (if (= (get (get p "type") "tag") ":pointer")
                                                 f"_{c}"
                                                 f"*{c}")))))]
      (setv (get tree "name") (+ (get tree "name") "_cl"))
      (.append bindings (translate-function tree False))
      (.append cheader f"{return-type}* {func-name}_cl({params});")
      (.append csource (.join "\n" [(+ f"{return-type}* {func-name}_ptr({params}) " "{")
                                    f"    {return-type}* result = malloc(sizeof({return-type}));"
                                    f"    {return-type} tmp = {func-name}({out-params});"
                                    f"    memcpy(result, (void*)&tmp, sizeof({return-type}));"
                                    "    return result;"
                                    "}\n"]))))
  (for [line cheader]
    (print line))
  (print)
  (for [line csource]
    (print line)))

(let [package-out (open "src/package.lisp" "w")
      export-list (lfor e (reduce [] (lfor s **struct-symbols** f"%{s}") **enum-symbols** **function-symbols**) f"#:{e}")]
  )
