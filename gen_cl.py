#!/usr/bin/env python3
import platform
import sys
import subprocess
import json
import argparse
from enum import Enum

def die(msg="dunno"):
    print(f"ERROR: {msg}", file=sys.stderr)
    sys.exit(1)

C2FFIPATH = ".\\deps\\c2ffi\\build\\bin\\c2ffi.exe" if platform.system() == 'Windows' else "./deps/c2ffi/build/bin/c2ffi"

def generate_json(path):
    cmd = [C2FFIPATH, "-Ideps/sokol", path]
    out = subprocess.run(cmd, capture_output=True, check=True, text=True)
    result = None
    try:
        result = json.loads(out.stdout)
    except json.JSONDecodeError as e:
        die(str(e))
    return result

parser = argparse.ArgumentParser(description="Generate wrapping for Common Lisp using c2ffi")
parser.add_argument("-j", "--json", action="store_true",
                    help="Dump JSON to stdout and quit")
parser.add_argument("-d", "--dump", action="store_true",
                    help="Dump all everything to stdout")
parser.add_argument("-i", "--input", type=str,
                    help="Specify the location to the header")
parser.add_argument("-o", "--output", type=str,
                    help="Specify the directory to dump to")
args = parser.parse_args()

if not args.input:
    args.input = "aux/sokol_all.h"
if not args.output:
    args.output = "aux"
else:
    if args.endswith('\\' if platform.system() == 'Windows' else "/"):
        args.output = args.output[:-1]
data = generate_json(args.input)
if not data:
    die(f"Failed to parse `{args.input}`")
if args.json:
    print(json.dumps(data))
    sys.exit(0)

valid_prefixes = ["sg_", "sapp_", "saudio_", "slog_", "stm_", "sfetch_", "sargs_"]

def is_prefix_valid(line):
    for v in valid_prefixes:
        if line.startswith(v) or line.startswith(v.upper()):
            yield v

fixed = {}

# Common Lisp doesn't handle non-pointer FFI values very well
# This function creates an alternative version of functions that
# return or take any non-pointer structs
def fix(fn):
    if fn['name'] == "sokol_main":
        return
    result_type = fn['return-type']['tag']
    for _ in is_prefix_valid(result_type):
        fn['return-type']['tag'] = ':pointer'
        fn['return-type']['type'] = {'tag': result_type, 'alloc': True}
    for p in fn['parameters']:
        old = p['type']['tag']
        for _ in is_prefix_valid(old):
            p['tag'] = ':pointer'
            p['type'] = {'tag': old, 'deref': True}
    old_name = fn['name']
    fn['name'] = old_name + "_cl"
    fixed[old_name] = fn

# Translate c2ffi `return-type` to C type
def gen_return_type(rt):
    match rt['tag']:
        case ":void" | ":int":
            return rt['tag'][1:]
        case "size_t":
            return "size_t"
        case ":_Bool":
            return "bool"
        case ":pointer":
            t = rt['type']['tag']
            return ("void" if t == ':void' else t) + "*"

# Translate c2ffi parameter to C type
def gen_param_type(p):
    match p['tag']:
        case ":pointer":
            return gen_return_type(p)
        case "parameter":
            return gen_return_type(p['type'])

def alpha(n):
    return chr(ord("a") + n)

# Generate the params for the declaration
def gen_wrap_params(params):
    if not params:
        return "void"
    else:
        return ", ".join([f"{gen_param_type(p)} {alpha(i)}" for i, p in enumerate(params)])

# Generate the params that are passed to the original function
def gen_pass_params(params):
    return ", ".join([("*" if "deref" in p['type'] else "") + alpha(i) for i, p in enumerate(params)])

# This function generates the C header + source for the functions that need
# to be wrapped (functions modified by `fix()` above)
def gen_return_wrap(old_name, fn):
    rettype = gen_return_type(fn['return-type'])
    params = gen_wrap_params(fn['parameters'])
    pass_params = gen_pass_params(fn['parameters'])
    header = f"{rettype} {fn['name']}({params})"
    source = [header + " {"]
    if fn['return-type']['tag'] == ":pointer":
        if "alloc" in fn['return-type']['type']:
            source.append(f"\t{rettype} result = malloc(sizeof({rettype[:-1]}));")
            source.append(f"\t{rettype[:-1]} tmp = {old_name}({pass_params});")
            source.append(f"\tmemcpy(result, (void*)&tmp, sizeof({rettype[:-1]}));")
            source.append("\treturn result;")
        else:
            source.append(f"\treturn {old_name}({pass_params});")
    else:
        match fn['return-type']['tag']:
            case ":void":
                source.append(f"\t{old_name}({pass_params});")
            case ":int" | ":_Bool":
                source.append(f"\treturn {old_name}({pass_params});")
    source.append("}")
    return (header + ";", "\n".join(source))

def valid_prefix(line):
    for v in valid_prefixes:
        if line.startswith(v):
            return True
    return False

# Loop through the functions returned by c2ffi and identify any functions
# that either return a non-pointer struct or take a non-pointer struct as
# an argument. This will make binding much easier later
for item in data:
    if item['tag'] == 'function':
        if valid_prefix(item['return-type']['tag']):
            fix(item)
        else:
            param = None
            for p in item['parameters']:
                if valid_prefix(p['type']['tag']):
                    param = p
                    break
            if param:
                fix(item)

if args.dump:
    print("Functions to wrap:")
    for k, v in fixed.items():
        print(k, str(v))

# Stores our C wrapper
cheader = { v[:-1]: [] for v in valid_prefixes }
csource = { v[:-1]: [] for v in valid_prefixes }
# Loop through the marked functions and generate the wrappers
for k, v in fixed.items():
    header, source = gen_return_wrap(k, v)
    key = k.split("_")[0]
    if key + "_" in valid_prefixes:
        cheader[key].append(header)
        csource[key].append(source)

# Converts sg_init_buffer to init-buffer
def to_lisp(name, ignore_prefix=False):
    if not ignore_prefix:
        for v in is_prefix_valid(name):
            name = name[len(v):]
    return name.replace("_", "-")

# Stores our Common Lisp bindings
class StorageType(Enum):
    ENUM = 0
    CONSTANT = 1
    STRUCT = 2
    FUNCTION = 3

class Storage:
    def __init__(self):
        self.data = { k.name: [] for k in StorageType }
        self.exports = []

    def add(self, stype, export, defn):
        self.data[stype.name].append(defn)
        self.exports.append(export)

modules = { v[:-1]: Storage() for v in valid_prefixes }

def export(name, stype, defn):
    for v in is_prefix_valid(name):
        modules[v[:-1]].add(stype, to_lisp(name, stype == StorageType.CONSTANT), defn)

# Unnamed C enums are translated to constants
def unnamed_enum(enum):
    for f in enum['fields']:
        export(f['name'], StorageType.CONSTANT, f"(defconstant +{to_lisp(f['name'])}+ {f['value']})")

# Named C enums are translated to a defcenum binding
def named_enum(enum):
    lines = [f"(defcenum {to_lisp(enum['name'])}"]
    for f in enum['fields']:
        lines.append(f"\t(:{to_lisp(f['name'])} {f['value']})")
    lines[-1] += ")"
    export(enum['name'], StorageType.ENUM, "\n".join(lines))

def lisp_struct_ptr(p):
    tag = p['tag']
    for v in valid_prefixes:
        if tag.startswith(v):
            return f"(:pointer (:struct {to_lisp(tag)}))"
    return f"(:pointer {tag})"

# Translates c2ffi to Common Lisp CFFI type
def gen_fun_return(rt):
    match rt['tag']:
        case ":pointer":
            return lisp_struct_ptr(rt['type'])
        case ":_Bool":
            return ":boolean"
        case "uint32_t":
            return ":uint32"
        case "uint64_t":
            return ":uint64"
        case _:
            return rt['tag']

# Generates function parameter bindings for Common Lisp defcfun functions
def gen_lisp_param(p):
    match p['tag']:
        case "parameter":
            if p['type']['tag'] == ":pointer":
                return lisp_struct_ptr(p['type']['type'])
            else:
                match p['type']['tag']:
                    case ":int" | "size_t":
                        return ":int"
                    case "uint32_t":
                        return ":uint32"
                    case "uint64_t":
                        return ":uint64"
                    case ":_Bool":
                        return ":boolean"
                    case _:
                        return p['type']['tag']
        case ":pointer":
            return lisp_struct_ptr(p['type'])

# Translates c2ffi to Common Lisp function wrapper
def gen_lisp_fun(fn):
    cl_name = to_lisp(fn['name'][:-3] if fn['name'].endswith("_cl") else fn['name'])
    if cl_name == "sokol-main":
        return
    lines = [f"(defcfun ({cl_name} \"{fn['name']}\") {gen_fun_return(fn['return-type'])}"]
    if not fn['parameters']:
        lines[0] += ")"
    else:
        for p in fn['parameters']:
            lines.append(f"\t({to_lisp(p['name'], True)} {gen_lisp_param(p)})")
        lines[-1] += ")"
    export(fn['name'], StorageType.FUNCTION, "\n".join(lines))

def gen_struct_field(f):
    match f['tag']:
        case ":pointer":
            return f"(:pointer {gen_struct_field(f['type'])})"
        case ":_Bool":
            return ":boolean"
        case ":function-pointer":
            return "(:pointer :void)"
        case ":array":
            tag = f['type']['tag']
            return f"(:array {gen_struct_field(f['type']) if tag == ':array' else tag} :count {f['size']})"
        case "uint32_t":
            return ":uint32"
        case "uint64_t":
            return ":uint64"
        case _:
            for v in valid_prefixes:
                if f['tag'].startswith(v):
                    return f"(:struct {to_lisp(f['tag'])})"
            return f['tag']

# Translates c2ffi to Common Lisp struct wrapper
def gen_lisp_struct(s):
    if s['name'].startswith("_"):
        return
    lines = [f"(defcstruct ({to_lisp(s['name'])} :size {s['bit-size']})"]
    for f in s['fields']:
        lines.append( f"\t({to_lisp(f['name'], True)} :offset {f['bit-offset']} :size {f['bit-size']} " + gen_struct_field(f['type']) + ")")
    lines[-1] += ")"
    export(s['name'], StorageType.STRUCT, "\n".join(lines))

# Loop over all the c2ffi data and generate all the Common Lisp bindings
for item in data:
    match item['tag']:
        case "enum":
            if not item['name']:
                unnamed_enum(item)
            else:
                named_enum(item)
        case "struct":
            gen_lisp_struct(item)
        case "function":
            gen_lisp_fun(item)

if args.dump:
    print("Common Lisp Constants:")
    for c in constants:
        print(c)
    print("Common Lisp Enums")
    for e in enums:
        print(e)
    print("Common Lisp Structs")
    for s in structs:
        print(s)
    print("Common Lisp Functions")
    for f in functions:
        print(f)
    sys.exit(0)

header_msg = 'Generated by gen_cl.py -- part of cl-sokol https://github.com/takeiteasy/cl-sokol'
cmit = ["/* " + header_msg]
with open('LICENSE') as fh:
    cmit += ['   ' + line.strip() for line in fh.readlines()]
cmit[-1] += ' */'
cmit.append('\n')

outnames = {"sg": "gfx", "sapp": "app", "saudio": "audio", "slog": "log", "stm": "time", "sfetch": "fetch", "sargs": "args"}

lispmit = [';; ' + header_msg]
with open('LICENSE') as fh:
    lispmit += [';; ' + line.strip() for line in fh.readlines()]
lispmit.append('\n')

def flush(fh, lines):
    fh.writelines(l + '\n' for l in lines)

for k, v in cheader.items():
    fname = outnames[k]
    with open(args.output + f"/sokol_{fname}_cl.h", "w") as fh:
        header = ["#pragma once", f"#include \"sokol_{fname}.h\""]
        flush(fh, cmit)
        flush(fh, header)
        flush(fh, v)
for k, v in csource.items():
    fname = outnames[k]
    with open(args.output + f"/sokol_{fname}_cl.c", "w") as fh:
        header = ["#define SOKOL_IMPL", f"#include \"sokol_{fname}_cl.h\""]
        flush(fh, cmit)
        flush(fh, header)
        flush(fh, v)
with open(args.output + "/package.lisp", "w") as fh:
    flush(fh, lispmit)
    for k, v in modules.items():
        fname = outnames[k]
        header = [f"(defpackage #:cl-sokol-{fname}",
                  f"  (:nicknames :%{k})",
                  "  (:use #:cl #:cffi)",
                  "  (:export"]
        export = ["\t#:" + vv for vv in v.exports]
        export[-1] += "))"
        flush(fh, header)
        flush(fh, export)
        with open(args.output + f"/sokol_{fname}.lisp", "w") as fh2:
            flush(fh2, lispmit)
            loader = [f"(in-package #:cl-sokol-{fname})\n",
                      f"(pushnew (asdf:system-relative-pathname :cl-sokol-{fname} \"build/\") *foreign-library-directories*)",
                      f"(define-foreign-library libsokol-{fname}",
                      f"  (t (:default \"libsokol_{fname}\")))",
                      f"(unless (foreign-library-loaded-p 'libsokol-{fname})",
                      f"  (use-foreign-library libsokol-{fname}))\n"]
            flush(fh2, loader)
            flush(fh2, v.data['CONSTANT'])
            flush(fh2, v.data['ENUM'])
            flush(fh2, v.data['STRUCT'])
            flush(fh2, v.data['FUNCTION'])
