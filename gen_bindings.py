#!/usr/bin/env python3
import sys
import os
import shutil

project_root = os.path.dirname(os.path.abspath(__file__))
bindgen_path = os.path.join(project_root, 'sokol', 'bindgen')
sys.path.insert(0, bindgen_path)

orig_dir = os.getcwd()
os.chdir(bindgen_path)

import gen_lisp

# Tasks to generate bindings for
tasks = [
    [ '../sokol_log.h',     'slog_',     [] ],
    [ '../sokol_gfx.h',     'sg_',       [] ],
    [ '../sokol_app.h',     'sapp_',     [] ],
    [ '../sokol_glue.h',    'sglue_',    ['sg_'] ],
    [ '../sokol_time.h',    'stm_',      [] ],
    [ '../sokol_audio.h',   'saudio_',   [] ],
]

print("Generating Common Lisp bindings for Sokol...")
gen_lisp.prepare()
for task in tasks:
    [c_header_path, main_prefix, dep_prefixes] = task
    gen_lisp.gen(c_header_path, main_prefix, dep_prefixes)

# Copy generated files to project root
print("\nCopying generated files to project root...")
src_lisp = os.path.join(bindgen_path, 'src')
dst_lisp = os.path.join(project_root, 'src')

os.makedirs(dst_lisp, exist_ok=True)
os.makedirs(os.path.join(dst_lisp, 'c'), exist_ok=True)

for filename in os.listdir(src_lisp):
    if filename.endswith('.lisp'):
        src = os.path.join(src_lisp, filename)
        dst = os.path.join(dst_lisp, filename)
        shutil.copy2(src, dst)
        print(f"  Copied {filename}")
src_c = os.path.join(src_lisp, 'c')
dst_c = os.path.join(dst_lisp, 'c')
for filename in os.listdir(src_c):
    if filename.endswith('.c'):
        src = os.path.join(src_c, filename)
        dst = os.path.join(dst_c, filename)
        shutil.copy2(src, dst)
        print(f"  Copied c/{filename}")

os.chdir(orig_dir)

print("\nBindings generated successfully!")
print(f"Output: {dst_lisp}/*.lisp")
print(f"C sources: {dst_c}/*.c")
