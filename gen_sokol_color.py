#!/usr/bin/env python3
#-------------------------------------------------------------------------------
#   Generate Common Lisp bindings for sokol_color.h
#-------------------------------------------------------------------------------
#   Based on sokol/util/gen_sokol_color.py
#   Generates color constants and utility functions for Common Lisp

colors = [
("Alice Blue",          0xF0F8FFFF),
("Antique White",       0xFAEBD7FF),
("Aqua",                0x00FFFFFF),
("Aquamarine",          0x7FFFD4FF),
("Azure",               0xF0FFFFFF),
("Beige",               0xF5F5DCFF),
("Bisque",              0xFFE4C4FF),
("Black",               0x000000FF),
("Blanched Almond",     0xFFEBCDFF),
("Blue",                0x0000FFFF),
("Blue Violet",         0x8A2BE2FF),
("Brown",               0xA52A2AFF),
("Burlywood",           0xDEB887FF),
("Cadet Blue",          0x5F9EA0FF),
("Chartreuse",          0x7FFF00FF),
("Chocolate",           0xD2691EFF),
("Coral",               0xFF7F50FF),
("Cornflower Blue",     0x6495EDFF),
("Cornsilk",            0xFFF8DCFF),
("Crimson",             0xDC143CFF),
("Cyan",                0x00FFFFFF),
("Dark Blue",           0x00008BFF),
("Dark Cyan",           0x008B8BFF),
("Dark Goldenrod",      0xB8860BFF),
("Dark Gray",           0xA9A9A9FF),
("Dark Green",          0x006400FF),
("Dark Khaki",          0xBDB76BFF),
("Dark Magenta",        0x8B008BFF),
("Dark Olive Green",    0x556B2FFF),
("Dark Orange",         0xFF8C00FF),
("Dark Orchid",         0x9932CCFF),
("Dark Red",            0x8B0000FF),
("Dark Salmon",         0xE9967AFF),
("Dark Sea Green",      0x8FBC8FFF),
("Dark Slate Blue",     0x483D8BFF),
("Dark Slate Gray",     0x2F4F4FFF),
("Dark Turquoise",      0x00CED1FF),
("Dark Violet",         0x9400D3FF),
("Deep Pink",           0xFF1493FF),
("Deep Sky Blue",       0x00BFFFFF),
("Dim Gray",            0x696969FF),
("Dodger Blue",         0x1E90FFFF),
("Firebrick",           0xB22222FF),
("Floral White",        0xFFFAF0FF),
("Forest Green",        0x228B22FF),
("Fuchsia",             0xFF00FFFF),
("Gainsboro",           0xDCDCDCFF),
("Ghost White",         0xF8F8FFFF),
("Gold",                0xFFD700FF),
("Goldenrod",           0xDAA520FF),
("Gray",                0xBEBEBEFF),
("Web Gray",            0x808080FF),
("Green",               0x00FF00FF),
("Web Green",           0x008000FF),
("Green Yellow",        0xADFF2FFF),
("Honeydew",            0xF0FFF0FF),
("Hot Pink",            0xFF69B4FF),
("Indian Red",          0xCD5C5CFF),
("Indigo",              0x4B0082FF),
("Ivory",               0xFFFFF0FF),
("Khaki",               0xF0E68CFF),
("Lavender",            0xE6E6FAFF),
("Lavender Blush",      0xFFF0F5FF),
("Lawn Green",          0x7CFC00FF),
("Lemon Chiffon",       0xFFFACDFF),
("Light Blue",          0xADD8E6FF),
("Light Coral",         0xF08080FF),
("Light Cyan",          0xE0FFFFFF),
("Light Goldenrod",     0xFAFAD2FF),
("Light Gray",          0xD3D3D3FF),
("Light Green",         0x90EE90FF),
("Light Pink",          0xFFB6C1FF),
("Light Salmon",        0xFFA07AFF),
("Light Sea Green",     0x20B2AAFF),
("Light Sky Blue",      0x87CEFAFF),
("Light Slate Gray",    0x778899FF),
("Light Steel Blue",    0xB0C4DEFF),
("Light Yellow",        0xFFFFE0FF),
("Lime",                0x00FF00FF),
("Lime Green",          0x32CD32FF),
("Linen",               0xFAF0E6FF),
("Magenta",             0xFF00FFFF),
("Maroon",              0xB03060FF),
("Web Maroon",          0x800000FF),
("Medium Aquamarine",   0x66CDAAFF),
("Medium Blue",         0x0000CDFF),
("Medium Orchid",       0xBA55D3FF),
("Medium Purple",       0x9370DBFF),
("Medium Sea Green",    0x3CB371FF),
("Medium Slate Blue",   0x7B68EEFF),
("Medium Spring Green", 0x00FA9AFF),
("Medium Turquoise",    0x48D1CCFF),
("Medium Violet Red",   0xC71585FF),
("Midnight Blue",       0x191970FF),
("Mint Cream",          0xF5FFFAFF),
("Misty Rose",          0xFFE4E1FF),
("Moccasin",            0xFFE4B5FF),
("Navajo White",        0xFFDEADFF),
("Navy Blue",           0x000080FF),
("Old Lace",            0xFDF5E6FF),
("Olive",               0x808000FF),
("Olive Drab",          0x6B8E23FF),
("Orange",              0xFFA500FF),
("Orange Red",          0xFF4500FF),
("Orchid",              0xDA70D6FF),
("Pale Goldenrod",      0xEEE8AAFF),
("Pale Green",          0x98FB98FF),
("Pale Turquoise",      0xAFEEEEFF),
("Pale Violet Red",     0xDB7093FF),
("Papaya Whip",         0xFFEFD5FF),
("Peach Puff",          0xFFDAB9FF),
("Peru",                0xCD853FFF),
("Pink",                0xFFC0CBFF),
("Plum",                0xDDA0DDFF),
("Powder Blue",         0xB0E0E6FF),
("Purple",              0xA020F0FF),
("Web Purple",          0x800080FF),
("Rebecca Purple",      0x663399FF),
("Red",                 0xFF0000FF),
("Rosy Brown",          0xBC8F8FFF),
("Royal Blue",          0x4169E1FF),
("Saddle Brown",        0x8B4513FF),
("Salmon",              0xFA8072FF),
("Sandy Brown",         0xF4A460FF),
("Sea Green",           0x2E8B57FF),
("Seashell",            0xFFF5EEFF),
("Sienna",              0xA0522DFF),
("Silver",              0xC0C0C0FF),
("Sky Blue",            0x87CEEBFF),
("Slate Blue",          0x6A5ACDFF),
("Slate Gray",          0x708090FF),
("Snow",                0xFFFAFAFF),
("Spring Green",        0x00FF7FFF),
("Steel Blue",          0x4682B4FF),
("Tan",                 0xD2B48CFF),
("Teal",                0x008080FF),
("Thistle",             0xD8BFD8FF),
("Tomato",              0xFF6347FF),
("Transparent",         0x00000000),
("Turquoise",           0x40E0D0FF),
("Violet",              0xEE82EEFF),
("Wheat",               0xF5DEB3FF),
("White",               0xFFFFFFFF),
("White Smoke",         0xF5F5F5FF),
("Yellow",              0xFFFF00FF),
("Yellow Green",        0x9ACD32FF)
]

def unpack_rgba(color):
    red   = (color & 0xFF000000) >> 24
    green = (color & 0xFF0000) >> 16
    blue  = (color & 0xFF00) >> 8
    alpha = (color & 0xFF)
    return (red, green, blue, alpha)

def to_lisp_name(name):
    """Convert color name to Lisp convention (lowercase with dashes)"""
    return name.lower().replace(" ", "-")

def to_constant_name(name):
    """Convert color name to Lisp constant convention (+NAME+)"""
    return "+" + name.upper().replace(" ", "-") + "+"

def generate_lisp_bindings():
    output = open("src/sokol-color.lisp", "w")

    # Collect all symbols for export
    all_symbols = []

    # Add color constants (both float and packed versions)
    for color in colors:
        lisp_name = to_lisp_name(color[0])
        all_symbols.append("+" + lisp_name + "+")
        all_symbols.append("+" + lisp_name + "-rgba32+")

    # Add function names
    all_symbols.extend([
        "make-color-4b",
        "make-color-1i",
        "color-lerp",
        "color-lerp-precise",
        "color-multiply"
    ])

    # Write package definition
    output.write(""";;;; Auto-generated Common Lisp bindings for sokol_color
;;;; DO NOT EDIT - generated by gen_sokol_color.py
;;;;
;;;; This file provides color constants and utility functions based on X11 color names.

(defpackage :sokol-color
  (:use :cl :cffi)
  (:nicknames :sg-color)
  (:export
""")

    for sym in sorted(all_symbols):
        output.write(f"   {sym}\n")

    output.write("""   ))

(in-package :sokol-color)

;;;; Color Constants
;;;;
;;;; This package provides X11 color names as Common Lisp constants.
;;;; Each color is available in two forms:
;;;;
;;;; 1. As a foreign sg_color struct (e.g., +red+)
;;;;    - Use with: (foreign-slot-value +red+ '(:struct sokol-gfx:sg-color) 'sokol-gfx:r)
;;;;    - Or better: use make-color-* functions to create colors at runtime
;;;;
;;;; 2. As a packed RGBA32 integer (e.g., +red-rgba32+)
;;;;    - Use with: (make-color-1i +red-rgba32+)
;;;;    - Useful for passing to functions like sgl:c1i

""")

    # Generate float color constants (as lists that can be used to make sg_color structs)
    output.write(";;;; Float color values (RGBA, 0.0-1.0)\n\n")

    for color in colors:
        rgba = unpack_rgba(color[1])
        lisp_name = to_constant_name(color[0])
        r = rgba[0] / 255.0
        g = rgba[1] / 255.0
        b = rgba[2] / 255.0
        a = rgba[3] / 255.0

        # Format floats nicely
        r_text = "{:.1f}".format(r) if r == int(r) else "{:.9g}".format(r)
        g_text = "{:.1f}".format(g) if g == int(g) else "{:.9g}".format(g)
        b_text = "{:.1f}".format(b) if b == int(b) else "{:.9g}".format(b)
        a_text = "{:.1f}".format(a) if a == int(a) else "{:.9g}".format(a)

        output.write(f";; {color[0]} - R:{rgba[0]} G:{rgba[1]} B:{rgba[2]} A:{rgba[3]}\n")
        output.write(f"(defparameter {lisp_name} '({r_text} {g_text} {b_text} {a_text}))\n\n")

    # Generate packed RGBA32 constants
    output.write("\n;;;; Packed RGBA32 integer values (0xRRGGBBAA)\n\n")

    for color in colors:
        rgba = unpack_rgba(color[1])
        lisp_name = to_constant_name(color[0]) + "-RGBA32"
        hex_color = "#x{0:08X}".format(color[1])

        output.write(f";; {color[0]} - R:{rgba[0]} G:{rgba[1]} B:{rgba[2]} A:{rgba[3]}\n")
        output.write(f"(defconstant {lisp_name} {hex_color})\n\n")

    # Generate utility functions
    output.write("""
;;;; Utility Functions
;;;;
;;;; These functions wrap the C functions from sokol_color.h

(cffi:defcfun ("sg_make_color_4b" make-color-4b) (:struct sokol-gfx:sg-color)
  "Create an sg_color from separate R, G, B, A bytes (0-255)"
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(cffi:defcfun ("sg_make_color_1i" make-color-1i) (:struct sokol-gfx:sg-color)
  "Create an sg_color from a packed RGBA32 integer (0xRRGGBBAA)"
  (rgba :uint32))

(cffi:defcfun ("sg_color_lerp" color-lerp) (:struct sokol-gfx:sg-color)
  "Linearly interpolate between two colors"
  (color-a (:pointer (:struct sokol-gfx:sg-color)))
  (color-b (:pointer (:struct sokol-gfx:sg-color)))
  (amount :float))

(cffi:defcfun ("sg_color_lerp_precise" color-lerp-precise) (:struct sokol-gfx:sg-color)
  "Linearly interpolate between two colors (more precise but slower)"
  (color-a (:pointer (:struct sokol-gfx:sg-color)))
  (color-b (:pointer (:struct sokol-gfx:sg-color)))
  (amount :float))

(cffi:defcfun ("sg_color_multiply" color-multiply) (:struct sokol-gfx:sg-color)
  "Multiply each color component by a scale factor"
  (color (:pointer (:struct sokol-gfx:sg-color)))
  (scale :float))

;;;; Helper macros for common color operations

(defmacro with-color ((var color-list) &body body)
  "Convenience macro to create a temporary sg_color from a list of (r g b a) floats"
  `(cffi:with-foreign-object (,var '(:struct sokol-gfx:sg-color))
     (setf (cffi:foreign-slot-value ,var '(:struct sokol-gfx:sg-color) 'sokol-gfx::r) ,(first color-list))
     (setf (cffi:foreign-slot-value ,var '(:struct sokol-gfx:sg-color) 'sokol-gfx::g) ,(second color-list))
     (setf (cffi:foreign-slot-value ,var '(:struct sokol-gfx:sg-color) 'sokol-gfx::b) ,(third color-list))
     (setf (cffi:foreign-slot-value ,var '(:struct sokol-gfx:sg-color) 'sokol-gfx::a) ,(fourth color-list))
     ,@body))

(defun color-from-list (color-list)
  "Create an sg_color from a list of (r g b a) floats.
   Returns a foreign pointer that must be freed by the caller."
  (let ((color-ptr (cffi:foreign-alloc '(:struct sokol-gfx:sg-color))))
    (setf (cffi:foreign-slot-value color-ptr '(:struct sokol-gfx:sg-color) 'sokol-gfx::r) (first color-list))
    (setf (cffi:foreign-slot-value color-ptr '(:struct sokol-gfx:sg-color) 'sokol-gfx::g) (second color-list))
    (setf (cffi:foreign-slot-value color-ptr '(:struct sokol-gfx:sg-color) 'sokol-gfx::b) (third color-list))
    (setf (cffi:foreign-slot-value color-ptr '(:struct sokol-gfx:sg-color) 'sokol-gfx::a) (fourth color-list))
    color-ptr))
""")

    output.close()
    print("Generated src/sokol-color.lisp")

if __name__ == "__main__":
    generate_lisp_bindings()
