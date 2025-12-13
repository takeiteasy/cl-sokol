# cl-sokol

> **WARNING** This is a work in progress

Unofficial Common Lisp bindings + wrapper for [sokol](https://github.com/floooh/sokol). Bindings are working, however the wrapper is a WIP.

## Build

This will be cleaned up I swear.

```
git submodule update --init
cd sokol
git checkout lisp-bindings
cd ../sokol-tools
git checkout lisp-bindings
git submodule update --init
./fips set <config for your platform> # see sokol-tools/README.md
./fips build # takes a while
cd ..
python3 gen_bindings.py
sbcl --load example-simple-window.lisp
```

## LICENSE

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
