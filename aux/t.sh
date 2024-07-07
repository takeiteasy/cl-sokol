#!/usr/bin/env sh

sh aux/gen_types.sh "src/sokol/sokol_$1.h" | python3 aux/tally.py | jq
